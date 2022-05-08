# -*- coding: utf-8 -*-
"""
Created on Sun Mar 20 22:02:54 2022

@author: Karel
"""

import matplotlib.pyplot as plt
import cv2
import os
import kornia as K
import kornia.feature as KF
import numpy as np
import torch
import pydegensac
from kornia_moons.feature import *
import glob
import pandas as pd
import time
from joblib import Parallel, delayed
import multiprocessing

def get_local_descriptors(img, cv2_sift_kpts, kornia_descriptor):
    if len(cv2_sift_kpts) == 0:
        return np.array([])

    # We will not train anything, so let's save time and memory by no_grad()
    with torch.no_grad():
        kornia_descriptor.eval()
        timg = K.color.rgb_to_grayscale(K.image_to_tensor(img, False).float() / 255.)
        lafs = laf_from_opencv_SIFT_kpts(cv2_sift_kpts)
        patches = KF.extract_patches_from_pyramid(timg, lafs, 32)
        B, N, CH, H, W = patches.size()
        # Descriptor accepts standard tensor [B, CH, H, W], while patches are [B, N, CH, H, W] shape
        # So we need to reshape a bit :)
        descs = kornia_descriptor(patches.view(B * N, CH, H, W)).view(B * N, -1)
    return descs.detach().cpu().numpy()


def get_local_descriptors_aff(img, cv2_sift_kpts, kornia_descriptor):
  if len(cv2_sift_kpts) == 0:
    return np.array([])

  # We will not train anything, so let's save time and memory by no_grad()
  with torch.no_grad():
    kornia_descriptor.eval()
    timg = K.color.rgb_to_grayscale(K.image_to_tensor(img, False).float() / 255.)
    lafs = laf_from_opencv_SIFT_kpts(cv2_sift_kpts)
    affine = KF.LAFAffNetShapeEstimator(True)
    orienter = KF.LAFOrienter(32, angle_detector=KF.OriNet(True))
    orienter.eval()
    affine.eval()
    lafs_new = orienter(affine(lafs,timg),timg)
    patches = KF.extract_patches_from_pyramid(timg, lafs_new, 32)
    B, N, CH, H, W = patches.size()
    # Descriptor accepts standard tensor [B, CH, H, W], while patches are [B, N, CH, H, W] shape
    # So we need to reshape a bit :)
    descs = kornia_descriptor(patches.view(B * N, CH, H, W)).view(B * N, -1)
  return descs.detach().cpu().numpy()


def sift_korniadesc_matching(fname1, fname2, descriptor):
    img1 = cv2.cvtColor(cv2.imread(fname1), cv2.COLOR_BGR2RGB)
    img2 = cv2.cvtColor(cv2.imread(fname2), cv2.COLOR_BGR2RGB)

    sift = cv2.SIFT_create(8000)
    kps1 = sift.detect(img1, None)
    kps2 = sift.detect(img2, None)
    # That is the only change in the pipeline -- descriptors
    descs1 = get_local_descriptors(img1, kps1, descriptor)
    descs2 = get_local_descriptors(img2, kps2, descriptor)
    if ((len(kps1)) == 0 ):
      return(0,len(kps2),0)
    if (len(kps2) <= 7 ):
      return(len(kps1),len(kps2),0)
    # The rest is the same, as for SIFT
    dists, idxs = KF.match_smnn(torch.from_numpy(descs1), torch.from_numpy(descs2), 0.95)
    #print(len(kps1), len(kps2), (idxs.shape[0]))
    if ((idxs.shape[0]) < 8 ):
      return(len(kps1),len(kps2),0)

    tentatives = cv2_matches_from_kornia(dists, idxs)
    src_pts = np.float32([kps1[m.queryIdx].pt for m in tentatives]).reshape(-1, 2)
    dst_pts = np.float32([kps2[m.trainIdx].pt for m in tentatives]).reshape(-1, 2)
    F, inliers_mask = pydegensac.findFundamentalMatrix(src_pts, dst_pts, 0.75, 0.99, 100000)
    #draw_params = dict(matchColor=(255, 255, 0), # draw matches in yellow color
    #                  singlePointColor=None,
    #                  matchesMask=inliers_mask.ravel().tolist(), # draw only inliers
    #                  flags = 2)
    #img_out = cv2.drawMatches(img1, kps1, img2, kps2, tentatives, None, **draw_params)
    #plt.figure()
    #fig, ax = plt.subplots(figsize=(15, 15))
    #ax.imshow(img_out, interpolation='nearest')
    #plt.show()
    nMatches = inliers_mask.sum();
    #print(f'{inliers_mask.sum()} inliers found')
    #print(inliers_mask)
    return (len(kps1),len(kps2),nMatches)

def compute(test_images,r_image,id):
    hardnet = KF.HardNet(True)
    print(r_image)
    start = time.time()
    #for r_image in ref_images:
    df = pd.DataFrame(columns=['ref', 'test', 'ref_id', 'test_id', 'matches', 'nkey_ref', 'nkey_test'])
    for t_image in test_images:
        nkey_ref, nkey_test, nMatchesFound = sift_korniadesc_matching(r_image, t_image, hardnet)
        #print(nkey_test,nkey_ref,nMatchesFound,t_image[-7:-4])
        data = {'ref': os.path.split(r_image)[1],
                'test': os.path.split(t_image)[1],
                'ref_id': r_image[-7:-4],
                'test_id': t_image[-7:-4],
                'matches': nMatchesFound,
                'nkey_ref': nkey_ref,
                'nkey_test': nkey_test
        }
        df = df.append(data,ignore_index=True)
        #print(str(r_image[-7:-4]) + " " + str(t_image[-7:    -4]) + " " + str(nMatchesFound))
    filename = "./" + str(id) + "/hardnet_sub_0" + str(id) + "_ref_none_" + str(r_image[-7:-4]) + "_atlas.csv"
    #filename = "./4/hardnet_sub_04_ref_rseqb_toOrig_" + str(r_image[-7:-4]) + "_test.csv"
    df.to_csv(filename,header=None, index=None, sep=' ', mode='w')
    #print(filename)
    end = time.time()
    #print("Time to compute: " + str(end - start))

#####################################################################################
fname1 = "./images/subj07_anatomical_3T_Glasgow_1mm_Crop_1_defaced_z098.png"
##input image
filelist1 = glob.glob("f:\\work\\OwnCloud\\projects\\ImProCo\\data\\OpenNeuroDataset\\sub-03\\anat\\min-rot-sculp-eq\\*.png")
##testing atlas
filelist2 = glob.glob("f:\\work\\OwnCloud\\projects\\ImProCo\\data\\atlas\\t1_icbm_normal_1mm_pn0_rf0\\sculp-eq\\*.png")
#filelist2 = glob.glob("f:\\work\\OwnCloud\\projects\\ImProCo\\data\\OpenNeuroDataset\\noise-07\\gauss\\30\\*.png")
#filelist2 = glob.glob("f:\\work\\OwnCloud\\projects\\ImProCo\\data\\OpenNeuroDataset\\sub-02\\min-rot\\*.png")
#filelist1 = glob.glob("f:\\work\\OwnCloud\\projects\\ImProCo\\data\\OpenNeuroDataset\\sub-01\\anat\\png\\*.png")

for fichier in filelist1[:]: # filelist[:] makes a copy of filelist.
    if not(fichier.endswith(".png")):
        filelist1.remove(fichier)
ref_images = filelist1

for fichier in filelist2[:]: # filelist[:] makes a copy of filelist.
    if not(fichier.endswith(".png")):
        filelist2.remove(fichier)
test_images = filelist2

num_cores = multiprocessing.cpu_count()
start = time.time()
Parallel(n_jobs=2)(delayed(compute)(test_images,ref,3) for ref in ref_images)
end = time.time()
print("Time to compute all: " + str(end - start))
