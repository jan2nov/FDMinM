"""
Created on Sun Mar 20 22:02:54 2022

@author: Jan
"""

import cv2 as cv
import multiprocessing
import os
import glob
import pandas as pd
import time
from joblib import Parallel, delayed

def gftt_sift_matching(fname1, fname2):
    img1 = cv.cvtColor(cv.imread(fname1), cv.COLOR_BGR2RGB)
    img2 = cv.cvtColor(cv.imread(fname2), cv.COLOR_BGR2RGB)

    gftt = cv.GFTTDetector_create()
    kps1 = gftt.detect(img1, None)
    kps2 = gftt.detect(img2, None)

    sift = cv.SIFT_create()
    skpt1, descr1 = sift.compute(img1, kps1)
    skpt2, descr2 = sift.compute(img2, kps2)

    if ((len(kps1)) == 0 ):
      return(0,len(kps2),0)
    if (len(kps2) == 0 ):
      return(len(kps1),0,0)

    bf = cv.BFMatcher(cv.NORM_L1)
    matches = bf.knnMatch(descr1, descr2, k=2)

    # Apply ratio test
    good = []
    for m, n in matches:
        if m.distance < 0.75 * n.distance:
            good.append([m])
    return (len(kps1),len(kps2),len(good))

def compute(test_images,r_image,id):
    print(r_image)
    df = pd.DataFrame(columns=['ref', 'test', 'ref_id', 'test_id', 'matches', 'nkey_ref', 'nkey_test'])
    for t_image in test_images:
        #print(t_image)
        nkey_ref, nkey_test, nMatchesFound = gftt_sift_matching(r_image, t_image)
        data = {'ref': os.path.split(r_image)[1],
                'test': os.path.split(t_image)[1],
                'ref_id': r_image[-7:-4],
                'test_id': t_image[-7:-4],
                'matches': nMatchesFound,
                'nkey_ref': nkey_ref,
                'nkey_test': nkey_test
        }
        #print(nkey_ref,nkey_test,nMatchesFound)
        df = df.append(data,ignore_index=True)
    filename = "./" + str(id) + "/gftt-sift_sub_0" + str(id) + "_ref_" + str(r_image[-7:-4]) + "_acc_test.csv"
    df.to_csv(filename,header=None, index=None, sep=' ', mode='w')

# list of the png or other
filelist1 = glob.glob("f:\\work\\OwnCloud\\projects\\ImProCo\\data\\OpenNeuroDataset\\sub-01\\anat\\rot-scale\\*.png")
filelist2 = glob.glob("f:\\work\\OwnCloud\\projects\\ImProCo\\data\\OpenNeuroDataset\\sub-07\\anat\\rot-scale\\*.png")

for fichier in filelist1[:]: # filelist[:] makes a copy of filelist.
    if not(fichier.endswith(".png")):
        filelist1.remove(fichier)
ref_images = filelist1

for fichier in filelist2[:]: # filelist[:] makes a copy of filelist.
    if not(fichier.endswith(".png")):
        filelist2.remove(fichier)
test_images = filelist2

start = time.time()
Parallel(n_jobs=1)(delayed(compute)(test_images,ref,1) for ref in ref_images)
end = time.time()
print("Time to compute all: " + str(end - start))
############################################################################################
