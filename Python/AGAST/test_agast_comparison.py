"""
Created on Sun Mar 24 22:02:54 2022

@author: Jan
"""

import cv2 as cv
import multiprocessing
import os
import glob
import pandas as pd
import time
from joblib import Parallel, delayed

def agast_sift_matching(fname1, fname2):
    img1 = cv.cvtColor(cv.imread(fname1), cv.COLOR_RGB2GRAY)
    img2 = cv.cvtColor(cv.imread(fname2), cv.COLOR_RGB2GRAY)

    agast = cv.AgastFeatureDetector_create()
    kps1 = agast.detect(img1, None)
    kps2 = agast.detect(img2, None)

#    img1_keypoints = cv.drawKeypoints(img1,kps1,None,(0,255,0))
#    img2_keypoints = cv.drawKeypoints(img2, kps2, None, (0, 255, 0))

#    cv.imshow("Testing AGAST img1",img1_keypoints)
#    cv.imshow("Testing AGAST img2", img2_keypoints)
   # cv.waitKey()

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
#        print(m.distance)
        if m.distance < 0.75 * n.distance:
            good.append([m])

#    cv.waitKey()
    return (len(kps1),len(kps2),len(good))

def compute(test_images,r_image,id):
    print(r_image)
    df = pd.DataFrame(columns=['ref', 'test', 'ref_id', 'test_id', 'matches', 'nkey_ref', 'nkey_test'])
    for t_image in test_images:
        #print(t_image)
        nkey_ref, nkey_test, nMatchesFound = agast_sift_matching(r_image, t_image)
        data = {'ref': os.path.split(r_image)[1],
                'test': os.path.split(t_image)[1],
                'ref_id': r_image[-7:-4],
                'test_id': t_image[-7:-4],
                'matches': nMatchesFound,
                'nkey_ref': nkey_ref,
                'nkey_test': nkey_test
        }
#        print(nkey_ref,nkey_test,nMatchesFound)
        df = df.append(data,ignore_index=True)
    filename = "./" + str(id) + "/agast-sift_sub_0" + str(id) + "_ref_" + str(r_image[-7:-4]) + "_acc_test.csv"
    df.to_csv(filename,header=None, index=None, sep=' ', mode='w')

# list of the png or other
filelist1 = glob.glob("f:\\work\\OwnCloud\\projects\\ImProCo\\data\\OpenNeuroDataset\\sub-01\\anat\\ellipse-scale-sculp\\*.png")
filelist2 = glob.glob("f:\\work\\OwnCloud\\projects\\ImProCo\\data\\OpenNeuroDataset\\sub-07\\anat\\ellipse-scale-sculp\\*.png")

for fichier in filelist1[:]: # filelist[:] makes a copy of filelist.
    if not(fichier.endswith(".png")):
        filelist1.remove(fichier)
ref_images = filelist1

for fichier in filelist2[:]: # filelist[:] makes a copy of filelist.
    if not(fichier.endswith(".png")):
        filelist2.remove(fichier)
test_images = filelist2

start = time.time()
Parallel(n_jobs=2)(delayed(compute)(test_images,ref,1) for ref in ref_images)
end = time.time()
print("Time to compute all: " + str(end - start))
