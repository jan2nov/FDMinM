The names of the directories corresponds to each Feature detectors and/or extractors. 

To run the there is a python script where in `filelist1` you need to specify the directory of the input images and in `filelist2` the directory of the ``atlas/comparing'' png files.
Accordingly the line 
> Parallel(n_jobs=2)(delayed(compute)(test_images,ref,3) for ref in ref_images)

change `n_jobs` for changing number of cores used, and the number 3 to an equivalent number of the tested subject. The code produce a csv files for each input png file. The csv files are located in directory with the number of the subject 
