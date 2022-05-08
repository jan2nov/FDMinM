rm(list = ls())

#load helping functions, like rollmean, accuracy_plot, read_data and libraries
source("../functions_source/functions.R")

#load parameters for the plots, accuracy, image_start, image_end, npatient
source("../functions_source/params.R")

#load lists of data; each file corresponds to one input with all other
patient1 = list.files(path = "../../data/methods_testing/hardnet/orig/1-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/orig/2-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/hardnet/orig/3-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/hardnet/orig/4-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/hardnet/orig/5-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/hardnet/orig/6-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

#combine all patients/subjects to a dataframe
data_diff <- read_data(patient1,patient2,patient3,patient4,patient5,patient6)

#compute rollmean with roll_mean function(data,number_of_images_per_subject, number_of_subjects)
means <- roll_mean(data_diff,nslices,npatient) 

#compute the slices and regularization to all subjects and images
c(center_metrix,final) %<-% compute_slices(means,nslices,npatient)
center_metrix

# cut the results to the part of interest
best <- final %>% filter(input_id>img_start) %>% filter(input_id<=img_end)
#save for the where_plot graph
hardnet_none <- best

#plot the accuracy plot
p <- accuracy_plot(best,accuracy_n,"HardNet","none", accuracy_n)
p
#not embedding fonts?
ggsave("./accuracy_HardNet_none.pdf", plot = p,width = 6, height = 4, dpi=300,device=cairo_pdf)

#####################################################################################################
### Rotation 5
#load lists of data; each file corresponds to one input with all other
patient1 = list.files(path = "../../data/methods_testing/hardnet/acc-R/1-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/acc-R/2-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/hardnet/acc-R/3-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/hardnet/acc-R/4-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/hardnet/acc-R/5-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/hardnet/acc-R/6-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

#combine all patients/subjects to a dataframe
data_diff <- read_data(patient1,patient2,patient3,patient4,patient5,patient6)

#compute rollmean with roll_mean function(data,number_of_images_per_subject, number_of_subjects)
means <- roll_mean(data_diff,nslices,npatient) 

#compute the slices and regularization to all subjects and images
c(center_metrix,final) %<-% compute_slices(means,nslices,npatient)
center_metrix

# cut the results to the part of interest
best <- final %>% filter(input_id>img_start) %>% filter(input_id<=img_end)
#save for the where_plot graph
hardnet_r <- best


#plot the accuracy plot
p <- accuracy_plot(best,accuracy_n,"HardNet", "r", accuracy_n)
p
ggsave("./accuracy_HardNet_r.pdf", plot = p,width = 6, height = 4, dpi=300,device=cairo_pdf)
#####################################################################################################
### Equalize
#load lists of data; each file corresponds to one input with all other
patient1 = list.files(path = "../../data/methods_testing/hardnet/acc-E/1-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/acc-E/2-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/hardnet/acc-E/3-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/hardnet/acc-E/4-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/hardnet/acc-E/5-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/hardnet/acc-E/6-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

#combine all patients/subjects to a dataframe
data_diff <- read_data(patient1,patient2,patient3,patient4,patient5,patient6)

#compute rollmean with roll_mean function(data,number_of_images_per_subject, number_of_subjects)
means <- roll_mean(data_diff,nslices,npatient) 

#compute the slices and regularization to all subjects and images
c(center_metrix,final) %<-% compute_slices(means,nslices,npatient)
center_metrix

# cut the results to the part of interest
best <- final %>% filter(input_id>img_start) %>% filter(input_id<=img_end)
#save for the where_plot graph
hardnet_e <- best


#plot the accuracy plot
p <- accuracy_plot(best,accuracy_n,"HardNet", "eq", accuracy_n)
p
ggsave("./accuracy_HardNet_eq.pdf", plot = p,width = 6, height = 4, dpi=300,device=cairo_pdf)
#####################################################################################################
### Rotation + Brain
#load lists of data; each file corresponds to one input with all other
patient1 = list.files(path = "../../data/methods_testing/hardnet/acc-R-B/1-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/acc-R-B/2-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/hardnet/acc-R-B/3-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/hardnet/acc-R-B/4-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/hardnet/acc-R-B/5-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/hardnet/acc-R-B/6-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

#combine all patients/subjects to a dataframe
data_diff <- read_data(patient1,patient2,patient3,patient4,patient5,patient6)

#compute rollmean with roll_mean function(data,number_of_images_per_subject, number_of_subjects)
means <- roll_mean(data_diff,nslices,npatient) 

#compute the slices and regularization to all subjects and images
c(center_metrix,final) %<-% compute_slices(means,nslices,npatient)
center_metrix

# cut the results to the part of interest
best <- final %>% filter(input_id>img_start) %>% filter(input_id<=img_end)
#save for the where_plot graph
hardnet_rb <- best

#plot the accuracy plot
p <- accuracy_plot(best,accuracy_n,"HardNet", "r+b", accuracy_n)
p
ggsave("./accuracy_HardNet_rb.pdf", plot = p,width = 6, height = 4, dpi=300,device=cairo_pdf)
#####################################################################################################
### Rotation + Scaling
#load lists of data; each file corresponds to one input with all other
patient1 = list.files(path = "../../data/methods_testing/hardnet/acc-R-S/1-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/acc-R-S/2-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/hardnet/acc-R-S/3-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/hardnet/acc-R-S/4-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/hardnet/acc-R-S/5-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/hardnet/acc-R-S/6-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

#combine all patients/subjects to a dataframe
data_diff <- read_data(patient1,patient2,patient3,patient4,patient5,patient6)

#compute rollmean with roll_mean function(data,number_of_images_per_subject, number_of_subjects)
means <- roll_mean(data_diff,nslices,npatient) 

#compute the slices and regularization to all subjects and images
c(center_metrix,final) %<-% compute_slices(means,nslices,npatient)
center_metrix

# cut the results to the part of interest
best <- final %>% filter(input_id>img_start) %>% filter(input_id<=img_end)
#save for the where_plot graph
hardnet_rs <- best

#plot the accuracy plot
p <- accuracy_plot(best,accuracy_n,"HardNet", "r+s", accuracy_n)
p
ggsave("./accuracy_HardNet_rs.pdf", plot = p,width = 6, height = 4, dpi=300,device=cairo_pdf)
#####################################################################################################
### Rotation + Brain + Scaling 
#load lists of data; each file corresponds to one input with all other
patient1 = list.files(path = "../../data/methods_testing/hardnet/acc-R-B-S/1-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/acc-R-B-S/2-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/hardnet/acc-R-B-S/3-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/hardnet/acc-R-B-S/4-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/hardnet/acc-R-B-S/5-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/hardnet/acc-R-B-S/6-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

#combine all patients/subjects to a dataframe
data_diff <- read_data(patient1,patient2,patient3,patient4,patient5,patient6)

#compute rollmean with roll_mean function(data,number_of_images_per_subject, number_of_subjects)
means <- roll_mean(data_diff,nslices,npatient) 

#compute the slices and regularization to all subjects and images
c(center_metrix,final) %<-% compute_slices(means,nslices,npatient)
center_metrix

# cut the results to the part of interest
best <- final %>% filter(input_id>img_start) %>% filter(input_id<=img_end)
#save for the where_plot graph
hardnet_rbs <- best

#plot the accuracy plot
p <- accuracy_plot(best,accuracy_n,"HardNet", "r+b+s", accuracy_n)
p
ggsave("./accuracy_HardNet_rbs.pdf", plot = p,width = 6, height = 4, dpi=300,device=cairo_pdf)
#####################################################################################################
### Rotation + Equalize + Brain + Scaling
#load lists of data; each file corresponds to one input with all other
patient1 = list.files(path = "../../data/methods_testing/hardnet/acc-E-B-S-R/1-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/acc-E-B-S-R/2-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/hardnet/acc-E-B-S-R/3-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/hardnet/acc-E-B-S-R/4-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/hardnet/acc-E-B-S-R/5-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/hardnet/acc-E-B-S-R/6-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

#combine all patients/subjects to a dataframe
data_diff <- read_data(patient1,patient2,patient3,patient4,patient5,patient6)

#compute rollmean with roll_mean function(data,number_of_images_per_subject, number_of_subjects)
means <- roll_mean(data_diff,nslices,npatient) 

#compute the slices and regularization to all subjects and images
c(center_metrix,final) %<-% compute_slices(means,nslices,npatient)
center_metrix

# cut the results to the part of interest
best <- final %>% filter(input_id>img_start) %>% filter(input_id<=img_end)
#save for the where_plot graph
hardnet_rebs <- best

#plot the accuracy plot
p <- accuracy_plot(best,accuracy_n,"HardNet", "r+e+b+s", accuracy_n)
p
ggsave("./accuracy_HardNet_rebs.pdf", plot = p,width = 6, height = 4, dpi=300,device=cairo_pdf)
#####################################################################################################
# locations of the true and false images

name <- "HardNet"
w_none <- where_id_plot(hardnet_none, name, "None", 6, 0, 0)
w_r <- where_id_plot(hardnet_r, name, "r", 6, 1, 0)
w_e <- where_id_plot(hardnet_e, name, "e", 6, 1, 0)
w_rb <- where_id_plot(hardnet_rb, name, "r+b", 6, 1, 0)
w_rs <- where_id_plot(hardnet_r, name, "r+s", 6, 1, 0)
w_rbs <- where_id_plot(hardnet_rbs, name, "r+b+s", 6, 1, 0)
w_rebs <- where_id_plot(hardnet_rebs, name, "r+e+b+s", 6, 1, 1)
where <- ggarrange(w_none,w_r,w_rb,w_rs,w_rbs,w_e,w_rebs,nrow = 7)
where

ggsave("./hardnet_where_06.pdf", plot = where, width = 6, height = 4, dpi=300, device=cairo_pdf)
