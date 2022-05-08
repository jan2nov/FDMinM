rm(list = ls())

source("../functions_source/functions.R")

######################################################################################
################### Original #########################################################
#load lists of data; each file corresponds to one input with all other
patient1 = list.files(path = "../../data/methods_testing/agast/orig/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/agast/orig/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/agast/orig/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/agast/orig/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/agast/orig/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/agast/orig/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/agast/orig/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

agast <- NULL
agast <- read_data_orig(patient1,patient2,patient3,patient4,patient5,patient6,patient7)
means <- NULL

#on higher slices there are NAs
temp <- agast %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),mean)
temp2 <- agast %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
means <- rbind(means,temp)

print(mean(agast[agast$input_id==agast$test_id,]$snr,na.rm = TRUE))

result <- agast %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))
result2 <- means %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))

p <- snr_plot(results2,"AGAST",12.1)
p
ggsave("./snr_agast_identity.pdf", plot = p,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)

######################################################################################
################### rotation #########################################################
patient1 = list.files(path = "../../data/methods_testing/agast/robustness-rotation/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/agast/robustness-rotation/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/agast/robustness-rotation/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/agast/robustness-rotation/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/agast/robustness-rotation/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/agast/robustness-rotation/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/agast/robustness-rotation/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

agast_rot <- NULL
agast_rot <- read_data_orig(patient1,patient2,patient3,patient4,patient5,patient6,patient7)
means <- NULL

#on higher slices there are NAs
temp <- agast_rot %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),mean)
temp2 <- agast_rot %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
means <- rbind(means,temp)

print(mean(agast_rot[agast_rot$input_id==agast_rot$test_id,]$snr,na.rm = TRUE))

result <- agast_rot %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))
result2 <- means %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))

p <- snr_plot(results2,"AGAST -- rotation 5 °",12.1)
p
ggsave("./snr_agast_rotation.pdf", plot = p,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)
######################################################################################
################### noise #########################################################
patient1 = list.files(path = "../../data/methods_testing/agast/robustness-noise/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/agast/robustness-noise/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/agast/robustness-noise/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/agast/robustness-noise/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/agast/robustness-noise/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/agast/robustness-noise/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/agast/robustness-noise/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

agast_noise <- NULL
agast_noise <- read_data_orig(patient1,patient2,patient3,patient4,patient5,patient6,patient7)
means <- NULL

#on higher slices there are NAs
temp <- agast_noise %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),mean)
temp2 <- agast_noise %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
means <- rbind(means,temp)

print(mean(agast_noise[agast_noise$input_id==agast_noise$test_id,]$snr,na.rm = TRUE))

result <- agast_noise %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))
result2 <- means %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))

p <- snr_plot(results2,"AGAST -- Gauss 30 STD",12.1)
p
ggsave("./snr_agast_gauss.pdf", plot = p,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)
######################################################################################
################### upscale #########################################################
patient1 = list.files(path = "../../data/methods_testing/agast/robustness-scale/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/agast/robustness-scale/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/agast/robustness-scale/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/agast/robustness-scale/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/agast/robustness-scale/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/agast/robustness-scale/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/agast/robustness-scale/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

agast_scale <- NULL
agast_scale <- read_data_orig(patient1,patient2,patient3,patient4,patient5,patient6,patient7)
means <- NULL

#on higher slices there are NAs
temp <- agast_scale %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),mean)
temp2 <- agast_scale %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
means <- rbind(means,temp)

print(mean(agast_scale[agast_scale$input_id==agast_scale$test_id,]$snr,na.rm = TRUE))

result <- agast_scale %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))
result2 <- means %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))

p <- snr_plot(results2,"AGAST -- scale +5%",12.1)
p
ggsave("./snr_agast_scale.pdf", plot = p,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)