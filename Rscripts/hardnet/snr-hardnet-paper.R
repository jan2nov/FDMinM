rm(list = ls())

source("../functions_source/functions.R")

######################################################################################
################### Original #########################################################
#load lists of data; each file corresponds to one input with all other
patient1 = list.files(path = "../../data/methods_testing/hardnet/orig/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/orig/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/hardnet/orig/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/hardnet/orig/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/hardnet/orig/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/hardnet/orig/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/hardnet/orig/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

hardnet <- NULL
hardnet <- read_data_orig(patient1,patient2,patient3,patient4,patient5,patient6,patient7)
means <- NULL

#on higher slices there are NAs
temp <- hardnet %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),mean)
temp2 <- hardnet %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
means <- rbind(means,temp)

print(mean(hardnet[hardnet$input_id==hardnet$test_id,]$snr,na.rm = TRUE))

result <- hardnet %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))
result2 <- means %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))

p <- snr_plot(results2,"HardNet",12.1)
p
ggsave("./snr_HardNet_identity.pdf", plot = p,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)

######################################################################################
################### rotation #########################################################
patient1 = list.files(path = "../../data/methods_testing/hardnet/robustness-rotation/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/robustness-rotation/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/hardnet/robustness-rotation/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/hardnet/robustness-rotation/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/hardnet/robustness-rotation/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/hardnet/robustness-rotation/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/hardnet/robustness-rotation/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

hardnet_rot <- NULL
hardnet_rot <- read_data_orig(patient1,patient2,patient3,patient4,patient5,patient6,patient7)
means <- NULL

#on higher slices there are NAs
temp <- hardnet_rot %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),mean)
temp2 <- hardnet_rot %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
means <- rbind(means,temp)

print(mean(hardnet_rot[hardnet_rot$input_id==hardnet_rot$test_id,]$snr,na.rm = TRUE))

result <- hardnet_rot %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))
result2 <- means %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))

p <- snr_plot(results2,"HardNet -- rotation 5 °",12.1)
p
ggsave("./snr_HardNet_rotation.pdf", plot = p,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)
######################################################################################
################### noise #########################################################
patient1 = list.files(path = "../../data/methods_testing/hardnet/robustness-noise/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/robustness-noise/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/hardnet/robustness-noise/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/hardnet/robustness-noise/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/hardnet/robustness-noise/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/hardnet/robustness-noise/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/hardnet/robustness-noise/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

hardnet_noise <- NULL
hardnet_noise <- read_data_orig(patient1,patient2,patient3,patient4,patient5,patient6,patient7)
means <- NULL

#on higher slices there are NAs
temp <- hardnet_noise %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),mean)
temp2 <- hardnet_noise %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
means <- rbind(means,temp)

print(mean(hardnet_noise[hardnet_noise$input_id==hardnet_noise$test_id,]$snr,na.rm = TRUE))

result <- hardnet_noise %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))
result2 <- means %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))

p <- snr_plot(results2,"HardNet -- Gauss 30 STD",12.1)
p
ggsave("./snr_HardNet_gauss.pdf", plot = p,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)
######################################################################################
################### upscale #########################################################
patient1 = list.files(path = "../../data/methods_testing/hardnet/robustness-scale/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/robustness-scale/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/hardnet/robustness-scale/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/hardnet/robustness-scale/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/hardnet/robustness-scale/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/hardnet/robustness-scale/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/hardnet/robustness-scale/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

hardnet_scale <- NULL
hardnet_scale <- read_data_orig(patient1,patient2,patient3,patient4,patient5,patient6,patient7)
means <- NULL

#on higher slices there are NAs
temp <- hardnet_scale %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),mean)
temp2 <- hardnet_scale %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
means <- rbind(means,temp)

print(mean(hardnet_scale[hardnet_scale$input_id==hardnet_scale$test_id,]$snr,na.rm = TRUE))

result <- hardnet_scale %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))
result2 <- means %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))

p <- snr_plot(results2,"HardNet -- scale +5%",12.1)
p
ggsave("./snr_HardNet_scale.pdf", plot = p,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)