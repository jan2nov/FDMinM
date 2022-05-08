rm(list = ls())

source("../functions_source/functions.R")

######################################################################################
################### Original #########################################################
#load lists of data; each file corresponds to one input with all other
patient1 = list.files(path = "../../data/methods_testing/gftt/orig/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/gftt/orig/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/gftt/orig/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/gftt/orig/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/gftt/orig/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/gftt/orig/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/gftt/orig/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

gftt <- NULL
gftt <- read_data_orig(patient1,patient2,patient3,patient4,patient5,patient6,patient7)
means <- NULL

#on higher slices there are NAs
temp <- gftt %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),mean)
temp2 <- gftt %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
means <- rbind(means,temp)

print(mean(gftt[gftt$input_id==gftt$test_id,]$snr,na.rm = TRUE))

result <- gftt %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))
result2 <- means %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))

p <- snr_plot(results2,"GFTT",12.1)
p
ggsave("./snr_gftt_identity.pdf", plot = p,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)

######################################################################################
################### rotation #########################################################
patient1 = list.files(path = "../../data/methods_testing/gftt/robustness-rotation/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/gftt/robustness-rotation/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/gftt/robustness-rotation/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/gftt/robustness-rotation/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/gftt/robustness-rotation/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/gftt/robustness-rotation/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/gftt/robustness-rotation/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

gftt_rot <- NULL
gftt_rot <- read_data_orig(patient1,patient2,patient3,patient4,patient5,patient6,patient7)
means <- NULL

#on higher slices there are NAs
temp <- gftt_rot %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),mean)
temp2 <- gftt_rot %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
means <- rbind(means,temp)

print(mean(gftt_rot[gftt_rot$input_id==gftt_rot$test_id,]$snr,na.rm = TRUE))

result <- gftt_rot %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))
result2 <- means %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))

p <- snr_plot(results2,"GFTT -- rotation 5 °",12.1)
p
ggsave("./snr_gftt_rotation.pdf", plot = p,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)
######################################################################################
################### noise #########################################################
patient1 = list.files(path = "../../data/methods_testing/gftt/robustness-noise/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/gftt/robustness-noise/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/gftt/robustness-noise/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/gftt/robustness-noise/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/gftt/robustness-noise/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/gftt/robustness-noise/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/gftt/robustness-noise/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

gftt_noise <- NULL
gftt_noise <- read_data_orig(patient1,patient2,patient3,patient4,patient5,patient6,patient7)
means <- NULL

#on higher slices there are NAs
temp <- gftt_noise %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),mean)
temp2 <- gftt_noise %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
means <- rbind(means,temp)

print(mean(gftt_noise[gftt_noise$input_id==gftt_noise$test_id,]$snr,na.rm = TRUE))

result <- gftt_noise %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))
result2 <- means %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))

p <- snr_plot(results2,"GFTT -- Gauss 30 STD",12.1)
p
ggsave("./snr_gftt_gauss.pdf", plot = p,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)
######################################################################################
################### upscale #########################################################
patient1 = list.files(path = "../../data/methods_testing/gftt/robustness-scale/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/gftt/robustness-scale/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/gftt/robustness-scale/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/gftt/robustness-scale/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/gftt/robustness-scale/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/gftt/robustness-scale/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/gftt/robustness-scale/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

gftt_scale <- NULL
gftt_scale <- read_data_orig(patient1,patient2,patient3,patient4,patient5,patient6,patient7)
means <- NULL

#on higher slices there are NAs
temp <- gftt_scale %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),mean)
temp2 <- gftt_scale %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
means <- rbind(means,temp)

print(mean(gftt_scale[gftt_scale$input_id==gftt_scale$test_id,]$snr,na.rm = TRUE))

result <- gftt_scale %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))
result2 <- means %>% filter( (input_id==25) | (input_id==50) | (input_id==100) | (input_id==150))

p <- snr_plot(results2,"GFTT -- scale +5%",12.1)
p
ggsave("./snr_gftt_scale.pdf", plot = p,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)