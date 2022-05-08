rm(list = ls())

#load helping functions, like rollmean, accuracy_plot, read_data and libraries
source("../functions_source/functions.R")

keypoints_all <- NULL
#load lists of data; each file corresponds to one input with all other
patient1 = list.files(path = "../../data/methods_testing/gftt/orig/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/gftt/orig/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/gftt/orig/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/gftt/orig/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/gftt/orig/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/gftt/orig/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/gftt/orig/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

data1 <- read_data_one(patient1,1) %>% filter(test_id==input_id)
data2 <- read_data_one(patient2,2) %>% filter(test_id==input_id)
data3 <- read_data_one(patient3,3) %>% filter(test_id==input_id)
data4 <- read_data_one(patient4,4) %>% filter(test_id==input_id)
data5 <- read_data_one(patient5,5) %>% filter(test_id==input_id)
data6 <- read_data_one(patient6,6) %>% filter(test_id==input_id)
data7 <- read_data_one(patient7,7) %>% filter(test_id==input_id)

data <- rbind(data1,data2,data3,data4,data5,data6,data7)

n_keypoints <- do.call("data.frame", aggregate(data$nkey_in,list(y=data$input_id),function(x) c(mean=round(mean(x)),sd=round(sd(x)))))
colnames(n_keypoints) <- c("id", "number", "sdeviation")
n_keypoints$method <- "None"

keypoints_all <- rbind(keypoints_all,n_keypoints)
####################################################################
#load lists of data; each file corresponds to one input with all other
patient1 = list.files(path = "../../data/methods_testing/gftt/robustness-rotation/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/gftt/robustness-rotation/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/gftt/robustness-rotation/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/gftt/robustness-rotation/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/gftt/robustness-rotation/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/gftt/robustness-rotation/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/gftt/robustness-rotation/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

data1 <- read_data_one(patient1,1) %>% filter(test_id==input_id)
data2 <- read_data_one(patient2,2) %>% filter(test_id==input_id)
data3 <- read_data_one(patient3,3) %>% filter(test_id==input_id)
data4 <- read_data_one(patient4,4) %>% filter(test_id==input_id)
data5 <- read_data_one(patient5,5) %>% filter(test_id==input_id)
data6 <- read_data_one(patient6,6) %>% filter(test_id==input_id)
data7 <- read_data_one(patient7,7) %>% filter(test_id==input_id)

data <- rbind(data1,data2,data3,data4,data5,data6,data7)

n_keypoints <- do.call("data.frame", aggregate(data$nkey_in,list(y=data$input_id),function(x) c(mean=round(mean(x)),sd=round(sd(x)))))
colnames(n_keypoints) <- c("id", "number", "sdeviation")
n_keypoints$method <- "Rotation 5 °"

keypoints_all <- rbind(keypoints_all,n_keypoints)
######################################################################
patient1 = list.files(path = "../../data/methods_testing/gftt/robustness-scale/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/gftt/robustness-scale/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/gftt/robustness-scale/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/gftt/robustness-scale/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/gftt/robustness-scale/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/gftt/robustness-scale/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/gftt/robustness-scale/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

data1 <- read_data_one(patient1,1) %>% filter(test_id==input_id)
data2 <- read_data_one(patient2,2) %>% filter(test_id==input_id)
data3 <- read_data_one(patient3,3) %>% filter(test_id==input_id)
data4 <- read_data_one(patient4,4) %>% filter(test_id==input_id)
data5 <- read_data_one(patient5,5) %>% filter(test_id==input_id)
data6 <- read_data_one(patient6,6) %>% filter(test_id==input_id)
data7 <- read_data_one(patient7,7) %>% filter(test_id==input_id)

data <- rbind(data1,data2,data3,data4,data5,data6,data7)

n_keypoints <- do.call("data.frame", aggregate(data$nkey_in,list(y=data$input_id),function(x) c(mean=round(mean(x)),sd=round(sd(x)))))
colnames(n_keypoints) <- c("id", "number", "sdeviation")
n_keypoints$method <- "Upscale 5 %"

keypoints_all <- rbind(keypoints_all,n_keypoints)
######################################################################
patient1 = list.files(path = "../../data/methods_testing/gftt/robustness-noise/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/gftt/robustness-noise/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/gftt/robustness-noise/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/gftt/robustness-noise/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/gftt/robustness-noise/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/gftt/robustness-noise/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/gftt/robustness-noise/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

data1 <- read_data_one(patient1,1) %>% filter(test_id==input_id)
data2 <- read_data_one(patient2,2) %>% filter(test_id==input_id)
data3 <- read_data_one(patient3,3) %>% filter(test_id==input_id)
data4 <- read_data_one(patient4,4) %>% filter(test_id==input_id)
data5 <- read_data_one(patient5,5) %>% filter(test_id==input_id)
data6 <- read_data_one(patient6,6) %>% filter(test_id==input_id)
data7 <- read_data_one(patient7,7) %>% filter(test_id==input_id)

data <- rbind(data1,data2,data3,data4,data5,data6,data7)

n_keypoints <- do.call("data.frame", aggregate(data$nkey_in,list(y=data$input_id),function(x) c(mean=round(mean(x)),sd=round(sd(x)))))
colnames(n_keypoints) <- c("id", "number", "sdeviation")
n_keypoints$method <- "Gaussian noise 30 STD"

keypoints_all <- rbind(keypoints_all,n_keypoints)
######################################################################

#plot the n_keypoints per image ID
p1 <- plot_nkeypoints(keypoints_all, "GFTT")
p1
ggsave("./number_keypoints_gftt.pdf", plot = p1, width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)
