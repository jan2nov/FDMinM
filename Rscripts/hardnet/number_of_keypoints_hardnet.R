rm(list = ls())

#load helping functions, like rollmean, accuracy_plot, read_data and libraries
source("../functions_source/functions.R")

keypoints_all <- NULL
#load lists of data; each file corresponds to one input with all other
patient1 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_01_ref_e_001_robustness_identity.csv"
patient2 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_02_ref_e_001_robustness_identity.csv"
patient3 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_03_ref_e_001_robustness_identity.csv"
patient4 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_04_ref_e_001_robustness_identity.csv"
patient5 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_05_ref_e_001_robustness_identity.csv"
patient6 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_06_ref_e_001_robustness_identity.csv"
patient7 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_07_ref_e_001_robustness_identity.csv"
#list.files(path = "../../data/methods_testing/hardnet/nkey/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

data1 <- read_data_one(patient1,1)
data2 <- read_data_one(patient2,2)
data3 <- read_data_one(patient3,3)
data4 <- read_data_one(patient4,4)
data5 <- read_data_one(patient5,5)
data6 <- read_data_one(patient6,6) 
data7 <- read_data_one(patient7,7)

data <- rbind(data1,data2,data3,data4,data5,data6,data7)

n_keypoints <- do.call("data.frame", aggregate(data$nkey_test,list(y=data$test_id),function(x) c(mean=round(mean(x)),sd=round(sd(x)))))
colnames(n_keypoints) <- c("id", "number", "sdeviation")
n_keypoints$method <- "None"

keypoints_all <- rbind(keypoints_all,n_keypoints)

######################################################################
#load lists of data; each file corresponds to one input with all other
patient1 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_01_ref_e_001_robustness_rot.csv"
patient2 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_02_ref_e_001_robustness_rot.csv"
patient3 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_03_ref_e_001_robustness_rot.csv"
patient4 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_04_ref_e_001_robustness_rot.csv"
patient5 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_05_ref_e_001_robustness_rot.csv"
patient6 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_06_ref_e_001_robustness_rot.csv"
patient7 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_07_ref_e_001_robustness_rot.csv"
#list.files(path = "../../data/methods_testing/hardnet/nkey/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

data1 <- read_data_one(patient1,1)
data2 <- read_data_one(patient2,2)
data3 <- read_data_one(patient3,3)
data4 <- read_data_one(patient4,4)
data5 <- read_data_one(patient5,5)
data6 <- read_data_one(patient6,6) 
data7 <- read_data_one(patient7,7)

data <- rbind(data1,data2,data3,data4,data5,data6,data7)

n_keypoints <- do.call("data.frame", aggregate(data$nkey_test,list(y=data$test_id),function(x) c(mean=round(mean(x)),sd=round(sd(x)))))
colnames(n_keypoints) <- c("id", "number", "sdeviation")
n_keypoints$method <- "Rotation 5 °"

keypoints_all <- rbind(keypoints_all,n_keypoints)
##########################################################################
#load lists of data; each file corresponds to one input with all other
patient1 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_01_ref_e_001_robustness_scale.csv"
patient2 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_02_ref_e_001_robustness_scale.csv"
patient3 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_03_ref_e_001_robustness_scale.csv"
patient4 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_04_ref_e_001_robustness_scale.csv"
patient5 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_05_ref_e_001_robustness_scale.csv"
patient6 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_06_ref_e_001_robustness_scale.csv"
patient7 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_07_ref_e_001_robustness_scale.csv"
#list.files(path = "../../data/methods_testing/hardnet/nkey/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

data1 <- read_data_one(patient1,1)
data2 <- read_data_one(patient2,2)
data3 <- read_data_one(patient3,3)
data4 <- read_data_one(patient4,4)
data5 <- read_data_one(patient5,5)
data6 <- read_data_one(patient6,6) 
data7 <- read_data_one(patient7,7)

data <- rbind(data1,data2,data3,data4,data5,data6,data7)

n_keypoints <- do.call("data.frame", aggregate(data$nkey_test,list(y=data$test_id),function(x) c(mean=round(mean(x)),sd=round(sd(x)))))
colnames(n_keypoints) <- c("id", "number", "sdeviation")
n_keypoints$method <- "Upscale 5 %"

keypoints_all <- rbind(keypoints_all,n_keypoints)
##########################################################################
#load lists of data; each file corresponds to one input with all other
patient1 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_01_ref_e_001_robustness_noise.csv"
patient2 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_02_ref_e_001_robustness_noise.csv"
patient3 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_03_ref_e_001_robustness_noise.csv"
patient4 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_04_ref_e_001_robustness_noise.csv"
patient5 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_05_ref_e_001_robustness_noise.csv"
patient6 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_06_ref_e_001_robustness_noise.csv"
patient7 = "../../data/methods_testing/hardnet/nkey/hardnet_sub_07_ref_e_001_robustness_noise.csv"
#list.files(path = "../../data/methods_testing/hardnet/nkey/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

data1 <- read_data_one(patient1,1)
data2 <- read_data_one(patient2,2)
data3 <- read_data_one(patient3,3)
data4 <- read_data_one(patient4,4)
data5 <- read_data_one(patient5,5)
data6 <- read_data_one(patient6,6) 
data7 <- read_data_one(patient7,7)

data <- rbind(data1,data2,data3,data4,data5,data6,data7)

n_keypoints <- do.call("data.frame", aggregate(data$nkey_test,list(y=data$test_id),function(x) c(mean=round(mean(x)),sd=round(sd(x)))))
colnames(n_keypoints) <- c("id", "number", "sdeviation")
n_keypoints$method <- "Gaussian noise 30 STD"

keypoints_all <- rbind(keypoints_all,n_keypoints)

#########################################################################
#plot the n_keypoints per image ID
p1 <- plot_nkeypoints(keypoints_all, "HardNet")
p1
ggsave("./number_keypoints_hardnet.pdf", plot = p1, width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)

#without smoothing
#ggplot(data=keypoints_all, aes(x=id, y=number,ymin=(number-sdeviation),ymax=(number+sdeviation),fill=method, color=method)) + geom_line() + geom_ribbon(alpha=0.3) + geom_smooth()

