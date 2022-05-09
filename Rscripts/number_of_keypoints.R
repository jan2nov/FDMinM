rm(list = ls())
library(ggplot2)
library(dplyr)
library(png)
library(imager)

akaze <- read.table(file = "./orig-akaze_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
akaze$method <- "None"

orb <- read.table(file = "./orig-orb_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
orb$method <- "None"

brisk <- read.table(file = "./orig-brisk_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
brisk$method <- "None"

data <- NULL
data <- rbind(data,akaze)
data <- rbind(data,orb)
data <- rbind(data,brisk)


ggplot(data = data, aes(x = id, y=number, fill=method,  ymin=(number-sdeviation), ymax = (number+sdeviation) )) + 
  geom_line(lwd=1.0) +
  labs(y = "Average number of keypoints", x = "Image ID", fill= "Method", title = "Average number of keypoints found") +
  geom_ribbon(alpha=0.3)


#img_25 <- (readPNG(source = "AKAZE-test.png"))
#img_25 <- load.image("../data/OpenNeuroDataset/sub-07/anat/png/subj07_anatomical_3T_Glasgow_1mm_Crop_1_defaced_z025.png")
#img_50 <- load.image("../data/OpenNeuroDataset/sub-07/anat/png/subj07_anatomical_3T_Glasgow_1mm_Crop_1_defaced_z050.png")
#img_100 <- load.image("../data/OpenNeuroDataset/sub-07/anat/png/subj07_anatomical_3T_Glasgow_1mm_Crop_1_defaced_z100.png")
#img_150 <- load.image("../data/OpenNeuroDataset/sub-07/anat/png/subj07_anatomical_3T_Glasgow_1mm_Crop_1_defaced_z150.png")
#gr_img_25 <- grayscale(img_25)
#hist(img_25)
#hist(img_50)
#hist(img_100)
#hist(img_150)

#--------------------------- AKAZE ------------------------------
akaze_g10 <- read.table(file = "./gauss-10-akaze_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_g10$method <- "Gaussian noise 10 STD"

akaze_g20 <- read.table(file = "./gauss-20-akaze_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_g20$method <- "Gauss 20 STD"
akaze_g30 <- read.table(file = "./gauss-30-akaze_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_g30$method <- "Gaussian noise 30 STD"
akaze_s05 <- read.table(file = "./scale-05-akaze_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_s05$method <- "Upscale 5 %"
akaze_r05 <- read.table(file = "./rotation-05-akaze_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_r05$method <- "Rotation 5°"
akaze_all_max <- read.table(file = "./all-max-akaze_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_all_max$method <- "AKAZE -- ALL"

#akaze$diff <- 0
#akaze_g10$diff <- akaze_g10$number - akaze$number
#akaze_g20$diff <- akaze_g20$number - akaze$number
#akaze_g30$diff <- akaze_g30$number - akaze$number

data <- NULL
data <- rbind(data,akaze)
#data <- rbind(data,akaze_g10)
#data <- rbind(data,akaze_g20)
data <- rbind(data,akaze_g30)
data <- rbind(data,akaze_s05)
data <- rbind(data,akaze_r05)
#data <- rbind(data,akaze_all_max)
ggplot(data = data, aes(x = id, y=number, fill=method, color=method,  ymin=(number-sdeviation), ymax = (number+sdeviation) )) + 
  geom_smooth(method="loess", alpha=0.3) + 
  #geom_point(aes(color=method)) +
  #geom_bar() +
  #geom_point(aes(color=method)) +
  labs(y = "Average number of keypoints", x = "image ID", title = "AKAZE", color="Image degradation", color="Image degradation", fill="Image degradation") +
  #ylim(NA,400)
  #coord_cartesian(ylim = NULL)
  # geom_ribbon(alpha=0.3)
  theme_bw() + 
  guides(fill=guide_legend(ncol=2), color=guide_legend(ncol=2)) +
  scale_color_manual(values=c("#e6550d", "#54278f", "#3182bd","#31a354")) +
  scale_fill_manual(values=c("#e6550d", "#54278f", "#3182bd","#31a354")) +
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.4,0.12),
        #legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=20,b=-20), hjust=0.02, face="plain", size=12))

#ggsave(filename = "images/average_number_of_keypoints_AKAZE.png",device = "png")



#--------------------------- ORB ------------------------------
orb_g10 <- read.table(file = "./gauss-10-orb_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
orb_g10$method <- "Gaussian noise 10 STD"
orb_g20 <- read.table(file = "./gauss-20-orb_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
orb_g20$method <- "ORB -- Gauss 20 STD"
orb_g30 <- read.table(file = "./gauss-30-orb_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
orb_g30$method <- "Gaussian noise 30 STD"
orb_s05 <- read.table(file = "./scale-05-orb_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
orb_s05$method <- "Upscale 5 %"
orb_r05 <- read.table(file = "./rotation-05-orb_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
orb_r05$method <- "Rotation 5°"
orb_all_max <- read.table(file = "./all-max-orb_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
orb_all_max$method <- "ORB -- ALL"

#akaze$diff <- 0
#akaze_g10$diff <- akaze_g10$number - akaze$number
#akaze_g20$diff <- akaze_g20$number - akaze$number
#akaze_g30$diff <- akaze_g30$number - akaze$number

data <- NULL
data <- rbind(data,orb)
#data <- rbind(data,orb_g10)
#data <- rbind(data,orb_g20)
data <- rbind(data,orb_g30)
data <- rbind(data,orb_s05)
data <- rbind(data,orb_r05)
#data <- rbind(data,orb_all_max)
ggplot(data = data, aes(x = id, y=number, fill=method, color=method,  ymin=(number-sdeviation), ymax = (number+sdeviation) )) + 
  geom_smooth(method="loess", alpha=0.3) + 
  #geom_point(aes(color=method)) +
  #geom_bar() +
  #geom_point(aes(color=method)) +
  labs(y = "Average number of keypoints", x = "image ID", title = "ORB", color="Image degradation", color="Image degradation", fill="Image degradation") +
  #ylim(NA,400)
  #coord_cartesian(ylim = NULL)
  # geom_ribbon(alpha=0.3)
  theme_bw() +
  coord_cartesian(ylim = c(250, 500)) + 
  guides(fill=guide_legend(ncol=2), color=guide_legend(ncol=2)) +
  scale_color_manual(values=c("#e6550d", "#54278f", "#3182bd","#31a354")) +
  scale_fill_manual(values=c("#e6550d", "#54278f", "#3182bd","#31a354")) +
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.3,0.2),
        #legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=25,b=-25), hjust=0.05, face="plain", size=12))
ggsave(filename = "images/average_number_of_keypoints_ORB-2.png",device = "png")


#--------------------------- BRISK ------------------------------
brisk_g10 <- read.table(file = "./gauss-10-brisk_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
brisk_g10$method <- "BRISK -- Gauss 10 STD"
brisk_g20 <- read.table(file = "./gauss-20-brisk_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
brisk_g20$method <- "BRISK -- Gauss 20 STD"
brisk_g30 <- read.table(file = "./gauss-30-brisk_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
brisk_g30$method <- "Gaussian noise 30 STD"
brisk_s05 <- read.table(file = "./scale-05-brisk_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
brisk_s05$method <- "Upscale 5 %"
brisk_r05 <- read.table(file = "./rotation-05-brisk_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
brisk_r05$method <- "Rotation 5�"
brisk_all_max <- read.table(file = "./all-max-brisk_n_keypoints.dat",header = TRUE, fill = TRUE, sep = " ")
brisk_all_max$method <- "BRISK -- ALL"

#akaze$diff <- 0
#akaze_g10$diff <- akaze_g10$number - akaze$number
#akaze_g20$diff <- akaze_g20$number - akaze$number
#akaze_g30$diff <- akaze_g30$number - akaze$number

data <- NULL
data <- rbind(data,brisk)
#data <- rbind(data,brisk_g10)
#data <- rbind(data,brisk_g20)
data <- rbind(data,brisk_g30)
data <- rbind(data,brisk_s05)
data <- rbind(data,brisk_r05)
#data <- rbind(data,brisk_all_max)
ggplot(data = data, aes(x = id, y=number, fill=method, color=method,  ymin=(number-sdeviation), ymax = (number+sdeviation) )) + 
  geom_smooth(method="loess", alpha=0.3) + 
  #geom_point(aes(color=method)) +
  #geom_bar() +
  #geom_point(aes(color=method)) +
  labs(y = "Average number of keypoints", x = "image ID", title = "BRISK", color="Image degradation", color="Image degradation", fill="Image degradation") +
  #ylim(NA,400)
  #coord_cartesian(ylim = NULL)
  # geom_ribbon(alpha=0.3)
  theme_bw() + 
  guides(fill=guide_legend(ncol=2), color=guide_legend(ncol=2)) +
  scale_color_manual(values=c("#e6550d", "#54278f", "#3182bd","#31a354")) +
  scale_fill_manual(values=c("#e6550d", "#54278f", "#3182bd","#31a354")) +
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.4,0.12),
        #legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=20,b=-20), hjust=0.02, face="plain", size=12))
#ggsave(filename = "images/average_number_of_keypoints_BRISK.png",device = "png")

