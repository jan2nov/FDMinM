rm(list = ls())
library(ggplot2)
library(dplyr)

patient1 = list.files(path = "../data/methods_testing/orb/orig/1-1", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/orb/orig/2-2", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/orb/orig/3-3", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/orb/orig/4-4", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/orb/orig/5-5", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/orb/orig/6-6", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../data/methods_testing/orb/orig/7-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

orb <- NULL
means <- NULL
for (i in 1:170){
  filename1 = patient1[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  filename7 = patient7[i]
  data1 = read.table(filename1,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  data7 = read.table(filename7,header = FALSE, fill = TRUE, sep = " ")
  colnames(data1) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data7) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  data1$snr <- (data1$n_matches - mean(data1$n_matches))/sd(data1$n_matches)
  data1$type <- "1-1"
  data1$ref_id <- i
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data2$type <- "2-2"
  data2$ref_id <- i
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data3$type <- "3-3"
  data3$ref_id <- i
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data4$type <- "4-4"
  data4$ref_id <- i
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data5$type <- "5-5"
  data5$ref_id <- i
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data6$type <- "6-6"
  data6$ref_id <- i
  data7$snr <- (data7$n_matches - mean(data7$n_matches))/sd(data7$n_matches)
  data7$type <- "7-7"
  data7$ref_id <- i
  orb <- rbind(orb,data1, data2, data3, data4, data5, data6, data7) #rbind(data,data_diff)
}

temp <- orb %>% group_by(ref_id,ref_z) %>% summarise_at(vars("snr"),mean)
temp2 <- orb %>% group_by(ref_id,ref_z) %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
means <- rbind(means,temp)

print(paste("Identity SNR: ",mean(orb[orb$ref_id==orb$ref_z,]$snr)))

result <- orb%>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
result2 <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))

p1 <- ggplot(result2, aes(x=ref_z,
                    y=snr, 
                    color=as.factor(ref_id),
                    group=ref_id, 
                    ymin=(snr-sd), 
                    ymax=(snr+sd),
                    fill=as.factor(ref_id)
                    )
       ) +
  geom_line(lwd=1.1) + 
  geom_point(size=2.5) + 
  labs(y=expression("SNR"[i]), fill="Image ID", color="Image ID", x="Compared image (n)", title = "ORB") +
  theme_bw() + 
  ylim(-1, 12.1) +
  geom_ribbon(alpha=0.4, colour = NA) +
  scale_color_manual(values=c("#ca0020", "#f4a582", "#92c5de","#0571b0")) +
  scale_fill_manual(values=c("#ca0020", "#f4a582", "#92c5de","#0571b0")) +
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.7,0.95),
        legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=20,b=-25), hjust=0.05, face="plain", size=12))
p1
ggsave("./snr_orb_identity.pdf", plot = p1,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)
###########################################################################################################################
##################### rotation ###################
###############################################################
patient1 = list.files(path = "../data/methods_testing/orb/rotation/05/1-1", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/orb/rotation/05/2-2", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/orb/rotation/05/3-3", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/orb/rotation/05/4-4", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/orb/rotation/05/5-5", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/orb/rotation/05/6-6", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../data/methods_testing/orb/rotation/05/7-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

orb <- NULL
means <- NULL
for (i in 1:170){
  filename1 = patient1[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  filename7 = patient7[i]
  data1 = read.table(filename1,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  data7 = read.table(filename7,header = FALSE, fill = TRUE, sep = " ")
  colnames(data1) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data7) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  data1$snr <- (data1$n_matches - mean(data1$n_matches))/sd(data1$n_matches)
  data1$type <- "1-1"
  data1$ref_id <- i
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data2$type <- "2-2"
  data2$ref_id <- i
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data3$type <- "3-3"
  data3$ref_id <- i
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data4$type <- "4-4"
  data4$ref_id <- i
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data5$type <- "5-5"
  data5$ref_id <- i
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data6$type <- "6-6"
  data6$ref_id <- i
  data7$snr <- (data7$n_matches - mean(data7$n_matches))/sd(data7$n_matches)
  data7$type <- "7-7"
  data7$ref_id <- i
  orb <- rbind(orb,data1, data2, data3, data4, data5, data6, data7) #rbind(data,data_diff)
}

temp <- orb %>% group_by(ref_id,ref_z) %>% summarise_at(vars("snr"),mean)
temp2 <- orb %>% group_by(ref_id,ref_z) %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
means <- rbind(means,temp)

print(paste("Rotation SNR: ",mean(orb[orb$ref_id==orb$ref_z,]$snr)))

result <- orb%>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
result2 <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))

p1 <- ggplot(result2, aes(x=ref_z,
                    y=snr, 
                    color=as.factor(ref_id),
                    group=ref_id, 
                    ymin=(snr-sd), 
                    ymax=(snr+sd),
                    fill=as.factor(ref_id)
)
) +
  geom_line(lwd=1.1) + 
  geom_point(size=2.5) + 
  labs(y=expression("SNR"[i]), fill="Image ID", color="Image ID", x="Compared image (n)", title = "ORB -- rotation 5°") +
  theme_bw() + 
  ylim(-1, 12.1) +
  geom_ribbon(alpha=0.4, colour = NA) +
  scale_color_manual(values=c("#ca0020", "#f4a582", "#92c5de","#0571b0")) +
  scale_fill_manual(values=c("#ca0020", "#f4a582", "#92c5de","#0571b0")) +
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.7,0.95),
        legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=20,b=-25), hjust=0.05, face="plain", size=12))
p1
ggsave("./snr_orb_rotation.pdf", plot = p1,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)

###########################################################################################################################
##################### scale ###################
###############################################################
patient1 = list.files(path = "../data/methods_testing/orb/scale/1-1/05/larger/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/orb/scale/2-2/05/larger/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/orb/scale/3-3/05/larger/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/orb/scale/4-4/05/larger/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/orb/scale/5-5/05/larger/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/orb/scale/6-6/05/larger/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../data/methods_testing/orb/scale/7-7/05/larger/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

orb <- NULL
means <- NULL
for (i in 1:170){
  filename1 = patient1[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  filename7 = patient7[i]
  data1 = read.table(filename1,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  data7 = read.table(filename7,header = FALSE, fill = TRUE, sep = " ")
  colnames(data1) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data7) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  data1$snr <- (data1$n_matches - mean(data1$n_matches))/sd(data1$n_matches)
  data1$type <- "1-1"
  data1$ref_id <- i
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data2$type <- "2-2"
  data2$ref_id <- i
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data3$type <- "3-3"
  data3$ref_id <- i
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data4$type <- "4-4"
  data4$ref_id <- i
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data5$type <- "5-5"
  data5$ref_id <- i
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data6$type <- "6-6"
  data6$ref_id <- i
  data7$snr <- (data7$n_matches - mean(data7$n_matches))/sd(data7$n_matches)
  data7$type <- "7-7"
  data7$ref_id <- i
  orb <- rbind(orb,data1, data2, data3, data4, data5, data6, data7) #rbind(data,data_diff)
}

temp <- orb %>% group_by(ref_id,ref_z) %>% summarise_at(vars("snr"),mean)
temp2 <- orb %>% group_by(ref_id,ref_z) %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
means <- rbind(means,temp)

print(paste("Scale SNR: ",mean(orb[orb$ref_id==orb$ref_z,]$snr)))

result <- orb%>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
result2 <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))

p1 <- ggplot(result2, aes(x=ref_z,
                    y=snr, 
                    color=as.factor(ref_id),
                    group=ref_id, 
                    ymin=(snr-sd), 
                    ymax=(snr+sd),
                    fill=as.factor(ref_id)
)
) +
  geom_line(lwd=1.1) + 
  geom_point(size=2.5) + 
  labs(y=expression("SNR"[i]), fill="Image ID", color="Image ID", x="Compared image (n)", title = "ORB -- scale +5%") +
  theme_bw() + 
  ylim(-1, 12.1) +
  geom_ribbon(alpha=0.4, colour = NA) +
  scale_color_manual(values=c("#ca0020", "#f4a582", "#92c5de","#0571b0")) +
  scale_fill_manual(values=c("#ca0020", "#f4a582", "#92c5de","#0571b0")) +
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.7,0.95),
        legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=20,b=-25), hjust=0.05, face="plain", size=12))
p1
ggsave("./snr_orb_scale.pdf", plot = p1,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)

###########################################################################################################################
##################### gauss ###################
###############################################################
patient1 = list.files(path = "../data/methods_testing/orb/noise/gauss/30/1-1", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/orb/noise/gauss/30/2-2", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/orb/noise/gauss/30/3-3", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/orb/noise/gauss/30/4-4", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/orb/noise/gauss/30/5-5", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/orb/noise/gauss/30/6-6", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../data/methods_testing/orb/noise/gauss/30/7-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

orb <- NULL
means <- NULL
for (i in 1:170){
  filename1 = patient1[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  filename7 = patient7[i]
  data1 = read.table(filename1,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  data7 = read.table(filename7,header = FALSE, fill = TRUE, sep = " ")
  colnames(data1) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data7) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  data1$snr <- (data1$n_matches - mean(data1$n_matches))/sd(data1$n_matches)
  data1$type <- "1-1"
  data1$ref_id <- i
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data2$type <- "2-2"
  data2$ref_id <- i
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data3$type <- "3-3"
  data3$ref_id <- i
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data4$type <- "4-4"
  data4$ref_id <- i
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data5$type <- "5-5"
  data5$ref_id <- i
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data6$type <- "6-6"
  data6$ref_id <- i
  data7$snr <- (data7$n_matches - mean(data7$n_matches))/sd(data7$n_matches)
  data7$type <- "7-7"
  data7$ref_id <- i
  orb <- rbind(orb,data1, data2, data3, data4, data5, data6, data7) #rbind(data,data_diff)
}

temp <- orb %>% group_by(ref_id,ref_z) %>% summarise_at(vars("snr"),mean)
temp2 <- orb %>% group_by(ref_id,ref_z) %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
means <- rbind(means,temp)

print(paste("Gauss30 SNR: ",mean(orb[orb$ref_id==orb$ref_z,]$snr)))

result <- orb%>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
result2 <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))

p1 <- ggplot(result2, aes(x=ref_z,
                    y=snr, 
                    color=as.factor(ref_id),
                    group=ref_id, 
                    ymin=(snr-sd), 
                    ymax=(snr+sd),
                    fill=as.factor(ref_id)
)
) +
  geom_line(lwd=1.1) + 
  geom_point(size=2.5) + 
  labs(y=expression("SNR"[i]), fill="Image ID", color="Image ID", x="Compared image (n)", title = "ORB -- Gauss 30 STD") +
  theme_bw() + 
  ylim(-1, 12.1) +
  geom_ribbon(alpha=0.4, colour = NA) +
  scale_color_manual(values=c("#ca0020", "#f4a582", "#92c5de","#0571b0")) +
  scale_fill_manual(values=c("#ca0020", "#f4a582", "#92c5de","#0571b0")) +
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.7,0.95),
        legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=20,b=-25), hjust=0.05, face="plain", size=12))
ggsave("./snr_orb_gauss30.pdf", plot = p1,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)