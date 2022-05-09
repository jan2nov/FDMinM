rm(list = ls())
library(ggplot2)
library(dplyr)

#list_data = list.files(path = "../source/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

patient = list.files(path = "../data/methods_testing/akaze/orig/7-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/akaze/orig/1-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/akaze/orig/2-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/akaze/orig/3-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/akaze/orig/4-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/akaze/orig/5-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../data/methods_testing/akaze/orig/6-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

akaze <- NULL
means <- NULL
for (i in 1:170){
  filename = patient[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  filename7 = patient7[i]
  data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  data7 = read.table(filename7,header = FALSE, fill = TRUE, sep = " ")
  colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data7) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  
  data$snr <- (data$n_matches - mean(data$n_matches))/sd(data$n_matches)
  data$type <- "7-7"
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data2$type <- "1-7"
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data3$type <- "2-7"
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data4$type <- "3-7"
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data5$type <- "4-7"
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data6$type <- "5-7"
  data7$snr <- (data7$n_matches - mean(data7$n_matches))/sd(data7$n_matches)
  data7$type <- "6-7"
  data_diff <- rbind(data2,data3,data4,data5,data6,data7)
  means <- rbind(means,data.frame(ref_z=seq(1,170),snr=with(data_diff,tapply(snr, ref_z, mean)),type="x-7",ref_id=i))
  data$ref_id <- i
  akaze <- rbind(akaze,data) #rbind(data,data_diff)
}

write.table(akaze, "akaze-snr-orig-7-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(means, "akaze-snr-orig-x-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)


result <- akaze%>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
means_filter <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
ggplot(result, aes(x=ref_z,y=snr, color=as.factor(ref_id),group=ref_id, lty=type)) +
  geom_line(size=1.2) + 
  geom_point(size=3.2) + 
  #  scale_linetype_manual(values = c(50 = "solid", east = "dashed")) +
  geom_line(data=means_filter,size=1.2) +
  labs(title = "AKAZE - orig") +
  coord_cartesian(ylim = c(-1,10))

ggplot(result, aes(x=ref_z,y=snr, color=as.factor(ref_id),group=ref_id, lty=type)) +
  geom_line(size=1.2) + 
  geom_point(size=3.2) + 
  labs(y=expression("SNR"[i])) +
  theme_bw() + 
  theme(text=element_text(family="Times New Roman"))

#########
patient =  list.files(path = "../data/methods_testing/akaze/rotation/05/7-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/akaze/rotation/05/1-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/akaze/rotation/05/2-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/akaze/rotation/05/3-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/akaze/rotation/05/4-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/akaze/rotation/05/5-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../data/methods_testing/akaze/rotation/05/6-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

akaze_rot5 <- NULL
means <- NULL
for (i in 1:170){
  filename = patient[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  filename7 = patient7[i]
  data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  data7 = read.table(filename7,header = FALSE, fill = TRUE, sep = " ")
  colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data7) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  
  data$snr <- (data$n_matches - mean(data$n_matches))/sd(data$n_matches)
  data$type <- "7-7"
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data2$type <- "1-7"
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data3$type <- "2-7"
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data4$type <- "3-7"
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data5$type <- "4-7"
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data6$type <- "5-7"
  data7$snr <- (data7$n_matches - mean(data7$n_matches))/sd(data7$n_matches)
  data7$type <- "6-7"
  data_diff <- rbind(data2,data3,data4,data5,data6,data7)
  means <- rbind(means,data.frame(ref_z=seq(1,170),snr=with(data_diff,tapply(snr, ref_z, mean)),type="x-7",ref_id=i))
  data$ref_id <- i
  akaze_rot5 <- rbind(akaze_rot5,data) #rbind(data,data_diff)
}
write.table(akaze_rot5, "akaze-snr-rot05-7-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(means, "akaze-snr-rot05-x-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)
#akaze_rot5$robustness <-akaze_rot5$snr/akaze$snr 

result <- akaze_rot5 %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
result2 <- akaze %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
means_filter <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
ggplot(result, aes(x=ref_z,y=snr, color=as.factor(ref_id),group=ref_id, lty=type)) +
  geom_line(size=1.2) + 
  geom_point(size=3.2) + 
  #  scale_linetype_manual(values = c(50 = "solid", east = "dashed")) +
  geom_line(data = means_filter,size=1.2) +
  labs(title = "AKAZE - ROTATION") +
  coord_cartesian(ylim = c(-1,10))

result$type <- "rotation"
result2$type <- "orig"
result$diff <- result$snr - result2$snr
plot(result$diff)
result <- rbind(result,result2)
ggplot(data=result,aes(x=ref_z, y=snr, color=type)) + 
  geom_line()+
  geom_point()


#####################
#########
patient =  list.files(path = "../data/methods_testing/akaze/noise/gauss/30/7-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/akaze/noise/gauss/30/1-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/akaze/noise/gauss/30/2-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/akaze/noise/gauss/30/3-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/akaze/noise/gauss/30/4-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/akaze/noise/gauss/30/5-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../data/methods_testing/akaze/noise/gauss/30/6-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

akaze_gauss30 <- NULL
means <- NULL
for (i in 1:170){
  filename = patient[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  filename7 = patient7[i]
  data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  data7 = read.table(filename7,header = FALSE, fill = TRUE, sep = " ")
  colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data7) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  
  data$snr <- (data$n_matches - mean(data$n_matches))/sd(data$n_matches)
  data$type <- "7-7"
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data2$type <- "1-7"
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data3$type <- "2-7"
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data4$type <- "3-7"
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data5$type <- "4-7"
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data6$type <- "5-7"
  data7$snr <- (data7$n_matches - mean(data7$n_matches))/sd(data7$n_matches)
  data7$type <- "6-7"
  data_diff <- rbind(data2,data3,data4,data5,data6,data7)
  means <- rbind(means,data.frame(ref_z=seq(1,170),snr=with(data_diff,tapply(snr, ref_z, mean)),type="x-7",ref_id=i))
  data$ref_id <- i
  akaze_gauss30 <- rbind(akaze_gauss30,data) #rbind(data,data_diff)
}
write.table(akaze_gauss30, "akaze-snr-gauss30-7-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(means, "akaze-snr-gauss30-x-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)

result <- akaze_gauss30 %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
result2 <- akaze %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
means_filter <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
ggplot(result, aes(x=ref_z,y=snr, color=as.factor(ref_id),group=ref_id, lty=type)) +
  geom_line(size=1.2) + 
  geom_point(size=3.2) + 
  #  scale_linetype_manual(values = c(50 = "solid", east = "dashed")) +
  geom_line(data = means_filter,size=1.2) +
  labs(title = "AKAZE - gauss 30 STD") +
  coord_cartesian(ylim = c(-1,10))
###########################################
#########
patient =  list.files(path = "../data/methods_testing/akaze/scale/7-7/05/larger", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/akaze/scale/1-7/05/larger", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/akaze/scale/2-7/05/larger", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/akaze/scale/3-7/05/larger", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/akaze/scale/4-7/05/larger", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/akaze/scale/5-7/05/larger", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../data/methods_testing/akaze/scale/6-7/05/larger", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

akaze_scale05 <- NULL
means <- NULL
for (i in 1:170){
  filename = patient[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  filename7 = patient7[i]
  data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  data7 = read.table(filename7,header = FALSE, fill = TRUE, sep = " ")
  colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data7) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  
  data$snr <- (data$n_matches - mean(data$n_matches))/sd(data$n_matches)
  data$type <- "7-7"
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data2$type <- "1-7"
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data3$type <- "2-7"
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data4$type <- "3-7"
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data5$type <- "4-7"
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data6$type <- "5-7"
  data7$snr <- (data7$n_matches - mean(data7$n_matches))/sd(data7$n_matches)
  data7$type <- "6-7"
  data_diff <- rbind(data2,data3,data4,data5,data6,data7)
  means <- rbind(means,data.frame(ref_z=seq(1,170),snr=with(data_diff,tapply(snr, ref_z, mean)),type="x-7",ref_id=i))
  data$ref_id <- i
  akaze_scale05 <- rbind(akaze_scale05,data) #rbind(data,data_diff)
}
write.table(akaze_scale05, "akaze-snr-scale05-7-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(means, "akaze-snr-scale05-x-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)

result <- akaze_scale05 %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
result2 <- akaze %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
means_filter <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
ggplot(result, aes(x=ref_z,y=snr, color=as.factor(ref_id),group=ref_id, lty=type)) +
  geom_line(size=1.2) + 
  geom_point(size=3.2) + 
  #  scale_linetype_manual(values = c(50 = "solid", east = "dashed")) +
  geom_line(data = means_filter,size=1.2) +
  labs(title = "AKAZE - scale 5%") +
  coord_cartesian(ylim = c(-1,10))
####################################
patient =  list.files(path = "../data/methods_testing/akaze/all-max/7-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/akaze/all-max/1-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/akaze/all-max/2-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/akaze/all-max/3-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/akaze/all-max/4-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/akaze/all-max/5-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../data/methods_testing/akaze/all-max/6-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

akaze_allmax <- NULL
means <- NULL
for (i in 1:170){
  filename = patient[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  filename7 = patient7[i]
  data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  data7 = read.table(filename7,header = FALSE, fill = TRUE, sep = " ")
  colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data7) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  
  data$snr <- (data$n_matches - mean(data$n_matches))/sd(data$n_matches)
  data$type <- "7-7"
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data2$type <- "1-7"
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data3$type <- "2-7"
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data4$type <- "3-7"
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data5$type <- "4-7"
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data6$type <- "5-7"
  data7$snr <- (data7$n_matches - mean(data7$n_matches))/sd(data7$n_matches)
  data7$type <- "6-7"
  data_diff <- rbind(data2,data3,data4,data5,data6,data7)
  means <- rbind(means,data.frame(ref_z=seq(1,170),snr=with(data_diff,tapply(snr, ref_z, mean)),type="x-7",ref_id=i))
  data$ref_id <- i
  akaze_allmax <- rbind(akaze_allmax,data) #rbind(data,data_diff)
}
write.table(akaze_allmax, "akaze-snr-allmax-7-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(means, "akaze-snr-allmax-x-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)

result <- akaze_allmax %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
result2 <- akaze %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
means_filter <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
ggplot(result, aes(x=ref_z,y=snr, color=as.factor(ref_id),group=ref_id, lty=type)) +
  geom_line(size=1.2) + 
  geom_point(size=3.2) + 
  #  scale_linetype_manual(values = c(50 = "solid", east = "dashed")) +
  geom_line(data = means_filter,size=1.2) +
  labs(title = "AKAZE - all") +
  coord_cartesian(ylim = c(-1,10))
##########################################
#rm(list = ls())
library(ggplot2)
library(dplyr)

#list_data = list.files(path = "../source/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

patient = list.files(path = "../data/methods_testing/orb/orig/7-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/orb/orig/1-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/orb/orig/2-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/orb/orig/3-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/orb/orig/4-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/orb/orig/5-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../data/methods_testing/orb/orig/6-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

orb <- NULL
means <- NULL
for (i in 1:170){
  filename = patient[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  filename7 = patient7[i]
  data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  data7 = read.table(filename7,header = FALSE, fill = TRUE, sep = " ")
  colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data7) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  
  data$snr <- (data$n_matches - mean(data$n_matches))/sd(data$n_matches)
  data$type <- "7-7"
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data2$type <- "1-7"
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data3$type <- "2-7"
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data4$type <- "3-7"
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data5$type <- "4-7"
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data6$type <- "5-7"
  data7$snr <- (data7$n_matches - mean(data7$n_matches))/sd(data7$n_matches)
  data7$type <- "6-7"
  
  data_diff <- rbind(data2,data3,data4,data5,data6,data7)
  means <- rbind(means,data.frame(ref_z=seq(1,170),snr=with(data_diff,tapply(snr, ref_z, mean)),type="x-7",ref_id=i))
  data$ref_id <- i
  orb <- rbind(orb,data) #rbind(data,data_diff)
}

result <- orb %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
means_filter <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
ggplot(result, aes(x=ref_z,y=snr, color=as.factor(ref_id),group=ref_id, lty=type)) +
  geom_line(size=1.2) + 
  geom_point(size=3.2) + 
  #  scale_linetype_manual(values = c(50 = "solid", east = "dashed")) +
  geom_line(data = means_filter,size=1.2) +
  labs(title = "ORB") +
  coord_cartesian(ylim = c(-1,10))

###########################################
#rm(list = ls())
library(ggplot2)
library(dplyr)

#list_data = list.files(path = "../source/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

patient = list.files(path =  "../data/methods_testing/brisk/orig/7-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/brisk/orig/1-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/brisk/orig/2-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/brisk/orig/3-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/brisk/orig/4-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/brisk/orig/5-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../data/methods_testing/brisk/orig/6-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

brisk <- NULL
means <- NULL
for (i in 1:170){
  filename = patient[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  filename7 = patient7[i]
  data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  data7 = read.table(filename7,header = FALSE, fill = TRUE, sep = " ")
  colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data7) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  
  data$snr <- (data$n_matches - mean(data$n_matches))/sd(data$n_matches)
  data$type <- "7-7"
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data2$type <- "1-7"
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data3$type <- "2-7"
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data4$type <- "3-7"
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data5$type <- "4-7"
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data6$type <- "5-7"
  data7$snr <- (data7$n_matches - mean(data7$n_matches))/sd(data7$n_matches)
  data7$type <- "6-7"
  
  data_diff <- rbind(data2,data3,data4,data5,data6,data7)
  means <- rbind(means,data.frame(ref_z=seq(1,170),snr=with(data_diff,tapply(snr, ref_z, mean)),type="x-7",ref_id=i))
  data$ref_id <- i
  brisk <- rbind(brisk,data) #rbind(data,data_diff)
}

result <- brisk %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
means_filter <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
ggplot(result, aes(x=ref_z,y=snr, color=as.factor(ref_id),group=ref_id, lty=type)) +
  geom_line(size=1.2) + 
  geom_point(size=3.2) + 
  #  scale_linetype_manual(values = c(50 = "solid", east = "dashed")) +
  geom_line(data = means_filter,size=1.2) +
  labs(title = "BRISK") +
  coord_cartesian(ylim = c(-1,10))

############

filter <- brisk[brisk$test_z==brisk$ref_z,]
filter$metod <- "brisk"
filter2 <- orb[orb$test_z==orb$ref_z,]
filter2$metod <- "orb"
filter3 <- akaze[akaze$test_z==akaze$ref_z,]
filter3$metod <- "akaze"
filter <- rbind(filter,filter2, filter3)

ggplot(filter, aes(x=ref_z, y=snr, color=metod)) + 
  geom_line() + 
  geom_point() + 
  coord_cartesian(ylim=c(5,12.5))

