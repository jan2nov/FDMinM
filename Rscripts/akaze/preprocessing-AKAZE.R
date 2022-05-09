rm(list = ls())
library(ggplot2)
library(dplyr)
require(cowplot)
#require(smooth)
#require(Mcomp)
require(zoo)

img_start = 0
img_end = 170
window = 7

#list_data = list.files(path = "../source/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

# patient  = list.files(path = "../data/methods_testing/akaze/preprocessing/1-7/min_rotation/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
# patient2 = list.files(path = "../data/methods_testing/akaze/preprocessing/2-7/min_rotation/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
# patient3 = list.files(path = "../data/methods_testing/akaze/preprocessing/3-7/min_rotation/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
# patient4 = list.files(path = "../data/methods_testing/akaze/preprocessing/4-7/min_rotation/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
# patient5 = list.files(path = "../data/methods_testing/akaze/preprocessing/5-7/min_rotation/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
# patient6 = list.files(path = "../data/methods_testing/akaze/preprocessing/6-7/min_rotation/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

#patient  = list.files(path = "../data/methods_testing/akaze/preprocessing/1-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
#patient2 = list.files(path = "../data/methods_testing/akaze/preprocessing/2-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
#patient3 = list.files(path = "../data/methods_testing/akaze/preprocessing/3-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
#patient4 = list.files(path = "../data/methods_testing/akaze/preprocessing/4-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
#patient5 = list.files(path = "../data/methods_testing/akaze/preprocessing/5-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
#patient6 = list.files(path = "../data/methods_testing/akaze/preprocessing/6-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

patient = list.files(path = "../data/methods_testing/akaze/orig/7-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient = list.files(path = "../data/methods_testing/akaze/orig/1-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/akaze/orig/2-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/akaze/orig/3-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/akaze/orig/4-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/akaze/orig/5-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/akaze/orig/6-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

means <- NULL
for (i in 1:length(patient)){
#for (i in 1:140){  
  filename = patient[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  data$snr  <- (data$n_matches  - mean(data$n_matches)) /sd(data$n_matches)
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data$type <- "1-7"
  data2$type <- "2-7"
  data3$type <- "3-7"
  data4$type <- "4-7"
  data5$type <- "5-7"
  data6$type <- "6-7"
  data_diff <- rbind(data,data2,data3,data4,data5,data6)
  #means <- rbind(means,data.frame(ref_z=seq(1,length(patient)),snr=with(data_diff,tapply(snr, ref_z, mean)),type="x-7",ref_id=i))
  temp <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),mean)
  temp2 <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),sd)
  temp$sd <- temp2$snr
  temp$ref_id <- i
  means <- rbind(means,temp)
}

  write.table(means, "akaze-snr-orig-x-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)

# plot(data$snr, type="o")
# lines(data2$snr, col="red")
# lines(data3$snr, col="blue")
# lines(data4$snr, col="brown")
# lines(data5$snr, col="green")
# lines(data6$snr, col="yellow")
# plot(means[means$ref_id==150,]$snr) 

akaze <- read.table("akaze-snr-orig-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
result <- akaze%>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
means_filter <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
#pmeans_filter <- perc_means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
ggplot(result, aes(x=ref_z,y=snr, color=as.factor(ref_id),group=ref_id)) +
  geom_line(size=1.2) +
  geom_point(size=3.2) +
  #  scale_linetype_manual(values = c(50 = "solid", east = "dashed")) +
  geom_line(data=means_filter,size=1.2) +
  #geom_line(data=pmeans_filter,size=1.0) +
  labs(title = "AKAZE - preprocessing") +
  coord_cartesian(ylim = c(-1,10))

ci <- qnorm(0.95,mean=0,sd=1) # 0.9+0.05
std <-10/(2*ci)
std2 <-20/(2*ci)
x <- seq(-170, 170, by = 0.1)
y <- dnorm(x, mean = 0, sd = std)
data_tab <- data.frame(x=x,y=y)

akaze_identity_diff <- means
akaze_identity_diff$rollmean_snr <- rollmean(akaze_identity_diff$snr, k = window,fill=NA, align = c("center"))
akaze_identity_diff <- akaze_identity_diff %>% filter(ref_z > img_start) %>% filter(ref_z <= img_end)
akaze_identity_diff <- akaze_identity_diff %>% filter(ref_id > img_start) %>% filter(ref_id <= img_end)
#akaze_identity_diff <- perc_means
sorted_data <- akaze_identity_diff %>% group_by(ref_id) %>% slice(which.max(rollmean_snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
tt <- table(best$stred)
as.integer(names(tt[which.max(tt)]))
#best$stred <- best$stred - as.integer(names(tt[which.max(tt)])) # median(best$stred)
best$stred <- best$stred - median(best$stred)


results <- NULL
best$z <- pnorm(best$stred,mean=0,sd=std)
best$z2 <- pnorm(best$stred,mean=0,sd=std2)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
# results <- best
# best$color <- ifelse(( (best$z2>=0.05) & (best$z2<=0.95)),"true2","false2")
# best$inside <- ifelse(( (best$z2>=0.05) & (best$z2<=0.95)),1,0)
# results <- rbind(results, best)
best[95:105,]
best[25:35,]

p1 <- ggplot(data=data_tab, aes(x=x,y=60*y)) + 
  geom_line() + 
  geom_point(data=best,aes(x=stred,y=snr, color=as.factor(color))) +
  #geom_point(data=best,aes(x=stred,y=snr, color=ref_id)) + 
  labs(title="AKAZE - preprocessing; comparing with etalon; equalize; nosculp", color="Inside", x="Distance from expected", y="") + 
  annotate("label", x=150, y=7.5, label= paste0("Hit: ",sum(best$inside),"/",nrow(best)),size=4) #+
#theme(panel.background = element_rect(fill = NA,color="black"),
#panel.grid.major = element_line(colour = "grey50"))
#scale_color_gradientn(colours = rainbow(10))
#geom_line(data=best,aes(x=stred,y=snr/60),color="red")

p2 <- ggplot(data=best, aes(x=ref_id,y=1,fill=as.factor(color))) +
  geom_tile() + 
  labs(y="", x="Input image ID", fill="Inside") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA)
  )

plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))

print(paste("Orig cum.distance:",sum(abs(best$stred))))
print(paste("Orig SNR:",mean(best$snr)))
print(paste("Accuracy: ", sum(best$inside), "/170"))
    

data <- means[means$test_z==100,]
roll <- data.frame(x=seq(1,170), y=rollmean(data$snr,window,fill=NA, align = c("left")))
ggplot(data,aes(x=ref_z,y=snr,color="red")) + 
  geom_point() + 
  geom_line() +
  geom_line(data=roll, aes(x=x, y=y,color="blue"),color="blue")
#  geom_line(aes(y=(snr+sd)), color="blue")
#roll[which.max(roll$y),]
######## for image average mean ############
akaze$rollmean_snr <- rollmean(akaze$snr, k = window,fill=NA, align = c("center"))
data <- akaze[akaze$test_z==150,]
roll <- data.frame(x=seq(1,170), y=rollmean(data$snr,window,fill=NA, align = c("center")))
ggplot(data,aes(x=ref_z,y=snr)) + 
  geom_point(size=2.5, color="#2c7bb6") + 
  geom_line(lwd=1.1, color="#2c7bb6") +
  geom_line(data=roll, aes(x=x, y=y), color="#d7191c",lwd=1.1) + 
  labs(x="image ID", y = expression("SNR"[100]), colour="") +
  ggtitle("AKAZE, input ID image = 100") +
  #scale_fill_discrete(name = "", labels = c("SNR", "", "C"))
  theme_bw() + 
  theme(plot.title = element_text(margin=margin(t=20,b=-30), hjust=0.05, face="plain", size=12), 
        legend.position = c(0.9,0.9), 
        text=element_text(family="Times New Roman"))

########################################################################################################

patient  = list.files(path = "../data/methods_testing/akaze/preprocessing/1-7/orig-eq/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/akaze/preprocessing/2-7/orig-eq/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/akaze/preprocessing/3-7/orig-eq/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/akaze/preprocessing/4-7/orig-eq/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/akaze/preprocessing/5-7/orig-eq/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/akaze/preprocessing/6-7/orig-eq/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)


means <- NULL
for (i in 1:length(patient)){
  #for (i in 1:140){  
  filename = patient[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  data$snr  <- (data$n_matches  - mean(data$n_matches)) /sd(data$n_matches)
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data$type <- "1-7"
  data2$type <- "2-7"
  data3$type <- "3-7"
  data4$type <- "4-7"
  data5$type <- "5-7"
  data6$type <- "6-7"
  data_diff <- rbind(data,data2,data3,data4,data5,data6)
  #means <- rbind(means,data.frame(ref_z=seq(1,length(patient)),snr=with(data_diff,tapply(snr, ref_z, mean)),type="x-7",ref_id=i))
  temp <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),mean)
  temp2 <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),sd)
  temp$sd <- temp2$snr
  temp$ref_id <- i
  means <- rbind(means,temp)
}

write.table(means, "akaze-snr-orig-eq-sculp-x-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)


akaze <- read.table("akaze-snr-orig-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
result <- akaze%>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
means_filter <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
#pmeans_filter <- perc_means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
ggplot(result, aes(x=ref_z,y=snr, color=as.factor(ref_id),group=ref_id)) +
  geom_line(size=1.2) +
  geom_point(size=3.2) +
  #  scale_linetype_manual(values = c(50 = "solid", east = "dashed")) +
  geom_line(data=means_filter,size=1.2) +
  #geom_line(data=pmeans_filter,size=1.0) +
  labs(title = "AKAZE - preprocessing") +
  coord_cartesian(ylim = c(-1,10))

ci <- qnorm(0.95,mean=0,sd=1) # 0.9+0.05
std <-10/(2*ci)
std2 <-20/(2*ci)
x <- seq(-170, 170, by = 0.1)
y <- dnorm(x, mean = 0, sd = std)
data_tab <- data.frame(x=x,y=y)

akaze_identity_diff <- means
akaze_identity_diff$rollmean_snr <- rollmean(akaze_identity_diff$snr, k = 5,fill=NA, align = c("center"))
akaze_identity_diff <- akaze_identity_diff %>% filter(ref_z > img_start) %>% filter(ref_z <= img_end)
akaze_identity_diff <- akaze_identity_diff %>% filter(ref_id > img_start) %>% filter(ref_id <= img_end)
#akaze_identity_diff <- perc_means
sorted_data <- akaze_identity_diff %>% group_by(ref_id) %>% slice(which.max(rollmean_snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
tt <- table(best$stred)
as.integer(names(tt[which.max(tt)]))
#best$stred <- best$stred - as.integer(names(tt[which.max(tt)])) # median(best$stred)
best$stred <- best$stred - median(best$stred)

results <- NULL
best$z <- pnorm(best$stred,mean=0,sd=std)
best$z2 <- pnorm(best$stred,mean=0,sd=std2)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
# results <- best
# best$color <- ifelse(( (best$z2>=0.05) & (best$z2<=0.95)),"true2","false2")
# best$inside <- ifelse(( (best$z2>=0.05) & (best$z2<=0.95)),1,0)
# results <- rbind(results, best)
best[95:105,]
best[25:35,]

p1 <- ggplot(data=data_tab, aes(x=x,y=60*y)) + 
  geom_line() + 
  geom_point(data=best,aes(x=stred,y=snr, color=as.factor(color))) +
  #geom_point(data=best,aes(x=stred,y=snr, color=ref_id)) + 
  labs(title="AKAZE - preprocessing; comparing with etalon; equalize; nosculp", color="Inside", x="Distance from expected", y="") + 
  annotate("label", x=150, y=7.5, label= paste0("Hit: ",sum(best$inside),"/",nrow(best)),size=4) #+
#theme(panel.background = element_rect(fill = NA,color="black"),
#panel.grid.major = element_line(colour = "grey50"))
#scale_color_gradientn(colours = rainbow(10))
#geom_line(data=best,aes(x=stred,y=snr/60),color="red")

p2 <- ggplot(data=best, aes(x=ref_id,y=1,fill=as.factor(color))) +
  geom_tile() + 
  labs(y="", x="Input image ID", fill="Inside") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA)
  )

plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
print(paste("Orig-eq cum.distance:",sum(abs(best$stred))))
print(paste("Orig-eq SNR:",mean(best$snr)))
print(paste("Accuracy: ", sum(best$inside), "/170"))

data <- means[means$test_z==100,]
roll <- data.frame(x=seq(1,169), y=rollmean(data$snr, window, fill=NA, align = c("left")))
ggplot(data,aes(x=ref_z,y=snr,color="red")) + 
  geom_point() + 
  geom_line() +
  geom_line(data=roll, aes(x=x, y=y,color="blue"),color="blue")
#  geom_line(aes(y=(snr+sd)), color="blue")
roll[which.max(roll$y),]
##############################################################################################

patient  = list.files(path = "../data/methods_testing/akaze/preprocessing/1-7/min_rotation_scale/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/akaze/preprocessing/2-7/min_rotation_scale/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/akaze/preprocessing/3-7/min_rotation_scale/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/akaze/preprocessing/4-7/min_rotation_scale/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/akaze/preprocessing/5-7/min_rotation_scale/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/akaze/preprocessing/6-7/min_rotation_scale/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

means <- NULL
for (i in 1:length(patient)){
  #for (i in 1:140){  
  filename = patient[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  data$snr  <- (data$n_matches  - mean(data$n_matches)) /sd(data$n_matches)
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data$type <- "1-7"
  data2$type <- "2-7"
  data3$type <- "3-7"
  data4$type <- "4-7"
  data5$type <- "5-7"
  data6$type <- "6-7"
  data_diff <- rbind(data,data2,data3,data4,data5,data6)
  #means <- rbind(means,data.frame(ref_z=seq(1,length(patient)),snr=with(data_diff,tapply(snr, ref_z, mean)),type="x-7",ref_id=i))
  temp <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),mean)
  temp2 <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),sd)
  temp$sd <- temp2$snr
  temp$ref_id <- i
  means <- rbind(means,temp)
}

write.table(means, "akaze-snr-rot-scale-x-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)


akaze <- read.table("akaze-snr-orig-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
result <- akaze%>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
means_filter <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
#pmeans_filter <- perc_means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
ggplot(result, aes(x=ref_z,y=snr, color=as.factor(ref_id),group=ref_id)) +
  geom_line(size=1.2) +
  geom_point(size=3.2) +
  #  scale_linetype_manual(values = c(50 = "solid", east = "dashed")) +
  geom_line(data=means_filter,size=1.2) +
  #geom_line(data=pmeans_filter,size=1.0) +
  labs(title = "AKAZE - preprocessing") +
  coord_cartesian(ylim = c(-1,10))

ci <- qnorm(0.95,mean=0,sd=1) # 0.9+0.05
std <-10/(2*ci)
std2 <-20/(2*ci)
x <- seq(-170, 170, by = 0.1)
y <- dnorm(x, mean = 0, sd = std)
data_tab <- data.frame(x=x,y=y)

akaze_identity_diff <- means
akaze_identity_diff$rollmean_snr <- rollmean(akaze_identity_diff$snr, k = window,fill=NA, align = c("center"))
akaze_identity_diff <- akaze_identity_diff %>% filter(ref_z > img_start) %>% filter(ref_z <= img_end)
akaze_identity_diff <- akaze_identity_diff %>% filter(ref_id > img_start) %>% filter(ref_id <= img_end)
#akaze_identity_diff <- perc_means
sorted_data <- akaze_identity_diff %>% group_by(ref_id) %>% slice(which.max(rollmean_snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
tt <- table(best$stred)
as.integer(names(tt[which.max(tt)]))
#best$stred <- best$stred - as.integer(names(tt[which.max(tt)])) # median(best$stred)
best$stred <- best$stred - median(best$stred)

results <- NULL
best$z <- pnorm(best$stred,mean=0,sd=std)
best$z2 <- pnorm(best$stred,mean=0,sd=std2)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
# results <- best
# best$color <- ifelse(( (best$z2>=0.05) & (best$z2<=0.95)),"true2","false2")
# best$inside <- ifelse(( (best$z2>=0.05) & (best$z2<=0.95)),1,0)
# results <- rbind(results, best)
best[95:105,]
best[25:35,]

p1 <- ggplot(data=data_tab, aes(x=x,y=60*y)) + 
  geom_line() + 
  geom_point(data=best,aes(x=stred,y=snr, color=as.factor(color))) +
  #geom_point(data=best,aes(x=stred,y=snr, color=ref_id)) + 
  labs(title="AKAZE - preprocessing; comparing with etalon; equalize; nosculp", color="Inside", x="Distance from expected", y="") + 
  annotate("label", x=150, y=7.5, label= paste0("Hit: ",sum(best$inside),"/",nrow(best)),size=4) #+
#theme(panel.background = element_rect(fill = NA,color="black"),
#panel.grid.major = element_line(colour = "grey50"))
#scale_color_gradientn(colours = rainbow(10))
#geom_line(data=best,aes(x=stred,y=snr/60),color="red")

p2 <- ggplot(data=best, aes(x=ref_id,y=1,fill=as.factor(color))) +
  geom_tile() + 
  labs(y="", x="Input image ID", fill="Inside") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA)
  )

plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
print(paste("Rot-scale cum.distance:",sum(abs(best$stred))))
print(paste("Rot-scale SNR:",mean(best$snr)))
print(paste("Accuracy: ", sum(best$inside), "/170"))


data <- means[means$test_z==100,]
roll <- data.frame(x=seq(1,170), y=rollmean(data$snr, k=window, fill=NA, align = c("center")))
ggplot(data,aes(x=ref_z,y=snr,color="red")) + 
  geom_point() + 
  geom_line() +
  geom_line(data=roll, aes(x=x, y=y,color="blue"),color="blue")
#  geom_line(aes(y=(snr+sd)), color="blue")
roll[which.max(roll$y),]

############################################# all #################################################

patient  = list.files(path = "../data/methods_testing/akaze/preprocessing/1-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/akaze/preprocessing/2-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/akaze/preprocessing/3-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/akaze/preprocessing/4-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/akaze/preprocessing/5-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/akaze/preprocessing/6-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

means <- NULL
for (i in 1:length(patient)){
  #for (i in 1:140){  
  filename = patient[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  data$snr  <- (data$n_matches  - mean(data$n_matches)) /sd(data$n_matches)
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data$type <- "1-7"
  data2$type <- "2-7"
  data3$type <- "3-7"
  data4$type <- "4-7"
  data5$type <- "5-7"
  data6$type <- "6-7"
  data_diff <- rbind(data,data2,data3,data4,data5,data6)
  #means <- rbind(means,data.frame(ref_z=seq(1,length(patient)),snr=with(data_diff,tapply(snr, ref_z, mean)),type="x-7",ref_id=i))
  temp <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),mean)
  temp2 <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),sd)
  temp$sd <- temp2$snr
  temp$ref_id <- i
  means <- rbind(means,temp)
}

write.table(means, "akaze-snr-rot-scale-sculp-eq-x-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)


akaze <- read.table("akaze-snr-orig-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
result <- akaze%>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
means_filter <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
#pmeans_filter <- perc_means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
ggplot(result, aes(x=ref_z,y=snr, color=as.factor(ref_id),group=ref_id)) +
  geom_line(size=1.2) +
  geom_point(size=3.2) +
  #  scale_linetype_manual(values = c(50 = "solid", east = "dashed")) +
  geom_line(data=means_filter,size=1.2) +
  #geom_line(data=pmeans_filter,size=1.0) +
  labs(title = "AKAZE - preprocessing") +
  coord_cartesian(ylim = c(-1,10))

ci <- qnorm(0.95,mean=0,sd=1) # 0.9+0.05
std <-10/(2*ci)
std2 <-20/(2*ci)
x <- seq(-170, 170, by = 0.1)
y <- dnorm(x, mean = 0, sd = std)
data_tab <- data.frame(x=x,y=y)

akaze_identity_diff <- means
akaze_identity_diff$rollmean_snr <- rollmean(akaze_identity_diff$snr, k = window,fill=NA, align = c("center"))
akaze_identity_diff <- akaze_identity_diff %>% filter(ref_z > img_start) %>% filter(ref_z <= img_end)
akaze_identity_diff <- akaze_identity_diff %>% filter(ref_id > img_start) %>% filter(ref_id <= img_end)
#akaze_identity_diff <- perc_means
sorted_data <- akaze_identity_diff %>% group_by(ref_id) %>% slice(which.max(rollmean_snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
tt <- table(best$stred)
as.integer(names(tt[which.max(tt)]))
#best$stred <- best$stred - as.integer(names(tt[which.max(tt)])) # median(best$stred)
best$stred <- best$stred - median(best$stred)

results <- NULL
best$z <- pnorm(best$stred,mean=0,sd=std)
best$z2 <- pnorm(best$stred,mean=0,sd=std2)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
# results <- best
# best$color <- ifelse(( (best$z2>=0.05) & (best$z2<=0.95)),"true2","false2")
# best$inside <- ifelse(( (best$z2>=0.05) & (best$z2<=0.95)),1,0)
# results <- rbind(results, best)
best[95:105,]
best[25:35,]

p1 <- ggplot(data=data_tab, aes(x=x,y=60*y)) + 
  geom_line() + 
  geom_point(data=best,aes(x=stred,y=snr, color=as.factor(color))) +
  #geom_point(data=best,aes(x=stred,y=snr, color=ref_id)) + 
  labs(title="AKAZE - preprocessing; comparing with etalon; equalize; nosculp", color="Inside", x="Distance from expected", y="") + 
  annotate("label", x=150, y=7.5, label= paste0("Hit: ",sum(best$inside),"/",nrow(best)),size=4) #+
#theme(panel.background = element_rect(fill = NA,color="black"),
#panel.grid.major = element_line(colour = "grey50"))
#scale_color_gradientn(colours = rainbow(10))
#geom_line(data=best,aes(x=stred,y=snr/60),color="red")

p2 <- ggplot(data=best, aes(x=ref_id,y=1,fill=as.factor(color))) +
  geom_tile() + 
  labs(y="", x="Input image ID", fill="Inside") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA)
  )

plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
print(paste("Rot-scale-sculp-eq cum.distance:",sum(abs(best$stred))))
print(paste("Rot-scale-sculp-eq SNR:",mean(best$snr)))
print(paste("Accuracy: ", sum(best$inside), "/170"))


data <- means[means$test_z==100,]
roll <- data.frame(x=seq(1,170), y=rollmean(data$snr, window, fill=NA, align = c("center")))
ggplot(data,aes(x=ref_z,y=snr,color="red")) + 
  geom_point() + 
  geom_line() +
  geom_line(data=roll, aes(x=x, y=y,color="blue"),color="blue")
#  geom_line(aes(y=(snr+sd)), color="blue")
roll[which.max(roll$y),]

############################################# all #################################################

patient  = list.files(path = "../data/methods_testing/akaze/preprocessing/1-7/min_rotation_scale_sculp", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/akaze/preprocessing/2-7/min_rotation_scale_sculp", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/akaze/preprocessing/3-7/min_rotation_scale_sculp", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/akaze/preprocessing/4-7/min_rotation_scale_sculp", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/akaze/preprocessing/5-7/min_rotation_scale_sculp", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/akaze/preprocessing/6-7/min_rotation_scale_sculp", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

means <- NULL
for (i in 1:length(patient)){
  #for (i in 1:140){  
  filename = patient[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  data$snr  <- (data$n_matches  - mean(data$n_matches)) /sd(data$n_matches)
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data$type <- "1-7"
  data2$type <- "2-7"
  data3$type <- "3-7"
  data4$type <- "4-7"
  data5$type <- "5-7"
  data6$type <- "6-7"
  data_diff <- rbind(data,data2,data3,data4,data5,data6)
  #means <- rbind(means,data.frame(ref_z=seq(1,length(patient)),snr=with(data_diff,tapply(snr, ref_z, mean)),type="x-7",ref_id=i))
  temp <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),mean)
  temp2 <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),sd)
  temp$sd <- temp2$snr
  temp$ref_id <- i
  means <- rbind(means,temp)
}

write.table(means, "akaze-snr-rot-scale-sculp-x-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)


akaze <- read.table("akaze-snr-orig-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
result <- akaze%>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
means_filter <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
#pmeans_filter <- perc_means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
ggplot(result, aes(x=ref_z,y=snr, color=as.factor(ref_id),group=ref_id)) +
  geom_line(size=1.2) +
  geom_point(size=3.2) +
  #  scale_linetype_manual(values = c(50 = "solid", east = "dashed")) +
  geom_line(data=means_filter,size=1.2) +
  #geom_line(data=pmeans_filter,size=1.0) +
  labs(title = "AKAZE - preprocessing") +
  coord_cartesian(ylim = c(-1,10))

ci <- qnorm(0.95,mean=0,sd=1) # 0.9+0.05
std <-10/(2*ci)
std2 <-20/(2*ci)
x <- seq(-170, 170, by = 0.1)
y <- dnorm(x, mean = 0, sd = std)
data_tab <- data.frame(x=x,y=y)

akaze_identity_diff <- means
akaze_identity_diff$rollmean_snr <- rollmean(akaze_identity_diff$snr, k = window,fill=NA, align = c("center"))
akaze_identity_diff <- akaze_identity_diff %>% filter(ref_z > img_start) %>% filter(ref_z <= img_end)
akaze_identity_diff <- akaze_identity_diff %>% filter(ref_id > img_start) %>% filter(ref_id <= img_end)
#akaze_identity_diff <- perc_means
sorted_data <- akaze_identity_diff %>% group_by(ref_id) %>% slice(which.max(rollmean_snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
tt <- table(best$stred)
as.integer(names(tt[which.max(tt)]))
#best$stred <- best$stred - as.integer(names(tt[which.max(tt)])) # median(best$stred)
best$stred <- best$stred - median(best$stred)

results <- NULL
best$z <- pnorm(best$stred,mean=0,sd=std)
best$z2 <- pnorm(best$stred,mean=0,sd=std2)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
# results <- best
# best$color <- ifelse(( (best$z2>=0.05) & (best$z2<=0.95)),"true2","false2")
# best$inside <- ifelse(( (best$z2>=0.05) & (best$z2<=0.95)),1,0)
# results <- rbind(results, best)
best[95:105,]
best[25:35,]

p1 <- ggplot(data=data_tab, aes(x=x,y=60*y)) + 
  geom_line() + 
  geom_point(data=best,aes(x=stred,y=snr, color=as.factor(color))) +
  #geom_point(data=best,aes(x=stred,y=snr, color=ref_id)) + 
  labs(title="AKAZE - preprocessing; comparing with etalon; equalize; nosculp", color="Inside", x="Distance from expected", y="") + 
  annotate("label", x=150, y=7.5, label= paste0("Hit: ",sum(best$inside),"/",nrow(best)),size=4) #+
#theme(panel.background = element_rect(fill = NA,color="black"),
#panel.grid.major = element_line(colour = "grey50"))
#scale_color_gradientn(colours = rainbow(10))
#geom_line(data=best,aes(x=stred,y=snr/60),color="red")

p2 <- ggplot(data=best, aes(x=ref_id,y=1,fill=as.factor(color))) +
  geom_tile() + 
  labs(y="", x="Input image ID", fill="Inside") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA)
  )

plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
print(paste("Rot-scale-sculp cum.distance:",sum(abs(best$stred))))
print(paste("Rot-scale-sculp SNR:",mean(best$snr)))
print(paste("Accuracy: ", sum(best$inside), "/170"))


data <- means[means$test_z==100,]
roll <- data.frame(x=seq(1,170), y=rollmean(data$snr, window, fill=NA, align = c("center")))
ggplot(data,aes(x=ref_z,y=snr,color="red")) + 
  geom_point() + 
  geom_line() +
  geom_line(data=roll, aes(x=x, y=y,color="blue"),color="blue")
#  geom_line(aes(y=(snr+sd)), color="blue")
roll[which.max(roll$y),]

############################################# min-rot sculp #################################################

patient  = list.files(path = "../data/methods_testing/akaze/preprocessing/1-7/min-rot-sculp/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/akaze/preprocessing/2-7/min-rot-sculp/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/akaze/preprocessing/3-7/min-rot-sculp/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/akaze/preprocessing/4-7/min-rot-sculp/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/akaze/preprocessing/5-7/min-rot-sculp/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/akaze/preprocessing/6-7/min-rot-sculp/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

means <- NULL
for (i in 1:length(patient)){
  #for (i in 1:140){  
  filename = patient[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  data$snr  <- (data$n_matches  - mean(data$n_matches)) /sd(data$n_matches)
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data$type <- "1-7"
  data2$type <- "2-7"
  data3$type <- "3-7"
  data4$type <- "4-7"
  data5$type <- "5-7"
  data6$type <- "6-7"
  data_diff <- rbind(data,data2,data3,data4,data5,data6)
  #means <- rbind(means,data.frame(ref_z=seq(1,length(patient)),snr=with(data_diff,tapply(snr, ref_z, mean)),type="x-7",ref_id=i))
  temp <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),mean)
  temp2 <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),sd)
  temp$sd <- temp2$snr
  temp$ref_id <- i
  means <- rbind(means,temp)
}

write.table(means, "akaze-snr-rot-sculp-x-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)


akaze <- read.table("akaze-snr-orig-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
result <- akaze%>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
means_filter <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
#pmeans_filter <- perc_means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
ggplot(result, aes(x=ref_z,y=snr, color=as.factor(ref_id),group=ref_id)) +
  geom_line(size=1.2) +
  geom_point(size=3.2) +
  #  scale_linetype_manual(values = c(50 = "solid", east = "dashed")) +
  geom_line(data=means_filter,size=1.2) +
  #geom_line(data=pmeans_filter,size=1.0) +
  labs(title = "AKAZE - preprocessing") +
  coord_cartesian(ylim = c(-1,10))

ci <- qnorm(0.95,mean=0,sd=1) # 0.9+0.05
std <-10/(2*ci)
std2 <-20/(2*ci)
x <- seq(-170, 170, by = 0.1)
y <- dnorm(x, mean = 0, sd = std)
data_tab <- data.frame(x=x,y=y)

akaze_identity_diff <- means
akaze_identity_diff$rollmean_snr <- rollmean(akaze_identity_diff$snr, k = window,fill=NA, align = c("center"))
akaze_identity_diff <- akaze_identity_diff %>% filter(ref_z > img_start) %>% filter(ref_z <= img_end)
akaze_identity_diff <- akaze_identity_diff %>% filter(ref_id > img_start) %>% filter(ref_id <= img_end)
#akaze_identity_diff <- perc_means
sorted_data <- akaze_identity_diff %>% group_by(ref_id) %>% slice(which.max(rollmean_snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
tt <- table(best$stred)
as.integer(names(tt[which.max(tt)]))
#best$stred <- best$stred - as.integer(names(tt[which.max(tt)])) # median(best$stred)
best$stred <- best$stred - median(best$stred)

results <- NULL
best$z <- pnorm(best$stred,mean=0,sd=std)
best$z2 <- pnorm(best$stred,mean=0,sd=std2)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
# results <- best
# best$color <- ifelse(( (best$z2>=0.05) & (best$z2<=0.95)),"true2","false2")
# best$inside <- ifelse(( (best$z2>=0.05) & (best$z2<=0.95)),1,0)
# results <- rbind(results, best)
best[95:105,]
best[25:35,]

p1 <- ggplot(data=data_tab, aes(x=x,y=60*y)) + 
  geom_line() + 
  geom_point(data=best,aes(x=stred,y=snr, color=as.factor(color))) +
  #geom_point(data=best,aes(x=stred,y=snr, color=ref_id)) + 
  labs(title="AKAZE - preprocessing; comparing with etalon; equalize; nosculp", color="Inside", x="Distance from expected", y="") + 
  annotate("label", x=150, y=7.5, label= paste0("Hit: ",sum(best$inside),"/",nrow(best)),size=4) #+
#theme(panel.background = element_rect(fill = NA,color="black"),
#panel.grid.major = element_line(colour = "grey50"))
#scale_color_gradientn(colours = rainbow(10))
#geom_line(data=best,aes(x=stred,y=snr/60),color="red")

p2 <- ggplot(data=best, aes(x=ref_id,y=1,fill=as.factor(color))) +
  geom_tile() + 
  labs(y="", x="Input image ID", fill="Inside") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA)
  )

plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
print(paste("Rot-sculp cum.distance:",sum(abs(best$stred))))
print(paste("Rot-sculp SNR:",mean(best$snr)))
print(paste("Accuracy: ", sum(best$inside), "/170"))


data <- means[means$test_z==25,]
roll <- data.frame(x=seq(1,169), y=rollmean(data$snr, window, fill=NA, align = c("center")))
ggplot(data,aes(x=ref_z,y=snr,color="red")) + 
  geom_point() + 
  geom_line() +
  geom_line(data=roll, aes(x=x, y=y,color="blue"),color="blue")
#  geom_line(aes(y=(snr+sd)), color="blue")
roll[which.max(roll$y),]

############################################# min-rot #################################################

patient  = list.files(path = "../data/methods_testing/akaze/preprocessing/1-7/min_rotation/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/akaze/preprocessing/2-7/min_rotation/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/akaze/preprocessing/3-7/min_rotation/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/akaze/preprocessing/4-7/min_rotation/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/akaze/preprocessing/5-7/min_rotation/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/akaze/preprocessing/6-7/min_rotation/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

means <- NULL
for (i in 1:length(patient)){
  #for (i in 1:140){  
  filename = patient[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  data$snr  <- (data$n_matches  - mean(data$n_matches)) /sd(data$n_matches)
  data2$snr <- (data2$n_matches - mean(data2$n_matches))/sd(data2$n_matches)
  data3$snr <- (data3$n_matches - mean(data3$n_matches))/sd(data3$n_matches)
  data4$snr <- (data4$n_matches - mean(data4$n_matches))/sd(data4$n_matches)
  data5$snr <- (data5$n_matches - mean(data5$n_matches))/sd(data5$n_matches)
  data6$snr <- (data6$n_matches - mean(data6$n_matches))/sd(data6$n_matches)
  data$type <- "1-7"
  data2$type <- "2-7"
  data3$type <- "3-7"
  data4$type <- "4-7"
  data5$type <- "5-7"
  data6$type <- "6-7"
  data_diff <- rbind(data,data2,data3,data4,data5,data6)
  #means <- rbind(means,data.frame(ref_z=seq(1,length(patient)),snr=with(data_diff,tapply(snr, ref_z, mean)),type="x-7",ref_id=i))
  temp <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),mean)
  temp2 <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),sd)
  temp$sd <- temp2$snr
  temp$ref_id <- i
  means <- rbind(means,temp)
}

write.table(means, "akaze-snr-rot-x-7.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)


akaze <- read.table("akaze-snr-orig-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
result <- akaze%>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
means_filter <- means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
#pmeans_filter <- perc_means %>% filter( (ref_id==25) | (ref_id==50) | (ref_id==100) | (ref_id==150))
ggplot(result, aes(x=ref_z,y=snr, color=as.factor(ref_id),group=ref_id)) +
  geom_line(size=1.2) +
  geom_point(size=3.2) +
  #  scale_linetype_manual(values = c(50 = "solid", east = "dashed")) +
  geom_line(data=means_filter,size=1.2) +
  #geom_line(data=pmeans_filter,size=1.0) +
  labs(title = "AKAZE - preprocessing") +
  coord_cartesian(ylim = c(-1,10))

ci <- qnorm(0.95,mean=0,sd=1) # 0.9+0.05
std <-10/(2*ci)
std2 <-20/(2*ci)
x <- seq(-170, 170, by = 0.1)
y <- dnorm(x, mean = 0, sd = std)
data_tab <- data.frame(x=x,y=y)

akaze_identity_diff <- means
akaze_identity_diff$rollmean_snr <- rollmean(akaze_identity_diff$snr, k = window,fill=NA, align = c("center"))
akaze_identity_diff <- akaze_identity_diff %>% filter(ref_z > img_start) %>% filter(ref_z <= img_end)
akaze_identity_diff <- akaze_identity_diff %>% filter(ref_id > img_start) %>% filter(ref_id <= img_end)
#akaze_identity_diff <- perc_means
sorted_data <- akaze_identity_diff %>% group_by(ref_id) %>% slice(which.max(rollmean_snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
tt <- table(best$stred)
as.integer(names(tt[which.max(tt)]))
#best$stred <- best$stred - as.integer(names(tt[which.max(tt)])) # median(best$stred)
best$stred <- best$stred - median(best$stred)

results <- NULL
best$z <- pnorm(best$stred,mean=0,sd=std)
best$z2 <- pnorm(best$stred,mean=0,sd=std2)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
# results <- best
# best$color <- ifelse(( (best$z2>=0.05) & (best$z2<=0.95)),"true2","false2")
# best$inside <- ifelse(( (best$z2>=0.05) & (best$z2<=0.95)),1,0)
# results <- rbind(results, best)
best[95:105,]
best[25:35,]

p1 <- ggplot(data=data_tab, aes(x=x,y=60*y)) + 
  geom_line() + 
  geom_point(data=best,aes(x=stred,y=snr, color=as.factor(color))) +
  #geom_point(data=best,aes(x=stred,y=snr, color=ref_id)) + 
  labs(title="AKAZE - preprocessing; comparing with etalon; equalize; nosculp", color="Inside", x="Distance from expected", y="") + 
  annotate("label", x=150, y=7.5, label= paste0("Hit: ",sum(best$inside),"/",nrow(best)),size=4) #+
#theme(panel.background = element_rect(fill = NA,color="black"),
#panel.grid.major = element_line(colour = "grey50"))
#scale_color_gradientn(colours = rainbow(10))
#geom_line(data=best,aes(x=stred,y=snr/60),color="red")

p2 <- ggplot(data=best, aes(x=ref_id,y=1,fill=as.factor(color))) +
  geom_tile() + 
  labs(y="", x="Input image ID", fill="Inside") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA)
  )

plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
print(paste("Rot-sculp cum.distance:",sum(abs(best$stred))))
print(paste("Rot-sculp SNR:",mean(best$snr)))
print(paste("Accuracy: ", sum(best$inside), "/170"))


data <- means[means$test_z==25,]
roll <- data.frame(x=seq(1,170), y=rollmean(data$snr, window, fill=NA, align = c("center")))
ggplot(data,aes(x=ref_z,y=snr,color="red")) + 
  geom_point() + 
  geom_line() +
  geom_line(data=roll, aes(x=x, y=y,color="blue"),color="blue")
#  geom_line(aes(y=(snr+sd)), color="blue")
roll[which.max(roll$y),]
