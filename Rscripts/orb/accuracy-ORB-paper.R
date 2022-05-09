rm(list = ls())
require(zoo)
require(dplyr)
require(ggplot2)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

img_start = 20
img_end = 150
window = 7

patient1 = list.files(path = "../data/methods_testing/orb/orig/1-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/orb/orig/2-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/orb/orig/3-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/orb/orig/4-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/orb/orig/5-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/orb/orig/6-7", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

data_diff <- NULL
#means <- NULL
for (i in 1:length(patient1)){
  filename1 = patient1[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  data1 = read.table(filename1,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  colnames(data1) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  
  data1$snr  <- (data1$n_matches  - mean(data1$n_matches)) /sd(data1$n_matches)
  data1$type <- 1
  data2$snr  <- (data2$n_matches  - mean(data2$n_matches)) /sd(data2$n_matches)
  data2$type <- 2
  data3$snr  <- (data3$n_matches  - mean(data3$n_matches)) /sd(data3$n_matches)
  data3$type <- 3
  data4$snr  <- (data4$n_matches  - mean(data4$n_matches)) /sd(data4$n_matches)
  data4$type <- 4
  data5$snr  <- (data5$n_matches  - mean(data5$n_matches)) /sd(data5$n_matches)
  data5$type <- 5
  data6$snr  <- (data6$n_matches  - mean(data6$n_matches)) /sd(data6$n_matches)
  data6$type <- 6
  
  data_diff <- rbind(data_diff,data1, data2, data3, data4, data5, data6)
  #temp <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),mean)
  #temp2 <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),sd)
  #temp$sd <- temp2$snr
  #temp$ref_id <- i
  #means <- rbind(means,temp)
}

test <- data_diff %>% filter(type==3) %>% filter(test_z==142)

#data_diff$rollmean_snr <- rollmean(data_diff$snr, k = window,fill=NA, align = c("center"))
means <- NULL #data.frame(rollmean_snr=NULL)
for (i in 1:170){
  for (j in 1:6){
    filter <- data_diff[(data_diff$test_z == i) & (data_diff$type==j),]
    temp <- rollmean(filter$snr, k = window, fill=NA, align=c("center"))
    filter$rollmean_snr <- temp
    means <- rbind(means,filter) #append(means,temp)
  }
}

test <- means %>% filter(type==3) %>% filter(test_z==142)
plot(test$snr, type="o")
points(test$rollmean_snr, col="red")
lines(test$rollmean_snr, col="red")

###bacha na sort!!
final <- NULL
for (i in 1:6){
  filter <- means %>% filter(type==i) %>% group_by(test_z) %>% filter(test_z>2) %>% filter(test_z<169) %>% slice(which.max(rollmean_snr))
  filter$stred <- filter$ref_z - filter$test_z
  #print(c(i,median(filter$stred)))
  filter$stred <- filter$stred - median(filter$stred)
  #filter$stred <- filter$stred - getmode(filter$stred)
  
  final <- rbind(final, filter)
  #plot(filter$test_z, filter$ref_z)
  #plot(filter$stred)
}


#tt <- means[(means$test_z==17) & (means$type==2),]
#plot(tt$snr)

#data_diff$rollmean_snr <- rollmean(means$snr, k = window,fill=NA, align = c("center"))
#sorted_data <- means %>% group_by(test_z) %>% filter(type==2) %>% filter(test_z>2) %>% filter(test_z<169) %>% slice(which.max(rollmean_snr))

best <- final %>% filter(test_z>img_start) %>% filter(test_z<=img_end) #%>% filter(type==2) #as.data.frame(final)
#best$stred <- best$ref_z - best$test_z
#best$stred <- best$stred - median(best$stred)

best$color <- ifelse(( (best$stred>=-5) & (best$stred<=5)),"1","0")
best$inside <- ifelse(( (best$stred>=-5) & (best$stred<=5)),1,0)
print(paste("None, Accuracy",sum(best$inside)/nrow(best)))
print(paste("None Cum.distance: ",sum(abs(best$stred))))
print(paste("None Average SNR: ",mean(best$snr)))

a <- sum(best$inside)
#graph_hit = expression(paste(A["5,none"],": 65 %"))
graph_hit = expression("A"["5,none"]~"="~"57 %")
graph_cum = expression("C"["none"]~"="~"7610")
ggplot(data=best, aes(x=stred,y=rollmean_snr, color=as.factor(color))) + 
  geom_point() + 
  theme_bw() +
  xlim(c(-150,150)) +
  ylim(c(0.5,4.2)) +
  labs(y=expression("running mean Signal to Noise ratio"[]), fill=graph_hit, color=expression("x"["i"]), x="Distance from the expected", title = "ORB") +
  scale_color_manual(values=c("#d7191c", "#2c7bb6")) +
  scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  #annotate("label", x=100, y=5.8, label= paste0(graph_hit,sum(best$inside),"/",nrow(best)),size=4) +
  annotate("text", x=110, y=3.8, label= graph_hit, size=4) +
  annotate("text", x=110, y=3.6, label= graph_cum, size=4) + 
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.85,0.1),
        legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=20,b=-20), hjust=0.05, face="plain", size=12)
  )

orb_none <- best
#################################################################################

patient1 = list.files(path = "../data/methods_testing/orb/preprocessing/1-7/orig-eq/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/orb/preprocessing/2-7/orig-eq/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/orb/preprocessing/3-7/orig-eq/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/orb/preprocessing/4-7/orig-eq/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/orb/preprocessing/5-7/orig-eq/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/orb/preprocessing/6-7/orig-eq/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

data_diff <- NULL
#means <- NULL
for (i in 1:length(patient1)){
  filename1 = patient1[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  data1 = read.table(filename1,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  colnames(data1) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  
  data1$snr  <- (data1$n_matches  - mean(data1$n_matches)) /sd(data1$n_matches)
  data1$type <- 1
  data2$snr  <- (data2$n_matches  - mean(data2$n_matches)) /sd(data2$n_matches)
  data2$type <- 2
  data3$snr  <- (data3$n_matches  - mean(data3$n_matches)) /sd(data3$n_matches)
  data3$type <- 3
  data4$snr  <- (data4$n_matches  - mean(data4$n_matches)) /sd(data4$n_matches)
  data4$type <- 4
  data5$snr  <- (data5$n_matches  - mean(data5$n_matches)) /sd(data5$n_matches)
  data5$type <- 5
  data6$snr  <- (data6$n_matches  - mean(data6$n_matches)) /sd(data6$n_matches)
  data6$type <- 6
  
  data_diff <- rbind(data_diff,data1, data2, data3, data4, data5, data6)
}

means <- NULL #data.frame(rollmean_snr=NULL)
for (i in 1:170){
  for (j in 1:6){
    filter <- data_diff[(data_diff$test_z == i) & (data_diff$type==j),]
    temp <- rollmean(filter$snr, k = window, fill=NA, align=c("center"))
    filter$rollmean_snr <- temp
    means <- rbind(means,filter) #append(means,temp)
  }
}

###bacha na sort!!
final <- NULL
for (i in 1:6){
  filter <- means %>% filter(type==i) %>% group_by(test_z) %>% filter(test_z>2) %>% filter(test_z<169) %>% slice(which.max(rollmean_snr))
  filter$stred <- filter$ref_z - filter$test_z
  #print(c(i,getmode(filter$stred)))
  filter$stred <- filter$stred - median(filter$stred)
  
  final <- rbind(final, filter)
}

best <- final %>% filter(test_z>img_start) %>% filter(test_z<=img_end) #%>% filter(type==2) #as.data.frame(final)

best$color <- ifelse(( (best$stred>=-5) & (best$stred<=5)),"1","0")
best$inside <- ifelse(( (best$stred>=-5) & (best$stred<=5)),1,0)
print(paste("Equalize, Accuracy",sum(best$inside)/nrow(best)))
print(paste("Equalize Cum.distance: ",sum(abs(best$stred))))
print(paste("Equalize Average SNR: ",mean(best$snr)))

a <- sum(best$inside)
print(a)

graph_hit = expression("A"["5,eq"]~"="~"58 %")
graph_cum = expression("C"["eq"]~"="~"10748")
ggplot(data=best, aes(x=stred,y=rollmean_snr, color=as.factor(color))) + 
  geom_point() + 
  theme_bw() +
  xlim(c(-150,150)) +
  ylim(c(0.5,4.2)) +
  labs(y=expression("running mean Signal to Noise ratio"[]), fill=graph_hit, color=expression("x"["i"]), x="Distance from the expected", title = "ORB") +
  scale_color_manual(values=c("#d7191c", "#2c7bb6")) +
  scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  #annotate("label", x=100, y=5.8, label= paste0(graph_hit,sum(best$inside),"/",nrow(best)),size=4) +
  annotate("text", x=110, y=3.8, label= graph_hit, size=4) +
  annotate("text", x=110, y=3.6, label= graph_cum, size=4) + 
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.1,0.1),
        legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=20,b=-20), hjust=0.05, face="plain", size=12)
  )

orb_eq <- best
#################################################################################################################

patient1 = list.files(path = "../data/methods_testing/orb/preprocessing/1-7/min_rotation_scale/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/orb/preprocessing/2-7/min_rotation_scale/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/orb/preprocessing/3-7/min_rotation_scale/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/orb/preprocessing/4-7/min_rotation_scale/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/orb/preprocessing/5-7/min_rotation_scale/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/orb/preprocessing/6-7/min_rotation_scale/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

data_diff <- NULL
#means <- NULL
for (i in 1:length(patient1)){
  filename1 = patient1[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  data1 = read.table(filename1,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  colnames(data1) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  
  data1$snr  <- (data1$n_matches  - mean(data1$n_matches)) /sd(data1$n_matches)
  data1$type <- 1
  data2$snr  <- (data2$n_matches  - mean(data2$n_matches)) /sd(data2$n_matches)
  data2$type <- 2
  data3$snr  <- (data3$n_matches  - mean(data3$n_matches)) /sd(data3$n_matches)
  data3$type <- 3
  data4$snr  <- (data4$n_matches  - mean(data4$n_matches)) /sd(data4$n_matches)
  data4$type <- 4
  data5$snr  <- (data5$n_matches  - mean(data5$n_matches)) /sd(data5$n_matches)
  data5$type <- 5
  data6$snr  <- (data6$n_matches  - mean(data6$n_matches)) /sd(data6$n_matches)
  data6$type <- 6
  
  data_diff <- rbind(data_diff,data1, data2, data3, data4, data5, data6)
}

means <- NULL #data.frame(rollmean_snr=NULL)
for (i in 1:170){
  for (j in 1:6){
    filter <- data_diff[(data_diff$test_z == i) & (data_diff$type==j),]
    temp <- rollmean(filter$snr, k = window, fill=NA, align=c("center"))
    filter$rollmean_snr <- temp
    means <- rbind(means,filter) #append(means,temp)
  }
}

###bacha na sort!!
final <- NULL
for (i in 1:6){
  filter <- means %>% filter(type==i) %>% group_by(test_z) %>% filter(test_z>2) %>% filter(test_z<169) %>% slice(which.max(rollmean_snr))
  filter$stred <- filter$ref_z - filter$test_z
  #print(c(i,median(filter$stred)))
  filter$stred <- filter$stred - median(filter$stred)
  
  final <- rbind(final, filter)
}

best <- final %>% filter(test_z>img_start) %>% filter(test_z<=img_end) #%>% filter(type==2) #as.data.frame(final)

best$color <- ifelse(( (best$stred>=-5) & (best$stred<=5)),"1","0")
best$inside <- ifelse(( (best$stred>=-5) & (best$stred<=5)),1,0)
print(paste("r+s, Accuracy",sum(best$inside)/nrow(best)))
print(paste("r+s Cum.distance: ",sum(abs(best$stred))))
print(paste("r+s Average SNR: ",mean(best$rollmean_snr)))

a <- sum(best$inside)
print(a)

graph_hit = expression("A"["5,r+s"]~"="~"43 %")
graph_cum = expression("C"["r+s"]~"="~"12046")
ggplot(data=best, aes(x=stred,y=rollmean_snr, color=as.factor(color))) + 
  geom_point() + 
  theme_bw() +
  xlim(c(-150,150)) +
  ylim(c(0.5,4.2)) +
  labs(y=expression("running mean Signal to Noise ratio"[]), fill=graph_hit, color=expression("x"["i"]), x="Distance from the expected", title = "ORB") +
  scale_color_manual(values=c("#d7191c", "#2c7bb6")) +
  scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  #annotate("label", x=100, y=5.8, label= paste0(graph_hit,sum(best$inside),"/",nrow(best)),size=4) +
  annotate("text", x=110, y=3.8, label= graph_hit, size=4) +
  annotate("text", x=110, y=3.6, label= graph_cum, size=4) + 
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.85,0.1),
        legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=20,b=-20), hjust=0.05, face="plain", size=12)
  )

orb_rs <- best
#####################################################################################

patient1 = list.files(path = "../data/methods_testing/orb/preprocessing/1-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/orb/preprocessing/2-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/orb/preprocessing/3-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/orb/preprocessing/4-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/orb/preprocessing/5-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/orb/preprocessing/6-7/min_rotation_scale_sculp_equalize/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

data_diff <- NULL
#means <- NULL
for (i in 1:length(patient1)){
  filename1 = patient1[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  data1 = read.table(filename1,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  colnames(data1) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  
  data1$snr  <- (data1$n_matches  - mean(data1$n_matches)) /sd(data1$n_matches)
  data1$type <- 1
  data2$snr  <- (data2$n_matches  - mean(data2$n_matches)) /sd(data2$n_matches)
  data2$type <- 2
  data3$snr  <- (data3$n_matches  - mean(data3$n_matches)) /sd(data3$n_matches)
  data3$type <- 3
  data4$snr  <- (data4$n_matches  - mean(data4$n_matches)) /sd(data4$n_matches)
  data4$type <- 4
  data5$snr  <- (data5$n_matches  - mean(data5$n_matches)) /sd(data5$n_matches)
  data5$type <- 5
  data6$snr  <- (data6$n_matches  - mean(data6$n_matches)) /sd(data6$n_matches)
  data6$type <- 6
  
  data_diff <- rbind(data_diff,data1, data2, data3, data4, data5, data6)
}

means <- NULL #data.frame(rollmean_snr=NULL)
for (i in 1:170){
  for (j in 1:6){
    filter <- data_diff[(data_diff$test_z == i) & (data_diff$type==j),]
    temp <- rollmean(filter$snr, k = window, fill=NA, align=c("center"))
    filter$rollmean_snr <- temp
    means <- rbind(means,filter) #append(means,temp)
  }
}

###bacha na sort!!
final <- NULL
for (i in 1:6){
  filter <- means %>% filter(type==i) %>% group_by(test_z) %>% filter(test_z>2) %>% filter(test_z<169) %>% slice(which.max(rollmean_snr))
  filter$stred <- filter$ref_z - filter$test_z
  #print(c(i,getmode(filter$stred)))
  filter$stred <- filter$stred - median(filter$stred)
  
  final <- rbind(final, filter)
}

best <- final %>% filter(test_z>img_start) %>% filter(test_z<=img_end) #%>% filter(type==2) #as.data.frame(final)

best$color <- ifelse(( (best$stred>=-5) & (best$stred<=5)),"1","0")
best$inside <- ifelse(( (best$stred>=-5) & (best$stred<=5)),1,0)
print(paste("R+e+b+s, Accuracy",sum(best$inside)/nrow(best)))
print(paste("R+e+b+s Cum.distance: ",sum(abs(best$stred))))
print(paste("R+e+b+s Average SNR: ",mean(best$snr)))

a <- sum(best$inside)
print(a)

graph_hit = expression("A"["5,r+e+b+s"]~"="~"33 %")
graph_cum = expression("C"["r+e+b+s"]~"="~"20342")
ggplot(data=best, aes(x=stred,y=rollmean_snr, color=as.factor(color))) + 
  geom_point() + 
  theme_bw() +
  xlim(c(-150,150)) +
  ylim(c(0.5,4.2)) +
  labs(y=expression("running mean Signal to Noise ratio"[]), fill=graph_hit, color=expression("x"["i"]), x="Distance from the expected", title = "ORB") +
  scale_color_manual(values=c("#d7191c", "#2c7bb6")) +
  scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  #annotate("label", x=100, y=5.8, label= paste0(graph_hit,sum(best$inside),"/",nrow(best)),size=4) +
  annotate("text", x=110, y=3.8, label= graph_hit, size=4) +
  annotate("text", x=110, y=3.6, label= graph_cum, size=4) + 
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.85,0.1),
        legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=20,b=-20), hjust=0.05, face="plain", size=12)
  )

orb_rebs <- best
###################################################################################################################

patient1 = list.files(path = "../data/methods_testing/orb/preprocessing/1-7/min_rotation_scale_sculp", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/orb/preprocessing/2-7/min_rotation_scale_sculp", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/orb/preprocessing/3-7/min_rotation_scale_sculp", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/orb/preprocessing/4-7/min_rotation_scale_sculp", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/orb/preprocessing/5-7/min_rotation_scale_sculp", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/orb/preprocessing/6-7/min_rotation_scale_sculp", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

data_diff <- NULL
#means <- NULL
for (i in 1:length(patient1)){
  filename1 = patient1[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  data1 = read.table(filename1,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  colnames(data1) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  
  data1$snr  <- (data1$n_matches  - mean(data1$n_matches)) /sd(data1$n_matches)
  data1$type <- 1
  data2$snr  <- (data2$n_matches  - mean(data2$n_matches)) /sd(data2$n_matches)
  data2$type <- 2
  data3$snr  <- (data3$n_matches  - mean(data3$n_matches)) /sd(data3$n_matches)
  data3$type <- 3
  data4$snr  <- (data4$n_matches  - mean(data4$n_matches)) /sd(data4$n_matches)
  data4$type <- 4
  data5$snr  <- (data5$n_matches  - mean(data5$n_matches)) /sd(data5$n_matches)
  data5$type <- 5
  data6$snr  <- (data6$n_matches  - mean(data6$n_matches)) /sd(data6$n_matches)
  data6$type <- 6
  
  data_diff <- rbind(data_diff,data1, data2, data3, data4, data5, data6)
}

means <- NULL #data.frame(rollmean_snr=NULL)
for (i in 1:170){
  for (j in 1:6){
    filter <- data_diff[(data_diff$test_z == i) & (data_diff$type==j),]
    temp <- rollmean(filter$snr, k = window, fill=NA, align=c("center"))
    filter$rollmean_snr <- temp
    means <- rbind(means,filter) #append(means,temp)
  }
}

###bacha na sort!!
final <- NULL
for (i in 1:6){
  filter <- means %>% filter(type==i) %>% group_by(test_z) %>% filter(test_z>2) %>% filter(test_z<169) %>% slice(which.max(rollmean_snr))
  filter$stred <- filter$ref_z - filter$test_z
  #print(c(i,getmode(filter$stred)))
  filter$stred <- filter$stred - median(filter$stred)
  
  final <- rbind(final, filter)
}

best <- final %>% filter(test_z>img_start) %>% filter(test_z<=img_end) #%>% filter(type==2) #as.data.frame(final)

best$color <- ifelse(( (best$stred>=-5) & (best$stred<=5)),"1","0")
best$inside <- ifelse(( (best$stred>=-5) & (best$stred<=5)),1,0)
print(paste("R+b+s, Accuracy",sum(best$inside)/nrow(best)))
print(paste("R+b+s Cum.distance: ",sum(abs(best$stred))))
print(paste("R+b+s Average SNR: ",mean(best$snr)))

a <- sum(best$inside)
print(a)

graph_hit = expression("A"["5,r+b+s"]~"="~"8 %")
graph_cum = expression("C"["r+b+s"]~"="~"25910")
ggplot(data=best, aes(x=stred,y=rollmean_snr, color=as.factor(color))) + 
  geom_point() + 
  theme_bw() +
  xlim(c(-150,150)) +
  ylim(c(0.5,4.2)) +
  labs(y=expression("running mean Signal to Noise ratio"[]), fill=graph_hit, color=expression("x"["i"]), x="Distance from the expected", title = "ORB") +
  scale_color_manual(values=c("#d7191c", "#2c7bb6")) +
  scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  #annotate("label", x=100, y=5.8, label= paste0(graph_hit,sum(best$inside),"/",nrow(best)),size=4) +
  annotate("text", x=110, y=3.8, label= graph_hit, size=4) +
  annotate("text", x=110, y=3.6, label= graph_cum, size=4) + 
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.85,0.1),
        legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=20,b=-20), hjust=0.05, face="plain", size=12)
  )

orb_rbs <- best

#######################################################################################################
patient1 = list.files(path = "../data/methods_testing/orb/preprocessing/1-7/min-rot-sculp/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/orb/preprocessing/2-7/min-rot-sculp/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/orb/preprocessing/3-7/min-rot-sculp/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/orb/preprocessing/4-7/min-rot-sculp/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/orb/preprocessing/5-7/min-rot-sculp/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/orb/preprocessing/6-7/min-rot-sculp/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

data_diff <- NULL
#means <- NULL
for (i in 1:length(patient1)){
  filename1 = patient1[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  data1 = read.table(filename1,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  colnames(data1) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  
  data1$snr  <- (data1$n_matches  - mean(data1$n_matches)) /sd(data1$n_matches)
  data1$type <- 1
  data2$snr  <- (data2$n_matches  - mean(data2$n_matches)) /sd(data2$n_matches)
  data2$type <- 2
  data3$snr  <- (data3$n_matches  - mean(data3$n_matches)) /sd(data3$n_matches)
  data3$type <- 3
  data4$snr  <- (data4$n_matches  - mean(data4$n_matches)) /sd(data4$n_matches)
  data4$type <- 4
  data5$snr  <- (data5$n_matches  - mean(data5$n_matches)) /sd(data5$n_matches)
  data5$type <- 5
  data6$snr  <- (data6$n_matches  - mean(data6$n_matches)) /sd(data6$n_matches)
  data6$type <- 6
  
  data_diff <- rbind(data_diff,data1, data2, data3, data4, data5, data6)
}

means <- NULL #data.frame(rollmean_snr=NULL)
for (i in 1:170){
  for (j in 1:6){
    filter <- data_diff[(data_diff$test_z == i) & (data_diff$type==j),]
    temp <- rollmean(filter$snr, k = window, fill=NA, align=c("center"))
    filter$rollmean_snr <- temp
    means <- rbind(means,filter) #append(means,temp)
  }
}

###bacha na sort!!
final <- NULL
for (i in 1:6){
  filter <- means %>% filter(type==i) %>% group_by(test_z) %>% filter(test_z>2) %>% filter(test_z<169) %>% slice(which.max(rollmean_snr))
  filter$stred <- filter$ref_z - filter$test_z
  # print(c(i,getmode(filter$stred)))
  filter$stred <- filter$stred - median(filter$stred)
  
  final <- rbind(final, filter)
}

best <- final %>% filter(test_z>img_start) %>% filter(test_z<=img_end) #%>% filter(type==2) #as.data.frame(final)

best$color <- ifelse(( (best$stred>=-5) & (best$stred<=5)),"1","0")
best$inside <- ifelse(( (best$stred>=-5) & (best$stred<=5)),1,0)
print(paste("R+b, Accuracy",sum(best$inside)/nrow(best)))
print(paste("R+b Cum.distance: ",sum(abs(best$stred))))
print(paste("R+b Average SNR: ",mean(best$snr)))

a <- sum(best$inside)
print(a)

graph_hit = expression("A"["5,r+b"]~"="~"43 %")
graph_cum = expression("C"["r+b"]~"="~"12385")
ggplot(data=best, aes(x=stred,y=rollmean_snr, color=as.factor(color))) + 
  geom_point() + 
  theme_bw() +
  xlim(c(-150,150)) +
  ylim(c(0.5,4.2)) +
  labs(y=expression("running mean Signal to Noise ratio"[]), fill=graph_hit, color=expression("x"["i"]), x="Distance from the expected", title = "ORB") +
  scale_color_manual(values=c("#d7191c", "#2c7bb6")) +
  scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  #annotate("label", x=100, y=5.8, label= paste0(graph_hit,sum(best$inside),"/",nrow(best)),size=4) +
  annotate("text", x=110, y=3.8, label= graph_hit, size=4) +
  annotate("text", x=110, y=3.6, label= graph_cum, size=4) + 
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.15,0.1),
        legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=20,b=-20), hjust=0.05, face="plain", size=12)
  )

orb_rb <- best
#######################################################################################################
patient1 = list.files(path = "../data/methods_testing/orb/preprocessing/1-7/min-rot/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/orb/preprocessing/2-7/min-rot/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/orb/preprocessing/3-7/min-rot/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/orb/preprocessing/4-7/min-rot/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/orb/preprocessing/5-7/min-rot/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/orb/preprocessing/6-7/min-rot/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

data_diff <- NULL
#means <- NULL
for (i in 1:length(patient1)){
  filename1 = patient1[i]
  filename2 = patient2[i]
  filename3 = patient3[i]
  filename4 = patient4[i]
  filename5 = patient5[i]
  filename6 = patient6[i]
  data1 = read.table(filename1,header = FALSE, fill = TRUE, sep = " ")
  data2 = read.table(filename2,header = FALSE, fill = TRUE, sep = " ")
  data3 = read.table(filename3,header = FALSE, fill = TRUE, sep = " ")
  data4 = read.table(filename4,header = FALSE, fill = TRUE, sep = " ")
  data5 = read.table(filename5,header = FALSE, fill = TRUE, sep = " ")
  data6 = read.table(filename6,header = FALSE, fill = TRUE, sep = " ")
  colnames(data1) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data2) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data3) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data4) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data5) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  colnames(data6) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
  
  data1$snr  <- (data1$n_matches  - mean(data1$n_matches)) /sd(data1$n_matches)
  data1$type <- 1
  data2$snr  <- (data2$n_matches  - mean(data2$n_matches)) /sd(data2$n_matches)
  data2$type <- 2
  data3$snr  <- (data3$n_matches  - mean(data3$n_matches)) /sd(data3$n_matches)
  data3$type <- 3
  data4$snr  <- (data4$n_matches  - mean(data4$n_matches)) /sd(data4$n_matches)
  data4$type <- 4
  data5$snr  <- (data5$n_matches  - mean(data5$n_matches)) /sd(data5$n_matches)
  data5$type <- 5
  data6$snr  <- (data6$n_matches  - mean(data6$n_matches)) /sd(data6$n_matches)
  data6$type <- 6
  
  data_diff <- rbind(data_diff,data1, data2, data3, data4, data5, data6)
}

means <- NULL #data.frame(rollmean_snr=NULL)
for (i in 1:170){
  for (j in 1:6){
    filter <- data_diff[(data_diff$test_z == i) & (data_diff$type==j),]
    temp <- rollmean(filter$snr, k = window, fill=NA, align=c("center"))
    filter$rollmean_snr <- temp
    means <- rbind(means,filter) #append(means,temp)
  }
}

###bacha na sort!!
final <- NULL
for (i in 1:6){
  filter <- means %>% filter(type==i) %>% group_by(test_z) %>% filter(test_z>2) %>% filter(test_z<169) %>% slice(which.max(rollmean_snr))
  filter$stred <- filter$ref_z - filter$test_z
  #print(c(i,getmode(filter$stred)))
  filter$stred <- filter$stred - median(filter$stred)
  final <- rbind(final, filter)
}

best <- final %>% filter(test_z>img_start) %>% filter(test_z<=img_end) #%>% filter(type==2) #as.data.frame(final)

best$color <- ifelse(( (best$stred>=-5) & (best$stred<=5)),"1","0")
best$inside <- ifelse(( (best$stred>=-5) & (best$stred<=5)),1,0)
print(paste("R, Accuracy",sum(best$inside)/nrow(best)))
print(paste("R Cum.distance: ",sum(abs(best$stred))))
print(paste("R Average SNR: ",mean(best$snr)))

a <- sum(best$inside)
print(a)

graph_hit = expression("A"["5,r"]~"="~"41 %")
graph_cum = expression("C"["r"]~"="~"12133")
ggplot(data=best, aes(x=stred,y=rollmean_snr, color=as.factor(color))) + 
  geom_point() + 
  theme_bw() +
  xlim(c(-150,150)) +
  ylim(c(0.5,4.2)) +
  labs(y=expression("running mean Signal to Noise ratio"[]), fill=graph_hit, color=expression("x"["i"]), x="Distance from the expected", title = "ORB") +
  scale_color_manual(values=c("#d7191c", "#2c7bb6")) +
  scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  #annotate("label", x=100, y=5.8, label= paste0(graph_hit,sum(best$inside),"/",nrow(best)),size=4) +
  annotate("text", x=110, y=3.8, label= graph_hit, size=4) +
  annotate("text", x=110, y=3.6, label= graph_cum, size=4) + 
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.85,0.1),
        legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=20,b=-20), hjust=0.05, face="plain", size=12)
  )

orb_r <- best

###################################################
require(cowplot)

orb_none_6 <- orb_none %>% filter(type==6)
p1 <- ggplot(data=orb_none_6, aes(x=test_z,y=1,fill=as.factor(color))) +
  geom_tile() + 
  labs(y="None", x="image ID", fill=expression("x"["i"]),title="ORB – subject 6") +
  scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA),
        text=element_text(family="Times New Roman"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        #plot.margin = unit(c(0,0,0,0), "cm")
  )

orb_r_6 <- orb_r %>% filter(type==6)
p2 <- ggplot(data=orb_r_6, aes(x=test_z,y=1,fill=as.factor(color))) +
  geom_tile() + 
  labs(y="r", x="image ID", fill=expression("x"["i"])) +
  scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA),
        text=element_text(family="Times New Roman"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0,0,0,0), "cm")
  )

orb_rb_6 <- orb_rb %>% filter(type==6)
p3 <- ggplot(data=orb_rb_6, aes(x=test_z,y=1,fill=as.factor(color))) +
  geom_tile() + 
  labs(y="r+b", x="image ID", fill=expression("x"["i"])) +
  scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA),
        text=element_text(family="Times New Roman"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0,0,0,0), "cm")
  )

orb_rs_6 <- orb_rs %>% filter(type==6)
p4 <- ggplot(data=orb_rs, aes(x=test_z,y=1,fill=as.factor(color))) +
  geom_tile() + 
  labs(y="r+s", x="image ID", fill=expression("x"["i"])) +
  scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA),
        text=element_text(family="Times New Roman"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0,0,0,0), "cm")
  )

orb_rbs_6 <- orb_rbs %>% filter(type==6)
p5 <- ggplot(data=orb_rbs_6, aes(x=test_z,y=1,fill=as.factor(color))) +
  geom_tile() + 
  labs(y="r+b+s", x="image ID", fill=expression("x"["i"])) +
  scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA),
        text=element_text(family="Times New Roman"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0,0,0,0), "cm")
  )

orb_rebs_6 <- orb_rebs %>% filter(type==6)
p6 <- ggplot(data=orb_rebs_6, aes(x=test_z,y=1,fill=as.factor(color))) +
  geom_tile() + 
  labs(y="r+e+b+s", x="image ID", fill=expression("x"["i"])) +
  scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA),
        text=element_text(family="Times New Roman"),
        #axis.ticks.x = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0,0,0,0), "cm")
  )

orb_eq_6 <- orb_eq %>% filter(type==6)
p7 <- ggplot(data=orb_eq_6, aes(x=test_z,y=1,fill=as.factor(color))) +
  geom_tile() + 
  labs(y="e", x="image ID", fill=expression("x"["i"])) +
  scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA),
        text=element_text(family="Times New Roman"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")
  )

require(egg)
#plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(1/2, 1/2))
ggarrange(p1,p2,p3,p4,p5,p7,p6,nrow = 7)

