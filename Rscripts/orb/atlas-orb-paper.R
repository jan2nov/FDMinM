rm(list = ls())
library(ggplot2)
library(dplyr)
require(zoo)

patient1 = list.files(path = "../data/methods_testing/orb/atlas/1-7/brainweb-orig/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../data/methods_testing/orb/atlas/2-7/brainweb-orig/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../data/methods_testing/orb/atlas/3-7/brainweb-orig/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../data/methods_testing/orb/atlas/4-7/brainweb-orig/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../data/methods_testing/orb/atlas/5-7/brainweb-orig/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../data/methods_testing/orb/atlas/6-7/brainweb-orig/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../data/methods_testing/orb/atlas/7-7/brainweb-orig/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

img_start = 20
img_end = 150
window = 7

data_diff <- NULL
#means <- NULL
for (i in 1:length(patient1)){
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
  data7$snr  <- (data7$n_matches  - mean(data7$n_matches)) /sd(data7$n_matches)
  data7$type <- 7
  
  data_diff <- rbind(data_diff,data1, data2, data3, data4, data5, data6, data7)
  #temp <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),mean)
  #temp2 <- data_diff %>% group_by(test_z,ref_z) %>% summarise_at(vars("snr"),sd)
  #temp$sd <- temp2$snr
  #temp$ref_id <- i
  #means <- rbind(means,temp)
}

#data_diff$rollmean_snr <- rollmean(data_diff$snr, k = window,fill=NA, align = c("center"))
means <- NULL #data.frame(rollmean_snr=NULL)
for (i in 1:170){
  for (j in 1:7){
    filter <- data_diff[(data_diff$test_z == i) & (data_diff$type==j),]
    temp <- rollmean(filter$snr, k = window, fill=NA, align=c("center"))
    filter$rollmean_snr <- temp
    means <- rbind(means,filter) #append(means,temp)
  }
}

test <- means %>% filter(type==3) %>% filter(test_z==100)
plot(test$snr, type="o")
points(test$rollmean_snr, col="red")
lines(test$rollmean_snr, col="red")

###bacha na sort!!
final <- NULL
for (i in 1:7){
  filter <- means %>% filter(type==i) %>% group_by(test_z) %>% filter(test_z>3) %>% filter(test_z<168) %>% slice(which.max(rollmean_snr))
  filter$stred <- filter$ref_z - filter$test_z
  print(c(i,median(filter$stred)))
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
graph_hit = expression("A"["5,none"]~"="~"12 %")
graph_cum = expression("C"["none"]~"="~"26804")
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
