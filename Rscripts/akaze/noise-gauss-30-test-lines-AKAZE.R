rm(list = ls())
library(ggplot2)
library(dplyr)

#list_data = list.files(path = "../source/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
lengths <- NULL
patient = list.files(path = "../data/methods_testing/akaze/noise/gauss/30/7-7/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
lengths <- c(lengths,length(patient))
patient2 = list.files(path = "../data/methods_testing/akaze/noise/gauss/30/6-6/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
lengths <- c(lengths,length(patient2))
patient3 = list.files(path = "../data/methods_testing/akaze/noise/gauss/30/5-5/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
lengths <- c(lengths,length(patient3))
patient4 = list.files(path = "../data/methods_testing/akaze/noise/gauss/30/4-4/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
lengths <- c(lengths,length(patient4))
patient5 = list.files(path = "../data/methods_testing/akaze/noise/gauss/30/3-3/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
lengths <- c(lengths,length(patient5))
patient6 = list.files(path = "../data/methods_testing/akaze/noise/gauss/30/2-2/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
lengths <- c(lengths,length(patient6))
patient7 = list.files(path = "../data/methods_testing/akaze/noise/gauss/30/1-1/", pattern = ".txt",include.dirs = TRUE,full.names = TRUE)
lengths <- c(lengths,length(patient7))

dimensions <- length(patient)

patient <- c(patient, patient2, patient3, patient4, patient5, patient6, patient7)
# results <- matrix(nrow=dimensions, ncol=dimensions)
# a_results <- matrix(nrow=dimensions, ncol=dimensions)
# 
# for(i in 1:length(patient)){
#   filename = patient[i] 
#   data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
#   colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
#   matches <- data$n_matches
#   results[,i] <- matches
#   a_results[,i] <- matches/data$keypoints_refImage
# }

mean_number_keypoints <- data.frame(x=NULL, id=NULL)
for (j in 1:170){
  for (i in 1:length(lengths)){
    filename = patient[(i-1)*170 + j]
    data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
    colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
    add_line <- data.frame(id_image=data[1,]$test_z,id_pat=i,x=data[1,]$keypoints_refImage)  
    mean_number_keypoints <- rbind(mean_number_keypoints,add_line)
  }
}
n_keypoints <- do.call("data.frame", aggregate(mean_number_keypoints$x,list(y=mean_number_keypoints$id_image),function(x) c(mean=round(mean(x)),sd=round(sd(x)))))
#with(mean_number_keypoints,tapply(x, id, mean))
#n_keypoints <- data.frame(id=seq(1,length(n_keypoints)),number = round(n_keypoints))
colnames(n_keypoints) <- c("id", "number", "sdeviation")
write.table(n_keypoints, "gauss-30-akaze_n_keypoints.dat", append = FALSE, sep = " ",dec = ".",
            row.names = FALSE, col.names = TRUE, quote = FALSE)


number <- length(patient)%/%dimensions
results2 <- data.frame()
for(j in 1:number){
  print(j)
  ifelse(j>1,start <- (j-1)*lengths[j-1], start<-0)
  for(i in 1:lengths[j]){
    filename = patient[i+(start)] 
    data = read.table(filename,header = FALSE, fill = TRUE, sep = " ")
    colnames(data) <- c("test_z", "ref_z", "keypoints_refImage", "keypoints_testedImage", "n_matches")
    matches <- data$n_matches
    percentage <- matches*100/data$keypoints_testedImage
    for(k in 1:length(matches)){
      add_line <- c(i,k,matches[k], percentage[k],j)
      results2 <- rbind(results2,add_line)
    }
  }
}

colnames(results2) <- c("x","y","n", "percentage", "test_id")
# colnames(results3) <- c("x","y","n", "percentage")

test <- do.call("data.frame", aggregate(results2$percentage,list(y=results2$y,x=results2$x),function(x) c(mean=mean(x),sd=sd(x))))
colnames(test) <- c("y","x", "percentage_mean", "percentage_sd")

write.table(test, "gauss-30-akaze_results.dat", append = FALSE, sep = " ",dec = ".",
          row.names = FALSE, col.names = TRUE, quote = FALSE)

temp <- results2 %>% filter( (x==25) | (x==50) | (x==100) | (x==150))
temp3 <- test %>% filter( (x==25) | (x==50) | (x==100) | (x==150))
# temp2 <- results3 %>% filter( (x==25) | (x==50) | (x==100) | (x==150))
means <- with(temp,tapply(percentage, x, mean))
means_all <- with(results2,tapply(percentage, x, median))


ggplot(temp, aes(x=y, y=percentage,color=as.factor(x))) +
  geom_line() + geom_point() + 
  labs(colour = "Image #", x = "Image ID = position [1mm]", y="Number of matches [%]", title = "Comparing images -- same patient")

# ggplot(temp2, aes(x=y, y=percentage,color=as.factor(x))) +
#   geom_line() + geom_point() + 
#   labs(colour = "Image #", x = "Image ID = position [1mm]", y="Number of matches [%]", title = "Comparing images -- different patient")

ggplot(temp3, aes(x=y, 
                  y=percentage_mean,
                  #                  color=as.factor(x), 
                  ymin=(percentage_mean-percentage_sd), 
                  ymax=(percentage_mean+percentage_sd),
                  fill=as.factor(x)
)
) +
  geom_line() + 
  geom_point() + 
  geom_ribbon(alpha=0.5) + 
  coord_cartesian(ylim = c(-1,101)) + 
  labs(colour = "Image #", fill= "Image #", x = "Image ID = position [1mm]", y="Number of matches [%]", title = "Method: AKAZE, comparing images -- same patient; Gauss -- 30 STD")

#heatmap(results,Colv = NA, Rowv = NA, scale="row")

own_snr <- function (x,m,s) {(x-m)/s}
snr <- results2 %>% filter(x==1) %>% select(n) %>% summarize(mean=mean(n), sd=sd(n), snr=own_snr(n,mean(n),sd(n)))

hist <- ggplot(data=n_keypoints, aes(x = id, y = number)) +
  geom_line() +
  #geom_bar(stat="identity", fill="steelblue", width = 0.5) +
  #geom_text(aes(label=number), vjust=1.6, color="white", size=3.5) +
  coord_cartesian(ylim = c(-1,max(n_keypoints$number)+10)) + 
  labs(y = "Average number of keypoints", x = "Image ID")
hist

