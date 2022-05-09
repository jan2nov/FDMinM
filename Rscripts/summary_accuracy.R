rm(list = ls())

require(ggplot2)
library(ggrepel)

names <- c("None", "R", "R+B", "R+S", "R+B+S", "E", "R+E+B+S")

akaze_acc <- c(67,70,72,67,73,87,82)
akaze_cd <- c(9083,8639,6888,10233,4315,3149,3272)
akaze_snr <- c(2.26,2.23,2.2,2.09,2.27,2.74,2.76)
akaze <- data.frame(names=names, acc=as.numeric(akaze_acc), cd=as.numeric(akaze_cd), snr=as.numeric(akaze_snr))
akaze$fda <- "AKAZE"

brisk_acc <- c(58,49,57,45,11,63,7)
brisk_cd <- c(9925,15063,9276,13299,23708,8383,25774)
brisk_snr <- c(1.35,1.34,1.57,1.64,1.94,1.49,1.61)
brisk <- data.frame(names=names, acc=as.numeric(brisk_acc), cd=as.numeric(brisk_cd), snr=as.numeric(brisk_snr))
brisk$fda <- "BRISK"

orb_acc <- c(57,41,43,45,8,58,33)
orb_cd <- c(7610,12133,12046,13299,25910,10748,20342)
orb_snr <- c(1.2,1.16,1.58,1.64,1.92,1.18,1.44)
orb <- data.frame(names=names, acc=as.numeric(orb_acc), cd=as.numeric(orb_cd), snr=as.numeric(orb_snr))
orb$fda <- "ORB"

gftt_acc <- c(81,83,87,68,70,53,73)
gftt_cd <- c(2616,2373,2075,9727,4128,17027,5760)
gftt_snr <- c(2.46,2.47,4.16,3.12,3.51,2.71,3.56)
gftt <- data.frame(names=names, acc=as.numeric(gftt_acc), cd=as.numeric(gftt_cd), snr=as.numeric(gftt_snr))
gftt$fda <- "GFTT"

agast_acc <- c(72,78,65,68,65,78,74)
agast_cd <- c(3396,4939,9077,4013,7099,2854,4538)
agast_snr <- c(2.93,2.64,3.19,2.78,2.94,3.12,3.2)
agast <- data.frame(names=names, acc=as.numeric(agast_acc), cd=as.numeric(agast_cd), snr=as.numeric(agast_snr))
agast$fda <- "AGAST"

hardnet_acc <- c(90,91,89,88,89,89,93)
hardnet_cd <- c(2000,1918,1934,2045,1923,2062,1648)
hardnet_snr <- c(2.24,2.32,2.02,2.56,2.25,3.11,3.53)
hardnet <- data.frame(names=names, acc=as.numeric(hardnet_acc), cd=as.numeric(hardnet_cd), snr=as.numeric(hardnet_snr))
hardnet$fda <- "HardNet"
names <- c("None", "R", "R+B", "R+S", "R+B+S", "E", "R+E+B+S")


all <- rbind(akaze, brisk, orb, gftt, agast, hardnet)

max_acc <- function(x,d){
  (780*6 - as.integer(x/d) )/46.8
}
lin <- data.frame(x=seq(0,30000), y=max_acc(seq(0,30000),6))

filter_all <- all # %>% filter(names!="E")
# #66a61e #e6ab02
all <- ggplot(data=filter_all, aes(x=cd,y=acc, fill=fda, color=fda)) + 
  geom_point(aes(size=snr)) +
  #geom_line(data=lin, aes(x=x,y=y, color="black", fill=x)) + 
  labs(x="Cumulative distance", y="Accuracy", color="FD\nalgorithm", fill="FD\nalgorithm", size="SNR") +
  theme_bw() +
  #guides(size= FALSE) + 
  ylim(0,100) +
  xlim(0,30000) + 
  scale_color_manual(values=c("#d95f02", "#1b9e77","#7570b3", "#e7298a", "#66a61e", "#e6ab02")) +
  scale_fill_manual(values=c("#d95f02", "#1b9e77","#7570b3", "#e7298a", "#66a61e", "#e6ab02")) +
  geom_label_repel(aes(label = names, fill=NULL),
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50'
  ) +
  theme(
    text=element_text(family="Times New Roman")
  )
all
ggsave("./FD_SNR_summary.pdf", plot = all, width = 12, height = 4, dpi=300, device=cairo_pdf)

#ggplot(data=all, aes(x=acc, y=snr, fill=fda, color=fda)) + 
#  geom_point()

  