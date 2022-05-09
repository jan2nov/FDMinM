rm(list = ls())
library(ggplot2)
library(dplyr)
require(tigerstats)
require(gridExtra)
require(cowplot)


akaze_identity          <- read.table("akaze-snr-orig-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_identity_diff     <- read.table("akaze-snr-orig-x-7.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_identity$id <- "None"
#akaze_identity_diff$id <- "orig"
akaze_identity$robustness <- akaze_identity$snr/akaze_identity$snr
#akaze_identity_diff$robustness <- akaze_identity_diff$snr/akaze_identity$snr

ak <- akaze_identity[akaze_identity$ref_id==100,]
ak_diff <- akaze_identity_diff[akaze_identity_diff$ref_id==100,]

akaze_rotation          <- read.table("akaze-snr-rot05-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_rotation_diff     <- read.table("akaze-snr-rot05-x-7.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_rotation$id <- "Rotation 5°"
akaze_rotation$robustness <- akaze_rotation$snr/akaze_identity$snr

akaze_scale             <- read.table("akaze-snr-scale05-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_scale_diff        <- read.table("akaze-snr-scale05-x-7.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_scale$id <- "Upscale 5 %"
akaze_scale$robustness <- akaze_scale$snr/akaze_identity$snr

akaze_gauss30           <- read.table("akaze-snr-gauss30-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_gauss30_diff      <- read.table("akaze-snr-gauss30-x-7.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_gauss30$id <- "Gaussian noise 30 STD"
akaze_gauss30$robustness <- akaze_gauss30$snr/akaze_identity$snr

akaze_allmax            <- read.table("akaze-snr-allmax-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_allmax_diff       <- read.table("akaze-snr-allmax-x-7.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_allmax$id <- "All"
akaze_allmax$robustness <- akaze_allmax$snr/akaze_identity$snr

akaze_intensity     <- read.table("akaze-snr-intensity-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
akaze_intensity$type <- "7-7"
akaze_intensity$ref_id <- 7
akaze_intensity$id <- "Intensity"

#akaze_identity_diff$id <- "orig"
akaze_intensity$robustness <- akaze_intensity$snr/akaze_identity$snr

#akaze_all <- rbind(akaze_allmax, akaze_identity, akaze_scale, akaze_gauss30, akaze_rotation)
akaze_all <- rbind(akaze_identity, akaze_scale, akaze_gauss30, akaze_rotation, akaze_intensity)
results <- akaze_all %>% filter(test_z == ref_z)
temp <- results %>% group_by(id) %>% summarise_at(vars("robustness"),mean)
print(paste("AKAZE:", temp, mean(temp[c(1,3,4),]$robustness)))

ggplot(data=results, aes(x=ref_z,y=snr, group=id,color=id)) + 
  geom_point() + 
  geom_line()

ggplot(data=results, aes(x=ref_z,y=robustness, group=id,color=id, fill=id), size=1.2) + 
  scale_color_manual(values=c("#e6550d", "#54278f","#000000" ,"#3182bd","#31a354")) +
  scale_fill_manual(values=c("#e6550d", "#54278f", "#000000","#3182bd","#31a354")) +
  geom_smooth(method = "lm", formula = y ~ poly(x,6)) +
  coord_cartesian(ylim = c(0.0,1.01)) + 
  labs(x="image ID", y=expression("Robustness R"[x]), color="AKAZE\nImage degradation", fill="AKAZE\nImage degradation") + 
  theme_bw() + 
  guides(fill=guide_legend(ncol=2), color=guide_legend(ncol=2)) +
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.32,0.15),
        #legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=25,b=-25), hjust=0.05, face="plain", size=12))


###################################################
#rm(list = ls())

orb_identity          <- read.table("orb-snr-orig-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
orb_identity_diff     <- read.table("orb-snr-orig-x-7.dat",header = TRUE, fill = TRUE, sep = " ")
orb_identity$id <- "None"
orb_identity$robustness <- orb_identity$snr/orb_identity$snr

orb_rotation          <- read.table("orb-snr-rot05-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
orb_rotation_diff     <- read.table("orb-snr-rot05-x-7.dat",header = TRUE, fill = TRUE, sep = " ")
orb_rotation$id <- "Rotation 5°"
orb_rotation$robustness <- orb_rotation$snr/orb_identity$snr

orb_scale             <- read.table("orb-snr-scale05-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
orb_scale_diff        <- read.table("orb-snr-scale05-x-7.dat",header = TRUE, fill = TRUE, sep = " ")
orb_scale$id <- "Upscale 5 %"
orb_scale$robustness <- orb_scale$snr/orb_identity$snr

orb_gauss30           <- read.table("orb-snr-gauss30-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
orb_gauss30_diff      <- read.table("orb-snr-gauss30-x-7.dat",header = TRUE, fill = TRUE, sep = " ")
orb_gauss30$id <- "Gaussian noise 30 STD"
orb_gauss30$robustness <- orb_gauss30$snr/orb_identity$snr

orb_allmax            <- read.table("orb-snr-allmax-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
orb_allmax_diff       <- read.table("orb-snr-allmax-x-7.dat",header = TRUE, fill = TRUE, sep = " ")
orb_allmax$id <- "All"
orb_allmax$robustness <- orb_allmax$snr/orb_identity$snr

#orb_all <- rbind(orb_allmax, orb_identity, orb_scale, orb_gauss30, orb_rotation)
orb_all <- rbind(orb_identity, orb_scale, orb_gauss30, orb_rotation)
results <- orb_all %>% filter(test_z == ref_z)

temp <- results %>% group_by(id) %>% summarise_at(vars("robustness"),mean)
print(paste("ORB:", temp, mean(temp[c(1,3,4),]$robustness)))

ggplot(data=results, aes(x=ref_z,y=snr, group=id,color=id)) + 
  geom_point() + 
  geom_line() + 
  labs(title="ORB", y="SNR", x="input image ID")

ggplot(data=results, aes(x=ref_z,y=robustness, group=id,color=id, fill=id), size=1.2) + 
  scale_color_manual(values=c("#e6550d", "#54278f", "#3182bd","#31a354")) +
  scale_fill_manual(values=c("#e6550d", "#54278f", "#3182bd","#31a354")) +
  geom_smooth(method = "lm", formula = y ~ poly(x,6)) +
  coord_cartesian(ylim = c(0.0,1.01)) + 
  labs(x="image ID", y=expression("Robustness R"[x]), color="ORB\nImage degradation", fill="ORB\nImage degradation") + 
  theme_bw() + 
  guides(fill=guide_legend(ncol=2), color=guide_legend(ncol=2)) +
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.32,0.15),
        #legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=25,b=-25), hjust=0.05, face="plain", size=12))

###################################################
#rm(list = ls())

brisk_identity          <- read.table("brisk-snr-orig-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
brisk_identity_diff     <- read.table("brisk-snr-orig-x-7.dat",header = TRUE, fill = TRUE, sep = " ")
brisk_identity$id <- "None"
brisk_identity$robustness <- brisk_identity$snr/brisk_identity$snr

brisk_rotation          <- read.table("brisk-snr-rot05-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
brisk_rotation_diff     <- read.table("brisk-snr-rot05-x-7.dat",header = TRUE, fill = TRUE, sep = " ")
brisk_rotation$id <- "Rotation 5°"
brisk_rotation$robustness <- brisk_rotation$snr/brisk_identity$snr

brisk_scale             <- read.table("brisk-snr-scale05-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
brisk_scale_diff        <- read.table("brisk-snr-scale05-x-7.dat",header = TRUE, fill = TRUE, sep = " ")
brisk_scale$id <- "Upscale 5 %"
brisk_scale$robustness <- brisk_scale$snr/brisk_identity$snr

brisk_gauss30           <- read.table("brisk-snr-gauss30-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
brisk_gauss30_diff      <- read.table("brisk-snr-gauss30-x-7.dat",header = TRUE, fill = TRUE, sep = " ")
brisk_gauss30$id <- "Gaussian noise 30 STD"
brisk_gauss30$robustness <- brisk_gauss30$snr/brisk_identity$snr

brisk_allmax            <- read.table("brisk-snr-allmax-7-7.dat",header = TRUE, fill = TRUE, sep = " ")
brisk_allmax_diff       <- read.table("brisk-snr-allmax-x-7.dat",header = TRUE, fill = TRUE, sep = " ")
brisk_allmax$id <- "All"
brisk_allmax$robustness <- brisk_allmax$snr/brisk_identity$snr

#brisk_all <- rbind(brisk_allmax, brisk_identity, brisk_scale, brisk_gauss30, brisk_rotation)
brisk_all <- rbind(brisk_identity, brisk_scale, brisk_gauss30, brisk_rotation)
results <- brisk_all %>% filter(test_z == ref_z)

temp <- results %>% group_by(id) %>% summarise_at(vars("robustness"),mean)
print(paste("BRISK:", temp, mean(temp[c(1,3,4),]$robustness)))

ggplot(data=results, aes(x=ref_z,y=snr, group=id,color=id)) + 
  geom_point() + 
  geom_line() + 
  labs(title="BRISK", y="SNR", x="input image ID")

ggplot(data=results, aes(x=ref_z,y=robustness, group=id,color=id, fill=id), size=1.2) + 
  scale_color_manual(values=c("#e6550d", "#54278f", "#3182bd","#31a354")) +
  scale_fill_manual(values=c("#e6550d", "#54278f", "#3182bd","#31a354")) +
  geom_smooth(method = "lm", formula = y ~ poly(x,6)) +
  coord_cartesian(ylim = c(0.0,1.01)) + 
  labs(x="image ID", y=expression("Robustness R"[x]), color="BRISK\nImage degradation", fill="BRISK\nImage degradation") + 
  theme_bw() + 
  guides(fill=guide_legend(ncol=2), color=guide_legend(ncol=2)) +
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.32,0.15),
        #legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=25,b=-25), hjust=0.05, face="plain", size=12))


b <- brisk_all %>% filter(test_z == ref_z) %>% filter(id=="All")
b$metod <- "BRISK"
a <- akaze_all %>% filter(test_z == ref_z) %>% filter(id=="All")
a$metod <- "AKAZE"
r <- orb_all %>% filter(test_z == ref_z) %>% filter(id=="All")
r$metod <- "ORB"

final <- rbind(a,b,r)

ggplot(data=final,aes(x=ref_z,y=robustness,group=metod,color=metod,fill=metod), size=1.2) + 
  #geom_point() + 
  geom_line() + 
  geom_smooth(method = "lm", formula = y ~ poly(x,6)) +
  labs(y="Robustness", x="input image ID") + 
  coord_cartesian(ylim = c(0,1.01))
#####################################
####################################

brisk_all <- brisk_all[is.finite(brisk_all$robustness),]
aggregate(brisk_all$robustness,list(brisk_all$id),FUN=mean,na.rm=TRUE,na.omit=TRUE)

x <- seq(-170, 170, by = 0.1)
ci <- qnorm(0.95,mean=0,sd=1) # 0.9+0.05
std <-10/(2*ci)
y <- dnorm(x, mean = 0, sd = std)
#y2 <- pnorm(x, mean = 100, sd = std)
plot(x,y)
#plot(x,y2)
pnormGC(c(-5,5),region="between",mean=0,
        sd=std,graph=TRUE)
tiles <- NULL

ak$z <- pnorm(ak$ref_z,mean=100,sd=std)
ak$color <- ifelse(( (ak$z>=0.05) & (ak$z<=0.95)),1,0)
data_tab <- data.frame(x=x,y=y)
ggplot(data=data_tab, aes(x=x,y=y)) + 
  geom_line() + 
  geom_point(data=ak,aes(x=ref_z,y=snr/60, color=color)) + 
  geom_line(data=ak,aes(x=ref_z,y=snr/60),color="red")

#aggregate(akaze_scale$snr,list(akaze_scale$ref_z),FUN=which.max,na.rm=TRUE,na.omit=TRUE)
sorted_data <- akaze_identity_diff %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best[95:105,]

p1 <- ggplot(data=data_tab, aes(x=x,y=60*y)) + 
  geom_line() + 
  geom_point(data=best,aes(x=stred,y=snr, color=as.factor(color))) +
  #geom_point(data=best,aes(x=stred,y=snr, color=ref_id)) + 
  labs(title="AKAZE - original; comparing with etalon", color="Inside", x="Distance from expected", y="") + 
  annotate("label", x=150, y=7.5, label= paste0("Hit: ",sum(best$inside),"/",nrow(best)),size=4) #+
  #theme(panel.background = element_rect(fill = NA,color="black"),
        #panel.grid.major = element_line(colour = "grey50"))
  #scale_color_gradientn(colours = rainbow(10))
  #geom_line(data=best,aes(x=stred,y=snr/60),color="red")


############
# sorted_data <- akaze_identity %>% group_by(ref_id) %>% slice(which.max(snr))
# best <- as.data.frame(sorted_data)
# best$stred <- best$ref_z-best$ref_id
# best$z <- pnorm(best$stred,mean=0,sd=std)
# best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
# best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
# best[95:105,]

#p1 <- ggplot(data=data_tab, aes(x=x,y=60*y)) + 
#  geom_line() + 
#  geom_point(data=best,aes(x=stred,y=snr, color=as.factor(color))) +
#  #geom_point(data=best,aes(x=stred,y=snr, color=ref_id)) + 
#  labs(title="AKAZE - original; same patient", color="Inside", x="Distance from expected") + 
#  annotate("text", x=150, y=7.5, label= paste0("Hit: ",sum(best$inside),"/",nrow(best))) #+ 
  #coord_cartesian(xlim = c(-10,10))
#scale_color_gradientn(colours = rainbow(10))
#geom_line(data=best,aes(x=stred,y=snr/60),color="red")

p2 <- ggplot(data=best, aes(x=ref_id,y=1,fill=as.factor(color))) +
  geom_tile() + 
  labs(y="", x="Input image ID", fill="Inside") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA)
        )
tiles <- data.frame(name="AKAZE", y=as.factor(best$color))  

# grid.arrange(p1, p2, ncol=1,heights = c(2, 1))

# gb1 <- ggplot_build(p1)
# gb2 <- ggplot_build(p2)
# n1 <- length(gb1$layout$ranges[[1]]$y.labels)
# n2 <- length(gb2$panel$ranges[[1]]$y.labels)
# gA <- ggplot_gtable(gb1)
# gB <- ggplot_gtable(gb2)
# g <- gtable:::rbind_gtable(gA, gB, "last")
# panels <- g$layout$t[grep("panel", g$layout$name)]
# g$heights[panels] <- list(unit(n1*5, "null"), unit(n2,"null"))
# grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
############# orb
sorted_data <- orb_identity_diff %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best[95:105,]

p1 <- ggplot(data=data_tab, aes(x=x,y=60*y)) + 
  geom_line() + 
  geom_point(data=best,aes(x=stred,y=snr, color=as.factor(color))) +
  #geom_point(data=best,aes(x=stred,y=snr, color=ref_id)) + 
  labs(title="ORB - original; comparing with etalon", color="Inside", x="Distance from expected", y="") + 
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
        panel.background = element_rect(fill = NA))

tiles <- rbind(tiles,data.frame(name="ORB", y=as.factor(best$color)))
        
plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
############# brisk
sorted_data <- brisk_identity_diff %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best[95:105,]
brisk_tile<-NULL

p1 <- ggplot(data=data_tab, aes(x=x,y=60*y)) + 
  geom_line() + 
  geom_point(data=best,aes(x=stred,y=snr, color=as.factor(color))) +
  #geom_point(data=best,aes(x=stred,y=snr, color=ref_id)) + 
  labs(title="BRISK - original; comparing with etalon", color="Inside", x="Distance from expected", y="") + 
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
        panel.background = element_rect(fill = NA))
tiles <- rbind(tiles,data.frame(name="BRISK", y=as.factor(best$color)))
brisk_tile <- rbind(brisk_tile,data.frame(name="orig", y=as.factor(best$color)))

plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))

ggplot(data=tiles, aes(x=rep(seq(1,170),3),y=name, fill=y)) + 
  geom_tile()


############## brisk gauss 30 ####
brisk_tile <- NULL
sorted_data <- brisk_gauss30_diff %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "Gauss 30 STD"
best[95:105,]
brisk_tile <- rbind(brisk_tile,data.frame(name="Gauss 30 STD", y=as.factor(best$color)))

result <- best

sorted_data <- brisk_identity_diff %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "ORIG"
result <- rbind(result,best)
brisk_tile <- rbind(brisk_tile,data.frame(name="Orig", y=as.factor(best$color)))

sorted_data <- brisk_scale_diff %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "scale"
result <- rbind(result,best)
brisk_tile <- rbind(brisk_tile,data.frame(name="Scale", y=as.factor(best$color)))

sorted_data <- brisk_rotation_diff %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "rotation"
brisk_tile <- rbind(brisk_tile,data.frame(name="Rotation", y=as.factor(best$color)))
result <- rbind(result,best)

sorted_data <- brisk_allmax_diff %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "all"
brisk_tile <- rbind(brisk_tile,data.frame(name="All", y=as.factor(best$color)))
result <- rbind(result,best)

tt <- aggregate(result$inside, by=list(Category=result$deg), FUN=sum)
tt <- data.frame(x=c(150,150,150,150,150),y=c(0.125,0.117,0.105,0.1,0.095),text = paste(tt$Category,tt$x))

p1 <- ggplot(data=data_tab, aes(x=x,y=60*y)) + 
  geom_line() + 
  geom_point(data=result,aes(x=stred,y=snr, shape=deg,color=as.factor(color))) +
  #geom_point(data=best,aes(x=stred,y=snr, color=ref_id)) + 
  labs(title="BRISK - gauss 30 STD; comparing with etalon", color="Inside", x="Distance from expected", y="") + 
  #annotate("label", x=150, y=7.5, label= paste0("Hit: ",sum(best$inside),"/",nrow(best)),size=4) #+
  geom_text(data=tt,aes(label = paste0("Hit: ", text, "/170")), nudge_y = -0.25)
 # annotate("label", x=150, y=7.5, label=tt,size=4) #+
#theme(panel.background = element_rect(fill = NA,color="black"),
#panel.grid.major = element_line(colour = "grey50"))
#scale_color_gradientn(colours = rainbow(10))
#geom_line(data=best,aes(x=stred,y=snr/60),color="red")

brisk_tile$id <- rep(seq(1,170),5)
p2 <- ggplot(data=brisk_tile, aes(x=id,y=name,fill=as.factor(y))) +
  geom_tile() + 
  labs(y="", x="Input image ID", fill="Inside") +
  theme(axis.ticks.y = element_blank(),
        #axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA))
#tiles <- rbind(tiles,data.frame(name="BRISK", y=as.factor(best$color)))

plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))

#ggplot(data=tiles, aes(x=rep(seq(1,170),3),y=name, fill=y)) + 
  #geom_tile()

################ akaze gauss30 #############
akaze_tile <- NULL
sorted_data <- akaze_gauss30_diff %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "Gauss 30 STD"
#best[95:105,]
result<- best
akaze_tile <- rbind(akaze_tile,data.frame(name="Gauss 30 STD", y=as.factor(best$color)))

sorted_data <- akaze_scale_diff %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "scale"
best[95:105,]
result <- rbind(result,best)
akaze_tile <- rbind(akaze_tile,data.frame(name="Scale", y=as.factor(best$color)))

sorted_data <- akaze_rotation_diff %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "rotation"
best[95:105,]
result <- rbind(result,best)
akaze_tile <- rbind(akaze_tile,data.frame(name="Rotation", y=as.factor(best$color)))

sorted_data <- akaze_allmax_diff %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "all"
best[95:105,]
result <- rbind(result,best)
akaze_tile <- rbind(akaze_tile,data.frame(name="All", y=as.factor(best$color)))

sorted_data <- akaze_identity_diff %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "ORIG"
best[95:105,]
result <- rbind(result,best)
akaze_tile <- rbind(akaze_tile,data.frame(name="Orig", y=as.factor(best$color)))

tt_akaze <- aggregate(result$inside, by=list(Category=result$deg), FUN=sum)
tt_akaze <- data.frame(x=c(150,150,150,150,150),y=c(0.125,0.117,0.105,0.1,0.095),text = paste(tt_akaze$Category,tt_akaze$x))

p1 <- ggplot(data=data_tab, aes(x=x,y=60*y)) + 
  geom_line() + 
  geom_point(data=best,aes(x=stred,y=snr, color=as.factor(color))) +
  #geom_point(data=best,aes(x=stred,y=snr, color=ref_id)) + 
  labs(title="AKAZE - original; comparing with etalon", color="Inside", x="Distance from expected", y="") + 
  #annotate("label", x=150, y=7.5, label= paste0("Hit: ",sum(best$inside),"/",nrow(best)),size=4) #+
  geom_text(data=tt_akaze,aes(label = paste0("Hit: ", text, "/170")), nudge_y = -0.25)
#theme(panel.background = element_rect(fill = NA,color="black"),
#panel.grid.major = element_line(colour = "grey50"))
#scale_color_gradientn(colours = rainbow(10))
#geom_line(data=best,aes(x=stred,y=snr/60),color="red")

akaze_tile$id <- rep(seq(1,170),5)
p2 <- ggplot(data=akaze_tile, aes(x=id,y=name,fill=as.factor(y))) +
  geom_tile() + 
  labs(y="", x="Input image ID", fill="Inside") +
  theme(axis.ticks.y = element_blank(),
        #axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA))
#tiles <- rbind(tiles,data.frame(name="AKAZE", y=as.factor(best$color)))

plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))

#ggplot(data=tiles, aes(x=rep(seq(1,170),3),y=name, fill=y)) + 
#geom_tile()


################################
akaze_tile <- NULL
result <- NULL
sorted_data <- akaze_identity %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "Orig"

result <- rbind(result,best)
akaze_tile <- rbind(akaze_tile,data.frame(name="Orig", y=as.factor(best$color)))
####
sorted_data <- akaze_scale %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "Scale"

result <- rbind(result,best)
akaze_tile <- rbind(akaze_tile,data.frame(name="Scale", y=as.factor(best$color)))
####
sorted_data <- akaze_rotation %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "Rotation"

result <- rbind(result,best)
akaze_tile <- rbind(akaze_tile,data.frame(name="Rotation", y=as.factor(best$color)))
####
sorted_data <- akaze_gauss30 %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "Gauss"

result <- rbind(result,best)
akaze_tile <- rbind(akaze_tile,data.frame(name="Gauss", y=as.factor(best$color)))
####
sorted_data <- akaze_allmax %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "All"

result <- rbind(result,best)
akaze_tile <- rbind(akaze_tile,data.frame(name="All", y=as.factor(best$color)))


x <- seq(-20, 20, by = 0.1)
ci <- qnorm(0.99,mean=0,sd=1) # 0.9+0.05
std <-10/(2*ci)
y <- dnorm(x, mean = 0, sd = std)
data_tab <- data.frame(x=x,y=y)

tt_akaze <- aggregate(result$inside, by=list(Category=result$deg), FUN=sum)
tt_akaze <- data.frame(x=rep(20,5),y=c(0.125,0.117,0.105,0.1,0.095),text = paste(tt_akaze$Category,tt_akaze$x))

p1 <- ggplot(data=data_tab, aes(x=x,y=60*y)) + 
  geom_line() + 
  geom_point(data=result,aes(x=stred,y=snr, shape=deg,color=as.factor(color))) +
  #geom_point(data=best,aes(x=stred,y=snr, color=ref_id)) + 
  labs(title="AKAZE; same patient", color="Inside", x="Distance from expected", y="") + 
  #annotate("label", x=150, y=7.5, label= paste0("Hit: ",sum(best$inside),"/",nrow(best)),size=4) #+
  geom_text(data=tt_akaze,aes(label = paste0("Hit: ", text, "/170")), nudge_y = -0.25)


akaze_tile$id <- rep(seq(1,170),5)
p2 <- ggplot(data=akaze_tile, aes(x=id,y=name,fill=as.factor(y))) +
  geom_tile() + 
  labs(y="", x="Input image ID", fill="Inside") +
  theme(axis.ticks.y = element_blank(),
        #axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA))
#tiles <- rbind(tiles,data.frame(name="AKAZE", y=as.factor(best$color)))

plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
#############################################
########################## orb same patient
################################
orb_tile <- NULL
result <- NULL
sorted_data <- orb_identity %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "Orig"

result <- rbind(result,best)
orb_tile <- rbind(orb_tile,data.frame(name="Orig", y=as.factor(best$color)))
####
sorted_data <- orb_scale %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "Scale"

result <- rbind(result,best)
orb_tile <- rbind(orb_tile,data.frame(name="Scale", y=as.factor(best$color)))
####
sorted_data <- orb_rotation %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "Rotation"

result <- rbind(result,best)
orb_tile <- rbind(orb_tile,data.frame(name="Rotation", y=as.factor(best$color)))
####
sorted_data <- orb_gauss30 %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "Gauss"

result <- rbind(result,best)
orb_tile <- rbind(orb_tile,data.frame(name="Gauss", y=as.factor(best$color)))
####
sorted_data <- orb_allmax %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "All"

result <- rbind(result,best)
orb_tile <- rbind(orb_tile,data.frame(name="All", y=as.factor(best$color)))


x <- seq(-20, 20, by = 0.1)
ci <- qnorm(0.99,mean=0,sd=1) # 0.9+0.05
std <-10/(2*ci)
y <- dnorm(x, mean = 0, sd = std)
data_tab <- data.frame(x=x,y=y)

tt_orb <- aggregate(result$inside, by=list(Category=result$deg), FUN=sum)
tt_orb <- data.frame(x=rep(20,5),y=c(0.125,0.117,0.105,0.1,0.095),text = paste(tt_orb$Category,tt_orb$x))

p1 <- ggplot(data=data_tab, aes(x=x,y=60*y)) + 
  geom_line() + 
  geom_point(data=result,aes(x=stred,y=snr, shape=deg,color=as.factor(color))) +
  #geom_point(data=best,aes(x=stred,y=snr, color=ref_id)) + 
  labs(title="orb; same patient", color="Inside", x="Distance from expected", y="") + 
  #annotate("label", x=150, y=7.5, label= paste0("Hit: ",sum(best$inside),"/",nrow(best)),size=4) #+
  geom_text(data=tt_orb,aes(label = paste0("Hit: ", text, "/170")), nudge_y = -0.25)


orb_tile$id <- rep(seq(1,170),5)
p2 <- ggplot(data=orb_tile, aes(x=id,y=name,fill=as.factor(y))) +
  geom_tile() + 
  labs(y="", x="Input image ID", fill="Inside") +
  theme(axis.ticks.y = element_blank(),
        #axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA))
#tiles <- rbind(tiles,data.frame(name="orb", y=as.factor(best$color)))

plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))


#############################################
########################## brisk same patient
################################
brisk_tile <- NULL
result <- NULL
sorted_data <- brisk_identity %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "Orig"

result <- rbind(result,best)
brisk_tile <- rbind(brisk_tile,data.frame(name="Orig", y=as.factor(best$color)))
####
sorted_data <- brisk_scale %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "Scale"

result <- rbind(result,best)
brisk_tile <- rbind(brisk_tile,data.frame(name="Scale", y=as.factor(best$color)))
####
sorted_data <- brisk_rotation %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "Rotation"

result <- rbind(result,best)
brisk_tile <- rbind(brisk_tile,data.frame(name="Rotation", y=as.factor(best$color)))
####
sorted_data <- brisk_gauss30 %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "Gauss"

result <- rbind(result,best)
brisk_tile <- rbind(brisk_tile,data.frame(name="Gauss", y=as.factor(best$color)))
####
sorted_data <- brisk_allmax %>% group_by(ref_id) %>% slice(which.max(snr))
best <- as.data.frame(sorted_data)
best$stred <- best$ref_z-best$ref_id
best$z <- pnorm(best$stred,mean=0,sd=std)
best$color <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),"true","false")
best$inside <- ifelse(( (best$z>=0.05) & (best$z<=0.95)),1,0)
best$deg <- "All"

result <- rbind(result,best)
brisk_tile <- rbind(brisk_tile,data.frame(name="All", y=as.factor(best$color)))


x <- seq(-20, 20, by = 0.1)
ci <- qnorm(0.99,mean=0,sd=1) # 0.9+0.05
std <-10/(2*ci)
y <- dnorm(x, mean = 0, sd = std)
data_tab <- data.frame(x=x,y=y)

tt_brisk <- aggregate(result$inside, by=list(Category=result$deg), FUN=sum)
tt_brisk <- data.frame(x=rep(20,5),y=c(0.125,0.117,0.105,0.1,0.095),text = paste(tt_brisk$Category,tt_brisk$x))

p1 <- ggplot(data=data_tab, aes(x=x,y=60*y)) + 
  geom_line() + 
  geom_point(data=result,aes(x=stred,y=snr, shape=deg,color=as.factor(color))) +
  #geom_point(data=best,aes(x=stred,y=snr, color=ref_id)) + 
  labs(title="brisk; same patient", color="Inside", x="Distance from expected", y="") + 
  #annotate("label", x=150, y=7.5, label= paste0("Hit: ",sum(best$inside),"/",nrow(best)),size=4) #+
  geom_text(data=tt_brisk,aes(label = paste0("Hit: ", text, "/170")), nudge_y = -0.25)


brisk_tile$id <- rep(seq(1,170),5)
p2 <- ggplot(data=brisk_tile, aes(x=id,y=name,fill=as.factor(y))) +
  geom_tile() + 
  labs(y="", x="Input image ID", fill="Inside") +
  theme(axis.ticks.y = element_blank(),
        #axis.text.y = element_blank(),
        panel.background = element_rect(fill = NA))
#tiles <- rbind(tiles,data.frame(name="brisk", y=as.factor(best$color)))

plot_grid(p1, p2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
