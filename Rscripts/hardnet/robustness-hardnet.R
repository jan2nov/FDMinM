rm(list = ls())

#load helping functions, like rollmean, accuracy_plot, read_data and libraries
source("../functions_source/functions.R")


patient1 = list.files(path = "../../data/methods_testing/hardnet/orig/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/orig/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/hardnet/orig/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/hardnet/orig/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/hardnet/orig/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/hardnet/orig/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/hardnet/orig/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

hardnet <- NULL
hardnet <- read_data_orig(patient1,patient2,patient3,patient4,patient5,patient6,patient7)
hardnet <- hardnet %>% filter(input_id == test_id)

#on higher slices there are NAs
temp <- hardnet %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),mean)
temp2 <- hardnet %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
h_identity <- temp
h_identity$id <- "None"
h_identity$robustness <- h_identity$snr/h_identity$snr

################### rotation #########################################################
patient1 = list.files(path = "../../data/methods_testing/hardnet/robustness-rotation/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/robustness-rotation/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/hardnet/robustness-rotation/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/hardnet/robustness-rotation/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/hardnet/robustness-rotation/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/hardnet/robustness-rotation/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/hardnet/robustness-rotation/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

hardnet <- NULL
hardnet <- read_data_orig(patient1,patient2,patient3,patient4,patient5,patient6,patient7)
hardnet <- hardnet %>% filter(input_id == test_id)
temp <- hardnet %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),mean)
temp2 <- hardnet %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
h_rot <- temp
h_rot$id <- "Rotation 5 °"
h_rot$robustness <- h_rot$snr/h_identity$snr

################### gauss #########################################################
patient1 = list.files(path = "../../data/methods_testing/hardnet/robustness-noise//1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/robustness-noise/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/hardnet/robustness-noise/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/hardnet/robustness-noise/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/hardnet/robustness-noise/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/hardnet/robustness-noise/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/hardnet/robustness-noise/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

hardnet <- NULL
hardnet <- read_data_orig(patient1,patient2,patient3,patient4,patient5,patient6,patient7)
hardnet <- hardnet %>% filter(input_id == test_id)
temp <- hardnet %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),mean)
temp2 <- hardnet %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
h_gauss <- temp
h_gauss$id <- "Gaussian noise 30 STD"
h_gauss$robustness <- h_gauss$snr/h_identity$snr

################### upscale #########################################################
patient1 = list.files(path = "../../data/methods_testing/hardnet/robustness-scale/1-1", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/robustness-scale/2-2", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/hardnet/robustness-scale/3-3", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/hardnet/robustness-scale/4-4", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/hardnet/robustness-scale/5-5", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/hardnet/robustness-scale/6-6", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/hardnet/robustness-scale/7-7", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

hardnet <- NULL
hardnet <- read_data_orig(patient1,patient2,patient3,patient4,patient5,patient6,patient7)
hardnet <- hardnet %>% filter(input_id == test_id)
temp <- hardnet %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),mean)
temp2 <- hardnet %>% group_by(input_id,test_id) %>% drop_na() %>% summarise_at(vars("snr"),sd)
temp$sd <- temp2$snr
#temp$ref_id <- i
h_scale <- temp
h_scale$id <- "Upscale 5%"
h_scale$robustness <- h_scale$snr/h_identity$snr


#############################################################################
results <- rbind(h_identity, h_gauss, h_rot, h_scale)

p <- ggplot(data=results, aes(x=input_id,y=robustness, group=id,color=id, fill=id), size=1.2) + 
  scale_color_manual(values=c("#e6550d", "#54278f","#3182bd","#31a354")) +
  scale_fill_manual(values=c("#e6550d", "#54278f", "#3182bd","#31a354")) +
  geom_smooth(method = "lm", formula = y ~ poly(x,6)) +
  coord_cartesian(ylim = c(0.0,2.01)) + 
  labs(x="image ID", y=expression("Robustness R"[x]), color="HardNet\nImage degradation", fill="HardNet\nImage degradation") + 
  theme_bw() + 
  guides(fill=guide_legend(ncol=2), color=guide_legend(ncol=2)) +
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(0.32,0.15),
        #legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=25,b=-25), hjust=0.05, face="plain", size=12))
p
ggsave("./robustness_hardnet.pdf", plot = p,width = 5.79, height = 4.65, dpi=300, device=cairo_pdf)
