rm(list = ls())

#load helping functions, like rollmean, accuracy_plot, read_data and libraries
source("../functions_source/functions.R")
#load parameters for the plots, accuracy, image_start, image_end, npatient
source("../functions_source/params.R")

name <- "HardNet"
img_start <- 40

#load lists of data; each file corresponds to one input with all other
patient1 = list.files(path = "../../data/methods_testing/hardnet/atlas/01/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/atlas/02/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient3 = list.files(path = "../../data/methods_testing/hardnet/atlas/03/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient4 = list.files(path = "../../data/methods_testing/hardnet/atlas/04/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient5 = list.files(path = "../../data/methods_testing/hardnet/atlas/05/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient6 = list.files(path = "../../data/methods_testing/hardnet/atlas/06/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient7 = list.files(path = "../../data/methods_testing/hardnet/atlas/07/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

#combine all patients/subjects to a dataframe
data1 <- read_data_one(patient1,1)#,patient2,patient3,patient4,patient5,patient6)
data1$type <- 1
data2 <- read_data_one(patient2,2)#,patient2,patient3,patient4,patient5,patient6)
data2$type <- 2
data3 <- read_data_one(patient3,3)#,patient2,patient3,patient4,patient5,patient6)
data3$type <- 3
data4 <- read_data_one(patient4,4)#,patient2,patient3,patient4,patient5,patient6)
data4$type <- 4
data5 <- read_data_one(patient5,5)#,patient2,patient3,patient4,patient5,patient6)
data5$type <- 5
data6 <- read_data_one(patient6,6)#,patient2,patient3,patient4,patient5,patient6)
data6$type <- 6
data7 <- read_data_one(patient7,7)#,patient2,patient3,patient4,patient5,patient6)
data7$type <- 7

data <- rbind(data1,data2,data3,data4,data5,data6)

#compute rollmean with roll_mean function(data,number_of_images_per_subject, number_of_subjects)
means <- roll_mean(data,170,7) 

#compute the slices and regularization to all subjects and images
c(center_metrix,final) %<-% compute_slices(means,170,7)
center_metrix

# cut the results to the part of interest
best <- final %>% filter(input_id>img_start) %>% filter(input_id<=img_end)
#save for the where_plot graph
hardnet_none <- best

#plot the accuracy plot
p <- accuracy_plot(hardnet_none,accuracy_n, name, "none", accuracy_n)
p
ggsave("./atlas_HardNet_none.pdf", plot = p,width = 6, height = 4, dpi=300,device=cairo_pdf)

################################################################################
#load lists of data; each file corresponds to one input with all other
patient1 = list.files(path = "../../data/methods_testing/hardnet/atlas-reb/01/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
patient2 = list.files(path = "../../data/methods_testing/hardnet/atlas-reb/02/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
#patient3 = list.files(path = "../../data/methods_testing/hardnet/atlas-reb/03/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
#patient4 = list.files(path = "../../data/methods_testing/hardnet/atlas-reb/04/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
#patient5 = list.files(path = "../../data/methods_testing/hardnet/atlas-reb/05/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
#patient6 = list.files(path = "../../data/methods_testing/hardnet/atlas-reb/06/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)
#patient7 = list.files(path = "../../data/methods_testing/hardnet/atlas-reb/07/", pattern = ".csv",include.dirs = TRUE,full.names = TRUE)

#combine all patients/subjects to a dataframe
data1 <- read_data_one(patient1,1)#,patient2,patient3,patient4,patient5,patient6)
data1$type <- 1
data2 <- read_data_one(patient2,2)#,patient2,patient3,patient4,patient5,patient6)
data2$type <- 2
#data3 <- read_data_one(patient3,3)#,patient2,patient3,patient4,patient5,patient6)
#data3$type <- 3
#data4 <- read_data_one(patient4,4)#,patient2,patient3,patient4,patient5,patient6)
#data4$type <- 4
#data5 <- read_data_one(patient5,5)#,patient2,patient3,patient4,patient5,patient6)
#data5$type <- 5
#data6 <- read_data_one(patient6,6)#,patient2,patient3,patient4,patient5,patient6)
#data6$type <- 6
#data7 <- read_data_one(patient7,7)#,patient2,patient3,patient4,patient5,patient6)
#data7$type <- 7

data <- rbind(data1,data2)#,data2,data3,data4,data5,data6)

#compute rollmean with roll_mean function(data,number_of_images_per_subject, number_of_subjects)
means <- roll_mean(data,170,2) 

#compute the slices and regularization to all subjects and images
c(center_metrix,final) %<-% compute_slices(means,170,2)
center_metrix

# cut the results to the part of interest
best <- final %>% filter(input_id>img_start) %>% filter(input_id<=img_end)
#save for the where_plot graph
hardnet_rebs <- best

#plot the accuracy plot
p <- accuracy_plot(hardnet_rebs,accuracy_n, name, "rebs", accuracy_n)
p
ggsave("./atlas_HardNet_rebs.pdf", plot = p,width = 6, height = 4, dpi=300,device=cairo_pdf)
################################################################################

w_none <- where_id_plot(hardnet_none, "HardNet comparing with atlas BRAINWEB", "None", 1, 0, 0)
w_rebs <- where_id_plot(hardnet_rebs, name, "rebs", 1, 1, 1)

w_none
w_rebs

where <- ggarrange(w_none,w_rebs,nrow = 2)
where

ggsave("./hardnet_where_atlas.pdf", plot = where, width = 6, height = 4, dpi=300, device=cairo_pdf)
