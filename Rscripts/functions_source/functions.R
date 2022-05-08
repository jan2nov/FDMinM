#libraries
require(zoo)
require(dplyr)
require(ggplot2)
require(zeallot)
require(extrafont)
require(cowplot)
require(egg)
library(tidyverse)

source("../functions_source/read_data.R")
source("../functions_source/compute_slices.R")
source("../functions_source/accuracy_plot.R")
source("../functions_source/snr_plot.R")
source("../functions_source/nkeypoints_plot.R")

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

round2 <- function(x, digits = 0) {  # Function to always round 0.5 up
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}

roll_mean <- function(data_diff,nimages,npatients){
  means <- NULL #data.frame(rollmean_snr=NULL)
  for (i in 1:nimages){
    for (j in 1:npatients){
      filter <- data_diff[(data_diff$input_id == i) & (data_diff$type==j),]
      temp <- rollmean(filter$snr, k = window, fill=NA, align=c("center"))
      filter$rollmean_snr <- temp
      means <- rbind(means,filter) #append(means,temp)
    }
  }
  return(means)
}