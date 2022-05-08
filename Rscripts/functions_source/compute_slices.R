compute_slices <- function(means, nslices, npatient){
  stred_metrix <- data.frame(type=NULL,hodnota=NULL)
  final <- NULL
  for (i in 1:npatient){
    filter <- means %>% filter(type==i) %>% group_by(input_id) %>% filter(input_id>2) %>% filter(input_id<(nslices-1)) %>% slice(which.max(rollmean_snr))
    filter$stred <- filter$test_id - filter$input_id
    #stred_metrix_tmp <- data.frame(type=i, hodnota=round2(median(filter$stred),digits=0))
    stred_metrix_tmp <- data.frame(type=i, hodnota=median(filter$stred))
    stred_metrix <- rbind(stred_metrix,stred_metrix_tmp)
    #print(c(i,median(filter$stred)))
    filter$stred <- filter$stred - round2(median(filter$stred),digits=0)
    #filter$stred <- filter$stred - getmode(filter$stred)
    
    final <- rbind(final, filter)
    #plot(filter$test_z, filter$ref_z)
    #plot(filter$stred)
  }
  return(list(stred_metrix,final))
  #return(final)
}