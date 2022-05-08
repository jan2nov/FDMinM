accuracy_plot <- function(best,accuracy_n,name,processing,distance){
  best$color <- ifelse(( (best$stred>=-accuracy_n) & (best$stred<=accuracy_n)),"1","0")
  best$inside <- ifelse(( (best$stred>=-accuracy_n) & (best$stred<=accuracy_n)),1,0)
  achieved_acc <-floor(sum(best$inside)/nrow(best)*100)
  achieved_cd <- sum(abs(best$stred))
  print(paste0(processing,", Accuracy: ",sum(best$inside)/nrow(best)))
  print(paste0(processing," Cum.distance: ",sum(abs(best$stred))))
  print(paste0(processing," Average SNR: ",mean(best$snr)))
  print(paste0(processing," Max SNR: ",max(best$rollmean_snr)))
  
  a <- sum(best$inside)
  small_text <- paste0("",distance,",",processing,"")
  graph_hit = deparse(bquote(paste(A[.(small_text)]," = ",.(achieved_acc), " %") ))
  graph_cum = deparse( bquote( paste("C"[.(processing)]," = ", .(achieved_cd))))
  graph <- ggplot(data=best, aes(x=stred,y=rollmean_snr, color=as.factor(color))) + 
    geom_point() + 
    theme_bw() +
    xlim(c(-150,150)) +
    ylim(c(0.5,4.6)) +
    labs(y=expression("running mean Signal to Noise ratio"[]), fill=graph_hit, color=expression("x"["i"]), x="Distance from the expected", title = name) +
    scale_color_manual(values=c("#d7191c", "#2c7bb6")) +
    scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
    geom_vline(xintercept=0, linetype="dashed", color = "black") +
    #annotate("label", x=100, y=5.8, label= paste0(graph_hit,sum(best$inside),"/",nrow(best)),size=4) +
    annotate("text", x=110, y=3.8, label= graph_hit, size=4, parse=TRUE) +
    annotate("text", x=110, y=3.6, label= graph_cum, size=4, parse=TRUE) + 
    theme(text=element_text(family="Times New Roman"), 
          legend.position = c(0.85,0.1),
          legend.direction="horizontal",
          plot.title = element_text(margin=margin(t=20,b=-20), hjust=0.05, face="plain", size=12)
    )
  return(graph)
}

where_id_plot<- function(data, method, procesing, id_subject, print_title, x_axis){
  data_filter <- data %>% filter(type==id_subject)
  data_filter$color  <- ifelse(( (data_filter$stred>=-accuracy_n) & (data_filter$stred<=accuracy_n)),"1","0")
  data_filter$inside <- ifelse(( (data_filter$stred>=-accuracy_n) & (data_filter$stred<=accuracy_n)),1,0)
  p1_title <- paste0(method, " – subject ", id_subject)
  
  p1 <- ggplot(data=data_filter, aes(x=input_id,y=1,fill=as.factor(color))) +
    geom_tile() + 
    labs(y=procesing, x="image ID", fill=expression("x"["i"])) +
    {if (print_title == 0) {
      labs(y=procesing, x="image ID", fill=expression("x"["i"]), title=p1_title)
    }} + 
#    labs(y=procesing, x="image ID", fill=expression("x"["i"]), title=ifelse(print_title == 0, p1_title, "")) + 
    scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.background = element_rect(fill = NA),
          text=element_text(family="Times New Roman"),
          plot.margin = unit(c(0,0,0,0), "cm")
    ) +
    {if (x_axis == 0){
      theme(
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            legend.position = "none",
      )
    }}
  return(p1)
}