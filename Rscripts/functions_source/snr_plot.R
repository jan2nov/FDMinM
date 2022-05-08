snr_plot <- function(results2,name,top) {
  graph <- ggplot(result2, aes(x=test_id,
                      y=snr, 
                      color=as.factor(input_id),
                      group=input_id, 
                      ymin=(snr-sd), 
                      ymax=(snr+sd),
                      fill=as.factor(input_id)
  )
  ) +
    geom_line(lwd=1.1) + 
    geom_point(size=2.5) + 
    labs(y=expression("SNR"[i]), fill="Image ID", color="Image ID", x="Compared image (n)", title = name) +
    theme_bw() + 
    ylim(-1, top) +
    geom_ribbon(alpha=0.4, colour = NA) +
    scale_color_manual(values=c("#ca0020", "#f4a582", "#92c5de","#0571b0")) +
    scale_fill_manual(values=c("#ca0020", "#f4a582", "#92c5de","#0571b0")) +
    theme(text=element_text(family="Times New Roman"), 
          legend.position = c(0.7,0.95),
          legend.direction="horizontal",
          plot.title = element_text(margin=margin(t=20,b=-25), hjust=0.05, face="plain", size=12))
  return(graph)
}