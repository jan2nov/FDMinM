plot_nkeypoints <- function(data,method_name,t_x=0.4,t_y=0.12){
  p <- ggplot(data = data, aes(x = id, y=number, fill=method, color=method,  ymin=(number-sdeviation), ymax = (number+sdeviation) )) + 
  geom_smooth(method="loess", alpha=0.3) + 
  #geom_point(aes(color=method)) +
  #geom_bar() +
  #geom_point(aes(color=method)) +
  labs(y = "Average number of keypoints", x = "image ID", title = method_name, color="Image degradation", color="Image degradation", fill="Image degradation") +
  #ylim(NA,1050) +
  #coord_cartesian(ylim = NULL)
  # geom_ribbon(alpha=0.3)
  theme_bw() + 
  guides(fill=guide_legend(ncol=2), color=guide_legend(ncol=2)) +
  scale_color_manual(values=c("#e6550d", "#54278f", "#3182bd","#31a354")) +
  scale_fill_manual(values=c("#e6550d", "#54278f", "#3182bd","#31a354")) +
  theme(text=element_text(family="Times New Roman"), 
        legend.position = c(t_x,t_y),
        #legend.direction="horizontal",
        plot.title = element_text(margin=margin(t=20,b=-20), hjust=0.02, face="plain", size=12))
  return(p)
}