main_plot <- ggplot(
  tibble(
    method = method_order$cov_est_method, 
    returns = all_avg_returns, 
    sd = all_avg_sd
  ) %>% 
    filter(!method %in% c("CovMve")),
  aes(x = sd, y = returns)) +
  geom_point() +
  geom_label_repel(
    aes(label = method),
    box.padding = 1,
    point.padding = 1,
    segment.color = "grey",
    color = "darkgreen"
  ) +
  theme_hsg() 

inset_plot <- main_plot +
  ylab(NULL) +
  xlab(NULL) +
  theme_minimal() +
  scale_x_continuous(breaks = 39.5,limits=c(39,40)) +
  scale_y_continuous(breaks = -5.5,limits=c(-6,-5)) +
  theme(axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15), 
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

main_plot2 <- main_plot +
  xlim(c(0,20)) +
  ylim(c(0,2.5))
  
inset_grob <- ggplotGrob(inset_plot)

library(grid)

main_plot2 +
  annotation_custom(
    grob = inset_grob, 
    xmin = 15, xmax = 21.2, 
    ymin = -0.1, ymax = 0.55
  ) 

ret_test <- lapply(method_order$cov_est_method, function(cov) 
  results_by_cov[[cov]] %>% 
    map_depth(1,1) %>%
    reduce(rbind) %>% 
    filter(!is.na(returns))
) 

ret_test[[16]] %>% 
  ggplot(aes(x=date,y=returns)) +
  geom_line()

ret_test[[9]]$returns %>% mean()*12

a <- ret_test[[9]] %>% mutate(returns=returns*12)
a$returns %>% sd  

ret_test[[9]] %>% view()






