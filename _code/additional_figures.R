# ------------------------------------------------------------------------------
#                 ROLLING WINDOW FIGURE
# ------------------------------------------------------------------------------

data_rw <- seq.Date(
  from = as.Date("1973-01-01"), 
  to = as.Date("1995-01-01"), 
  by = "months"
  ) %>% 
  as_tibble(.) %>% 
  mutate(p1 = 6, p2=5.9) %>% 
  rename(Time=value)

data_rw %>% 
  ggplot(aes(x=Time)) +
  geom_rect(aes(
    ymin = p1, 
    ymax = p2, 
    xmin = as.Date("1973-01-01"), 
    xmax = as.Date("1988-01-01"),
    fill = "cov. estimation window"
    )
    ) +
  geom_rect(aes(
    ymin = p1 - 0.2, 
    ymax = p2 -0.2, 
    xmin = as.Date("1973-07-01"), 
    xmax = as.Date("1988-07-01")
  ),
  fill = "#819c80"
  ) +
  geom_rect(aes(
    ymin = p1 - 0.4, 
    ymax = p2 - 0.4, 
    xmin = as.Date("1974-01-01"), 
    xmax = as.Date("1989-01-01")
  ),
  fill = "#819c80"
  ) + 
  geom_rect(aes(
    ymin = p1 - 0.4, 
    ymax = p2 - 0.4, 
    xmin = as.Date("1989-01-01"), 
    xmax = as.Date("1989-07-01")
  ),
  fill = "#8dadb5"
  ) +geom_rect(aes(
    ymin = p1 - 0.2, 
    ymax = p2 - 0.2, 
    xmin = as.Date("1988-07-01"), 
    xmax = as.Date("1989-01-01")
  ),
  fill = "#8dadb5"
  ) + geom_rect(aes(
    ymin = p1, 
    ymax = p2, 
    xmin = as.Date("1988-01-01"), 
    xmax = as.Date("1988-07-01")
  ),
  fill = "#8dadb5"
  ) +
  guides(y="none") +
  theme_classic() +
  ylim(c(5.3,6)) +
  geom_vline(xintercept = as.Date("1973-01-01"), linetype = 2) +
  geom_vline(xintercept = as.Date("1973-07-01"), linetype = 2)+
  geom_vline(xintercept = as.Date("1974-01-01"), linetype = 2)+
  geom_vline(xintercept = as.Date("1988-07-01"), linetype = 3)+
  geom_vline(xintercept = as.Date("1989-01-01"), linetype = 3)+
  geom_vline(xintercept = as.Date("1989-07-01"), linetype = 3) +
  scale_color_manual(name = "lengend", values = c("cov.estimation window" = "#819c80", "testing/ holding period" = "#8dadb5"))


  

