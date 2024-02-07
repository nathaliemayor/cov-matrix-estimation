# ##############################################################################
#                 THESIS FIGURES
# ##############################################################################
# results_rdata <- list() 
# 252 days estimation, 126 days holding, from 1970 to 2019
results_252_126 <- results_data
weights_252_126 <- complete_weights
returns_252_126 <- complete_returns
window_sd_252_126 <- complete_window_sd
window_sr_252_126 <- complete_window_sr

results_rdata$d252_126 <- list(results = results_252_126, 
                                weights = weights_252_126,
                                returns = returns_252_126,
                                window_sd = window_sd_252_126,
                                window_sr = window_sr_252_126)

# 252 days estimation, 21 days holding
results_252_21 <- results_data
weights_252_21 <- complete_weights
returns_252_21 <- complete_returns
window_sd_252_21 <- complete_window_sd
window_sr_252_21 <- complete_window_sr

results_rdata$d252_21 <- list(results = results_252_21, 
                               weights = weights_252_21,
                               returns = returns_252_21,
                               window_sd = window_sd_252_21,
                               window_sr = window_sr_252_21)

# 1260 days estimation, 126 days holding, from 1970 to 2019
results_1260_126 <- results_data
weights_1260_126 <- complete_weights
returns_1260_126 <- complete_returns
window_sd_1260_126 <- complete_window_sd
window_sr_1260_126 <- complete_window_sr

results_rdata$d1260_126 <- list(results = results_1260_126, 
                           weights = weights_1260_126,
                           returns = returns_1260_126,
                           window_sd = window_sd_1260_126,
                           window_sr = window_sr_1260_126)

# 1260 days estimation, 21 days holding
results_1260_21 <- results_data
weights_1260_21 <- complete_weights
returns_1260_21 <- complete_returns
window_sd_1260_21 <- complete_window_sd
window_sr_1260_21 <- complete_window_sr

results_rdata$d1260_21 <- list(results = results_1260_21, 
                                weights = weights_1260_21,
                                returns = returns_1260_21,
                                window_sd = window_sd_1260_21,
                                window_sr = window_sr_1260_21)

# 2520 days estimation, 126 days holding

results_2520_126 <- results_data
weights_2520_126 <- complete_weights
returns_2520_126 <- complete_returns
window_sd_2520_126 <- complete_window_sd
window_sr_2520_126 <- complete_window_sr

results_rdata$d2520_126 <- list(results = results_2520_126, 
                               weights = weights_2520_126,
                               returns = returns_2520_126,
                               window_sd = window_sd_2520_126,
                               window_sr = window_sr_2520_126)

# 2520 days estimation, 21 days holding

results_2520_21 <- results_data
weights_2520_21 <- complete_weights
returns_2520_21 <- complete_returns
window_sd_2520_21 <- complete_window_sd
window_sr_2520_21 <- complete_window_sr

results_rdata$d2520_21 <- list(results = results_2520_21, 
                               weights = weights_2520_21,
                               returns = returns_2520_21,
                               window_sd = window_sd_2520_21,
                               window_sr = window_sr_2520_21)

save(
  results_rdata, 
  file=file.path(core_path,data_path,"results","results_figures_tables_1970-2019D.RData")
)
# ------------------------------------------------------------------------------
#                 PORTFOLIOS CORRELATION - HEATMAP
# ------------------------------------------------------------------------------

cor(ff100_data$monthly[,-1]) %>% 
  reshape2::melt() %>% 
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(0,1), space = "Lab", 
                       name="Pearson\nCorrelation") 

# ------------------------------------------------------------------------------
#                 HEATMAP COVARIANCE ESTIMATION VS SAMPLE
# ------------------------------------------------------------------------------

cov(ff100_data$monthly[(776-60):776,-1]) %>% 
  reshape2::melt() %>% 
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") 

lapply(cov_est_method, function(cov){
  get_covariance_estimate(
    ff100_data$monthly[(776-60):776,-1],
    method = cov
    ) %>% 
    reshape2::melt() %>% 
    ggplot(aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") 
})

# ------------------------------------------------------------------------------
#                 PORTFOLIOS SD AND RETURNS - SCATTER
# ------------------------------------------------------------------------------
legend_setting <-  data.frame(
  method = c("cov1Para", "cov2Para", "covCor","covDiag", 
                                  "covMarket","gis","qis","lis",
                                  "CovMve","CovMcd",
                                  "huge_glasso",
                                  "CCM","factor1","factor3","RMT",
                                  "sample", "SP500","equal_weights",
             "sample_short_constraint"),
  label = c("LS-1P","LS-2P","LS-CCM","LS-D","LS-SIM","NS-GIS","NS-QIS","NS-LIS",
            "MVE","MCD","GLASSO","CCM","SIM","MIM","RMT",
            "Sample","SP500","EQW","W+"),
  color = c("tomato3","blue","forestgreen","orange","purple","red","blue","springgreen","chartreuse1",
            "darkblue","black","darkred","magenta","cornflowerblue","orange3","red4","cornflowerblue",
            "black","darkorange"),
  shape = c(19,19,19,19,19,25,25,25,15,15,10,17,17,17,17,8,8,8,8)
)

# estimation 252 days, test 126 days, 01-01-1970 TO 01-12-2019
data_ggplot <- tibble(
  method = results_rdata$d252_126$results$method,
  returns = results_rdata$d252_126$results$mu, 
  sd = results_rdata$d252_126$results$sd_window
) %>% left_join(legend_setting) %>%
  filter(!method %in% c("SP500"))

nls_labels <- data_ggplot %>% filter(method %in% c("gis","qis","lis"))

ggplot(data_ggplot,
       aes(x = sd, y = returns, color = method,shape = method)) +
  geom_point(size = 6) +
  scale_color_manual(values = setNames(legend_setting$color, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  scale_shape_manual(values = setNames(legend_setting$shape, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  labs(color = "Methods", shape = "Methods") +
  theme(legend.position = "right",
        legend.text = element_text(size=16),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(size=20),
        axis.text.y  = element_text(size=20)) +
  geom_label_repel(aes(label = label), 
                   data = nls_labels,
                   direction = "y",
                   segment.color = "grey",
                   color = "darkgreen",
                   nudge_y = 3,
                   segment.size=1) +
  theme_hsg() +
  xlab("returns standard deviation (%), annual") +
  ylab("returns mean (%), annual") +
  scale_x_continuous(breaks = 0:50) +
  ylim(c(9,25)) +
  scale_y_continuous(breaks = 0:24) 

# estimation 1260 days, test 21 days, 01-01-1970 TO 01-12-2019
data_ggplot <- tibble(
  method = results_rdata$d1260_21$results$method, 
  returns = results_rdata$d1260_21$results$mu, 
  sd = results_rdata$d1260_21$results$sd_overall
) %>% left_join(legend_setting) %>%
  filter(!method %in% c(""))

nls_labels <- data_ggplot %>% filter(method %in% c("gis","qis","lis"))

ggplot(data_ggplot,
       aes(x = sd, y = returns, color = method,shape = method)) +
  geom_point(size = 6) +
  scale_color_manual(values = setNames(legend_setting$color, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  scale_shape_manual(values = setNames(legend_setting$shape, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  labs(color = "Methods", shape = "Methods") +
  theme(legend.position = "right",
        legend.text = element_text(size=16),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(size=20),
        axis.text.y  = element_text(size=20)) +
  geom_label_repel(aes(label = label), 
                   data = nls_labels,
                   direction = "y",
                   segment.color = "grey",
                   color = "darkgreen",
                   nudge_y = 1) +
  theme_hsg() +
  xlab("returns standard deviation (%), annual") +
  ylab("returns mean (%), annual") +
  scale_x_continuous(breaks = 0:50) +
  scale_y_continuous(breaks = 0:24) 

# estimation 1260 days, test 126 days, 01-01-1970 TO 01-12-2019
  
data_ggplot <- tibble(
  method = results_rdata$d1260_126$results$method, 
  returns = results_rdata$d1260_126$results$mu, 
  sd = results_rdata$d1260_126$results$sd_overall
) %>% left_join(legend_setting) %>%
  filter(!method %in% c("SP0"))

nls_labels <- data_ggplot %>% filter(method %in% c("gis","qis","lis"))

ggplot(data_ggplot,
  aes(x = sd, y = returns, color = method,shape = method)) +
  geom_point(size = 6) +
  scale_color_manual(values = setNames(legend_setting$color, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  scale_shape_manual(values = setNames(legend_setting$shape, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  labs(color = "Methods", shape = "Methods") +
  theme(legend.position = "right",
        legend.text = element_text(size=16),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(size=20),
        axis.text.y  = element_text(size=20)) +
  geom_label_repel(aes(label = label), 
                   data = nls_labels,
                   direction = "y",
                   segment.color = "grey",
                   color = "darkgreen",
                   nudge_y = 1,
                   segment.size=1) +
  theme_hsg() +
  xlab("returns standard deviation (%), annual") +
  ylab("returns mean (%), annual") +
  scale_x_continuous(breaks = 0:50) +
  ylim(c(9,20)) +
  scale_y_continuous(breaks = 0:24) 

# estimation 2520 days, test 21 days, 01-01-1970 TO 01-12-2019
data_ggplot <- tibble(
  method = results_rdata$d2520_21$results$method, 
  returns = results_rdata$d2520_21$results$mu, 
  sd = results_rdata$d2520_21$results$sd_window
) %>% left_join(legend_setting) %>%
  filter(!method %in% c("SP500"))

nls_labels <- data_ggplot %>% 
  filter(method %in% 
           c("gis","qis","lis","covDiag","covMarket",
             "cov1Para","cov2Para"))

ggplot(data_ggplot,
       aes(x = sd, y = returns, color = method,shape = method)) +
  geom_point(size = 6) +
  scale_color_manual(values = setNames(legend_setting$color, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  scale_shape_manual(values = setNames(legend_setting$shape, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  labs(color = "Methods", shape = "Methods") +
  theme(legend.position = "right",
        legend.text = element_text(size=16),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(size=20),
        axis.text.y  = element_text(size=20)) +
  geom_label_repel(aes(label = label), 
                   data = nls_labels,
                   direction = "y",
                   segment.color = "grey",
                   color = "darkgreen",
                   # vjust = 1,
                   nudge_y = 1.5) +
  theme_hsg() +
  xlab("returns standard deviation (%), annual") +
  ylab("returns mean (%), annual") +
  scale_x_continuous(breaks = 0:25) +
  scale_y_continuous(breaks = 0:24)

# estimation 2520 days, test 126 days, 01-01-1970 TO 01-12-2019
data_ggplot <- tibble(
  method = results_data$method, 
  returns = results_data$mu, 
  sd = results_data$sd_overall
) %>% left_join(legend_setting) %>%
  filter(!method %in% c("factor1"))

nls_labels <- data_ggplot %>% 
  filter(method %in% 
           c("gis","qis","lis",
             "sample","cov1Para","cov2Para","covCor"
             ))

ggplot(data_ggplot,
       aes(x = sd, y = returns, color = method,shape = method)) +
  geom_point(size = 6) +
  scale_color_manual(values = setNames(legend_setting$color, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  scale_shape_manual(values = setNames(legend_setting$shape, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  labs(color = "Methods", shape = "Methods") +
  theme(legend.position = "right",
        legend.text = element_text(size=16),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(size=20),
        axis.text.y  = element_text(size=20)) +
  geom_label_repel(aes(label = label), 
                   data = nls_labels,
                   direction = "y",
                   segment.color = "grey",
                   color = "darkgreen",
                   # vjust = 1,
                   nudge_y = 1
                   ) +
  theme_hsg() +
  xlab("returns standard deviation (%), annual") +
  ylab("returns mean (%), annual") +
  ylim(c(12,21)) +
  scale_x_continuous(breaks = 0:30) +
  scale_y_continuous(breaks = 0:30) 


# ------------------------------------------------------------------------------
#                 NORMALIZED NAV EVOLUTION
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#                 WEIGHT DISTRIBUTION - BOXPLOT
# ------------------------------------------------------------------------------

all_weights_120 <- all_weights 

# ------------------------------------------------------------------------------
#                
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#                
# ------------------------------------------------------------------------------

# ##############################################################################
#                 APPENDIX FIGURES
# ##############################################################################

z <- zoo(ff100_data$daily[, -1], order.by = ff100_data$daily$Date) # assuming first column is Date and the rest are Asset prices

compute_pairwise_cor_with_percentiles <- function(data) {
  n <- ncol(data)
  correlations <- numeric()
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      cor_ij <- cor(data[, i], data[, j], use = "complete.obs")
      if(!is.na(cor_ij)) correlations <- c(correlations, cor_ij)
    }
  }
  
  avg_cor <- mean(correlations, na.rm = TRUE)
  p5_cor <- quantile(correlations, probs = 0.05, na.rm = TRUE)
  p95_cor <- quantile(correlations, probs = 0.95, na.rm = TRUE)
  
  # Return as a single-row matrix with named columns
  return(matrix(c(avg_cor, p5_cor, p95_cor), nrow = 1, dimnames = list(NULL, c("Average", "P5", "P95"))))
}

# Applying the function with rollapply
# Assuming 'z' is your zoo object with appropriate data
rolling_stats_1260 <- rollapply(z, width = 1260, FUN = compute_pairwise_cor_with_percentiles, by.column = FALSE, align = "right")

# The result will be a matrix where each row corresponds to a time point and each column to one of the statistics


rolling_stats %>% 
  fortify.zoo() %>% 
  ggplot(aes(Index, Average)) +
  geom_ribbon(aes(ymin = P5, ymax=P95), fill = "darkslategray4", alpha = 0.5) +
  geom_line(size = 1) +
  theme_minimal() +
  ylab("5 years rolling correlations, average, 95th and 5th percentiles") +
  xlab("Date") +
  ylim(c(0,1))







