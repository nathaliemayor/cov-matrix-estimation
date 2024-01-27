# ##############################################################################
#                 THESIS FIGURES
# ##############################################################################
# results_rdata <- list() 

# 252 DAYS, 01-01-1970 TO 01-12-2019
results_252d <- results_data
all_weights_252d <- all_weights
all_weights_252d <- all_weights_252d %>% 
  rbind(sample_daily_weights) %>% 
  rbind(short_restriction_weights)

results_rdata$d252 <- list(results = results_252d, 
                           weights = all_weights_252d,
                           complete_results = test_rolling_cov_method)

# 504 DAYS, 01-01-1970 TO 01-12-2019
results_504d <- results_data 
all_weights_504d <- all_weights %>% 
  rbind(short_restriction_weights)

results_rdata$d504 <- list(results = results_504d, 
                           weights = all_weights_504d,
                           complete_results = test_rolling_cov_method)

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
             "daily_sample","sample_short_constraint"),
  label = c("LS-1P","LS-2P","LS-CCM","LS-D","LS-SIM","NS-GIS","NS-QIS","NS-LIS",
            "MVE","MCD","GLASSO","CCM","SIM","MIM","RMT","SampleM","SP500","EQW",
            "SampleD","W+"),
  color = c("tomato3","blue","forestgreen","orange","purple","red","blue","springgreen","chartreuse1",
            "darkblue","black","darkred","magenta","cornflowerblue","orange3","red4","cornflowerblue",
            "black","darkorange","purple1"),
  shape = c(19,19,19,19,19,25,25,25,15,15,10,17,17,17,17,8,8,8,8,8)
)

# 240 MONTHS, 01-01-1970 TO 01-12-2023
ggplot(
  tibble(
    method = results_rdata$m240$results$method, 
    returns = results_rdata$m240$results$mu, 
    sd = results_rdata$m240$results$sd_window
  ) %>% left_join(legend_setting) %>%
    filter(!method %in% c("")),
  aes(x = sd, y = returns, color = method,shape = method)) +
  geom_point(size = 4) +
  scale_color_manual(values = setNames(legend_setting$color, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  scale_shape_manual(values = setNames(legend_setting$shape, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  labs(color = "Methods", shape = "Methods") +
  theme(legend.position = "right") +
  theme_hsg() +
  xlab("returns standard deviation, annual") +
  ylab("returns mean, annual")

# 180 MONTHS, 01-01-1970 TO 01-12-2019
ggplot(
  tibble(
    method = results_rdata$m180$results$method, 
    returns = results_rdata$m180$results$mu, 
    sd = results_rdata$m180$results$sd_overall
  ) %>% left_join(legend_setting) %>%
    filter(!method %in% c("")),
  aes(x = sd, y = returns, color = method,shape = method)) +
  geom_point(size = 4) +
  scale_color_manual(values = setNames(legend_setting$color, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  scale_shape_manual(values = setNames(legend_setting$shape, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  labs(color = "Methods", shape = "Methods") +
  theme(legend.position = "right") +
  theme_hsg() +
  xlab("returns standard deviation, annual") +
  ylab("returns mean, annual")

# 120 MONTHS, 01-01-1970 TO 01-12-2023
ggplot(
  tibble(
    method = results_rdata$m120$results$method, 
    returns = results_rdata$m120$results$mu, 
    sd = results_rdata$m120$results$sd_window
  ) %>% left_join(legend_setting) %>%
    filter(!method %in% c("cov2Para")),
  aes(x = sd, y = returns, color = method,shape = method)) +
  geom_point(size = 4) +
  scale_color_manual(values = setNames(legend_setting$color, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  scale_shape_manual(values = setNames(legend_setting$shape, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  labs(color = "Methods", shape = "Methods") +
  theme(legend.position = "right") +
  theme_hsg() +
  xlab("returns standard deviation, annual") +
  ylab("returns mean, annual") 

# 92 MONTHS, 01-01-1970 TO 01-12-2019
ggplot(
  tibble(
    method = results_rdata$m92$results$method_order.cov_est_method, 
    returns = results_rdata$m92$results$all_avg_returns*12, 
    sd = results_rdata$m92$results$all_avg_sd*sqrt(12)
  ) %>% left_join(legend_setting) %>%
    filter(!method %in% c("")),
  aes(x = sd, y = returns, color = method,shape = method)) +
  geom_point(size = 4) +
  scale_color_manual(values = setNames(legend_setting$color, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  scale_shape_manual(values = setNames(legend_setting$shape, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  labs(color = "Methods", shape = "Methods") +
  theme(legend.position = "right") +
  theme_hsg() +
  xlab("returns standard deviation, annual") +
  ylab("returns mean, annual")

# 60 MONTHS, 01-01-1970 TO 01-12-2019
ggplot(
  tibble(
    method = results_rdata$m60$results$method, 
    returns = results_rdata$m60$results$mu, 
    sd = results_rdata$m60$results$sd_overall
  ) %>% left_join(legend_setting) %>%
    filter(!method %in% c("")),
  aes(x = sd, y = returns, color = method,shape = method)) +
  geom_point(size = 4) +
  scale_color_manual(values = setNames(legend_setting$color, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  scale_shape_manual(values = setNames(legend_setting$shape, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  labs(color = "Methods", shape = "Methods") +
  theme(legend.position = "right") +
  theme_hsg() +
  xlab("returns standard deviation, annual") +
  ylab("returns mean, annual")

# 252 Days, 01-01-1970 TO 01-12-2019
ggplot(
  tibble(
    method = results_rdata$d252$results$method, 
    returns = results_rdata$d252$results$mu, 
    sd = results_rdata$d252$results$sd_overall
  ) %>% left_join(legend_setting) %>%
    filter(!method %in% c("sample")),
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
  theme_hsg() +
  xlab("returns standard deviation, annual") +
  ylab("returns mean, annual")

# 504 Days, 01-01-1980 TO 01-12-2019
ggplot(
  tibble(
    method = results_rdata$d504$results$method_order.cov_est_method, 
    returns = results_rdata$d504$results$all_avg_returns, 
    sd = results_rdata$d504$results$all_avg_sd
  ) %>% left_join(legend_setting) %>%
    filter(!method %in% c("")),
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
  theme_hsg() +
  xlab("returns standard deviation, annual") +
  ylab("returns mean, annual")

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
