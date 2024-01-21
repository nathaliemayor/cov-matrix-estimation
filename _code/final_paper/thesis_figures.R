# ##############################################################################
#                 THESIS FIGURES
# ##############################################################################
# results_rdata <- list() 

# 240 MONTHS, 01-01-1970 TO 01-12-2019
results_240 <- results_data
all_weights_240 <- all_weights
results_240 <- results_240 %>% 
  add_row(
    method_order.cov_est_method = "daily_sample", 
    all_avg_returns = ret_d$mean, 
    all_avg_sd = sd_d, 
    all_avg_sr = sr_d
  ) %>% 
  add_row(
    method_order.cov_est_method = "sample_short_constraint", 
    all_avg_returns = ret_short, 
    all_avg_sd = sd_short,
    all_avg_sr = sr_short
  )
all_weights_240 <- all_weights_240 %>% 
  rbind(sample_daily_weights) %>% 
  rbind(short_restriction_weights)
results_rdata$m240 <- list(results = results_240, weights = all_weights_240)

# 180 MONTHS, 01-01-1984 TO 01-12-2023
results_180 <- results_data
all_weights_180 <- all_weights
results_180 <- results_180 %>% 
  add_row(
    method_order.cov_est_method = "daily_sample", 
    all_avg_returns = ret_d$mean, 
    all_avg_sd = sd_d, 
    all_avg_sr = sr_d
  ) %>% 
  add_row(
    method_order.cov_est_method = "sample_short_constraint", 
    all_avg_returns = ret_short, 
    all_avg_sd = sd_short,
    all_avg_sr = sr_short
  )

all_weights_180 <- all_weights_180 %>% 
  rbind(sample_daily_weights) %>% 
  rbind(short_restriction_weights)
results_rdata$m180 <- list(results = results_180, weights = all_weights_180)

# 120 MONTHS, 01-01-1980 TO 01-12-2023
results_120 <- results_data
all_weights_120 <- all_weights
results_120 <- results_120 %>% 
  add_row(
    method_order.cov_est_method = "daily_sample", 
    all_avg_returns = ret_d$mean, 
    all_avg_sd = sd_d, 
    all_avg_sr = sr_d
  ) %>% 
  add_row(
    method_order.cov_est_method = "sample_short_constraint", 
    all_avg_returns = ret_short, 
    all_avg_sd = sd_short,
    all_avg_sr = sr_short
  )
all_weights_120 <- all_weights_120 %>% 
  rbind(sample_daily_weights) %>% 
  rbind(short_restriction_weights)
results_rdata$m120 <- list(results = results_120, weights = all_weights_120)

# 92 MONTHS, 01-01-1959 TO 01-12-2023
results_92 <- results_data
all_weights_92 <- all_weights
results_92 <- results_92 %>% 
  add_row(
    method_order.cov_est_method = "daily_sample", 
    all_avg_returns = ret_d, 
    all_avg_sd = sd_d, 
    all_avg_sr = sr_d
  ) %>% 
  add_row(
    method_order.cov_est_method = "sample_short_constraint", 
    all_avg_returns = ret_short, 
    all_avg_sd = sd_short,
    all_avg_sr = sr_short
  )
all_weights_92 <- all_weights_92 %>% 
  rbind(sample_daily_weights) %>% 
  rbind(short_restriction_weights)
results_rdata$m92 <- list(results = results_92, weights = all_weights_92)

# 60 MONTHS, 01-01-1959 TO 01-12-2023
results_60 <- results_data
all_weights_60 <- all_weights
results_60 <- results_60 %>% 
  add_row(
    method_order.cov_est_method = "daily_sample", 
    all_avg_returns = ret_d$mean, 
    all_avg_sd = sd_d, 
    all_avg_sr = sr_d
  ) 

all_weights_60 <- all_weights_60 %>% 
  rbind(sample_daily_weights) %>% 
  rbind(short_restriction_weights)
results_rdata$m60 <- list(results = results_60, weights = all_weights_60)

save(
  results_rdata, 
  file=file.path(core_path,data_path,"results","results_figures_tables_1970-2019.RData")
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
  color = c("red","blue","green","orange","purple","red","blue","green","purple",
            "blue","black","darkred","magenta","darkgreen","khaki","blue","red",
            "black","darkorange","green"),
  shape = c(19,19,19,19,19,25,25,25,24,24,10,17,17,17,17,8,8,8,8,8)
)

# 240 MONTHS, 01-01-1970 TO 01-12-2023
ggplot(
  tibble(
    method = results_rdata$m240$results$method_order.cov_est_method, 
    returns = results_rdata$m240$results$all_avg_returns, 
    sd = results_rdata$m240$results$all_avg_sd
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
  xlab("monthly returns standard deviation") +
  ylab("monthly returns mean")

# 180 MONTHS, 01-01-1970 TO 01-12-2019
ggplot(
  tibble(
    method = results_rdata$m180$results$method_order.cov_est_method, 
    returns = results_rdata$m180$results$all_avg_returns, 
    sd = results_rdata$m180$results$all_avg_sd
  ) %>% left_join(legend_setting) %>%
    filter(!method %in% c("")),
  aes(x = sd, y = returns, color = method,shape = method)) +
  geom_point(size = 3) +
  scale_color_manual(values = setNames(legend_setting$color, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  scale_shape_manual(values = setNames(legend_setting$shape, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  labs(color = "Methods", shape = "Methods") +
  theme(legend.position = "right") +
  theme_hsg() +
  xlab("monthly returns standard deviation") +
  ylab("monthly return mean")

# 120 MONTHS, 01-01-1970 TO 01-12-2023
ggplot(
  tibble(
    method = results_rdata$m120$results$method_order.cov_est_method, 
    returns = results_rdata$m120$results$all_avg_returns, 
    sd = results_rdata$m120$results$all_avg_sd
  ) %>% left_join(legend_setting) %>%
    filter(!method %in% c("")),
  aes(x = sd, y = returns, color = method,shape = method)) +
  geom_point(size = 3) +
  scale_color_manual(values = setNames(legend_setting$color, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  scale_shape_manual(values = setNames(legend_setting$shape, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  labs(color = "Methods", shape = "Methods") +
  theme(legend.position = "right") +
  theme_hsg() +
  xlab("monthly returns standard deviation") +
  ylab("monthly return mean")

# 92 MONTHS, 01-01-1959 TO 01-12-2023
ggplot(
  tibble(
    method = results_rdata$m92$results$method_order.cov_est_method, 
    returns = results_rdata$m92$results$all_avg_returns*12, 
    sd = results_rdata$m92$results$all_avg_sd*sqrt(12)
  ) %>% left_join(legend_setting) %>%
    filter(!method %in% c("")),
  aes(x = sd, y = returns, color = method,shape = method)) +
  geom_point(size = 3) +
  scale_color_manual(values = setNames(legend_setting$color, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  scale_shape_manual(values = setNames(legend_setting$shape, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  labs(color = "Methods", shape = "Methods") +
  theme(legend.position = "right") +
  theme_hsg() +
  xlab("monthly returns standard deviation") +
  ylab("monthly return mean")

# 60 MONTHS, 01-01-1980 TO 01-12-2023
ggplot(
  tibble(
    method = results_rdata$m60$results$method_order.cov_est_method, 
    returns = results_rdata$m60$results$all_avg_returns, 
    sd = results_rdata$m60$results$all_avg_sd
  ) %>% left_join(legend_setting) %>%
    filter(!method %in% c("")),
  aes(x = sd, y = returns, color = method,shape = method)) +
  geom_point(size = 3) +
  scale_color_manual(values = setNames(legend_setting$color, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  scale_shape_manual(values = setNames(legend_setting$shape, legend_setting$method),
                     breaks = legend_setting$method,
                     labels = legend_setting$label) +
  labs(color = "Methods", shape = "Methods") +
  theme(legend.position = "right") +
  theme_hsg() +
  xlab("yearly returns standard deviation") +
  ylab("yearly return mean")

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
