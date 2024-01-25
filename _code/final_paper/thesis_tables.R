# ##############################################################################
#                 THESIS TABLES
# ##############################################################################
load(file.choose())
# results_m <- results_rdata
# results_d <- results_rdata

legend_setting <-  data.frame(
  method = c("cov1Para", "cov2Para", "covCor","covDiag", 
             "covMarket","gis","qis","lis",
             "CovMve","CovMcd",
             "huge_glasso",
             "CCM","factor1","factor3","RMT",
             "sample", "SP500","equal_weights",
             "daily_sample","sample_short_constraint"),
  estimation_method = c("LS-1P","LS-2P","LS-CCM","LS-D","LS-SIM","NS-GIS","NS-QIS","NS-LIS",
            "MVE","MCD","GLASSO","CCM","SIM","MIM","RMT","SampleM","SP500","EQW",
            "SampleD","W+")
)
# ------------------------------------------------------------------------------
#                 MAIN RESULTS TABLE
# ------------------------------------------------------------------------------

res_thesis <- results_rdata$m180$results %>% 
  # rename(method = method_order.cov_est_method,
  #        mean = all_avg_returns,
  #        sd_window = all_avg_sd,
  #        sr_window = all_avg_sr,
  #        sd_overall = all_sd,
  #        sr_overall = sr_computed) %>% 
  left_join(legend_setting) %>%   
  select(-method) %>% 
  select(estimation_method,everything()) %>% 
  dplyr::arrange(
    factor(
      estimation_method, 
      levels = legend_setting$estimation_method
      )
    ) 

res_thesis %>% stargazer::stargazer(summary=F, rownames = F)

# ------------------------------------------------------------------------------
#                 WEIGHTS DISTRIBUTION
# ------------------------------------------------------------------------------

results_rdata$m240$weights %>% 
  dplyr::group_by(method) %>% 
  dplyr::summarise(
    sd = sd(value),
    median = median(value),
    min = min(value),
    max = max(value),
    q5 = quantile(value, probs = 0.05),
    q25 = quantile(value, probs = 0.25),
    q75 = quantile(value, probs = 0.75), 
    q95 = quantile(value, probs = 0.95)
  )

mad_ew <- results_rdata$d504$weights %>% 
  # filter(method == "CCM") %>% 
  group_by(date, method) %>% 
  dplyr::summarise(MAD = mean(abs(value - 1/n()))) %>% 
  ungroup() %>%
  group_by(method) %>% 
  dplyr::summarise(mean_mad = mean(MAD)*100) %>% 
  ungroup

get_global_turnover(weights = results_rdata$d504$weights, 
                    stock_returns = ff100_data$daily, 
                    frequency_day_month = "day", 
                    method_cov = "sample")


gtr_504d <- lapply("sample", 
       get_global_turnover,
       weights = results_rdata$d504$weights, 
       stock_returns = ff100_data$daily, 
       frequency_day_month = "day") %>% 
  reduce(append)

results_rdata$d252$weights$method %>% unique
gtr_252d <- lapply("sample", 
                   get_global_turnover,
                   weights = results_rdata$d252$weights, 
                   stock_returns = ff100_data$daily, 
                   frequency_day_month = "day") %>% 
  reduce(append)


test_tr <- results_rdata$d252$weights %>% 
  filter(method == "sample") %>% 
  select(-method) %>% 
  group_by(name) %>% 
  dplyr::summarise(lagged_value = abs(diff(value)), date = date[-1]) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  dplyr::summarise(sum(lagged_value))

test_tr$`sum(lagged_value)` %>% mean(na.rm = T)

# ------------------------------------------------------------------------------
#                 MEAN ABSOLUTE DEVIATION
# ------------------------------------------------------------------------------



# ##############################################################################
#                 APPENDIX TABLES
# ##############################################################################