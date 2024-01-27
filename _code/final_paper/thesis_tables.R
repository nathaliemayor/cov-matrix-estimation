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
#                CUMMULATIVE RETURN 
# ------------------------------------------------------------------------------
# test_rolling_cov_method <- results_rdata$d504$complete_results

method_order <- crossing(cov_est_method, roll) %>% 
  dplyr::select(cov_est_method) %>% 
  unique()

names(test_rolling_cov_method) <- rep(
  method_order$cov_est_method, 
  each=length(roll)
)

results_by_cov <- lapply(
  seq(1,(length(method_order$cov_est_method)-1)*length(roll)+1, length(roll)), 
  function(x) 
    test_rolling_cov_method[x:(x+length(roll)-1)]
)

names(results_by_cov) <- method_order$cov_est_method

returns <- lapply(method_order$cov_est_method, function(cov) 
  results_by_cov[[cov]] %>% 
    map_depth(1,1) %>%
    reduce(rbind) %>% 
    filter(!is.na(returns)) 
) 

# Calculate price-like index
prices <- lapply(returns, function(cov){
  log_returns <- log(1 + cov[,2]/100)
  cumulative_log_returns <- cumsum(log_returns)
  initial_price <- 1  # example initial price
  total_price_evolution <- initial_price * exp(cumulative_log_returns)
}) %>% reduce(cbind) 
colnames(prices) <- method_order$cov_est_method
dates <- returns[[1]][,1]
prices$date <- dates$date

plot <- prices %>% pivot_longer(!date) %>% 
  # filter(name =="sample") %>% 
  ggplot(aes(x=date, y=value,color=name)) +
  geom_line()

plotly::ggplotly(plot)



# ------------------------------------------------------------------------------
#                 HERFINDHAL INDEX
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#                 MAXIMUM DRAWDOWN
# ------------------------------------------------------------------------------

max_drawdown <- lapply(method_order$cov_est_method, function(cov){
  # Calculate the running maximum
  running_max <- cummax(prices[,cov])
  # Calculate drawdowns
  drawdowns <- (running_max - prices[,cov]) / running_max
  # Find the maximum drawdown
  max_drawdown <- max(drawdowns)
  date_max <- prices$date[which(drawdowns == max(drawdowns))]
  return(data.frame(maxdraw = max_drawdown, date_max_draw = date_max))
}) %>% reduce(rbind) %>% mutate(method = method_order$cov_est_method)


ret_vec <- returns[[2]][,-1]

returns[[1]][9918,]


library(fTrading)

maxDrawDown(cumsum(ret_vec$returns/100))

# ------------------------------------------------------------------------------
#                 WEIGHTS DISTRIBUTION
# ------------------------------------------------------------------------------

results_rdata$d252$weights %>% 
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

# ------------------------------------------------------------------------------
#                 GLOBAL TURNOVER RATE
# ------------------------------------------------------------------------------
gtr_504d <- lapply(results_rdata$d504$weights$method %>% unique, 
       get_global_turnover,
       weights = results_rdata$d504$weights, 
       stock_returns = ff100_data$daily, 
       frequency_day_month = "day") %>% 
  suppressMessages %>% 
  suppressWarnings %>% 
  reduce(append) %>%  
  data.frame(method = results_rdata$d504$weights$method %>% unique,
                                    gtr = .) %>% 
  mutate(gtr = 100*gtr)

gtr_252d <- lapply(results_rdata$d252$weights$method %>% unique, 
                   get_global_turnover,
                   weights = results_rdata$d252$weights, 
                   stock_returns = ff100_data$daily, 
                   frequency_day_month = "day") %>% 
  suppressMessages %>% 
  suppressWarnings %>%   
  reduce(append)   %>%  
  data.frame(method = results_rdata$d252$weights$method %>% unique,
             gtr = .) %>% 
  mutate(gtr = 100*gtr)

# ------------------------------------------------------------------------------
#                 SHORT SHARE OF THE PORTFOLO
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#                 MEAN ABSOLUTE DEVIATION
# ------------------------------------------------------------------------------
mad_ew_504d <- results_rdata$d504$weights %>% 
  group_by(date, method) %>% 
  dplyr::summarise(MAD = mean(abs(value - 1/n()))) %>% 
  ungroup() %>%
  group_by(method) %>% 
  dplyr::summarise(mean_mad = mean(MAD)*100) %>% 
  ungroup

mad_ew_252 <- results_rdata$d252$weights %>% 
  group_by(date, method) %>% 
  dplyr::summarise(MAD = mean(abs(value - 1/n()))) %>% 
  ungroup() %>%
  group_by(method) %>% 
  dplyr::summarise(mean_mad = mean(MAD)*100) %>% 
  ungroup


# ##############################################################################
#                 APPENDIX TABLES
# ##############################################################################