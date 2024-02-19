# ##############################################################################
#                 THESIS TABLES
# ##############################################################################
load(file.choose())
load(file.path(core_path,data_path,"results","results_figures_tables_1970-2019D.RData"))
legend_setting <-  data.frame(
  method = c("cov1Para", "cov2Para", "covCor","covDiag", 
             "covMarket","gis","qis","lis",
             "CovMve","CovMcd",
             "huge_glasso",
             "CCM","factor1","factor3","RMT","ewma",
             "sample", "SP500","equal_weights",
             "sample_short_constraint"),
  label = c("LS-1P","LS-2P","LS-CCM","LS-D","LS-SIM","NS-GIS","NS-QIS","NS-LIS",
            "MVE","MCD","GLASSO","CCM","SIM","MIM","RMT","EWMA",
            "Sample","SP500","EQW","W+"),
  color = c("tomato3","blue","forestgreen","orange","purple","red","blue","orange","magenta3",
            "darkblue","black","darkred","magenta","cornflowerblue","blue1","pink3","red4","cornflowerblue",
            "black","darkorange"),
  shape = c(19,19,19,19,19,25,25,25,15,15,10,17,17,17,17,11,8,8,8,8),
  line_shape =c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2)
)
# ------------------------------------------------------------------------------
#                 MAIN RESULTS TABLE
# ------------------------------------------------------------------------------

res_thesis <- results_rdata$d2520_21$results %>% 
  left_join(legend_setting) %>%   
  dplyr::select(-method, -shape, -line_shape, -shape, -color) %>% 
  dplyr::select(label,everything()) %>% 
  dplyr::arrange(
    factor(
      label, 
      levels = legend_setting$label
      )
    ) 

res_thesis %>% stargazer::stargazer(summary=F, rownames = F)

# ------------------------------------------------------------------------------
#                CUMMULATIVE RETURN 
# ------------------------------------------------------------------------------
all_returns <- results_rdata$d2520_21$returns

# Calculate price-like index
prices <- lapply(colnames(all_returns)[-1], function(method){
  log_returns <- log(1 + as.numeric(all_returns[,method])/100)
  cumulative_log_returns <- cumsum(log_returns)
  initial_price <- 1  # example initial price
  total_price_evolution <- initial_price * exp(cumulative_log_returns)
}) %>% reduce(cbind) %>% data.frame
colnames(prices) <- colnames(all_returns)[-1]
prices$date <- all_returns$date

last_points <- prices %>% 
  mutate(CCM = CCM) %>%
  pivot_longer(!date) %>% 
  rename(method = name) %>% 
  left_join(legend_setting, by = "method") %>% 
  group_by(label) %>% 
  dplyr::summarise(last = ceiling(last(value)), 
                   date_max = as.Date("2023-01-01")) %>% 
  ungroup()

prices %>% 
  mutate(CCM = CCM) %>%
  pivot_longer(!date) %>% 
  rename(method = name) %>% 
  left_join(legend_setting, by = "method") %>% 
  ggplot(aes(x=date, y=value,color=label)) +
  geom_line() +
  scale_color_manual(values = setNames(legend_setting$color, legend_setting$label),
                     breaks = legend_setting$label,
                     labels = legend_setting$label) +
  scale_shape_manual(values = setNames(legend_setting$line_shape, legend_setting$label),
                     breaks = legend_setting$label,
                     labels = legend_setting$label) +
  theme_minimal() +
  xlim(c(as.Date("1980-01-01"), as.Date("2025-01-01"))) +
  geom_label_repel(data = last_points,
                   aes(x = date_max, y = last, label = paste(label, last, sep=": ")),
                   nudge_y = 2,
                   size = 3,
                   max.overlaps =1000,
                   direction = "y", 
                   segment.size = 0) +
  ylab("Cummulative Performance") +
  theme(legend.position = "none")

# ------------------------------------------------------------------------------
#                 MAXIMUM DRAWDOWN
# ------------------------------------------------------------------------------

calculate_max_drawdown_with_period <- function(portfolio_values) {
  peak_value <- portfolio_values[1]
  drawdowns <- numeric(length(portfolio_values))
  start_index <- 1
  max_drawdown <- 0
  max_drawdown_start <- 1
  max_drawdown_end <- 1
  
  for (i in seq_along(portfolio_values)) {
    if (portfolio_values[i] > peak_value) {
      peak_value <- portfolio_values[i]
      start_index <- i
    }
    
    drawdowns[i] <- (peak_value - portfolio_values[i]) / peak_value
    
    if (drawdowns[i] > max_drawdown) {
      max_drawdown <- drawdowns[i]
      max_drawdown_start <- start_index
      max_drawdown_end <- i
    }
  }
  dates_prices <- prices$date

  c(max_drawdown, max_drawdown_start,max_drawdown_end)
}

MDD_2520_21 <- lapply(colnames(prices)[-length(prices)], function(x){
  calculate_max_drawdown_with_period(prices[,x])
}) %>% reduce(rbind) %>% 
  data.frame %>% 
  rename(MDD = X1, start = X2, end=X3) %>% 
  mutate(method = colnames(prices)[-length(prices)],
         start = prices$date[start],
         end = prices$date[end], 
         MDD = round(MDD*100,2)) 

rownames(MDD_2520_21) <- NULL

# ------------------------------------------------------------------------------
#                 HERFINDHAL INDEX
# ------------------------------------------------------------------------------

hhi_2520_21 <- results_rdata$d2520_21$weights %>% 
  group_by(method, date) %>% 
  dplyr::summarise(hhi = sum(value^2)) %>% 
  ungroup() %>% 
  group_by(method) %>% 
  dplyr::summarise(mean_hhi = round(mean(hhi),3))

# ------------------------------------------------------------------------------
#                 WEIGHTS DISTRIBUTION
# ------------------------------------------------------------------------------

w_distribution_2520_21 <- results_rdata$d2520_21$weights %>% 
  dplyr::group_by(method) %>% 
  dplyr::summarise(
    sd = round(sd(value)*100,2),
    median = round(median(value)*100,2),
    min = round(min(value)*100,2),
    max = round(max(value*100),2),
    q5 = round(quantile(value*100, probs = 0.05),2),
    q25 = round(quantile(value*100, probs = 0.25),2),
    q75 = round(quantile(value*100, probs = 0.75),2), 
    q95 = round(quantile(value*100, probs = 0.95),2)
  )

left_join(legend_setting %>% select(method,label), w_distribution_2520_21) %>% 
  select(-method) %>% 
  stargazer::stargazer(summary=F, align=T, rownames=NULL)

legend_setting %>% 
  left_join(results_rdata$d2520_21$weights) %>% 
  rename(weights = value) %>% 
  filter(!method %in% c("gis", "lis","equal_weights","cov2Para")) %>%
  filter(date > as.Date("2005-01-01")) %>%
  ggplot(aes(x=date, y=weights, group = date)) +
  geom_boxplot(fill = "orange") +
  theme_hsg() +
  facet_wrap(~label, ncol = 3) +
  theme(panel.spacing = unit(1, "lines"))
  
# ------------------------------------------------------------------------------
#                 GLOBAL TURNOVER RATE
# ------------------------------------------------------------------------------
gtr_2520_21 <- lapply(results_rdata$d2520_21$weights$method %>% unique, 
       get_global_turnover,
       weights = results_rdata$d2520_21$weights, 
       stock_returns = ff100_data$daily, 
       frequency_day_month = "day") %>% 
  suppressMessages %>% 
  suppressWarnings %>% 
  reduce(append) %>%  
  data.frame(method = results_rdata$d2520_21$weights$method %>% unique,
                                    gtr = .) %>% 
  mutate(gtr = round(gtr*100,2))

# ------------------------------------------------------------------------------
#                 SHORT SHARE OF THE PORTFOLO
# ------------------------------------------------------------------------------

short_2520_21 <- results_rdata$d2520_21$weights %>% 
  group_by(method, date) %>% 
  dplyr::summarise(abs_neg = sum(abs(value[value < 0])),
                   abs_all = sum(abs(value))) %>%
  ungroup() %>% 
  group_by(method) %>% 
  dplyr::summarise(short = round(mean(abs_neg/abs_all)*100,2)) %>% 
  ungroup()

# ------------------------------------------------------------------------------
#                 MEAN ABSOLUTE DEVIATION
# ------------------------------------------------------------------------------ 
mad_ew_2520_21 <- results_rdata$d2520_21$weights %>% 
  group_by(date, method) %>% 
  dplyr::summarise(MAD = mean(abs(value - 1/n()))) %>% 
  ungroup() %>%
  group_by(method) %>% 
  dplyr::summarise(mean_mad = round(mean(MAD*100),2)) %>% 
  ungroup

# ------------------------------------------------------------------------------
#                 MAIN WEIGHT TABLES
# ------------------------------------------------------------------------------

w_table_2520_21 <- left_join(legend_setting %>% select(method,label), res_thesis) %>% 
  left_join(short_2520_21) %>% 
  left_join(gtr_2520_21) %>% 
  left_join(mad_ew_2520_21) %>% 
  left_join(hhi_2520_21) %>% 
  left_join(MDD_2520_21 %>% select(method, MDD)) %>% 
  select(label, everything()) %>% 
  mutate(mu = round(mu,2), sd_overall = round(sd_overall,2), sr_roll = round(sr_roll,2)) %>% 
  select(-method, -sd_window,-sd_roll, -sr_window, -ret_roll) %>% as.matrix()

rownames(w_table_2520_21) <- NULL

w_table_2520_21 %>% stargazer::stargazer(
  summary = F, rownames = NULL
  )

# ------------------------------------------------------------------------------
#                 ANALYSIS - BOOTSTRAPPED PORTFOLIO DATA
# ------------------------------------------------------------------------------

files_bootstrap <- list.files(
  path = file.path(core_path, data_path, "bootstrap", "v2"), 
  full.names = T
)

bt_results <- lapply(files_bootstrap, function(x){
  get(load(x))
}) %>% reduce(rbind) %>% 
  mutate(sr = sr*sqrt(252),
         sd = sd*sqrt(252),
         returns = returns * 252) %>% 
  left_join(legend_setting,.) %>% 
  select(-method) %>% 
  rename(method = label) %>% 
  plyr::arrange(factor(method, legend_setting$label)) %>% 
  select(method, returns, sd, sr) %>% 
  filter(!is.na(returns)) 
  

# confidence intervals

# SR distributions

means <- bt_results %>%
  group_by(method) %>%
  dplyr::summarise(mean_sr = mean(sr, na.rm = TRUE),
                   lower_bound = quantile(sr, 0.025, na.rm = TRUE),
                   upper_bound = quantile(sr, 0.975, na.rm = TRUE)) %>% 
  plyr::arrange(factor(method, legend_setting$label)) 

bt_results$method <- factor(bt_results$method, levels = legend_setting$label)
# Plot
ggplot(bt_results, aes(x = sr)) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot of Sharpe Ratios",
       x = "Sharpe Ratio", y = "Density") +
  theme_minimal() +
  geom_vline(data = means, aes(xintercept = mean_sr, group = method, color = "mean"),
            linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = 0,
             color = "zero"), linetype = "dotted", size = 1) +
  geom_segment(data=means, 
               aes(x=lower_bound, y=0, xend=lower_bound, yend = 0.5, color = "95% CI bands")) +
  geom_segment(data=means, 
               aes(x=upper_bound, y=0, xend=upper_bound, yend = 0.5, color = "95% CI bands")) +
  geom_text(data = means, aes(x = mean_sr, y = 0, 
                              label = sprintf("%.2f", mean_sr), 
                              group = method),
            color = "red", vjust = -0.5, hjust = -0.1, size = 3, 
            check_overlap = TRUE) +
  facet_wrap(~method, ncol = 3, scales = 'free') +
  xlim(c(-1.5,1.5))+
  ylim(c(0,1.4))+
  scale_color_manual(name = "legend", values = c(mean = "red",`95% CI bands` = "blue", zero = "grey")) +
  theme(legend.position = "top")

# Note: Adjustments might be needed based on your data's structure and the specifics of your plot.

# summary stat bootstrap data

sr <- bt_results %>%
  group_by(method) %>%
  dplyr::summarise(mean_sr =round(mean(sr, na.rm = TRUE),2),
                   sd_sr = round(sd(sr),2),
                   CI95 = paste0("[",round(quantile(sr, 0.025, na.rm = TRUE),2),
                                 ";",round(quantile(sr, 0.975, na.rm = TRUE),2),
                                 "]")) %>% 
  plyr::arrange(factor(method, legend_setting$label)) %>% 
  mutate(method = as.character(method))

re <- bt_results %>%
  group_by(method) %>%
  dplyr::summarise(mean_ret = round(mean(returns, na.rm = TRUE),2),
                   sd_ret = round(sd(returns),2),
                   CI95 = paste0("[",round(quantile(returns, 0.025, na.rm = TRUE),2),
                                 ";",round(quantile(returns, 0.975, na.rm = TRUE),2),
                                 "]")) %>% 
  plyr::arrange(factor(method, legend_setting$label)) %>% 
  mutate(method = as.character(method))

sd <- bt_results %>%
  group_by(method) %>%
  dplyr::summarise(mean_sd = round(mean(sd, na.rm = TRUE),2),
                   sd_sd = round(sd(sd),2),
                   CI95 = paste0("[",round(quantile(sd, 0.025, na.rm = TRUE),2),
                                 ";",round(quantile(sd, 0.975, na.rm = TRUE),2),
                                 "]")) %>% 
  plyr::arrange(factor(method, legend_setting$label)) %>% 
  mutate(method = as.character(method))

re %>%  stargazer::stargazer(summary=F, rownames = F)


# ##############################################################################
#                 APPENDIX TABLES
# ##############################################################################