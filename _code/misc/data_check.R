# checking data
ff100_data$monthly %>% 
  pivot_longer(!Date) %>% 
  filter(name == "ME1 BM4") %>% 
  ggplot(aes(x=Date, y=value, color = name)) +
  geom_line()

ff100_data$monthly %>% 
  pivot_longer(!Date) %>% 
  group_by(Date) %>% 
  summarise(mean = mean(value)) %>% 
  ungroup %>% 
  ggplot(aes(x=Date, y=mean)) +
  geom_line()

ff100_data$monthly %>% 
  pivot_longer(!Date) %>% 
  filter(Date > "1973-05-01" & Date < "2018-03-01") %>% 
  group_by(Date) %>%
  summarise(mean = mean(value))

test_sample <- lapply(
  roll, 
  get_portfolio_metrics,
  stock_returns = ff100_data$monthly,
  cov_est_method = "sample",
  portfolio_optimization = "tangent"
) %>% 
  reduce(rbind) %>% 
  na.omit %>% 
  summarise(mean = mean(returns))

test_rolling_cov_method[901:990] %>% reduce(rbind) %>% summarise(mean = mean(na.omit(returns)))

test_rolling_cov_method %>% 
  reduce(rbind) %>%
  cbind(crossing(cov_est_method, roll)) %>% 
  filter(date == "1973-06-01")

cov_sample <- ff100_data$monthly %>% 
  filter(Date < "1973-06-01") %>% 
  dplyr::select(-Date) %>% 
  cov

mean_sample <- ff100_data$monthly %>% 
  filter(Date < "1973-06-01") %>% 
  dplyr::select(-Date) %>% 
  colMeans

inv_cov_sample <- solve(cov_sample) 

tp <- tangency.portfolio(mean_sample, cov_sample, 0.0001)

w <- tp$weights
ret_test <- ff100_data$monthly %>% 
  filter(Date == "1973-06-01") %>% 
  dplyr::select(-Date)

sum(ret_test*w)

w_hand <- (inv_cov_sample %*% mean_sample)/ 
  sum(inv_cov_sample %*% mean_sample)

rowSums(ret_test*w_hand)

ret <- c(38.92,-53,-12,-6,15,12)
ret %>% mean












  
  
  
  
  
  
  
  
