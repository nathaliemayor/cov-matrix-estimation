bootstrap_cov_estimates <- function( 
    roll,
    n_bootstraps,
    cov_est_method,
    data = ff100_data$daily,
    frequency = "daily", 
    factor_returns
    ) {
  stock_returns <- bootstrapped_portfolios(data, n_bootstraps)
  factors <- bootstrapped_portfolios(factor_returns %>% rename(Date=date),1)
  
  test_rolling_bootstrap <- pmap(
    crossing(stock_returns, roll),
    get_portfolio_metrics, 
    cov_est_method = cov_est_method,
    portfolio_optimization = "tangent",
    short = TRUE, 
    factor_returns = factors[[1]],
    frequency = frequency
  )
  
  names(test_rolling_bootstrap) <- rep(
    1:n_bootstraps, 
    each=length(roll)
  )
  
  results_by_bootstrap <- lapply(
    seq(1,(n_bootstraps-1)*length(roll)+1, length(roll)), 
    function(x) 
      test_rolling_bootstrap[x:(x+length(roll)-1)]
  )
  
  names(results_by_bootstrap) <- 1:n_bootstraps
  
  all_avg_returns_bootstrap <- lapply(1:n_bootstraps, function(x) 
    results_by_bootstrap[[x]] %>% 
      map_depth(1,1) %>%
      reduce(rbind) %>% 
      filter(!is.na(returns)) %>% 
      dplyr::summarise(mean = mean(returns))
  ) %>% 
    unlist %>% 
    reduce(append)
  
  all_sd_bootstrap <- lapply(1:n_bootstraps, function(x) 
    results_by_bootstrap[[x]] %>% 
      map_depth(1,1) %>%
      reduce(rbind) %>% 
      filter(!is.na(returns)) %>% 
      dplyr::summarise(sd = sd(returns))
  ) %>% 
    unlist %>% 
    reduce(append)
  
  all_avg_sd_bootstrap <- lapply(1:n_bootstraps, function(x) 
    results_by_bootstrap[[x]] %>% 
      map_depth(1,2) %>% 
      reduce(append) %>% 
      na.omit %>% 
      mean) %>% 
    reduce(append)
  
  avg_sr <- lapply(1:n_bootstraps, function(x) 
    all <- results_by_bootstrap[[x]] %>% 
      map_depth(1,1) %>%
      reduce(rbind) %>% 
      filter(!is.na(returns)) %>% 
      left_join(TNX$TNX.Adjusted %>% fortify.zoo %>% rename(date=Index)) %>% 
      mutate(year = year(date)) %>%
      group_by(year) %>% 
      dplyr::summarise(mean_returns = mean(returns),
                       sd = sd(returns),
                       rf = mean(TNX.Adjusted, na.rm = T)/252,
                       sr = mean((mean_returns-rf)/sd)
                       ) %>% ungroup %>% 
      dplyr::summarise(sr=mean(sr, na.rm = T))
  ) %>% 
    unlist %>% 
    reduce(append)
  
  df <- data.frame(method = cov_est_method, 
                   returns=all_avg_returns_bootstrap,
                   sd = all_avg_sd_bootstrap,
                   sr = avg_sr,
                   sd_overall = all_sd_bootstrap)
  return(df)
}
