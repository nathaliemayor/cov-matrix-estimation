bootstrap_cov_estimates <- function( 
    roll,
    n_bootstraps,
    cov_est_method
    ) {
  stock_returns <- bootstrapped_portfolios(ff100_data$monthly, n_bootstraps)
  
  test_rolling_bootstrap <- pmap(
    crossing(stock_returns, roll),
    get_portfolio_metrics, 
    cov_est_method = cov_est_method,
    portfolio_optimization = "tangent",
    short = TRUE, 
    factor_returns = factors
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
  
  all_avg_sd_bootstrap <- lapply(1:n_bootstraps, function(x) 
    results_by_bootstrap[[x]] %>% 
      map_depth(1,2) %>% 
      reduce(append) %>% 
      na.omit %>% 
      mean) %>% 
    reduce(append)
  
  df <- data.frame(method = cov_est_method, 
                   returns=all_avg_returns_bootstrap,
                   sd = all_avg_sd_bootstrap)
  return(df)
}
