get_global_turnover <- function(weights, 
                                stock_returns, 
                                frequency_day_month, 
                                method_cov
                                ){
  # global_turnover 
  weights_pft <- weights %>% 
    rename(asset_name = name,
           asset_weight = value) %>% 
    filter(method == method_cov) %>% 
    select(-method) %>% 
    mutate(asset_weight = asset_weight)

  assets_names <- weights %>% filter(method == "factor1")
  weights_pft$asset_name <- assets_names$name
  
  returns <- stock_returns %>% 
    pivot_longer(!Date) %>% 
    rename(asset_name = name,
           date=Date, 
           asset_daily_returns = value) %>% 
    mutate(asset_daily_returns = asset_daily_returns/100)
  # Convert dates to Date class if not already
  returns$date <- as.Date(returns$date)
  weights_pft$date <- as.Date(weights_pft$date)
  
  # Expand weights_pft to daily frequency
  expanded_weights <- weights_pft %>%
    complete(asset_name, date = seq(min(returns$date), max(returns$date), 
                                    by = frequency_day_month)) %>%
    group_by(asset_name) %>%
    fill(asset_weight, .direction = "down") %>%
    ungroup()
  # Merge returns with expanded weights
  adjusted_weights <- merge(returns, expanded_weights, 
                            by = c("asset_name", "date"))
  # Order by asset_name and date
  adjusted_weights <- adjusted_weights[order(adjusted_weights$asset_name, 
                                             adjusted_weights$date), ]
  
  rebalancing_dates <- adjusted_weights %>% 
    mutate(year = year(date),
           month = month(date)) %>% 
    filter(month %in% c(1,7)) %>% 
    group_by(year,month) %>% 
    dplyr::summarise(date = first(date),
                     reb_date = first(date)) %>% 
    ungroup 
  # Adjust the weights based on daily returns
  adjusted_weights <- adjusted_weights %>%
    filter(!is.na(asset_weight)) %>% 
    left_join(rebalancing_dates %>% select(date, reb_date)) %>% 
    fill(reb_date, .direction = "down") %>% 
    group_by(asset_name, reb_date) %>%
    dplyr::summarise(adjusted_weight = last(cumprod(1 + asset_daily_returns) * 
                                              asset_weight),
                     initial_weight = first(asset_weight)) %>%
    ungroup() %>% 
    group_by(asset_name) %>% 
    dplyr::summarise(adjusted_weight = c(0,adjusted_weight[-dim(.)[2]]),
                     reb_date = reb_date,
                     initial=initial_weight) %>% 
    ungroup() %>% 
    mutate(diff = abs(initial - adjusted_weight)) %>% 
    group_by(reb_date) %>% 
    dplyr::summarise(sum_date = sum(diff))
  
  average_turnover <- adjusted_weights$sum_date %>% mean()
    
  average_turnover
}
