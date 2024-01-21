library(tidyquant)
library(dplyr)
library(lubridate)

# importing SP500 data stocks
sp_info <- rio::import(file.choose())$Symbol


stock_data <- tidyquant::tq_get(sp_info,
                     from = "1980-01-01",
                     get = "stock.prices")

clean_stocks <- stock_data %>% select(date, symbol, adjusted) %>% 
  pivot_wider(names_from = symbol, values_from = adjusted) %>% 
  filter(date > "1990-01-01") 

na_col <- colnames(clean_stocks)[colSums(is.na(clean_stocks)) > 0]

no_na_daily <- clean_stocks %>% select(-na_col)

monthly_adj <- stock_data %>%
  group_by(month = floor_date(date, "month"), symbol) %>%
  dplyr::summarize(adj_close = last(adjusted)) %>% 
  pivot_wider(names_from = symbol, values_from = adj_close) %>% 
  filter(month > "1990-01-01") %>% 
  select(-na_col)

monthly_sp500_prices <- monthly_adj

daily_sp500_prices <- no_na_daily

sp500_stock_data <- list(monthly = monthly_sp500_prices, 
                         daily = daily_sp500_prices)

save(
  sp500_stock_data, 
     file="/Users/pro/Library/Mobile Documents/com~apple~CloudDocs/masters_thesis/data/sp500_stock_data.RData"
  )
