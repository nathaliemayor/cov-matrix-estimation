library(quantmod)
library(tseries)
library(stats)

# Assuming 'stock_data' is your dataframe with stock prices
# Rows: Dates, Columns: Stock Symbols
stock_data <- ff100_data$daily[,-1] %>% mutate_all(~ log(1+./100))

# Function to calculate tangency portfolio weights
calculate_tangency_weights <- function(mean_returns, cov_matrix, risk_free_rate = 0) {
  inv_cov_matrix <- solve(cov_matrix)
  excess_returns <- mean_returns - risk_free_rate
  weights <- inv_cov_matrix %*% excess_returns
  weights <- weights / sum(weights)  # Normalize weights to sum up to 1
  return(weights)
}


# Initialize variables
n_days <- nrow(stock_data)
estimation_window <- 504
holding_period <- 126
start_index <- 1
portfolio_returns <- vector()
price_index <- c(1)

# Rolling window loop
while((start_index + estimation_window + holding_period) <= n_days) {
  # Estimation window
  estimation_data <- stock_data[start_index:(start_index + estimation_window - 1),]
  returns <- estimation_data 
  mean_returns <- colMeans(returns, na.rm = TRUE)
  cov_matrix <- cov(returns)
  
  # Calculate tangency portfolio weights
  weights <- calculate_tangency_weights(mean_returns, cov_matrix)
  
  # Holding period
  holding_data <- stock_data[(start_index + estimation_window):(start_index + estimation_window + holding_period - 1), ]
  holding_returns <- holding_data  # Log returns
  
  daily_portfolio_performance <- rowSums(as.matrix(holding_returns) %*% weights)
  
  # Store the daily portfolio performance
  portfolio_returns <- c(portfolio_returns, daily_portfolio_performance)
  
  # Update the price index for each day
  for (daily_return in daily_portfolio_performance) {
    new_value <- tail(price_index, 1) * (1 + daily_return)
    price_index <- c(price_index, new_value)
  }
  
  # Move the window forward
  start_index <- start_index + holding_period
}
