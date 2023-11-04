library(tidyverse)
library(MASS)
library(Matrix)
# Set the random seed for reproducibility
set.seed(123)

# Number of stocks in the universe
num_stocks <- 200

rmse <- lapply(c(5, 25, 50, 100, 150, 300, 500, 1000),
       function(num_stocks){
         # Generate a random covariance matrix
         cov_matrix <- matrix(rnorm(num_stocks^2), nrow = num_stocks, ncol = num_stocks)
         cov_matrix <- crossprod(cov_matrix) / num_stocks
         
         # Generate artificial stock prices
         num_days <- 100
         
         # Generate initial prices
         initial_prices <- rnorm(num_stocks)*100
         
         # Create an empty matrix to store stock prices
         stock_prices <- matrix(0, nrow = num_days, ncol = num_stocks)
         
         # Assign initial prices to the first row
         stock_prices[1, ] <- initial_prices
         
         # Generate daily returns based on the covariance matrix
         daily_returns <- lapply(2:num_days, function(i) {
           prev_price <- stock_prices[i - 1, ]
           random_returns <- rnorm(num_stocks, mean = 0, sd = sqrt(diag(cov_matrix)))
           random_returns %*% chol(cov_matrix) 
         }) %>% reduce(rbind)
         
         
         tot_error <- (cov_matrix %>% as.vector() - cov(daily_returns) %>% as.vector())^2
         
         sqrt((mean(tot_error)))
})

data.frame(
  rmse=rmse %>% reduce(append),
  num_stocks = c(5, 25, 50, 100, 150, 300, 500, 1000)
  ) %>% ggplot(aes(x=num_stocks, y=rmse)) +
  geom_line()

# Generate a random covariance matrix
cov_matrix <- matrix(rnorm(num_stocks^2), nrow = num_stocks, ncol = num_stocks)
cov_matrix <- crossprod(cov_matrix) / num_stocks

# Generate artificial stock prices
num_days <- 100

# Generate initial prices
initial_prices <- rnorm(num_stocks)*100

# Create an empty matrix to store stock prices
stock_prices <- matrix(0, nrow = num_days, ncol = num_stocks)

# Assign initial prices to the first row
stock_prices[1, ] <- initial_prices

# Generate daily returns based on the covariance matrix
daily_returns <- lapply(2:num_days, function(i) {
  prev_price <- stock_prices[i - 1, ]
  random_returns <- rnorm(num_stocks, mean = 0, sd = sqrt(diag(cov_matrix)))
  random_returns %*% chol(cov_matrix) 
}) %>% reduce(rbind)


tot_error <- (cov_matrix %>% as.vector() - cov(daily_returns) %>% as.vector())^2

sqrt((mean(tot_error)))

# Fill in the remaining stock prices
for (i in 2:num_days) {
  stock_prices[i, ] <- stock_prices[i - 1, ] * exp(daily_returns[[i - 1]])
}


# Plot the stock prices
matplot(
  daily_returns, 
        type = "l", 
        col = 1:num_stocks, 
        xlab = "Day", 
        ylab = "Stock Price", 
        main = "Artificial Stock Prices",
  ylim = c(-10,10)
        )
###############################################################################

# Set the dimensionality of the covariance matrix
num_stocks <- 1000

# Generate a random covariance matrix
cov_matrix <- matrix(rnorm(num_stocks^2), nrow = num_stocks, ncol = num_stocks)
cov_matrix <- crossprod(cov_matrix) / num_stocks

# Generate data from multivariate normal distribution
n <- 89  # Number of samples
data <- mvrnorm(n, rep(0, num_stocks), cov_matrix)

# Print the true covariance matrix
cat("True Covariance Matrix:\n")
print(cov_matrix)

# Print the sample covariance matrix
cat("\nSample Covariance Matrix:\n")
sample_cov <- cov(data)
print(sample_cov)

sqrt(mean((as.vector(cov_matrix)-as.vector(sample_cov))^2))

###############################################################################
# GUILLAUME COQUERET

data <- rio::import("sp500data.csv") %>% 
  pivot_wider(id_cols = "date", names_from = "ticker", values_from = "price") %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

data_returns <- data %>% 
  mutate(across(where(is.numeric),  ~ ((./lag(.,1))-1))) %>% 
  filter(!is.na(`10104`))

library(corpcor)

# Set parameters
n_simulations <- 1000  # Number of Monte Carlo simulations
n_assets <- ncol(data_returns)  # Number of assets

# Define the estimators to be evaluated
estimators <- c("SAM", "CC", "ID", "PCL", "PCP", "PCR", "LWC", "LWF", "LWI")

# Initialize result vectors
results <- matrix(0, nrow = n_simulations, ncol = length(estimators))

# Monte Carlo simulation loop
for (i in 1:n_simulations) {
  # Generate simulated returns based on the true covariance matrix
  simulated_returns <- MASS::mvrnorm(n = nrow(data_returns), mu = rep(0, n_assets), Sigma = true_cov)
  
  # Calculate the sample covariance matrix
  sample_cov <- cov(simulated_returns)
  
  # Estimate the covariance matrix using each estimator
  for (j in 1:length(estimators)) {
    estimator <- estimators[j]
    
    # Estimate the covariance matrix
    if (estimator == "SAM") {
      cov_est <- cov(sample_cov)
    } else if (estimator == "CC") {
      cov_est <- cov.shrink(sample_cov, shrink.method = "cor")
    } else if (estimator == "ID") {
      cov_est <- cov.shrink(sample_cov, shrink.method = "oas")
    } else if (estimator %in% c("PCL", "PCP", "PCR")) {
      # Perform principal component analysis
      pca <- prcomp(simulated_returns)
      
      # Determine the number of components to retain based on the estimator
      if (estimator == "PCL") {
        n_components <- n_assets
      } else if (estimator == "PCP") {
        n_components <- sqrt(n_assets)
      } else if (estimator == "PCR") {
        n_components <- sum(pca$sdev > sqrt(n_assets))
      }
      
      # Reconstruct the covariance matrix using the retained components
      cov_est <- cov(pca$rotation[, 1:n_components] %*% diag(pca$sdev[1:n_components]) %*% t(pca$rotation[, 1:n_components]))
    } else if (estimator %in% c("LWC", "LWF", "LWI")) {
      cov_est <- cov.shrink(sample_cov, shrink.method = "lw")
    }
    
    # Compute the minimum volatility (ex-ante volatility) of the true MV portfolio
    true_min_volatility <- sqrt(min(portfolioRisk(portfolioWeights("mv", mu = true_expected_returns, cov = true_cov))))
    
    # Compute the increase between the true minimal volatility and estimated MV portfolio volatility
    est_min_volatility <- sqrt(min(portfolioRisk(portfolioWeights("mv", mu = true_expected_returns, cov = cov_est))))
    increase <- est_min_volatility - true_min_volatility
    
    # Store the increase for the current estimator and simulation
    results[i, j] <- increase
  }
}

# Compute the mean and standard deviation of the increases for each estimator
mean_increases <- apply(results, 2, mean)
sd_increases <- apply(results, 2, sd)

# Print the mean and standard deviation of increases for each estimator
for (j in 1:length(estimators)) {
  estimator <- estimators[j]
  mean_increase <- mean_increases[j]
  sd_increase <- sd_increases[j]
  
  cat("Estimator:", estimator, "\n")
  cat("Mean Increase:", mean_increase, "\n")
  cat("Standard Deviation of Increases:", sd_increase, "\n\n")
}

