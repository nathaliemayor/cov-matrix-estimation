
# factor test

library(quantmod)
library(covFactorModel)
# devtools::install_github("dppalomar/covFactorModel")

# Load historical factor data (market, SMB, HML)
factors <- rio::import(
  file.path(data_path, "F-F_Research_Data_Factors.CSV")
) %>% 
  mutate(V1 = as.Date(
    V1 %>% paste0("01"), 
    format = "%Y%m%d"
  )
  ) %>% 
  dplyr::filter(V1 >= "1982-02-01" & V1 < "2022-02-01") %>% 
  dplyr::select(-RF)

# Combine stock returns and factors
data <- cbind(
  log_returns %>% 
    dplyr::select(-market), 
  factors %>% 
    dplyr::select(-V1)
)

log_returns_xts <- xts(
  log_returns %>% 
    dplyr::select(-date), 
  order.by = log_returns$date
)
factors_xts <- xts(factors[,-1], order.by = factors$V1)

# 1-factor model
F_ <- cbind(ones = 1, factors$`Mkt-RF`)
Gamma <- t(solve(t(F_) %*% F_, t(F_) %*% as.matrix(log_returns[,-1])))
colnames(Gamma) <- c("alpha", "beta")
alpha <- Gamma[, 1]
beta <- Gamma[, 2]
E <- t(t(log_returns[,-1]) - 
         Gamma %*% 
         t(F_))
Psi <- (1/(nrow(log_returns)-2)) * t(E) %*% E
Sigma_SP500 <- as.numeric(var(factors$`Mkt-RF`)) * 
  beta %o% 
  beta +
  diag(diag(Psi))

# Fama-French 3-factors model
F_ <- cbind(ones = 1, factors_xts)
Gamma <- t(solve(t(F_) %*% F_, t(F_) %*% log_returns_xts))
colnames(Gamma) <- c("alpha", "beta1", "beta2", "beta3")
alpha <- Gamma[, 1]
B <- Gamma[, 2:4]
E <- xts(t(t(log_returns_xts) - Gamma %*% t(F_)), index(log_returns_xts))
Psi <- (1/(nrow(log_returns_xts)-2)) * t(E) %*% E
Sigma_FamaFrench <- B %*% 
  cov(factors_xts) %*% 
  t(B) +
  diag(diag(Psi))
# ------------------------------------------------------------------------------
# SIMULATIONS
# ------------------------------------------------------------------------------
# Load necessary libraries
library(xts)
library(MASS)  # For generating multivariate normal data

# Set random seed for reproducibility
set.seed(123)

# Define the number of assets (N) and factors (K)
N <- 10  # Number of assets
K <- 100  # Number of observations

# Simulate factor data (3 factors)
n_factors <- 3
factors_xts <- xts(
  matrix(
    rnorm(
      n_factors * K
    ), 
    ncol = n_factors
  ), 
  order.by = seq.Date(
    from = as.Date("2020-01-01"), 
    length.out = K, 
    by = "days"
  )
)

# Simulate asset returns (log_returns_xts)
true_alpha <- rnorm(N)  # True alpha values for each asset
true_beta <- rnorm(N)   # True beta values for each asset

# Generate factor returns based on the provided factors
factor_returns <- as.matrix(factors_xts)

# Simulate asset returns using the 1-factor model
simulate_asset_returns <- function(i) {
  asset_returns <- true_alpha[i] + true_beta[i] * factor_returns[, 2] + rnorm(K)
  return(asset_returns)
}

# Use lapply to simulate returns for all assets
asset_returns_list <- lapply(1:N, simulate_asset_returns)
log_returns_xts <- do.call(cbind, asset_returns_list)

# Estimate Sigma_SP500 based on the simulated data
F_ <- cbind(ones = 1, factors_xts[,-1])
Gamma <- t(solve(t(F_) %*% F_, t(F_) %*% log_returns_xts))
alpha <- Gamma[, 1]
B <- Gamma[, 2:3]
E <- xts(t(t(log_returns_xts) - 
             Gamma %*% 
             t(F_)
), 
rownames(log_returns_xts) %>% as.Date
)
Psi <- (1/(nrow(log_returns_xts)-2)) * t(E) %*% E
Sigma_SP500 <- B %*% 
  cov(factors_xts) %*% 
  t(B) +
  diag(diag(Psi))

# Print Sigma_SP500
print("Estimated Sigma_SP500:")
print(Sigma_SP500)