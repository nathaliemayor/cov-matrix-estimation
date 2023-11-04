#' Simulates random variables coming from covariance matrix 
#' 
#' @param n_obs number of observations (months)
#' @param p_variables number of variables (stocks)
#' @param method estimation method for the covariance matrix
#' @param criterion error measure, l2 norm or PRIAL
#' @returns PRIAL ratio
#' 

mc_simulation_cov <- function (
    n_obs,
    p_variables, 
    method,
    criterion,
    simulation
) {
  # Simulate data
  if(method %in% c("factor1", "factor3")) {
    # simulate factor data for and 1 and 3 factors models
    if(method == "factor1"){n_factors <- 1} else {n_factors <- 3}
    # Simulate a random true covariance matrix
    true_covariance <- matrix(runif(p_variables^2), nrow = p_variables)
    # to ensure it's positive semi-definite
    true_covariance <- true_covariance %*% t(true_covariance)  
    # Generate factor returns
    factor_returns <- matrix(rnorm(n_factors * n_obs), nrow = n_obs)
    # Generate asset-specific returns based on the factor model
    asset_data <- t(sapply(1:p_variables, function(i) {
      alpha_i <- rnorm(1)  # Generate alpha for each asset
      beta_i <- rnorm(n_factors)  # Generate beta for each asset
      
      asset_returns_i <- alpha_i + 
        factor_returns %*% 
        beta_i + 
        sqrt(true_covariance[i, i]) * 
        rnorm(n_obs)
      return(asset_returns_i)
    }))
    
    # Create a data frame with factor returns
    simulated_factors <- data.frame(factor_returns)
    
    # Simulate returns
    simulated_data <- data.frame(t(asset_data))
    
  } else {
    # True mean vector
    true_mean <- rep(0, p_variables)
    
    # Generate random eigenvalues
    random_eigenvalues <- runif(p_variables, 0.1, 1.5)
    
    # Generate random eigenvectors
    random_eigenvectors <- matrix(rnorm(p_variables^2), nrow = p_variables)
    
    # Create the positive definite covariance matrix
    true_covariance <- random_eigenvectors %*% 
      diag(random_eigenvalues) %*% 
      t(random_eigenvectors)
    
    simulated_data <- mvrnorm(n_obs, mu = true_mean, Sigma = true_covariance)
  }
  
  # Calculate the sample covariance matrix (Empirical estimator)
  sample_covariance <- cov(simulated_data)
  
  # Calculate the covariance estimate with chosen method
  estimate_covariance <- get_covariance_estimate(
    data = simulated_data, 
    method = method,
    factor_data = simulated_factors
  )
  
  if(criterion == "prial") {
    # Calculate PRIAL and relative efficiency for Ledoit-Wolf estimator
    error_measure <- get_prial(
      estimate_covariance, 
      sample_covariance, 
      true_covariance
    )
  } else if (criterion == "l2_norm") {
    error_measure <- get_l2_norm(
      estimate_covariance, 
      true_covariance
    )
  }
  return(error_measure)
}