#' computes metrics for portfolio computed based on various covariance matrix
#' methods
#' 
#' @param stock_returns data frame, stocks returns.
#' @param roll integer, used for the rolling window back-test
#' @param cov_est_method string, defines method to use to estimate cov matrix 
#' @param portfolio_optimization portfolio optimization method, tangent/ minvar
#' @param short TRUE short position allowed, FALSE not allowed
#' @param factor_returns DF with 3 Fama-French factor returns for factor models
#' @returns list with portfolio returns, risk, sharp ratio
#' 

get_portfolio_metrics <- function (
    stock_returns,  
    cov_est_method,
    roll,
    portfolio_optimization,
    short = TRUE, 
    frequency = "monthly", 
    factor_returns = NULL
) {
  if(frequency == "monthly"){
    freq <- 12
  }else if(frequency == "daily"){
    freq <- 252
  }
  training_data <- stock_returns[
    roll:(training_period+roll-1),
    -1
  ]
  training_date <- stock_returns[
    roll:(training_period+roll-1),
    1
  ]
  if(cov_est_method %in% c("factor1", "factor3")){
    training_factor_data <- factor_returns[
      roll:(training_period+roll-1),
      -1
    ]
  } else {
    training_factor_data <- NULL
  }
  
  testing_data <- stock_returns[
    (training_period+roll):(training_period+roll+rolling_period-1),
  ]
  date_test <- testing_data[,1]
  # covariance estimation
  ## linear shrinkage 
  if (!cov_est_method == "equal_weights") {
    sigma_hat = get_covariance_estimate(
      method = cov_est_method,
      data = training_data,
      factor_data = training_factor_data
    )
  } 
  # compute optimal portfolio weights
  if (cov_est_method == "equal_weights") {
    optimal_weights <- equal_weights(training_data)
  } else {
    inverse_sigma_hat = solve(sigma_hat)
    if (portfolio_optimization == "tangent") {
      # tangent portfolio from Markowitz formula
      rf <- window(TNX, 
                   start = first(training_date), 
                   end = last(training_date))$TNX.Adjusted %>% 
        # get average monthly rate, percentage to decimal
        mean(na.rm = T)/freq/100
      
      excess_er_hat <- colMeans(training_data - rf) 
      optimal_weights <- (inverse_sigma_hat %*% excess_er_hat)/ 
        sum(inverse_sigma_hat %*% excess_er_hat)
      
      if (short == FALSE) {
        optimal_weights <- quadprog::solve.QP(
          inverse_sigma_hat,
          excess_er_hat,
          cbind(rep(1, length(excess_er_hat)), 
                diag(1, nrow = length(excess_er_hat))),
          c(1, rep(0, length(excess_er_hat))),
        )$solution
      }
    } else if (portfolio_optimization == "minvar") {
      # minvar portfolio from Markowitz formula
      v_ones <- rep(1, dim(inverse_sigma_hat)[1])
      
      optimal_weights <- as.numeric(inverse_sigma_hat %*% v_ones)/
        as.numeric(v_ones %*% inverse_sigma_hat %*% v_ones)
    }
  }
  period_returns <- rowSums(testing_data[,-1]*optimal_weights) %>% 
    data_frame(date=date_test, returns = .)
  ptf_variance <- t(as.matrix(optimal_weights)) %*% 
    as.matrix(cov(testing_data[,-1])) %*% 
    as.matrix(optimal_weights)
  ptf_sd <- sqrt(ptf_variance)
  ptf_sd_simple <- sd(period_returns$returns)
  SR <- (mean(period_returns$returns)/ptf_sd)*sqrt(freq)
  results = list(period_returns, ptf_sd, SR)
}
