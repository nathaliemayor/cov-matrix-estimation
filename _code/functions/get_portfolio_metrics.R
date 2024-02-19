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
    multiplicator <- 1
  }else if(frequency == "daily"){
    freq <- 252
    # 21 trading days in one month
    multiplicator <- 21
  }
  training_data <- stock_returns[
    (roll):(training_period+roll-1),
    -1
  ]
  training_date <- stock_returns[
    (roll):(training_period+roll-1),
    1
  ]
  rf <- 0.5
  if(cov_est_method %in% c("factor1", "factor3")){
    training_factor_data <- factor_returns[
      (roll):(training_period+roll-1),
      -1
    ]
  } else {
    training_factor_data <- NULL
  }
  
  testing_data <- stock_returns[
    ((training_period+roll)):
      (((training_period+roll+rolling_period))-1),
  ]
  date_test <- testing_data[,1]
  
  rf_sr <- TNX$TNX.Adjusted %>%
    # get average monthly rate, percentage to decimal
    mean(na.rm = T)/freq
  # covariance estimation
  if (!cov_est_method %in% c("equal_weights", "SP500")) {
    sigma_hat = get_covariance_estimate(
      method = cov_est_method,
      data = training_data,
      factor_data = training_factor_data
    )
  } 
  # compute optimal portfolio weights
  if (cov_est_method == "equal_weights") {
    optimal_weights <- equal_weights(training_data)
  } else if(cov_est_method == "SP500"){
    sp <- GSPC$GSPC.Adjusted %>% fortify.zoo() %>% 
      filter(Index >= first(date_test) &
               Index <= last(date_test)) %>% 
      mutate(returns = (diff(GSPC.Adjusted)/lag(GSPC.Adjusted))*100)  %>% 
      suppressWarnings()
  }else {
    if(cov_est_method == "huge_glasse"){
      inverse_sigma_hat = sigma_hat
    }else{
      inverse_sigma_hat = solve(sigma_hat)  
    }
    if (portfolio_optimization == "tangent") {
      # tangent portfolio from Markowitz formula
      excess_er_hat <- colMeans(training_data - rf) 
      
      optimal_weights <- (inverse_sigma_hat %*% excess_er_hat)/
        sum(inverse_sigma_hat %*% excess_er_hat)
      if (short == FALSE) {
        n_assets <- length(excess_er_hat)
        # Set up the optimization problem
        Dmat <- 2 * sigma_hat
        dvec <- -excess_er_hat
        Amat <- cbind(rep(1, n_assets), diag(n_assets))
        bvec <- c(1, rep(0, n_assets))
        # Additional constraint: no short selling
        meq <- 1  # Constraint: weights sum to 1
        # Solve the quadratic programming problem
        optimal_weights <- solve.QP(Dmat, dvec, Amat, bvec, meq = meq)$
          solution %>% as.matrix
        rownames(optimal_weights) <- names(excess_er_hat)
      }
    } else if (portfolio_optimization == "minvar") {
      # minvar portfolio from Markowitz formula
      v_ones <- rep(1, dim(inverse_sigma_hat)[1])
      
      optimal_weights <- as.numeric(inverse_sigma_hat %*% v_ones)/
        as.numeric(v_ones %*% inverse_sigma_hat %*% v_ones)
    }
  }
  if(cov_est_method == "SP500"){
    period_returns <- sp$returns
    ptf_sd <- NULL
    SR <- NULL
    optimal_weights <- NULL
  }else{
    period_returns <- rowSums(testing_data[,-1]*optimal_weights) %>% 
      tibble(date=date_test, returns = .) %>% 
      mutate(returns = returns)
    ptf_sd <- sd(period_returns$returns)
    SR <- ((mean(period_returns$returns)-rf_sr)/ptf_sd)
  }
  results = list(period_returns, ptf_sd, SR, optimal_weights)
}
