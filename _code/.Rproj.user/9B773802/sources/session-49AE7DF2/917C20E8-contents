#' computes PRIMAL between estimated covariance and sample covariance 
#' 
#' @param estimate_cov matrix, estimated covariance
#' @param sample_cov matrix, sample covariance
#' @param true_cov matrix, true simulated covariance
#' @returns list with portfolio returns, risk, sharp ratio
#' 

get_prial <- function(
    estimate_cov, 
    sample_cov, 
    true_cov
    ) {
  mse_1 <- MSE(true_cov, sample_cov)
  mse_2 <- MSE(true_cov, estimate_cov)
  prial <- (mse_1-mse_2)/mse_1
  return(prial)
}

