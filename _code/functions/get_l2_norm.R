#' computes the matrix L2 norm between the true and estimated covariance matrix
#' 
#' @param estimate_cov
#' @param true_cov
#' @returns list with portfolio returns, risk, sharp ratio
#' 

get_l2_norm <- function(
  estimate_cov,
  true_cov
) {
  squared_diff <- (true_cov - estimate_cov)^2
  l2_norm <- sqrt(sum(squared_diff))
  return(l2_norm)
}

