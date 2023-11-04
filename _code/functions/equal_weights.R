#' computes weights for equally weighted portfolios
#' 
#' @param stock_returns data frame, stocks returns.
#' @returns data frame with weights and corresponding name  

equal_weights <- function (stock_returns) {
  
  if(is.Date(stock_returns[,1])){
    stock_returns <- stock_returns[,-1]
  }
  
  n <- dim(stock_returns)[2]
  k <- dim(stock_returns)[1]
  
  weight <- 1/n
  weights <- rep(weight, n)
  
  return(weights)
}
