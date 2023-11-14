#' resample portfolios/ asset data based on historical data 
#' 
#' @param data asset returns that we want to bootstrap
#' @param factor_data FF factors, only for factor models
#' @returns NxK re-sampled data-set
#' 

bootstrapped_portfolios <- function(
    data,
    n_bootstraps
    ) {
  set.seed(7895)
  
  n_rows <- nrow(data)
  dates <- data$Date
  data <- data[,-1]
  bootstrap_returns <- replicate(n_bootstraps, {
    sample_rows <- sample(1:n_rows, replace = TRUE)
    bootstrap_data <- data[sample_rows, ]
  })
  
  bs_list <- lapply(1:n_bootstraps, function(x){
    bootstrap_returns[x:(x+91)] %>% 
      reduce(cbind) %>% 
      as_tibble %>% 
      dplyr::mutate(Date=dates) %>% 
      dplyr::select(Date, everything())
  })
  return(bs_list)
}
