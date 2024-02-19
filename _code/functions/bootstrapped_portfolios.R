#' resample portfolios/ asset data based on historical data 
#' 
#' @param data asset returns that we want to bootstrap
#' @param n_bootstrap number of bootstraps needed
#' @param block_size block size 
#' @returns NxK re-sampled data-set
#' 
bootstrapped_portfolios <- function(
    data, 
    n_bootstraps, 
    block_size=126, 
    TNX = TNX
) {
  set.seed(7895)
  
  n_rows <- nrow(data)
  data_without_date <- data[, -1]
  
  bootstrap_returns <- replicate(n_bootstraps, {
    # Initialize an empty list to store blocks for this iteration
    blocks <- list()
    
    # Determine the number of blocks needed based on the block size
    num_blocks <- ceiling(n_rows / block_size)
    
    for (i in 1:num_blocks) {
      # Random starting point for each block
      start_index <- sample(1:(n_rows - block_size + 1), 1)
      end_index <- start_index + block_size - 1
      
      # Extract the block and add it to the list
      blocks[[i]] <- data_without_date[start_index:end_index, ]
    }
    
    # Combine the blocks into a single bootstrap sample
    do.call(rbind, blocks)
  }, simplify = FALSE)
  
  bs_list <- lapply(bootstrap_returns, function(bootstrap_data) {
    bootstrap_data_with_date <- cbind(data$Date[1:nrow(bootstrap_data)], bootstrap_data)
    colnames(bootstrap_data_with_date)[1] <- "Date"
    return(as.data.frame(bootstrap_data_with_date))
  })
  
  return(bs_list)
}
