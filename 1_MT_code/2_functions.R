# FILE 2 - SHRINKAGE ESTIMATION FOR COVARIANCE MATRICES - NATHALIE MAYOR
# Functions

# 1. COMPUTATION OF STOCK LOG RETURNS
# K the number of stocks, days the number of days and prices the data frame of prices
get_log_returns <- function(k,days,prices)
{
  last_day = length(prices[[1]])
  raw_output = sapply(1:k, function(i) prices[[i]][(last_day-days+1):last_day])
  data_df = data.frame(raw_output)
  data_matrix = as.matrix(as.data.frame(lapply(data_df, as.numeric)))
  log_returns = log(data_matrix[2:dim(data_matrix)[1],] / data_matrix[1:(dim(data_matrix)[1]-1),]) 
  return(as.data.frame(log_returns))
}



##### COVARIANCE
covariance <- function(x,y) {  return( mean( (x-mean(x)) * (y-mean(y)) ) ) }

##### 2. CLASSIC COVARIANCE MATRIX
# main_matrix is optional
# If main_matrix is specified, the demeaning is done using the mean of the columns of main_matrix
# Otherwise, the demeaning will be done using the mean of the columns of x
classic_covariance_matrix <- function(x,main_matrix)
{
  n = dim(x)[1]
  k = dim(x)[2]
  if (missing(main_matrix))
  {
    x_transpose = t(x) # Create the transpose. [k x n]
    m = demeaning_matrix(n) # Create the demeaning matrix. [n x n]
    covariance_matrix =  (x_transpose %*% m %*% x)/ n # [k x k] 
    
    return( covariance_matrix )
  }
  else
  {
    x = t(t(x) - colMeans(main_matrix) ) # Demeans using the main matrix mean
    x_transpose = t(x) # Create the transpose. [k x n]
    covariance_matrix =  (x_transpose %*%  x)/ n # [k x k] 
    
    return( covariance_matrix )
  }
}

### 3. BUILDING EQUALLY WEIGTED PORTFOLIOS
equally_weighted_portfolio <- function(ret,period,from,to){
  # ret: stock returns (rows=observations and cols=assets)
  # period: number of trading period in a year (12 if monthly data, 252 if daily)
  # from, to: index of data to be used
  eq_w <- rep(1/dim(ret)[2],dim(ret)[2])
  eq_dsr <- rowSums(eq_w * (exp(ret[from:to,]) - 1)) # return per year
  eq_dlr <- log(eq_dsr+1)
  eq_cumsum <- cumsum(eq_dlr)
  eq_normalized_price <- exp(eq_cumsum)
  initial_price <- 100 
  eq_100_price <- initial_price * eq_normalized_price
  an_r <- exp(period/length(eq_dlr)*sum(eq_dlr))-1 
  an_sd_r <- sd(eq_dlr)*sqrt(period)
  results <- list("norm_price"=eq_100_price, "annual_return"=an_r,"annual_sd"=an_sd_r)
  return(results)
}



























