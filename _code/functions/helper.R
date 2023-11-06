##### IDENTITY MATRIX (Function to create identity matrices)
identity_matrix <- function(dimension)
{ return(diag(dimension))}

##### ONES MATRIX (Function to create matrix of ones)
# n is for the number of rows, ncol is optional
# If only n is specified, then the matrix will be an [n x n]
# Otherwise it will be an [n x ncol]
ones_matrix <- function(n,ncol)
{ 
  if(missing(ncol)) {return( matrix(rep(1,len = n*n), nrow = n) )}
  else{return( matrix(rep(1,len = n*ncol), nrow = n) )}
} 

##### DEMEANING MATRIX  (Function to create the demeaning matrix used at the top of section 2.1 of the paper)
demeaning_matrix <- function(n_observations){
  result <- identity_matrix(n_observations) - 
    ones_matrix(n_observations)/
    n_observations
  return(result)
}


##### CLASSIC COVARIANCE MATRIX
# main_matrix is optional
# If main_matrix is specified, the demeaning is done using the mean of the columns of main_matrix
# Otherwise, the demeaning will be done using the mean of the columns of x

classic_covariance_matrix <- function(x) {
  n = dim(x)[1]
  k = dim(x)[2]
  x_transpose = t(x) # Create the transpose. [k x n]
  m = demeaning_matrix(n) # Create the demeaning matrix. [n x n]
  covariance_matrix =  (x_transpose %*% m %*% x)/ n # [k x k] 
  return(covariance_matrix)
}
