CCM <- function(returns) {
  # Calculate the sample correlation matrix
  cor_matrix <-  (t(as.matrix(returns)) %*% 
                    as.matrix(returns)) / 
    (dim(returns)[1]-1)   
  
  # Extract the average correlation
  avg_correlation <- mean(
    cor_matrix[
      lower.tri(
        cor_matrix
      )
    ]
  )
  
  # Create a correlation matrix with constant correlation
  constant_cor_matrix <- matrix(
    avg_correlation, 
    ncol = ncol(returns), 
    nrow = ncol(returns)
  )
  
  # Estimate covariance matrix using constant correlation
  ccm_cov_matrix <- constant_cor_matrix * 
    outer(
      apply(
        returns, 
        2, 
        sd
      ), 
      apply(
        returns, 
        2, 
        sd
      )
    ) + diag(1e-6, ncol = ncol(returns), nrow = ncol(returns)) 
  
  return(ccm_cov_matrix)
}
