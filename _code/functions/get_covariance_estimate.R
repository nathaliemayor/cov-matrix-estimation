#' computes estimates of the covariance matrix given the name of the method
#' and data
#' 
#' @param data asset returns or simulated data for which we need cov estimate
#              NxK dimension
#' @param method estimation method name
#' @param factor_data FF factors, only for factor models
#' @returns NxN covariance matrix estimate, matrix form
#' 

get_covariance_estimate <- function(
    data, 
    method,
    factor_data = NULL
) {
  if (method == "cov1Para") {
    sigma_hat = cov1Para(Y = data)
  } else if (method == "cov2Para") {
    sigma_hat = cov2Para(Y = data)
  } else if (method == "covCor") {
    sigma_hat = covCor(Y = data)
  } else if (method == "CCM") {
    sigma_hat = CCM(returns = data)
  } else if (method == "covDiag") {
    sigma_hat = covDiag(Y = data)
  } else if (method == "covMarket") {
    sigma_hat = covMarket(Y = data)
    ## nonlinear shrinkage
  } else if (method == "gis") {
    sigma_hat = gis(Y = data)
  } else if (method == "lis") {
    sigma_hat = lis(Y = data)
  } else if (method == "qis") {
    sigma_hat = qis(Y = data)
  }  else if (method == "CovMcd") {
    sigma_hat = rrcov::CovMcd(data)$cov
  } else if (method == "CovMve") {
    sigma_hat = rrcov::CovMve(data)$cov
  } else if (method == "huge_glasso") {
    #model selection using ric
    data_npn <- huge.npn(data, npn.func = "truncation")
    out_npn <- huge(
      data_npn, 
      method = "glasso", 
      nlambda = 40, 
      lambda.min.ratio = 0.4, 
      cov.output = TRUE
    )
    select_out <- huge.select(
      out_npn, 
      criterion = "ric"
    )
    sigma_hat <- select_out$opt.icov
  }  else if (method %in% c("factor1", "factor3")) {
    factor_data <- factor_data 
    if(method == "factor1"){
      # 1-factor model (market)
      F_ <- cbind(ones = 1, factor_data[,1]) %>%  as.matrix
    } else if(method == "factor3"){
      # 3-factors model (market, HML, SMB)
      F_ <- cbind(ones = 1, factor_data) %>% as.matrix
    }
    Gamma <- t(solve(t(F_) %*% F_, t(F_) %*% as.matrix(data)))
    alpha <- Gamma[,1]
    beta <- Gamma[,-1]
    E <- t(t(data) - 
             Gamma %*% 
             t(F_))
    Psi <- (1/(nrow(data)-2)) * t(E) %*% E
    if(method == "factor1"){
      # 1-factor model (market)
      sigma_hat <- as.numeric(var(F_[,-1])) * 
        beta %o% 
        beta +
        diag(diag(Psi))
    } else if(method == "factor3"){
      # 3-factors model (market, HML, SMB)
      sigma_hat <- beta %*% 
        cov(F_[,-1]) %*% 
        t(beta) +
        diag(diag(Psi))
    }
  } else if (method == "RMT") {
    sigma_hat <- covmat::estRMT(data)$cov
  } else if (method == "ewma") {
    sigma_hat = covEstimation(as.matrix(data), control = list(type="ewma", lambda=0.99734))
  }else if (method == "sample") {
    sigma_hat <- (t(as.matrix(data)) %*% as.matrix(data)) / (dim(data)[1]-1)
    # sigma_hat <- cov(data, method = "kendall")
  } 
  return(sigma_hat) 
}
