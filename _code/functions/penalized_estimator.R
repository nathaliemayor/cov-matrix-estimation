#' get penalized L1 and L2 norm estimators for the covariance matrix
#' 
#' @param data portfolio returns 
#' @param penalization_type L1 or L2
#' @returns covariance matrix estimate
#' 

penalized_estimator <- function(
    data,
    penalization_type
){
}
library(glmnet)                                    
library(tidyverse)                                
library(lubridate)                                  
library(ggrepel)                                    
library(scales)                                     
library(tictoc)                                     
library(ggcorrplot)                                 

returns <- ff100_data$monthly
sep_date <- as.Date("2000-01-01")        
# Out-of-sample dates (i.e., testing set)
t_oos <- ff100_data$monthly$Date[ff100_data$monthly$Date>sep_date] %>%  
  unique() %>%                            
  as.Date()          
returns <- ff100_data$monthly %>% select(-Date)
# Below, we initialize the variables used in the back testing loop
portf_weights <- matrix(0, nrow = length(t_oos), ncol = (length(returns)-1))
portf_returns <- vector(mode = "numeric", length = length(t_oos))                                                

weights_lasso <- function(returns, alpha, lambda){  # The parameters are defined here
  w <- 0                                          # Initiate weights
  for(i in 1:ncol(returns)){                      # Loop on the assets
    y <- returns[,i]                            # Dependent variable
    x <- returns[,-i]                           # Independent variable
    fit <- glmnet(x,y, family = "gaussian", alpha = alpha, lambda = lambda)
    err <- y-predict(fit, x)                    # Prediction errors
    w[i] <- (1-sum(fit$beta))/var(err)          # Output: weight of asset i
  }
  return(w / sum(w))                              # Normalization of weights
}

for(t in 1:length(t_oos)){
  temp_data <- returns %>% dplyr::filter(Date < t_oos[t]) %>%    # Extracting past data: expand. window 
    dplyr::select(-"Date") %>%                                   # Take out the date
    as.matrix()                                         # Into matrix: glmnet requires matrices
  portf_weights[t,] <- weights_lasso(temp_data, 0.01, 0.1)# Hard-coded parameters! User specified!
  realised_returns <- returns %>%                         # Realised returns:
    dplyr::filter(Date == t_oos[t]) %>%                        # Filtered by Date
    dplyr::select(-"Date")                                       # With Date removed
  portf_returns[t] <- sum(portf_weights[t,] * realised_returns) # Portfolio returns
}


perf_met <- function(portf_returns, weights, asset_returns){
  avg_ret <- mean(portf_returns, na.rm = T)                     # Arithmetic mean 
  vol <- sd(portf_returns, na.rm = T)                           # Volatility
  Sharpe_ratio <- avg_ret / vol                                 # Sharpe ratio
  VaR_5 <- quantile(portf_returns, 0.05)                        # Value-at-risk
  turn <- 0                                                     # Initialisation of turnover
  for(t in 2:dim(weights)[1]){
    realised_returns <- asset_returns %>% dplyr::filter(Date == t_oos[t]) %>% dplyr::select(-"Date")
    prior_weights <- weights[t-1,] * (1 + realised_returns)
    turn <- turn + apply(abs(weights[t,] - prior_weights/sum(prior_weights)),1,sum)
  }
  turn <- turn/(length(t_oos)-1)                                # Average over time
  met <- data.frame(avg_ret, vol, Sharpe_ratio, VaR_5, turn)    # Aggregation of all of this
  rownames(met) <- "metrics"
  return(met)
}

asset_returns <- dplyr::filter(returns, Date>sep_date)                   # Keep out-of-sample returns
perf_met(portf_returns, portf_weights, asset_returns)    

portf_weights <- bind_cols(t_oos, as_tibble(portf_weights))
colnames(portf_weights) <- c("Date",returns[,-1] %>% colnames())

portf_weights %>%
  pivot_longer(-Date, names_to = "stock", values_to = "weight") %>%
  ggplot(aes(x = Date, y = weight, color = stock)) + 
  geom_line() + theme_light() + 
  geom_text_repel(aes(label = stock), hjust = -0.2,
                  data = portf_weights %>% 
                    pivot_longer(-Date, names_to = "stock", values_to = "weight") %>%
                    dplyr::filter(Date == max(Date))) +
  annotate("text", x = as.Date("1998-02-01"), y = 0.14, label = "Ticker") +
  theme(legend.position = "none")

weights_multi <- function(returns, j, alpha, lambda){
  N <- ncol(returns)
  if(j == 1){                     # j = 1 => EW
    return(rep(1/N,N))
  }
  if(j == 2){                     # j = 2 => Minimum Variance
    sigma <- cov(returns)
    w <- solve(sigma) %*% rep(1,N)
    return(w / sum(w))
  }
  if(j == 3){                     # j = 3 => Maximum Sharpe Ratio
    m <- apply(returns, 2, mean)
    sigma <- cov(returns)
    w <- solve(sigma) %*% m
    return(w / sum(w))
  }
  if(j == 4){                     # j = 4 => Penalised / elasticnet
    w <- weights_lasso(returns, alpha, lambda)
  }
}

Tt <- length(t_oos)                                             # Nb of Dates, avoid T = TRUE
nb_port <- 4                                                    # Nb of portfolios
portf_weights <- array(0, dim = c(Tt, nb_port, length(returns[,-1] %>% colnames())))   # Store weights
portf_returns <- matrix(0, nrow = Tt, ncol = nb_port)           # Store returns

for(t in 1:length(t_oos)){
  temp_data <- returns %>% 
    dplyr::filter(Date < t_oos[t]) %>% 
    dplyr::select(-"Date") %>%
    as.matrix()
  realised_returns <- returns %>% 
    dplyr::filter(Date ==  t_oos[t]) %>% 
    dplyr::select(-"Date")
  for(j in 1:nb_port){                                     
    portf_weights[t,j,] <- weights_multi(temp_data, j, 0.1, 0.01)  # Hard-coded parameters!
    portf_returns[t,j] <- sum(portf_weights[t,j,] * realised_returns)
  }
}

port_names <- c("EW", "MV", "MSR", "LASSO") # Portfolio names
met <- c()                                  # Initialise metrics
for(i in 1:nb_port){
  met <- rbind(met, perf_met(portf_returns[,i], portf_weights[,i,], asset_returns)) # Ugly!
}
met %>% data.frame(row.names = port_names)  

# SENSITIVITY ANALYSIS

lasso_sensi <- function(alpha, lambda, t_oos, tick, returns){
  portf_weights <- matrix(0, nrow = length(t_oos), ncol = length(returns[,-1] %>% colnames())) 
  portf_returns <- c()       
  for(t in 1:length(t_oos)){
    temp_data <- returns %>% dplyr::filter(Date < t_oos[t]) %>%    # Extracting past data: expnd. wndw 
      dplyr::select(-"Date") %>%                                   # Take out the Date
      as.matrix()                                         # Into matrix
    portf_weights[t,] <- weights_lasso(temp_data, alpha, lambda)
    realised_returns <- returns %>%                         # Realised returns:
      dplyr::filter(Date ==  t_oos[t]) %>%                       # Filtered by Date
      dplyr::select(-"Date")                                       # With Date removed
    portf_returns[t] <- sum(portf_weights[t,] * realised_returns) # Portfolio returns
  }
  return(sd(portf_returns))
}

lasso_sensi(
  alpha = 0,
  lambda = 0.05572,
  t_oos = t_oos, 
  tick = returns[,-1] %>% colnames(), 
  returns = returns
  )

exp(252/length(portf_returns)*sum(portf_returns))-1 
sd(portf_returns)*sqrt(12)
0.0648/0.121

alpha <- c(0,1)             # alpha values
lambda <- c(0.075,0.1,0.2)         # lambda values
pars <- expand.grid(alpha, lambda)       # Exploring all combinations!
alpha <- pars[,1]
lambda <- pars[,2]
tic()                                    # Launch the clock
vols <- purrr::pmap(list(alpha, lambda),        # Parameters for the grid search
                    lasso_sensi,                # Function on which to apply pmap
                    t_oos = t_oos,              # The other inputs below
                    tick = returns[,-1] %>% colnames(),
                    returns = returns) 
toc()


vols <- vols %>% 
  unlist() %>%
  cbind(as.matrix(pars)) %>%
  data.frame()
colnames(vols) <- c("vol", "alpha", "lambda")
vols %>% ggplot(aes(x = as.factor(alpha), y = vol, fill = as.factor(alpha))) + 
  geom_col() + facet_grid(~lambda) + theme_light() +
  scale_y_continuous(limits = c(0.02, 0.04), oob = rescale_none)    

vols %>% View()

a<-optimise(
  lasso_sensi,
  interval = c(0,1000),
  tol = 0.1,
  alpha=0,
  t_oos=t_oos,
  tick=returns[,-1] %>% colnames(),
  returns = returns,maximum = FALSE
  )

a$objective*sqrt(252)
0.035*sqrt(12)
a


