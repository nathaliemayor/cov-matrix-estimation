current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
path = getwd()

# Source header and functions files 
source("0_header.R")
source("2_functions.R")

##################################################
#################### COQUERET ####################
##################################################

stock_log_returns_monthly_50$date <- stock_log_returns_monthly$date
stock_log_returns_daily_50$date <- stock_log_returns_daily$date

stock_log_returns_daily_50$date[1:274]
stock_log_returns_daily_test <- stock_log_returns_daily[1:274,]

sep_date <- as.Date("1997-07-03")         # This date separates in-sample vs out-of-sample
t_oos <- stock_log_returns_daily_test$date[stock_log_returns_daily_50$date[1:274]>sep_date] %>%  # Out-of-sample dates (i.e., testing set)
  unique() %>%                            # Remove duplicates
  as.Date(origin = "1997-01-03")          # Transform in date format
returns <- stock_log_returns_daily_50[1:274,] # Computing returns, in matrix format, in 2 steps:
# Below, we initialise the variables used in the back testing loop
portf_weights <- matrix(0, nrow = length(t_oos), ncol = (length(returns)-1))
portf_returns <- vector(mode = "numeric", length = length(t_oos))                                                
                                 # A look at the returns

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
  temp_data <- returns %>% dplyr::filter(date < t_oos[t]) %>%    # Extracting past data: expand. window 
    dplyr::select(-"date") %>%                                   # Take out the date
    as.matrix()                                         # Into matrix: glmnet requires matrices
  portf_weights[t,] <- weights_lasso(temp_data, 0.01, 0.1)# Hard-coded parameters! User specified!
  realised_returns <- returns %>%                         # Realised returns:
    dplyr::filter(date == t_oos[t]) %>%                        # Filtered by date
    dplyr::select(-"date")                                       # With date removed
  portf_returns[t] <- sum(portf_weights[t,] * realised_returns) # Portfolio returns
}


perf_met <- function(portf_returns, weights, asset_returns){
  avg_ret <- mean(portf_returns, na.rm = T)                     # Arithmetic mean 
  vol <- sd(portf_returns, na.rm = T)                           # Volatility
  Sharpe_ratio <- avg_ret / vol                                 # Sharpe ratio
  VaR_5 <- quantile(portf_returns, 0.05)                        # Value-at-risk
  turn <- 0                                                     # Initialisation of turnover
  for(t in 2:dim(weights)[1]){
    realised_returns <- asset_returns %>% dplyr::filter(date == t_oos[t]) %>% dplyr::select(-"date")
    prior_weights <- weights[t-1,] * (1 + realised_returns)
    turn <- turn + apply(abs(weights[t,] - prior_weights/sum(prior_weights)),1,sum)
  }
  turn <- turn/(length(t_oos)-1)                                # Average over time
  met <- data.frame(avg_ret, vol, Sharpe_ratio, VaR_5, turn)    # Aggregation of all of this
  rownames(met) <- "metrics"
  return(met)
}

asset_returns <- dplyr::filter(returns, date>sep_date)                   # Keep out-of-sample returns
perf_met(portf_returns, portf_weights, asset_returns)    

portf_weights <- bind_cols(t_oos, as_tibble(portf_weights))
colnames(portf_weights) <- c("date",stock_log_returns_monthly_50[,-51] %>% colnames())

portf_weights %>%
  pivot_longer(-date, names_to = "stock", values_to = "weight") %>%
  ggplot(aes(x = date, y = weight, color = stock)) + 
  geom_line() + theme_light() + 
  geom_text_repel(aes(label = stock), hjust = -0.2,
                  data = portf_weights %>% 
                    pivot_longer(-date, names_to = "stock", values_to = "weight") %>%
                    dplyr::filter(date == max(date))) +
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

Tt <- length(t_oos)                                             # Nb of dates, avoid T = TRUE
nb_port <- 4                                                    # Nb of portfolios
portf_weights <- array(0, dim = c(Tt, nb_port, length(stock_log_returns_monthly_50[,-51] %>% colnames())))   # Store weights
portf_returns <- matrix(0, nrow = Tt, ncol = nb_port)           # Store returns

for(t in 1:length(t_oos)){
  temp_data <- returns %>% 
    dplyr::filter(date < t_oos[t]) %>% 
    dplyr::select(-"date") %>%
    as.matrix()
  realised_returns <- returns %>% 
    dplyr::filter(date ==  t_oos[t]) %>% 
    dplyr::select(-"date")
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
  portf_weights <- matrix(0, nrow = length(t_oos), ncol = length(stock_log_returns_monthly_50[,-51] %>% colnames())) 
  portf_returns <- c()       
  for(t in 1:length(t_oos)){
    temp_data <- returns %>% dplyr::filter(date < t_oos[t]) %>%    # Extracting past data: expnd. wndw 
      dplyr::select(-"date") %>%                                   # Take out the date
      as.matrix()                                         # Into matrix
    portf_weights[t,] <- weights_lasso(temp_data, alpha, lambda)
    realised_returns <- returns %>%                         # Realised returns:
      dplyr::filter(date ==  t_oos[t]) %>%                       # Filtered by date
      dplyr::select(-"date")                                       # With date removed
    portf_returns[t] <- sum(portf_weights[t,] * realised_returns) # Portfolio returns
  }
  return(sd(portf_returns))
}

lasso_sensi(alpha = 0,lambda = 0.05572,t_oos = t_oos, tick = stock_log_returns_monthly_50[,-51] %>% colnames(), returns = returns)

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
             tick = stock_log_returns_monthly_50[,-51] %>% colnames(),
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

a<-optimise(lasso_sensi,interval = c(0,1000),tol = 0.1,alpha=0,t_oos=t_oos,tick=stock_log_returns_monthly_50[,-51] %>% colnames(),returns = returns,maximum = FALSE)

a$objective*sqrt(252)
0.035*sqrt(12)
a



##################################################
###################### RAGS2RIDGES ###############
##################################################

SampleCov<- rags2ridges::covML(stock_log_returns_monthly_50[1:12,] %>% as.matrix())
precision_m_test<-rags2ridges::ridgeP(SampleCov,lambda =0.06,type = "Alt",target=diag(1/diag(SampleCov)))
onevec<-rep(1,50)
top <- precision_m_test%*%onevec
bottom <- as.numeric(t(onevec)%*%precision_m_test%*%onevec)
m <- top/bottom

eq_dsr_50 <- rowSums(m * (exp(stock_log_returns_monthly_50) - 1)) # return per year
eq_dlr_50 <- log(eq_dsr_50+1)
eq_cumsum_50 <- cumsum(eq_dlr_50)
eq_normalized_price_50 <- exp(eq_cumsum_50)
initial_price <- 100 
eq_100_price_50 <- initial_price * eq_normalized_price_50
exp(12/length(eq_dlr_50)*sum(eq_dlr_50))-1 
sd(eq_dlr_50)*sqrt(12)
plot(eq_100_price_50,type="l",main="Performance Equally Weighted Porfolio (50)")

library(data.table)

# lambda: shrinkage parameter
portfolio_an_ret <- function(lambda,ret,size_cov){
  SampleCov<- rags2ridges::covML(ret[1:size_cov,] %>% as.matrix()) 
  precision_m_test<-rags2ridges::ridgeP(SampleCov,lambda = lambda,type = "Alt",target=diag(1/diag(SampleCov)))
  onevec<-rep(1,dim(ret)[2]) # size is the number of assets
  top < - precision_m_test%*%onevec
  bottom <- as.numeric(t(onevec)%*%precision_m_test%*%onevec)
  m <- top/bottom
  eq_dsr_50 <- (exp(ret[13:300,])-1) %>% as.matrix()%*%m[,1] # return per year
  eq_dlr_50 <- log(eq_dsr_50+1)
  eq_cumsum_50 <- cumsum(eq_dlr_50)
  eq_normalized_price_50 <- exp(eq_cumsum_50)
  initial_price <- 100 
  eq_100_price_50 <- initial_price * eq_normalized_price_50
  ptf_r <- exp(12/length(eq_dlr_50)*sum(eq_dlr_50))-1 
  sd<-sd(eq_dlr_50)*sqrt(12)
  return(ptf_r)
}

tic()
optimise(portfolio_an_ret,ret=stock_log_returns_monthly_50,maximum = TRUE,interval=c(0.00001,500),tol = 0.01)
toc()

## ret:                 a matrix of asset returns with the assets as columns (number of rolling window x number of assets)
## lamb:                the penalization parameter
## n_rolling_window:    the number of rolling window i.e. 288 if update each month; 48 if update each 6 months
## cov_size:            how many months should be taken into account for the estimation of the cov, i.e. usually 12 or 120
## period:              the length of a period, i.e. 1 if monthly data, 21 if daily data
## window_size:         window size in terms of months i.e. usually 1 or 6 
portfolio_weights_r2r <- function(ret, lamb, n_rolling_window, period, cov_size, window_size){
  w <- matrix(rep(0,dim(ret)[2]*n_rolling_window),nrow = n_rolling_window)
  onev <- rep(1,dim(ret)[2]) # size is the number of assets
  for (i in 0:(n_rolling_window-1)) {
    SampleCov <- rags2ridges::covML(ret[(1+period*window_size*i):(cov_size*period+period*window_size*i),] %>% as.matrix())
    precision <-rags2ridges::ridgeP(SampleCov,lambda = lamb,type = "Alt",target=diag(1/diag(SampleCov)))
    w[i,] <- (precision%*%onev)/(as.numeric(t(onev)%*%precision%*%onev))
  }
  return(w)
}

length(0:288)
resu<-portfolio_weights_r2r(stock_log_returns_daily_50[,-51],0.000001)

portfolio_ret_r2r <- function(ret, ret_day, a,lamb,n_rolling_window, period, cov_size, window_size){
  ptf_r <- 0
  weights <- matrix(rep(0,dim(ret)[2]*n_rolling_window),nrow = n_rolling_window)
  weights <- portfolio_weights_r2r(ret_day,lamb,n_rolling_window, period, cov_size, window_size)[rep(seq_len(n_rolling_window),each=window_size),]
  ptf_r <- sum(weights[a,]*ret[12+a,])
  return(ptf_r)
}

best_lambda <- function(ret, ret_day,n_rolling_window, period, cov_size, window_size){
  optimal_lamb <- matrix(rep(0,2*n_rolling_window),nrow = n_rolling_window)
  for (b in 1:n_rolling_window) {
    optimal_lamb[b,] <- optimise(portfolio_ret_r2r,interval=c(0.00000001,500),tol = 0.01,ret=ret, ret_day=ret_day,a=b,n_rolling_window=n_rolling_window, period=period, cov_size=cov_size, window_size=window_size,maximum = T)%>%unlist()
  }
  return(optimal_lamb)
}

# optimization using rags2ridges, Alt with target as diag(cov), rolling 252 days covariance matrix and 1 months windows (288 windows) 
# results: annual standard deviation = 0.153 and annual returns = 0.0252
# results with T=1/diag(S): standard deviation = 0.13 and annual returns = 0.065
tic()
opt1<- best_lambda(stock_log_returns_monthly_50[,-51], stock_log_returns_daily_50[,-51],288,21,12,1)
toc()

tic()
opt250_1<- best_lambda(stock_log_returns_monthly_250, stock_log_returns_daily_250,288/6,21,12,6)
toc()

opt250_1[,2] %>% mean()*12
opt250_1[,2] %>% sd()*sqrt(12)

opt1[,1] %>% summary()
opt1[,2] %>% sd()*sqrt(12)
opt1[,2] %>% plot(type="l")
opt1[,2] %>% mean()*12

tic()
opt250<- best_lambda(stock_log_returns_monthly_250, stock_log_returns_daily_250)
toc()


xxx<- matrix(1:12,nrow = 3,ncol=4)
xxx[rep(seq_len(nrow(xxx)),each=3),]


data[rep(seq_len(nrow(data)), each = 3), ]
##################################################
###################### GLASSO ####################
##################################################


### GLASSO using GLASSO package
portfolio_an_ret_glasso <- function(lambda,ret){
  precision_m_test<-glasso(var(ret[1:12,]),rho=lambda,thr = 0.01)$wi
  onevec<-rep(1,dim(ret)[2])
  top <- precision_m_test%*%onevec
  bottom <- as.numeric(t(onevec)%*%precision_m_test%*%onevec)
  m <- top/bottom
  eq_dsr_50 <- (exp(ret[13:300,])-1) %>% as.matrix()%*%m[,1] # return per year
  eq_dlr_50 <- log(eq_dsr_50+1)
  eq_cumsum_50 <- cumsum(eq_dlr_50)
  eq_normalized_price_50 <- exp(eq_cumsum_50)
  initial_price <- 100 
  eq_100_price_50 <- initial_price * eq_normalized_price_50
  ptf_r <- exp(12/length(eq_dlr_50)*sum(eq_dlr_50))-1 
  sd<-sd(eq_dlr_50)*sqrt(12)
  return(sd)
}

portfolio_an_ret_glasso(5000,stock_log_returns_monthly_50)

tic()
optimise(portfolio_an_ret_glasso,ret=stock_log_returns_monthly_50,maximum = F,interval=c(0.001,5000),tol = 0.01)
toc()

### Using HUGE package



out.glasso <- huge(as.matrix(stock_log_returns_monthly_1000[1:120,]), method="glasso" , nlambda=30, lambda.min.ratio=0.1)

out.select <- huge.select(out.glasso, criterion="ric", rep.num=20)
out.select$opt.sparsity

portfolio_an_ret_glasso_huge <- function(ret){
  out.glasso <- huge(as.matrix(ret[1:12,]), method="glasso" , nlambda=30, lambda.min.ratio=0.1)
  out.select <- huge.select(out.glasso, criterion="ric", rep.num=20)
  precision_m_test <- out.select$opt.icov
  onevec<-rep(1,dim(ret)[2])
  top <- precision_m_test%*%onevec
  bottom <- as.numeric(t(onevec)%*%precision_m_test%*%onevec)
  m <- top/bottom
  eq_dsr_50 <- (exp(ret[12:300,])-1) %>% as.matrix()%*%m[,1] 
  eq_dlr_50 <- log(eq_dsr_50+1)
  eq_cumsum_50 <- cumsum(eq_dlr_50)
  eq_normalized_price_50 <- exp(eq_cumsum_50)
  initial_price <- 100 
  eq_100_price_50 <- initial_price * eq_normalized_price_50
  ptf_r <- exp(12/length(eq_dlr_50)*sum(eq_dlr_50))-1 
  sd<-sd(eq_dlr_50)*sqrt(12)
  return(c(sd,ptf_r,out.select$opt.lambda,out.select$opt.sparsity))
}

portfolio_an_ret_glasso_huge(stock_log_returns_monthly_50)


portfolio_weights_huge_glasso <- function(ret){
  w <- matrix(rep(0,dim(ret)[2]*289),nrow = 289)
  onev <- rep(1,dim(ret)[2])
  for (i in 1:289) {
    out.glasso <- huge(as.matrix(ret[i:(11+i),]), method="glasso")
    out.select <- huge.select(out.glasso, criterion="ric", rep.num=30)
    precision_m_test <- out.select$opt.icov
    w[i,] <- (precision_m_test%*%onev)/(as.numeric(t(onev)%*%precision_m_test%*%onev))
  }
  return(w)
}

set.seed(123)
ret<-stock_log_returns_monthly_50
onev <- rep(1,dim(ret)[2])
out.glasso <- huge(as.matrix(ret[1:(11+1),]), method="glasso")
out.select <- huge.select(out.glasso, criterion="ric", rep.num=30)
precision_m_test <- out.select$opt.icov
(precision_m_test%*%onev)/(as.numeric(t(onev)%*%precision_m_test%*%onev))


huge_glasso_weights<-portfolio_weights_huge_glasso(stock_log_returns_monthly_50)
huge_glasso_weights_df<-huge_glasso_weights %>% as.data.frame()
colnames(huge_glasso_weights_df)<-colnames(stock_log_returns_monthly_50)
huge_glasso_weights_df$date<-stock_log_returns_monthly$date[12:300]

huge_glasso_weights_df %>% View()

huge_glasso_weights_df %>%
  pivot_longer(-date, names_to = "stock", values_to = "weight") %>%
  ggplot(aes(x = date, y = weight, color = stock)) + 
  geom_line() + theme_light() + 
  geom_text_repel(aes(label = stock), hjust = -0.2,
                  data = portf_weights %>% 
                    pivot_longer(-date, names_to = "stock", values_to = "weight") %>%
                    dplyr::filter(date == max(date))) +
  annotate("text", x = as.Date("1997-12-01"), y = c(-1,1), label = "Ticker") +
  theme(legend.position = "none")

eq_dsr_HG <- rowSums(huge_glasso_weights_df[,-51] * (exp(stock_log_returns_monthly_50[12:300,]) - 1)) # return per year
eq_dlr_HG <- log(eq_dsr_HG+1)
eq_cumsum_HG <- cumsum(eq_dlr_HG)
eq_normalized_price_HG <- exp(eq_cumsum_HG)
initial_price <- 100 
eq_100_price_HG <- initial_price * eq_normalized_price_HG
plot(eq_100_price_HG,type="l")
ptf_r <- exp(12/length(eq_dlr_HG)*sum(eq_dlr_HG))-1 
sd<-sd(eq_dlr_HG)*sqrt(12)

ptf_r/sd







































