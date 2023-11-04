current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
path = getwd()

# Source header and functions files 
source("0_header.R")
source("2_functions.R")

#### 1/N : equally weighted portfolio strategies - BENCHMARK for 50; 100, 250; 1000 sizes ####
dy <- 12   # 12 if monthly data, 252 if daily data
initial_price <- 100 
stock_log_returns_daily_50 %>% dim()

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

eq_50<-equally_weighted_portfolio(stock_log_returns_monthly_50,12,13,300)
eq_250<-equally_weighted_portfolio(stock_log_returns_monthly_250,12,13,300)
eq_500<-equally_weighted_portfolio(stock_log_returns_monthly_500,12,13,300)
eq_1000<-equally_weighted_portfolio(stock_log_returns_monthly_1000,12,13,300)

an_r_eq<-c(eq_50$annual_return,eq_250$annual_return,eq_500$annual_return,eq_1000$annual_return)
sd_r_eq<-c(eq_50$annual_sd,eq_250$annual_sd,eq_500$annual_sd,eq_1000$annual_sd)
sr_r_eq<-an_r_eq/sd_r_eq



# SP500

r_SPY <- diff(log(SPY$Adj.Close), lag=1)[11:299]
an_r_spy <- exp(12/length(r_SPY)*sum(r_SPY))-1 
sd_r_spy <- sd(r_SPY)*sqrt(12)
spy_cumsum <- cumsum(r_SPY)
spy_normalized_price <- exp(spy_cumsum)
spy_100_price <- initial_price * spy_normalized_price

plot(eq_50$norm_price,type="l",ylim=c(0,2500))
lines(eq_250$norm_price)
lines(eq_500$norm_price)
lines(eq_1000$norm_price)
lines(spy_100_price,col="orange")

# Efficient portfolio with target return (1/N)

sigma.mat <- cov(stock_log_returns_daily_50[1:252,])
mu.vec <- exp(colSums(stock_log_returns_daily_50[1:252,]))-1 
mu.vec_post <- exp(colSums(stock_log_returns_daily_50[253:505,]))-1 
top.mat = cbind(2*sigma.mat, mu.vec, rep(1, 50))
mid.vec = c(mu.vec, 0, 0)
bot.vec = c(rep(1, 50), 0, 0)
Ax.mat = rbind(top.mat, mid.vec, bot.vec)
bmsft.vec = c(rep(0, 50), an_r_50, 1)

z.mat = solve(Ax.mat)%*%bmsft.vec
x.vec = z.mat[1:50,]
x.vec %>% plot(ylim=c(-1,1))
mu.px = as.numeric(crossprod(x.vec, mu.vec_post))
sig2.px = as.numeric(t(x.vec)%*%sigma.mat%*%x.vec)*252
sig.px = sqrt(sig2.px)
c(sig2.px, sig.px)


eq_dsr_EF2 <- rowSums(x.vec %>% t *(exp(colSums(stock_log_returns_daily_50[1:252,])) - 1)) # return per year

ts_ret_ptf_EF <- (exp(stock_log_returns_daily_50[252:503,])-1) %>% as.matrix()%*%x.vec
log_ts_ptf_EF <- log(ts_ret_ptf_EF+1)
eq_cumsum_EF <- cumsum(log_ts_ptf_EF)
eq_normalized_price_EF<- exp(eq_cumsum_EF)
eq_100_price_EF <- initial_price * eq_normalized_price_EF
an_r_EF <- exp(252/length(log_ts_ptf_EF)*sum(log_ts_ptf_EF))-1 
sd_r_EF <- sd(log_ts_ptf_EF)*sqrt(252)
plot(eq_100_price_EF,type="l",main="Performance efficient portfolio with target return")

asset<-50
one.vec = rep(1, asset)
sigma.inv.mat = solve(cov(stock_log_returns_monthly_50))
top.mat = sigma.inv.mat%*%one.vec
bot.val = as.numeric((t(one.vec)%*%sigma.inv.mat%*%one.vec))
m.mat = top.mat/bot.val
#m.mat[,1]
ts_ret_ptf_GMV <- (exp(stock_log_returns_monthly_50)-1) %>% as.matrix()%*%m.mat[,1]
log_ts_ptf_GMV <- log(ts_ret_ptf_GMV+1)
eq_cumsum_GMV <- cumsum(log_ts_ptf_GMV)
eq_normalized_price_GMV<- exp(eq_cumsum_GMV)
eq_100_price_GMV <- initial_price * eq_normalized_price_GMV
an_r_GMV <- exp(12/length(log_ts_ptf_GMV)*sum(log_ts_ptf_GMV))-1 
sd_r_GMV<- sd(log_ts_ptf_GMV)*sqrt(12)
plot(eq_100_price_GMV,type="l",main="Performance GMV portfolio with 50 assets")


## Rolling average correlation
library(zoo)
rollapply(stock_log_returns_monthly_300[,-1], width=36, function(x) mean(cor(x)), by.column=FALSE) %>% plot(type="l",ylim=c(0,1))

stock_log_returns_monthly_300[,-1] %>% view()





