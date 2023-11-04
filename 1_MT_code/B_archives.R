# FILE B - SHRINKAGE ESTIMATION FOR COVARIANCE MATRICES - NATHALIE MAYOR
# Old code that was probably turned into functions

current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
path = getwd()

# Source header and functions files 
source("0_header.R")
source("2_functions.R")

##### EQ WEIGHTED PTF #####
# 50 assets
eq_w_50 <- rep(1/50,50)
eq_dsr_50 <- rowSums(eq_w_50 * (exp(stock_log_returns_monthly_50[12:300,-51]) - 1)) # return per year
eq_dlr_50 <- log(eq_dsr_50+1)
eq_cumsum_50 <- cumsum(eq_dlr_50)
eq_normalized_price_50 <- exp(eq_cumsum_50)
eq_100_price_50 <- initial_price * eq_normalized_price_50
an_r_50 <- exp(dy/length(eq_dlr_50)*sum(eq_dlr_50))-1 
sd_r_50 <- sd(eq_dlr_50)*sqrt(dy)
plot(eq_100_price_50,type="l",main="Performance Equally Weighted Porfolio (50)")


# 250 assets
eq_w_250 <- rep(1/250,250)
eq_dsr_250 <- rowSums(eq_w_250 * (exp(stock_log_returns_monthly_250[12:300,]) - 1)) # return per year
eq_dlr_250 <- log(eq_dsr_250+1)
eq_cumsum_250 <- cumsum(eq_dlr_250)
eq_normalized_price_250 <- exp(eq_cumsum_250)
eq_100_price_250 <- initial_price * eq_normalized_price_250
an_r_250 <- exp(dy/length(eq_dlr_250)*sum(eq_dlr_250))-1 
sd_r_250 <- sd(eq_dlr_250)*sqrt(dy)
plot(eq_100_price_250,type="l",main="Performance Equally Weighted Porfolio (250)")

# 500 assets
eq_w_500 <- rep(1/500,500)
eq_dsr_500 <- rowSums(eq_w_500 * (exp(stock_log_returns_monthly_500[12:300,]) - 1)) # return per year
eq_dlr_500 <- log(eq_dsr_500+1)
eq_cumsum_500 <- cumsum(eq_dlr_500)
eq_normalized_price_500 <- exp(eq_cumsum_500)
eq_100_price_500 <- initial_price * eq_normalized_price_500
an_r_500 <- exp(dy/length(eq_dlr_500)*sum(eq_dlr_500))-1 
sd_r_500 <- sd(eq_dlr_500)*sqrt(dy)
plot(eq_100_price_500,type="l",main="Performance Equally Weighted Porfolio (500)")

# 1000 assets
eq_w_1000 <- rep(1/1000,1000)
eq_dsr_1000 <- rowSums(eq_w_1000 * (exp(stock_log_returns_monthly_1000[13:300,]) - 1)) # return per year
eq_dlr_1000 <- log(eq_dsr_1000+1)
eq_cumsum_1000 <- cumsum(eq_dlr_1000)
eq_normalized_price_1000 <- exp(eq_cumsum_1000)
eq_100_price_1000 <- initial_price * eq_normalized_price_1000
an_r_1000 <- exp(dy/length(eq_dlr_1000)*sum(eq_dlr_1000))-1 
sd_r_1000 <- sd(eq_dlr_1000)*sqrt(dy)
plot(eq_100_price_1000,type="l",main="Performance Equally Weighted Porfolio (1000)")

eq_an_r <- c(an_r_spy, an_r_50,an_r_250,an_r_500,an_r_1000)
sd_an_r <- c(sd_r_spy, sd_r_50,sd_r_250,sd_r_500,sd_r_1000)
sr_an_eq <- eq_an_r/sd_an_r

plot(spy_100_price,type="l",main="SP500",xlim=c(0,300),ylim=c(0,2500),col="blue")
lines(eq_100_price_50,col="black")
lines(eq_100_price_250,col="orange")
lines(eq_100_price_500,col="red")
lines(eq_100_price_1000,col="violet")
lines(eq_100_price_HG,col="green")


























