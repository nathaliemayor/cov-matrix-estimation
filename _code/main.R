# ------------------------------------------------------------------------------
#                 MAIN - MASTER'S THESIS COCKPIT
# ------------------------------------------------------------------------------
# execute preamble with packages, own functions and package preferences
source("preamble.R")

# define paths
core_path <- "/Users/pro/Library/Mobile Documents/com~apple~CloudDocs"
data_path <- "masters_thesis/data"
from_date <- as.Date("1970-01-01")
to_date <- as.Date("2019-12-01")

# ------------------------------------------------------------------------------
#                 LOAD DATA
# ------------------------------------------------------------------------------
# load Fama-French data
data_files_names <- c(
  "100_Portfolios_10x10_Wout_Div.CSV", 
  "100_Portfolios_10x10_Daily.CSV"
)

ff100_data <- lapply(
  data_files_names, 
  get_data, 
  path = file.path(core_path,data_path)
)
names(ff100_data) <- c("monthly", "daily")

# get 10-year Treasury notes and S&P500 data monthly and daily
tickers <- c("^TNX","^GSPC")

getSymbols(
  tickers, 
  from = from_date, 
  to = to_date, 
  periodicity = "daily"
)
gspc_monthly <- GSPC$GSPC.Adjusted %>% 
  apply.monthly(mean) %>% 
  fortify.zoo %>% 
  mutate(returns = GSPC.Adjusted/lag(GSPC.Adjusted)-1) 

# Load historical factor data (market, SMB, HML)
# downloaded directly from 
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
# Fama/French 3 Factors, CSV
factors <- rio::import(
  file.path(core_path,data_path, "F-F_Research_Data_Factors.CSV")
) %>% 
  mutate(V1 = as.Date(
    V1 %>% paste0("01"), 
    format = "%Y%m%d"
  )
  ) %>% 
  dplyr::filter(V1 > from_date & V1 < to_date) %>% 
  dplyr::select(-RF) %>% 
  rename(date = V1)

factors_daily <- rio::import(
  file.path(core_path,data_path, "F-F_Research_Data_Factors_daily.CSV")
) %>% 
  mutate(date = as.Date(date, 
                        format = "%Y%m%d")) %>% 
  dplyr::filter(date > from_date & date < to_date) %>% 
  dplyr::select(-RF) 

# ------------------------------------------------------------------------------
#                 DEFINE PARAMETERS, SETTINGS
# ------------------------------------------------------------------------------
training_period <- 252*10 # obs
rolling_period <- 21  # obs
k <- dim(ff100_data$daily)[1]
frequency <- "daily" # "monthly"

# rules: 
# lis: n_obs > n_var
# CovMve: n_obs > 2*n_var
# slow: huge_glasso, covMve, covMcd, RMT

cov_est_method = c(
  "cov1Para", 
  "cov2Para", 
  "covCor", 
  "CCM",
  "covDiag", 
  "covMarket",
  "gis",
  "qis",
  "lis",
  "CovMve",
  "CovMcd",
  "huge_glasso",
  "equal_weights",
  "factor1",
  "factor3",
  "RMT",
  "sample",
  "ewma"
  # "SP500"
)

roll <- seq(1, k - training_period, rolling_period)
# ------------------------------------------------------------------------------
#                 HISTORICAL DATA - COMPUTE PORTFOLIOS
# ------------------------------------------------------------------------------
iterations <- crossing(cov_est_method, roll) 

test_rolling_cov_method <- pmap(
  iterations,
  get_portfolio_metrics, 
  stock_returns = ff100_data$daily,
  portfolio_optimization = "tangent",
  short = TRUE, 
  frequency = frequency, 
  factor_returns = factors_daily
)

method_order <- crossing(cov_est_method, roll) %>% 
  dplyr::select(cov_est_method) %>% 
  unique()

names(test_rolling_cov_method) <- rep(
  method_order$cov_est_method, 
  each=length(roll)
  )

results_by_cov <- lapply(
  seq(1,(length(method_order$cov_est_method)-1)*length(roll)+1, length(roll)), 
  function(x) 
    test_rolling_cov_method[x:(x+length(roll)-1)]
)

names(results_by_cov) <- method_order$cov_est_method

all_returns <- lapply(method_order$cov_est_method, function(cov) 
  results_by_cov[[cov]] %>% 
    map_depth(1,1) %>%
    reduce(rbind) %>% 
    filter(!is.na(returns)) %>% 
    rename(!!cov := returns)
  ) %>% reduce(cbind) %>% select(which(!duplicated(names(.))))

all_avg_window_returns <- all_returns[,-1] %>% colMeans()

all_avg_overall_sd <- lapply(method_order$cov_est_method, function(cov) 
  results_by_cov[[cov]] %>% 
    map_depth(1,1) %>%
    reduce(rbind) %>% 
    filter(!is.na(returns)) %>% 
    dplyr::summarise(sd = sd(returns))
) %>% 
  unlist %>% 
  reduce(append)

all_window_sd <- lapply(method_order$cov_est_method, function(cov) 
  results_by_cov[[cov]] %>% 
    map_depth(1,2) %>% 
    reduce(append)) %>% 
  reduce(cbind) %>% 
  magrittr::set_colnames(method_order$cov_est_method) %>% data.frame

all_avg_window_sd <- all_window_sd %>% colMeans(na.rm=T)

all_window_sr <- lapply(method_order$cov_est_method, function(cov) 
  results_by_cov[[cov]] %>% 
    map_depth(1,3) %>% 
    reduce(append)) %>% 
  reduce(cbind) %>% 
  magrittr::set_colnames(method_order$cov_est_method) %>% data.frame

all_avg_window_sr <- all_window_sr %>% colMeans(na.rm = T)

all_weights <- lapply(method_order$cov_est_method, function(cov) 
  data <- results_by_cov[[cov]] %>% 
    map_depth(1,4) %>% 
    reduce(cbind) %>% 
    t %>% 
    as_tibble %>% 
    mutate(date = ff100_data$daily$Date[(training_period+roll)]) %>% 
    dplyr::select(date, everything()) %>% 
    magrittr::set_colnames(c("date",colnames(ff100_data$daily)[-1])) %>% 
    pivot_longer(!date) %>% 
    mutate(method = cov)
  ) %>% 
  reduce(rbind)

# benchmark: short restriction
test_short_restriction <- lapply(
  roll, 
  get_portfolio_metrics,
  stock_returns = ff100_data$daily,
  cov_est_method = "sample",
  portfolio_optimization = "tangent",
  short = FALSE,
  frequency = frequency,
  factor_returns = NULL
  )

returns_short <- test_short_restriction %>% 
  map_depth(1,1) %>% 
  do.call(rbind,.) %>% 
  rename(sample_short_constraint = returns)

avg_returns_short <- mean(returns_short$sample_short_constraint %>% na.omit)

sd_avg_overall_short <- sd(returns_short$sample_short_constraint %>% na.omit)

sd_window_short <- test_short_restriction %>% 
  map_depth(1,2) %>% 
  reduce(append)
  
sd_avg_window_short <- mean(sd_window_short, na.rm = TRUE)

sr_window_short <- test_short_restriction %>%  
  map_depth(1,3) %>%
  reduce(append)

sr_avg_window_short <- sr_window_short %>% mean(na.rm =TRUE)

short_restriction_weights <- test_short_restriction %>% 
  map_depth(1,4) %>% 
  reduce(cbind) %>% 
  t %>% 
  as_tibble %>% 
  mutate(date = ff100_data$daily$Date[(training_period+roll)]) %>% 
  dplyr::select(date, everything()) %>% 
  magrittr::set_colnames(c("date",colnames(ff100_data$daily)[-1])) %>% 
  pivot_longer(!date) %>% 
  mutate(method = "sample_short_constraint")

# compute performances for long methods: huge_glasso
library(foreach)
library(doParallel)
registerDoParallel(cores = 4)
tictoc::tic()
cov_est_method <- "huge_glasso"
df_parallel <- foreach(roll = roll, 
             .combine = rbind) %dopar% {
               get_portfolio_metrics(cov_est_method = cov_est_method,
                                     roll,
                                     stock_returns = ff100_data$daily,
                                     portfolio_optimization = "tangent",
                                     short = TRUE, 
                                     frequency = frequency,
                                     factor_returns = factors_daily)
             }
stopImplicitCluster()
tictoc::toc()

parallel_returns <- df_parallel[1:(dim(df_parallel)[1])] %>% 
  reduce(rbind) %>% 
  rename(huge_glasso = returns)

parallel_returns_avg <- mean(parallel_returns$huge_glasso, na.rm=T)

parallel_sd_overall <- sd(parallel_returns$huge_glasso, na.rm=T)

parallel_sd_window <- df_parallel[(dim(df_parallel)[1]+1):
                                    (dim(df_parallel)[1]*2)] %>% 
  reduce(rbind)
parallel_sd_window_avg <- mean(parallel_sd_window,na.rm=T) 

parallel_sr_window <- df_parallel[(dim(df_parallel)[1]*2+1):
                                    (dim(df_parallel)[1]*3)] %>% 
  reduce(rbind)
parallel_sr_window_avg <- mean(parallel_sr_window, na.rm=T)
parallel_weights <- df_parallel[(dim(df_parallel)[1]*3+1):
                                    (dim(df_parallel)[1]*4)] %>% 
  reduce(cbind) %>% 
  t %>% 
  as_tibble %>% 
  mutate(date = ff100_data$daily$Date[(training_period+roll)]) %>% 
  dplyr::select(date, everything()) %>% 
  magrittr::set_colnames(c("date",colnames(ff100_data$daily)[-1])) %>% 
  pivot_longer(!date) %>% 
  mutate(method = "huge_glasso")

# annual statistics for SP500
stat_sp500 <- GSPC$GSPC.Adjusted %>% fortify.zoo() %>% 
  filter(Index > (ff100_data$daily$Date[training_period]) &
           Index < (ff100_data$daily$Date[nrow(ff100_data$daily)])) %>% 
  mutate(returns = diff(GSPC.Adjusted)/lag(GSPC.Adjusted)) %>% 
  dplyr::summarise(mean = (mean(returns)*252*100),
                   sd = (sd(returns)*sqrt(252))*100,
                   sr = (mean(returns)*252*100-6.3)/((sd(returns)*sqrt(252))*100)) %>% 
  suppressWarnings()

if(frequency == "daily"){
  to_annual <- 252
}else if(frequency == "monthly"){
  to_annual <- 12
}

complete_weights <- all_weights %>% 
  rbind(parallel_weights) %>% 
  rbind(short_restriction_weights)

complete_returns <- all_returns %>% 
  cbind(returns_short[,-1] %>% na.omit) %>% 
  cbind(parallel_returns[,-1] %>% na.omit) %>% 
  cbind(GSPC$GSPC.Adjusted %>% fortify.zoo() %>% 
          filter(Index >= (ff100_data$daily$Date[training_period]) &
                   Index <= (ff100_data$daily$Date[nrow(ff100_data$daily)])) %>% 
          mutate(returns = (diff(GSPC.Adjusted)/lag(GSPC.Adjusted))*100) %>% 
          filter(Index %in% all_returns$date) %>% 
          rename(date = Index,
                 SP500 = returns) %>% 
          select(SP500))

complete_window_sd <- all_window_sd %>% 
  cbind(sd_window_short) %>% 
  cbind(parallel_sd_window) %>% 
  rename(huge_glasso = parallel_sd_window, 
         sample_short_constraint=sd_window_short)

complete_window_sr <- all_window_sr %>% 
  cbind(sr_window_short) %>% 
  cbind(parallel_sr_window) %>% 
  rename(huge_glasso = parallel_sr_window, 
         sample_short_constraint=sr_window_short)

complete_returns

roll_sr <-  results_rdata$d1260_21$returns %>% 
  pivot_longer(!date) %>% 
  left_join(TNX$TNX.Adjusted %>% fortify.zoo %>% rename(date=Index)) %>% 
  mutate(year = year(date)) %>%
  filter(date > "1980-01-01") %>% 
  group_by(year,name) %>% 
  dplyr::summarise(mean_returns = mean(value),
                   sd = sd(value),
                   rf = mean(TNX.Adjusted, na.rm = T)/252,
                   sr = (mean_returns-rf)/sd) %>% 
  ungroup %>% 
  group_by(name) %>% 
  dplyr::summarise(sr_roll = mean(sr)*sqrt(252),
                   sd_roll = mean(sd)*sqrt(252),
                   ret_roll = mean(mean_returns,na.rm = T)*252) %>% 
  ungroup() %>% rename(method = name)

results_data <- data.frame(
  method = method_order$cov_est_method, 
  mu = all_avg_window_returns*to_annual,
  sd_window = all_avg_window_sd*sqrt(to_annual), 
  sr_window = all_avg_window_sr*sqrt(to_annual), 
  sd_overall = all_avg_overall_sd*sqrt(to_annual)
) %>% add_row(
  method = "sample_short_constraint", 
  mu = avg_returns_short*to_annual, 
  sd_window = sd_avg_window_short*sqrt(to_annual),
  sr_window = sr_avg_window_short*sqrt(to_annual),
  sd_overall = sd_avg_overall_short*sqrt(to_annual)
) %>% add_row(
  method = "huge_glasso", 
  mu = parallel_returns_avg*to_annual, 
  sd_window = parallel_sd_window_avg*sqrt(to_annual),
  sr_window = parallel_sr_window_avg*sqrt(to_annual),
  sd_overall = parallel_sd_overall*sqrt(to_annual)
) %>% 
  add_row(
    method = "SP500", 
    mu = stat_sp500$mean, 
    sd_overall = stat_sp500$sd
  ) %>% left_join(roll_sr)
 
# ------------------------------------------------------------------------------
#                 BOOTSTRAPPED PORTFOLIO DATA
# ------------------------------------------------------------------------------
library(foreach)
library(doParallel)
registerDoParallel(cores = 4)
tictoc::tic()
n_bootstraps <- 1000
bootstrap_factor1 <- foreach(cov_est_method = "factor1", .combine = rbind) %dopar% {
  bootstrap_cov_estimates(cov_est_method = cov_est_method, 
                          roll = roll, 
                          n_bootstraps = n_bootstraps,
                          data = ff100_data$daily,
                          frequency = frequency,
                          factor_returns = factors_daily)
}
stopImplicitCluster()
tictoc::toc()
save(bootstrap_factor1, file = file.path(core_path, data_path, "bootstrap_factor1_1000.RData"))


#getting the convex hull of each unique point set

load(file.path(core_path, data_path, "bootstrap_cov.RData"))


df_all_bis <- df_all %>% 
  mutate(returns = returns*252,
         sd=sd*sqrt(252))

hulls <- ddply(df_all_bis,"method", find_hull)

ggplot(data = df_all_bis, 
               aes(x = sd, y = returns, colour=method, fill = method)) +
  geom_point() + 
  geom_polygon(data = hulls, alpha = 0.5) +
  labs(x = "sd", y = "returns")

# save(df_all, file = file.path(core_path, data_path, "bootstrap_cov.Rdata"))

test_daily <- pmap(
  tibble(data = ff100_data, frequency = c("monthly", "daily")),
  bootstrap_cov_estimates,
  roll = roll,
  n_bootstraps = 100,
  cov_est_method = "sample"
)

test_daily$daily <- test_daily$daily %>% mutate(method = "sample_daily")

test_daily %>% 
  reduce(rbind) %>% 
  filter(returns < 50 & sd < 200) %>% 
  ggplot(aes(x=sd, y = returns, color = method)) +
  geom_point()








