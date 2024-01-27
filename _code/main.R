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
ff100_data$daily <- ff100_data$daily
training_period <- 252*5 # obs
rolling_period <- 21   # obs
n <- dim(ff100_data$daily)[2]
k <- dim(ff100_data$daily)[1]
frequency <- "daily" # "monthly"

if(frequency == "daily"){
  assets_returns <- ff100_data$daily
  factors_returns <- factors_daily 
}else if(frequency == "monthly"){
  assets_returns <- ff100_data$monthly
  factors_returns <- factors
}

# rules: 
# lis: n_obs > n_var
# CovMve: n_obs > 2*n_var

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
  "sample"
)

roll <- seq(1, k - training_period, rolling_period)
# ------------------------------------------------------------------------------
#                 HISTORICAL DATA - COMPUTE PORTFOLIOS
# ------------------------------------------------------------------------------
# cov_est_method <- "CCM"
test_rolling_cov_method <- pmap(
  crossing(cov_est_method, roll),
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

all_avg_returns <- lapply(method_order$cov_est_method, function(cov) 
  results_by_cov[[cov]] %>% 
    map_depth(1,1) %>%
    reduce(rbind) %>% 
    filter(!is.na(returns)) %>% 
    dplyr::summarise(mean = mean(returns))
  ) %>% 
  unlist %>% 
  reduce(append)

all_sd <- lapply(method_order$cov_est_method, function(cov) 
  results_by_cov[[cov]] %>% 
    map_depth(1,1) %>%
    reduce(rbind) %>% 
    filter(!is.na(returns)) %>% 
    dplyr::summarise(sd = sd(returns))
) %>% 
  unlist %>% 
  reduce(append)

all_avg_sd <- lapply(method_order$cov_est_method, function(cov) 
  results_by_cov[[cov]] %>% 
    map_depth(1,2) %>% 
    reduce(append) %>% 
    na.omit %>% 
    mean) %>% 
  reduce(append)

ggplot(
  tibble(
    method = method_order$cov_est_method, 
    returns = all_avg_returns, 
    sd = all_avg_sd
  ) %>% 
  filter(!method %in% c("CovMve")),
  aes(x = sd, y = returns)) +
  geom_point() +
  geom_label_repel(
    aes(label = method),
    box.padding = 1,
    point.padding = 1,
    segment.color = "grey",
    color = "darkgreen"
  ) +
  theme_hsg() +
  # xlim(c(11,24)) +
  ggtitle("Tangent portfolios with various covariance matrix estimation methods")

all_avg_sr <- lapply(method_order$cov_est_method, function(cov) 
  results_by_cov[[cov]] %>% 
    map_depth(1,3) %>% 
    reduce(append) %>% 
    na.omit %>% 
    mean) %>% 
  reduce(append)

all_weights <- lapply(method_order$cov_est_method, function(cov) 
  data <- results_by_cov[[cov]] %>% 
    map_depth(1,4) %>% 
    reduce(cbind) %>% 
    t %>% 
    as_tibble %>% 
    mutate(date = seq.Date(
      from = from_date + months(training_period), 
      to = to_date, 
      by = "6 month"
    )
    ) %>% 
    dplyr::select(date, everything()) %>% 
    pivot_longer(!date) %>% 
    mutate(method = cov)
  ) %>% 
  reduce(rbind)

all_weights %>% 
  filter(method == "cov1Para") %>% 
  ggplot(aes(x=date, y=value, group = name)) +
  geom_boxplot(fill = "lightblue") +
  facet_wrap(~method, scales = "free", ncol = 1) +
  theme_hsg()

all_weights %>% 
  dplyr::group_by(method) %>% 
  dplyr::summarise(
            sd = sd(value),
            median = median(value),
            min = min(value),
            max = max(value),
            q5 = quantile(value, probs = 0.05),
            q25 = quantile(value, probs = 0.25),
            q75 = quantile(value, probs = 0.75), 
            q95 = quantile(value, probs = 0.95)
            )

test_weights <- all_weights %>% 
  group_by(date)

test_weights <- all_weights %>% 
  filter(name == "ME1 BM4") %>% .[,"value"]

diff(test_weights$value) %>% 
  abs %>% 
  sum

test_daily <- lapply(
  roll,
  get_portfolio_metrics,
  stock_returns = ff100_data$daily,
  cov_est_method = "sample",
  portfolio_optimization = "tangent",
  short = TRUE,
  frequency = "daily",
  factor_returns  = NULL
)

ret_d <- test_daily %>%  
  map_depth(1,1) %>%
  reduce(rbind) %>% 
  filter(!is.na(returns)) %>% 
  dplyr::summarise(mean = mean(returns))

sd_d <- test_daily %>%  
  map_depth(1,2) %>%
  reduce(rbind) %>% 
  na.omit() %>% 
  mean()

sd_ret_d <- test_daily %>%  
  map_depth(1,1) %>%
  reduce(rbind) %>% 
  filter(!is.na(returns)) %>% 
  dplyr::summarise(sd = sd(returns))

sr_d <- test_daily %>%  
  map_depth(1,3) %>%
  reduce(rbind) %>% 
  na.omit() %>% 
  mean()

sample_daily_weights <- test_daily %>% 
  map_depth(1,4) %>% 
  reduce(cbind) %>% 
  t %>% 
  as_tibble %>% 
  mutate(date = seq.Date(
    from = from_date + months(training_period), 
    to = to_date, 
    by = "6 month"
  )
  ) %>% 
  dplyr::select(date, everything()) %>% 
  pivot_longer(!date) %>% 
  mutate(method = "daily_sample")

test_short_restriction <- lapply(
  roll, 
  get_portfolio_metrics,
  stock_returns = assets_returns,
  cov_est_method = "sample",
  portfolio_optimization = "tangent",
  short = FALSE,
  frequency = frequency,
  factor_returns = NULL
  )

ret <- test_short_restriction %>% 
  map_depth(1,1) %>% 
  do.call(rbind,.) 

ret_short <- mean(ret$returns %>% na.omit)

sd_ret_short <- sd(ret$returns %>% na.omit)

sd_short <- test_short_restriction %>% 
  map_depth(1,2) %>% 
  do.call(rbind,.) %>% 
  na.omit %>% 
  mean

sr_short <- test_short_restriction %>%  
  map_depth(1,3) %>%
  reduce(rbind) %>% 
  na.omit() %>% 
  mean()

short_restriction_weights <- test_short_restriction %>% 
  map_depth(1,4) %>% 
  reduce(cbind) %>% 
  t %>% 
  as_tibble %>% 
  mutate(date = seq.Date(
    from = from_date + months(training_period), 
    to = to_date, 
    by = "6 month"
  )
  ) %>% 
  dplyr::select(date, everything()) %>% 
  pivot_longer(!date) %>% 
  mutate(method = "sample_short_constraint")

# annual statistics for SP500
stat_sp500 <- GSPC$GSPC.Adjusted %>% fortify.zoo() %>% 
  filter(Index > from_date+months(training_period)&
           Index < to_date) %>% 
  mutate(returns = diff(GSPC.Adjusted)/lag(GSPC.Adjusted)) %>% 
  dplyr::summarise(mean = (mean(returns)*252*100),
                   sd = (sd(returns)*sqrt(252))*100,
                   sr = (mean(returns)*252*100-0.5)/((sd(returns)*sqrt(252))*100))

if(frequency == "daily"){
  to_annual <- 252
}else if(frequency == "monthly"){
  to_annual <- 12
}

results_data <- data.frame(
  method = method_order$cov_est_method, 
  mu = all_avg_returns*to_annual,
  sd_window = all_avg_sd*sqrt(to_annual), 
  sr_window = all_avg_sr*sqrt(to_annual), 
  sd_overall = all_sd*sqrt(to_annual)
) %>%  add_row(
  method = "daily_sample", 
  mu = ret_d$mean*252, 
  sd_window = sd_d*sqrt(252), 
  sr_window = sr_d*sqrt(252),
  sd_overall = sd_ret_short*sqrt(252)
) %>% 
  add_row(
    method = "sample_short_constraint", 
    mu = ret_short*to_annual, 
    sd_window = sd_short*sqrt(to_annual),
    sr_window = sr_short*sqrt(to_annual),
    sd_overall = sd_ret_short*sqrt(to_annual)
  ) %>% mutate(sr_overall = (mu-6)/sd_overall) %>% 
  add_row(
    method = "SP500", 
    mu = stat_sp500$mean, 
    sd_overall = stat_sp500$sd,
    sr_overall = stat_sp500$sr
  )
 
# ------------------------------------------------------------------------------
#                 BOOTSTRAPPED PORTFOLIO DATA
# ------------------------------------------------------------------------------
library(foreach)
library(doParallel)
registerDoParallel(cores = 4)
tictoc::tic()
n_bootstraps <- 1000
cov_est_method <- "factor1"
df_all <- foreach(cov_method = cov_est_method, .combine = rbind) %dopar% {
  bootstrap_cov_estimates(cov_method, 
                          roll = roll, 
                          n_bootstraps = n_bootstraps,
                          data = ff100_data$daily,
                          frequency = frequency,
                          factor_returns = factors_daily)
}
stopImplicitCluster()
tictoc::toc()

bootstrap_factor1 <- df_all %>% 
  mutate(sr = sqrt(252)*returns/sd)


tictoc::tic()
df_all <- lapply(cov_est_method, 
       bootstrap_cov_estimates, 
       roll=roll, 
       n_bootstraps=n_bootstraps,
       data = ff100_data$daily,
       frequency = frequency,
       factor_returns = factors_daily) %>% 
  reduce(rbind)
tictoc::toc()

#getting the convex hull of each unique point set

load(file.path(core_path, data_path, "bootstrap_cov.RData"))
# save(bootstrap_factor1, file = file.path(core_path, data_path, "bootstrap_factor1_1000.RData"))

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






