# ------------------------------------------------------------------------------
#                 MAIN - MASTER'S THESIS COCKPIT
# ------------------------------------------------------------------------------
# execute preamble with packages, own functions and package preferences
source("preamble.R")

# define paths 
core_path <- "/Users/pro/Library/Mobile Documents/com~apple~CloudDocs"
data_path <- "masters_thesis/data"
from_date <- as.Date("1958-02-01")
to_date <- as.Date("2018-02-01")

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

# ------------------------------------------------------------------------------
#                 DEFINE PARAMETERS, SETTINGS
# ------------------------------------------------------------------------------
training_period <- 184   # months
rolling_period <- 6         # months
n <- dim(ff100_data$monthly)[2]
k <- dim(ff100_data$monthly)[1]

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
  # "CovMve", 
  "CovMcd",
  "huge_glasso",
  "equal_weights",
  # "oracle", 
  # "pca",
  "factor1",
  "factor3",
  "sample"
)

roll <- seq(1, k - training_period, rolling_period)

# ------------------------------------------------------------------------------
#                 HISTORICAL DATA - COMPUTE PORTFOLIOS
# ------------------------------------------------------------------------------
# cov_est_method <- "sample"
test_rolling_cov_method <- pmap(
  crossing(cov_est_method, roll),
  get_portfolio_metrics, 
  stock_returns = ff100_data$monthly,
  portfolio_optimization = "tangent",
  short = TRUE, 
  factor_returns = factors
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
  ) %>% add_row(method = "daily_sample", returns = 0.924, sd = 2.65) %>% 
    add_row(method = "sample_short_constraint", returns = 0.933, sd = 3.87) %>% 
  filter(!method %in% "CovMcd"),
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

data.frame(
  method_order$cov_est_method, 
  all_avg_returns, 
  all_avg_sd, all_avg_sr
  ) %>% 
  dplyr::arrange(-all_avg_sr)

all_weights <- lapply(method_order$cov_est_method, function(cov) 
  data <- results_by_cov[[cov]] %>% 
    map_depth(1,4) %>% 
    reduce(cbind) %>% 
    t %>% 
    as_tibble %>% 
    mutate(date = seq.Date(
      from = as.Date("1973-06-01"), 
      to = as.Date("2018-03-01"), 
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
  cov_est_method = "cov1Para",
  portfolio_optimization = "tangent",
  short = TRUE,
  frequency = "daily",
  factor_returns  = NULL
)

test_daily %>%  
  map_depth(1,1) %>%
  reduce(rbind) %>% 
  filter(!is.na(returns)) %>% 
  dplyr::summarise(mean = mean(returns))

test_daily %>%  
  map_depth(1,2) %>%
  reduce(rbind) %>% 
  na.omit() %>% 
  mean()

test_daily %>%  
  map_depth(1,3) %>%
  reduce(rbind) %>% 
  na.omit() %>% 
  mean()

test_short_restriction <- lapply(
  roll, 
  get_portfolio_metrics,
  stock_returns = ff100_data$monthly,
  cov_est_method = "sample",
  portfolio_optimization = "tangent",
  short = FALSE,
  frequency = "monthly",
  factor_returns = NULL
  )

ret <- test_short_restriction %>% 
  map_depth(1,1) %>% 
  do.call(rbind,.) 

mean(ret$returns %>% na.omit)

sd <- test_short_restriction %>% 
  map_depth(1,2) %>% 
  do.call(rbind,.) %>% 
  na.omit %>% 
  mean


# ------------------------------------------------------------------------------
#                 BOOTSTRAPPED PORTFOLIO DATA
# ------------------------------------------------------------------------------
n_bootstraps <- 10

df_all <- lapply(cov_est_method, 
       bootstrap_cov_estimates, 
       roll=roll, 
       n_bootstraps=n_bootstraps) %>% 
  reduce(rbind)

#getting the convex hull of each unique point set

load(file.path(core_path, data_path, "bootstrap_cov.RData"))

df_all_bis <- df_all %>% filter(returns < 500, sd < 500)

hulls <- ddply(df_all_bis,"method", find_hull)

ggplot(data = df_all_bis, 
               aes(x = sd, y = returns, colour=method, fill = method)) +
  geom_point() + 
  geom_polygon(data = hulls, alpha = 0.5) +
  labs(x = "sd", y = "returns")

save(df_all, file = file.path(core_path, data_path, "bootstrap_cov.Rdata"))

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






