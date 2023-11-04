# ------------------------------------------------------------------------------
#                 MAIN - MASTER'S THESIS COCKPIT
# ------------------------------------------------------------------------------
# execute preamble with packages, own functions and package preferences
source("preamble.R")

# define paths 
data_path <- "/Users/pro/Library/Mobile Documents/com~apple~CloudDocs/masters_thesis/data/"
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

ff100_data <- lapply(data_files_names, function(file_name){
  data <- rio::import(
    file.path(
      data_path, 
      file_name
    ), skip = 15
  ) %>% 
    suppressWarnings %>% 
    mutate(Date = as.Date(paste0(V1,"01"), format = "%Y%m%d")) %>% 
    dplyr::select(
      Date, everything(), 
      -V1
    ) %>% 
    filter(Date => from_date & Date <= to_date)
  
  sum_of_col <- as.data.frame(data == -99.99) %>% 
    colSums %>% 
    data_frame(num = ., name = names(.)) %>% 
    filter(num > 0)
  
  data_clean <- data %>% 
    dplyr::select(-sum_of_col$name)
})
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
  file.path(data_path, "F-F_Research_Data_Factors.CSV")
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
training_period <- 184    # months
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
  "covDiag", 
  "covMarket",
  "gis", 
  "qis", 
  "lis",
  # "CovMve", 
  "CovMcd",
  "huge_glasso",
  # "equal_weights",
  # "oracle", 
  # "pca",
  "factor1",
  "factor3",
  "sample"
)

roll <- seq(1, k - training_period, rolling_period)

# ------------------------------------------------------------------------------
#                 SIMULATIONS
# ------------------------------------------------------------------------------
#########
# PRIAL #
#########

# N_seq <- c(3,4,5,8,10,16,20,25,32,40,50,80,100,160,200, 300, 500)
# p_seq <- N_seq %>% rev

N_seq <- seq(50,500, 50)
p_seq <- rev(N_seq)

Np <- data.frame(n_obs=N_seq, p_variables=p_seq) 
n_simulations <- 10

method <- cov_est_method
# method <- "huge_glasso"

tictoc::tic()
mc_sim <- lapply(1:n_simulations, function(sim){
  pmap(
    crossing(Np, method) %>% 
      filter(
        case_when(
          method %in% c("lis","gis", "CovMcd") ~ n_obs > p_variables,
          method == "CovMve" ~ n_obs > 2*p_variables,
          method %in% c("factor1", "pca") ~ n_obs > 2 & p_variables > 5,
          method == "factor3" ~ n_obs > 3,
          TRUE ~ n_obs > 1)
      ),
    mc_simulation_cov,
    simulation = sim,
    criterion = "prial"
  ) %>% reduce(append)
})
tictoc::toc()

mean_test_primal <- lapply(1:length(mc_sim[[1]]), function(x){
  mc_sim %>% map_depth(1,x) %>% reduce(append) %>% mean
}) %>% reduce(append)

# winner: gis N > K, in general cov1para
prial_plot <- crossing(Np, method) %>% 
  filter(
    case_when(
      method %in% c("gis", "lis", "CovMcd") ~ n_obs > p_variables,
      method == "CovMve" ~ n_obs > 2*p_variables,
      method %in% c("factor1", "pca") ~ n_obs > 2 & p_variables > 5,
      method == "factor3" ~ n_obs > 3,
      TRUE ~ n_obs > 1)) %>% 
  mutate(primal = mean_test_primal,
         pn = p_variables/n_obs) %>% 
  ggplot(aes(x = pn, y = primal, color = method)) +
  # geom_smooth(se = F) +
  geom_line() +
  geom_vline(xintercept = 1, color = "red") +
  geom_hline(yintercept = 0, color ="red") +
  theme_hsg() +
  xlab("Nb. variables/ Nb. observations") + 
  ylab("Percentage Relative Improvement in Average Loss (%)") + 
  ggtitle("PRIAL for estimators wrt. sample covariance") +
  ylim(c(-1,1)) +
  xlim(c(0,10))

plotly::ggplotly(prial_plot)

###########
# L2 Norm #
###########

n_obs <- seq(0,200, 40)
p_variables <- n_obs
method <- cov_est_method

l2_norm_cov <- lapply(1:10, function(sim){
  pmap(
    crossing(Np, method) %>% 
      filter(
        case_when(
          method %in% c("gis", "lis", "CovMcd") ~ n_obs > p_variables,
          method == "CovMve" ~ n_obs > 2*p_variables,
          method %in% c("factor1", "pca") ~ n_obs > 2 & p_variables > 5,
          method == "factor3" ~ n_obs > 3,
          TRUE ~ n_obs > 1)),
    mc_simulation_cov,
    criterion = "l2_norm",
    simulation = sim
  ) %>% reduce(append)
})

mean_l2 <- lapply(1:length(l2_norm_cov[[1]]), function(x){
  l2_norm_cov  %>% map_depth(1,x) %>% reduce(append) %>% mean
}) %>% reduce(append)

results_l2 <- crossing(Np, method) %>% 
  filter(
    case_when(
      method %in% c("gis", "lis", "CovMcd") ~ n_obs > p_variables,
      method == "CovMve" ~ n_obs > 2*p_variables,
      method %in% c("factor1", "pca") ~ n_obs > 2 & p_variables > 5,
      method == "factor3" ~ n_obs > 3,
      TRUE ~ n_obs > 1)) %>% 
  mutate(l2 = mean_l2)

colnames(results_l2) <- c("x","y","method", "z")

results_method <- lapply(method, function(met){
  results_l2 %>% filter(method == met) %>% mutate(z=z/y)
})
names(results_method) <- method

lapply(method, function(met){
  plot_ly(
    x = results_method[[met]]$x %>% unique,
    y = results_method[[met]]$y %>% unique,
    z = matrix(results_method[[met]]$z, ncol = 5),
    type = "contour"
  ) %>% 
    layout(title = met, 
           xaxis = list(title = "Nb. Observations"), 
           yaxis = list(title = "Nb. Variables")
    )
})

# ------------------------------------------------------------------------------
#                 HISTORICAL DATA - COMPUTE PORTFOLIOS
# ------------------------------------------------------------------------------

test_rolling_cov_method <- pmap(
  crossing(cov_est_method, roll),
  get_portfolio_metrics, 
  stock_returns = ff100_data$monthly,
  portfolio_optimization = "tangent",
  short = TRUE, 
  factor_returns = factors
)

names(test_rolling_cov_method) <- rep(cov_est_method, each=length(roll))

results_by_cov <- lapply(
  seq(1,(length(cov_est_method)-1)*length(roll)+1, length(roll)), 
  function(x) 
    test_rolling_cov_method[x:(x+length(roll)-1)]
)

names(results_by_cov) <- cov_est_method

all_avg_returns <- lapply(cov_est_method, function(cov) 
  results_by_cov[[cov]] %>% 
    map_depth(1,1) %>% 
    reduce(rbind) %>% 
    filter(!is.na(returns)) %>% 
    summarise(mean = mean(returns))) %>% 
  unlist %>% 
  reduce(append)

all_avg_sd <- lapply(cov_est_method, function(cov) 
  results_by_cov[[cov]] %>% 
    map_depth(1,2) %>% 
    reduce(append) %>% 
    na.omit %>% 
    mean) %>% 
  reduce(append)

ggplot(
  data_frame(
    method = cov_est_method, 
    returns = all_avg_returns, 
    sd = all_avg_sd
  ) %>% filter(!method %in% c("huge_glasso", "gis")),
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


