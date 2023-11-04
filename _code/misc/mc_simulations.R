# ------------------------------------------------------------------------------
#                 SIMULATIONS
# ------------------------------------------------------------------------------
# execute preamble with packages, own functions and package preferences
source("preamble.R")
# rules: 
# lis: n_obs > n_var
# CovMve: n_obs > 2*n_var

method = c(
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

#########
# PRIAL #
#########

# N_seq <- c(3,4,5,8,10,16,20,25,32,40,50,80,100,160,200, 300, 500)
# p_seq <- N_seq %>% rev

N_seq <- seq(50,500, 50)
p_seq <- rev(N_seq)

Np <- data.frame(n_obs=N_seq, p_variables=p_seq) 
n_simulations <- 10

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
