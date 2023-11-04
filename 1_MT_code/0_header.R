# FILE 0 - SHRINKAGE ESTIMATION FOR COVARIANCE MATRICES - NATHALIE MAYOR
# Loading required packages

# Set the working directory
path = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
path2output = file.path(path, "Output")

# install packages (uncomment if needed)
# BiocManager::install("RBGL")
# BiocManager::install("Rgraphviz")
# install.packages("fPortfolio")
# install.packages("viridis")               
# install.packages("PortfolioAnalytics")
# install.packages("CVXR")
# install.packages("GLassoElnetFast")
# install.packages("huge")

# load packages 
library(zoo)        # Time series
library(magrittr)   # pipes
library(dplyr)      # data manipulation
library(tidyr)
library(stargazer)
library(tictoc)

# Ridge L1 penalization
library(RBGL)
library(Rgraphviz)
library(rags2ridges)
library(glmnet)                                     # This is THE package for penalised regressions
library(tidyverse)                                  # ... the usual core packages
library(lubridate)                                  # Package for date management
library(ggrepel)                                    # Package for ggplot annotations
library(scales)                                     # Package for scales in ggplot
library(tictoc)                                     # Package for computation time
library(CVXR)

# GLASSO
library(huge)
library(glasso)

# Plots
library(ggcorrplot) 
library(viridis)

# Creating portfolios and benchmarks
library(fPortfolio)
library(PortfolioAnalytics)
library(tidyquant)

# tawny 
library(tawny)
library(tawny.types)
library(futile.matrix)
library(futile.logger)
library(PerformanceAnalytics)
library(xts)
library(quantmod)
library(tseries)
library(glmnet)
library(matrixcalc)
library(matlib)
library(GLassoEl)

# parallel processing
install.packages("doSNOW")
library(foreach)
library(doSNOW)
cl <- makeCluster(2, type="SOCK") # for 2 cores machine
registerDoSNOW(cl)


