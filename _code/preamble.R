# ------------------------------------------------------------------------------
#                 PREAMBLE - LOAD PACKAGES AND USER DEFINED FUNCTIONS
# ------------------------------------------------------------------------------
# install and load local libraries 
local_libs <- list.files("lib", full.names = T)
lapply(local_libs[1], install.packages, repos=NULL, type="source")
# general
library(tidyverse)
library(xts)
library(zoo)
library(lubridate)
library(conflicted)
# plotting
library(ggalt)
library(extrafont)
library(plotly)
# covariance specific 
library(cccp)
library(huge)           # glasso, RIC
library(cvCovEst)
library(rrcov)          # CovMcd and CovMve
library(tawny)
library(tawny.types)
library(covmat)
library(corpcor)
library(CovTools)       # Oracle
library(covFactorModel) # factor models
# finance specific
library(tidyquant)
library(quantmod)
# math/ stats tools
library(quadprog)
library(MASS)
library(StatPerMeCo)
# remotes::install_github("MatthewBJane/theme_park")
library(ThemePark)
library(ggrepel)

# reslove confilcts
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("rename", "dplyr")
conflicts_prefer(stats::lag)
conflict_prefer("first", "dplyr")

# load own functions
# load Ledoit-Wolf functions

lapply(
  c(
    list.files("functions", full.names = TRUE),
    list.files("covShrinkage-main", pattern = ".R", full.names = T)
  ),
  source
) %>% invisible

