current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
path = getwd()

# Source header and functions files 
source("0_header.R")
source("2_functions.R")

# Appendix: names of the companies used (1446 companies with complete dates)

mktcapdataall<-mktcapdata[mktcapdata$PERMNO %in% tickers[to_keep],]
goodtickers <- database[database$PERMNO %in% tickers[to_keep],]
namescompanies <- goodtickers$PERMNO %>% unique()
uniquetruc = data.matrix(unique(goodtickers[,1]))
col11 = goodtickers[,1]
indexes_start1 = match(uniquetruc, col11)
nomscomplets<-goodtickers$COMNAM 
nomscompletsuniques<-nomscomplets[indexes_start1] %>% unique() %>% as.data.frame()

noms<-stargazer::stargazer(nomscompletsuniques,
                           type = 'latex',
                           summary = FALSE,
                           rownames = FALSE)


nomscompletsuniques %>%  dim()



# WEIGHTS PLOT (STACKED)
portf_weights[,1:9] %>% 
  select(date, everything()) %>% 
  pivot_longer(cols = 2:ncol(.), values_to = "weights") %>% 
  ggplot(aes(fill = name, y = weights, x = date)) + 
  geom_bar(position = "stack", stat = "identity", width = 100) +
  scale_fill_viridis(option = "viridis", discrete = TRUE, name = "Asset") +
  theme_bw() +
  ggtitle("GMVP Rolling Portfolio Adjustments") +
  xlab("Date") +
  ylab("Weights")

djiSpec <- portfolioSpec()
setNFrontierPoints(djiSpec) <- 50
setEstimator(djiSpec) <- "shrinkEstimator"
b<-as.timeSeries(stock_log_returns_monthly_50)
djiFrontier <- portfolioFrontier(b, djiSpec)
col = seqPalette(50, "BuGn")
c<-weightsPlot(djiFrontier, col = col)
c$minRisk

