# FILE 1 - SHRINKAGE ESTIMATION FOR COVARIANCE MATRICES - NATHALIE MAYOR
# Preparing and cleaning the data 

# Data Extraction
# from WRDS: 'Compustat Daily Updates - Security Daily' > 
# in 'Step 1: Choose your date range' from 1997-01-01 to 2022-01-01 >
# in 'Step 2: Apply your company codes.' select GVKEY and select 'Search the entire database'>
# in 'Step 3: Choose query variables.' select 'PRCCD -- Price - Close - Daily (PRCCD)'>
# in 'Step 4: Select query output.', 'Output Format' select 'comma-delimited text (*.csv)'; 'Compression Type' select 'zip (*.zip); 'Date Format'select 'YYMMDDn8'
# Finally click Submit Form and download the zip file, (it should take around 5 minutes to proceed)
# We obtain a file with columns 'datedate', 'gvkey' and 'prccd'


# Cleaning the environment
graphics.off()
rm(list = ls())

# Set working directory
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
path = getwd()

# Source header and functions files 
source("0_header.R")
source("2_functions.R")

####### with monthly data from 31.01.1952 to 2022.01.2022 #######

monthly_stock_prices_1982_2022 <- read.table("monthly_stock_prices_19820131_20220131.csv",header = 1,sep = ",")
monthly_stock_prices_1982_2022$mktcap <- monthly_stock_prices_1982_2022$SHROUT*abs(monthly_stock_prices_1982_2022$PRC)
monthly_stock_prices_1982_2022_1 <- monthly_stock_prices_1982_2022[,c(1,2,5)] 
monthly_stock_prices_1982_2022_1$PRC <- abs(monthly_stock_prices_1982_2022_1$PRC)
stock_prices <- monthly_stock_prices_1982_2022_1
# Tickers and the index of each stock start and end date
tickers = data.matrix(unique(stock_prices[,1]))
col1 = stock_prices[,1]
indexes_start = match(tickers, col1)
indexes_end = indexes_start - 1
indexes_end[1:(length(indexes_end)-1)] = indexes_end[2:length(indexes_end)]
indexes_end[length(indexes_end)] = length(col1)

# We will keep only those having at least x days of data 
days_available<-indexes_end - indexes_start 
most_common_value<-sort(table(days_available),decreasing = TRUE)[1] %>% names() %>% as.numeric() 
# Shows that most of the stocks (576) contains 480 datapoints (represents 40y of monthly stock prices 40*12=48 trading days)
to_keep = (indexes_end - indexes_start) == most_common_value


# Create a vector of unique dates
sample = stock_prices[1:indexes_end[1],] # (indexes_end - indexes_start) shows also that the first stock contains those 480 dates
dates_vector = unique(as.character(sample[,2])) # unique list of dates
dates_vector = as.Date(dates_vector,format = "%Y%m%d") # better date format
dates_vector = dates_vector[ order(dates_vector) ] # put the dates in order

# Convert also  all the dates of the database to the same format
dates_column = as.character(stock_prices[,2])
dates_column = as.Date(dates_column,format = "%Y%m%d") # better date format
stock_prices[,2] = dates_column

# All stocks time series will have the same set of date: dates_vector
# When a price is missing at one point, we will carry over the last value
stocks_list=c() # Empty data container
temp_col = cbind(1:length(dates_vector)) # Empty data container
standardized_matrix = data.frame(dates_vector,temp_col)
for (i in 1:length(tickers))
{
  if(to_keep[i]==1) # If the current stock is to be kept, add it to the list
  {
    # Data of the current stock
    current_ticker = tickers[i]
    start = indexes_start[i]
    end = indexes_end[i]
    chunk = stock_prices[start:end,2:3]
    
    # Merge to standardized with same set of dates
    current_stock = merge(standardized_matrix,chunk,by=1,all=1)
    
    # Carry over the last value when NA
    stocks_zoo = zoo(current_stock)
    stocks_zoo = na.locf(stocks_zoo)
    
    # Add it to the list and name it
    stocks_list$temp = stocks_zoo
    index = length(stocks_list)
    names(stocks_list)[index] = current_ticker
    
    # Keep only the price column
    stocks_list[[index]] = stocks_list[[index]][,-2]
    stocks_list[[index]] = stocks_list[[index]][,-1]
  }
}
stock_prices = stocks_list
datacrspall<-dplyr::bind_rows(stock_prices) %>% as.data.frame() 
data1<-datacrspall %>% lapply(as.numeric)
data2<-data1 %>% as.data.frame()


# check for NA's:
row_sums1=rowSums(data2)
col_sums1=colSums(data2)
sum(is.na(row_sums1)) 
sum(is.na(col_sums1)) 
colNA <- which(is.na(data2),arr.ind=TRUE)[,2] %>% unique() # Column 1420 displays NA
stock_prices_all<-data2[,-colNA] # no more NA
indx <- apply(stock_prices_all, 2, function(x) any(is.na(x) | is.infinite(x)))
length(indx[indx==TRUE]) # no infinite value

# GET STOCK RETURNS
# Returns start on 1997-01-03
stock_log_returns<-get_log_returns(dim(stock_prices_all)[2],dim(stock_prices_all)[1],stock_prices_all)

# check NaN in the data
which(is.na(stock_log_returns), arr.ind=TRUE) # No NA 

# Check for infinite values
indx1 <- apply(stock_log_returns, 2, function(x) any(is.na(x) | is.infinite(x)))
length(indx1[indx1==TRUE]) # no infinite value


mktcapdata<-monthly_stock_prices_1982_2022[,c(1,2,7)] # keep only PERMNO, date and mktcap columns
mktcapdata$mktcap<-abs(mktcapdata$mktcap)
mktcapdataall <- mktcapdata[mktcapdata$PERMNO %in% tickers[to_keep],]# keep only id that has 480 entries
mktcapdataall <- subset(mktcapdataall, !(PERMNO %in% c(23799, 44230,49322,51530,52936,55029,75652,77973,79864))) # Removing the stocks that had NA, identified in the previous step
mktcapreshaped<-tidyr::spread(mktcapdataall, PERMNO, mktcap)

# n<-unique(which(is.na(mktcapreshaped),arr.ind = TRUE)[,2])
# mktcapreshaped1<-mktcapreshaped[,-c(n)]
# which(is.na(mktcapreshaped1))
# rownames(mktcapreshaped1)<-1:length(mktcapreshaped1$date)
# mktcapreshaped1$date<-as.Date(as.character(mktcapreshaped1$date),format="%Y%m%d")

lastdayvalue<-subset(mktcapreshaped,!duplicated(substr(date, 1, 6), fromLast = FALSE))
lastdayvaluend<-lastdayvalue[,-1]
lvnd<-lastdayvaluend[,-1420] %>% as.data.frame()

orderedindices<-apply(lvnd,1,order,decreasing=TRUE) %>% as.data.frame() 
colnames(orderedindices)<-lastdayvalue$date
mktposition<-data.table::transpose(orderedindices)
colnames(mktposition)<-1:dim(mktposition)[2]

mktposition_best_50<-(mktposition[420,1:50]+1) %>% t() # we add 1 for the date column at the beginning of the return data, 420 corresponds to 01-2017
mktposition_best_100<-(mktposition[420,1:100]+1) %>% t()
mktposition_best_200<-(mktposition[420,1:200]+1) %>% t()
mktposition_best_300<-(mktposition[420,1:300]+1) %>% t()
mktposition_best_400<-(mktposition[420,1:400]+1) %>% t()
mktposition_best_500<-(mktposition[420,1:500]+1) %>% t()
mktposition_best_567<-(mktposition[420,1:567]+1) %>% t()

colnames(stock_prices_all)<-colnames(lvnd)
colnames(stock_log_returns)<-colnames(lvnd)

# We take the first 50; 100; 200; 300; 400; 500; 567(all) stocks at t = 01-2017 having the largest market cap and use this as a stock universe for the ptf optimization
monthly_stock_prices_1982_2022$date %>% unique() %>% length()
dates_column %>% unique() %>% length()
stock_log_returns1<-stock_log_returns
stock_log_returns1$date<-mktcapreshaped$date[-1]
stock_log_returns1$date<-as.Date(as.character(stock_log_returns1$date),format="%Y%m%d")
stock_log_returns2<-stock_log_returns1 %>% dplyr::select(date,everything()) %>% as.data.frame()

# DF of monthly stock returns for the to 50; 250; 500 and 1000 market cap (we sum log returns across time)
stock_log_returns_monthly_50<-stock_log_returns2[,c(1,mktposition_best_50)]
stock_log_returns_monthly_100<-stock_log_returns2[,c(1,mktposition_best_100)]
stock_log_returns_monthly_200<-stock_log_returns2[,c(1,mktposition_best_200)]
stock_log_returns_monthly_300<-stock_log_returns2[,c(1,mktposition_best_300)]
stock_log_returns_monthly_400<-stock_log_returns2[,c(1,mktposition_best_400)]
stock_log_returns_monthly_500<-stock_log_returns2[,c(1,mktposition_best_500)]
stock_log_returns_monthly_567<-stock_log_returns2[,c(1,mktposition_best_567)]


# save(stock_log_returns_monthly_50,file=file.path(path, "stock_log_returns_monthly_50.rda"))
# save(stock_log_returns_monthly_100,file=file.path(path, "stock_log_returns_monthly_100.rda"))
# save(stock_log_returns_monthly_200,file=file.path(path, "stock_log_returns_monthly_200.rda"))
# save(stock_log_returns_monthly_300,file=file.path(path, "stock_log_returns_monthly_300.rda"))
# save(stock_log_returns_monthly_400,file=file.path(path, "stock_log_returns_monthly_400.rda"))
# save(stock_log_returns_monthly_500,file=file.path(path, "stock_log_returns_monthly_500.rda"))
# save(stock_log_returns_monthly_567,file=file.path(path, "stock_log_returns_monthly_567.rda"))

####### with daily data from 31.01.1997 to 2022.01.2022 #######
# CRSP database of the daily stock prices of the whole database
# database = read.table("prices25y.csv", header = 1, sep = ",", as.is = 1, quote="")
# SPY <- read.table("SPY.csv",header=1,sep=",") 
# database$mktcap<-database$PRC*database$SHROUT
# save(database, file=file.path(path, "database.rda"))
load(file.choose())
load("database.rda")
stock_prices = database[,c(1,2,6)]
# take absolute value of the prices 
stock_prices$PRC<-abs(stock_prices$PRC)


# Tickers and the index of each stock start and end date
tickers = data.matrix(unique(stock_prices[,1]))
col1 = stock_prices[,1]
indexes_start = match(tickers, col1)
indexes_end = indexes_start - 1
indexes_end[1:(length(indexes_end)-1)] = indexes_end[2:length(indexes_end)]
indexes_end[length(indexes_end)] = length(col1)

# We will keep only those having at least x days of data 
days_available<-indexes_end - indexes_start 
most_common_value<-sort(table(days_available),decreasing = TRUE)[1] %>% names() %>% as.numeric() 
# Shows that most of the stocks (1446) contains 6292 datapoints (represents 25y of daily stock prices 25*252=6300 trading days)
to_keep = (indexes_end - indexes_start) == most_common_value

# Create a vector of unique dates
sample = stock_prices[1:indexes_end[1],] # (indexes_end - indexes_start) shows also that the first stock contains those 756 dates
dates_vector = unique(as.character(sample[,2])) # unique list of dates
dates_vector = as.Date(dates_vector,format = "%Y%m%d") # better date format
dates_vector = dates_vector[ order(dates_vector) ] # put the dates in order

# Convert also  all the dates of the database to the same format
dates_column = as.character(stock_prices[,2])
dates_column = as.Date(dates_column,format = "%Y%m%d") # better date format
stock_prices[,2] = dates_column

# All stocks time series will have the same set of date: dates_vector
# When a price is missing at one point, we will carry over the last value
stocks_list=c() # Empty data container
temp_col = cbind(1:length(dates_vector)) # Empty data container
standardized_matrix = data.frame(dates_vector,temp_col)
for (i in 1:length(tickers))
{
  if(to_keep[i]==1) # If the current stock is to be kept, add it to the list
  {
    # Data of the current stock
    current_ticker = tickers[i]
    start = indexes_start[i]
    end = indexes_end[i]
    chunk = stock_prices[start:end,2:3]
    
    # Merge to standardized with same set of dates
    current_stock = merge(standardized_matrix,chunk,by=1,all=1)
    
    # Carry over the last value when NA
    stocks_zoo = zoo(current_stock)
    stocks_zoo = na.locf(stocks_zoo)
    
    # Add it to the list and name it
    stocks_list$temp = stocks_zoo
    index = length(stocks_list)
    names(stocks_list)[index] = current_ticker
    
    # Keep only the price column
    stocks_list[[index]] = stocks_list[[index]][,-2]
    stocks_list[[index]] = stocks_list[[index]][,-1]
  }
}
stock_prices = stocks_list
datacrspall<-dplyr::bind_rows(stock_prices) %>% as.data.frame() 
data1<-datacrspall %>% lapply(as.numeric)
data2<-data1 %>% as.data.frame()

# check for NA's:
row_sums1=rowSums(data2)
col_sums1=colSums(data2)
sum(is.na(row_sums1)) 
sum(is.na(col_sums1)) 
colNA <- which(is.na(data2),arr.ind=TRUE)[,2] %>% unique() # Column 1420 displays NA
stock_prices_all<-data2[,-colNA] # no more NA
indx <- apply(stock_prices_all, 2, function(x) any(is.na(x) | is.infinite(x)))
length(indx[indx==TRUE]) # no infinite value

# GET STOCK RETURNS
# Returns start on 1997-01-03
stock_log_returns<-get_log_returns(dim(stock_prices_all)[2],dim(stock_prices_all)[1],stock_prices_all)

# check NaN in the data
which(is.na(stock_log_returns), arr.ind=TRUE) # No NA 

# Check for infinite values
indx1 <- apply(stock_log_returns, 2, function(x) any(is.na(x) | is.infinite(x)))
length(indx1[indx1==TRUE]) # no infinite value

mktcapdata<-database[,c(1,2,8)] # keep only id, date and mktcap columns
mktcapdata$mktcap<-abs(mktcapdata$mktcap)
mktcapdataall<-mktcapdata[mktcapdata$PERMNO %in% tickers[to_keep],] # keep only id that has 6300 entries
mktcapreshaped<-tidyr::spread(mktcapdataall, PERMNO, mktcap)

# n<-unique(which(is.na(mktcapreshaped),arr.ind = TRUE)[,2])
# mktcapreshaped1<-mktcapreshaped[,-c(n)]
# which(is.na(mktcapreshaped1))
# rownames(mktcapreshaped1)<-1:length(mktcapreshaped1$date)
# mktcapreshaped1$date<-as.Date(as.character(mktcapreshaped1$date),format="%Y%m%d")

lastdayvalue<-subset(mktcapreshaped,!duplicated(substr(date, 1, 6), fromLast = FALSE))
lastdayvaluend<-lastdayvalue[,-1]
lvnd<-lastdayvaluend[,-1420] %>% as.data.frame()

orderedindices<-apply(lvnd,1,order,decreasing=TRUE) %>% as.data.frame() 
colnames(orderedindices)<-lastdayvalue$date
mktposition<-data.table::transpose(orderedindices)
colnames(mktposition)<-1:dim(mktposition)[2]

mktposition_best_50<-(mktposition[218,1:50]+1) %>% t() # we add one for the date column at the beginning of the return data 
mktposition_best_250<-(mktposition[218,1:250]+1) %>% t()
mktposition_best_500<-(mktposition[218,1:500]+1) %>% t()
mktposition_best_1000<-(mktposition[218,1:1000]+1) %>% t()

colnames(stock_prices_all)<-colnames(lvnd)
colnames(stock_log_returns)<-colnames(lvnd)

mktposition %>% View()

# We take the 50; 250; 500; 1000 stocks at t=0 having the largest market cap and use this as a stock universe 
# for the ptf optimization

database$date %>% unique() %>% length()
dates_column %>% unique() %>% length()
stock_log_returns1<-stock_log_returns
stock_log_returns1$date<-mktcapreshaped$date[-1]
stock_log_returns1$date<-as.Date(as.character(stock_log_returns1$date),format="%Y%m%d")
stock_log_returns2<-stock_log_returns1 %>% dplyr::select(date,everything()) %>% as.data.frame()
stock_log_returns_monthly<-data.table::setDT(stock_log_returns1)[, lapply(.SD, sum), by = .(date = lubridate::floor_date(date, "month"))] %>% as.data.frame()

# DF of monthly stock returns for the to 50; 250; 500 and 1000 market cap (we sum log returns accross time)
stock_log_returns_monthly_50<-stock_log_returns_monthly[,c(mktposition_best_50)]
stock_log_returns_monthly_250<-stock_log_returns_monthly[,c(mktposition_best_250)]
stock_log_returns_monthly_500<-stock_log_returns_monthly[,c(mktposition_best_500)]
stock_log_returns_monthly_1000<-stock_log_returns_monthly[,c(mktposition_best_1000)]

# DF of daily stock returns for the to 50; 250; 500 and 1000 market cap
stock_log_returns_daily_50<-stock_log_returns2[,c(1,mktposition_best_50)]
stock_log_returns_daily_250<-stock_log_returns2[,c(1,mktposition_best_250)]
stock_log_returns_daily_500<-stock_log_returns2[,c(1,mktposition_best_500)]
stock_log_returns_daily_1000<-stock_log_returns2[,c(1,mktposition_best_1000)]
stock_log_returns_daily<-stock_log_returns2

# Tickers and the index of each stock start and end date
tickers = names(stock_prices)
col1 = stock_prices[,1]
indexes_start = match(tickers, col1)
indexes_end = indexes_start - 1
indexes_end[1:(length(indexes_end)-1)] = indexes_end[2:length(indexes_end)]
indexes_end[length(indexes_end)] = length(col1)

# We will keep only those having at least x days of data 
days_available<-indexes_end - indexes_start 
most_common_value<-sort(table(days_available),decreasing = TRUE)[1] %>% names() %>% as.numeric() 
# Shows that most of the stocks (1446) contains 6292 datapoints (represents 25y of daily stock prices 25*252=6300 trading days)
to_keep = (indexes_end - indexes_start) == most_common_value

# Create a vector of unique dates
sample = stock_prices[1:indexes_end[1],] # (indexes_end - indexes_start) shows also that the first stock contains those 756 dates
dates_vector = unique(as.character(sample[,2])) # unique list of dates
dates_vector = as.Date(dates_vector,format = "%Y%m%d") # better date format
dates_vector = dates_vector[ order(dates_vector) ] # put the dates in order

# Convert also  all the dates of the database to the same format
dates_column = as.character(stock_prices[,2])
dates_column = as.Date(dates_column,format = "%Y%m%d") # better date format
stock_prices[,2] = dates_column

# All stocks time series will have the same set of date: dates_vector
# When a price is missing at one point, we will carry over the last value
stocks_list=c() # Empty data container
temp_col = cbind(1:length(dates_vector)) # Empty data container
standardized_matrix = data.frame(dates_vector,temp_col)
for (i in 1:length(tickers))
{
  if(to_keep[i]==1) # If the current stock is to be kept, add it to the list
  {
    # Data of the current stock
    current_ticker = tickers[i]
    start = indexes_start[i]
    end = indexes_end[i]
    chunk = stock_prices[start:end,2:3]
    
    # Merge to standardized with same set of dates
    current_stock = merge(standardized_matrix,chunk,by=1,all=1)
    
    # Carry over the last value when NA
    stocks_zoo = zoo(current_stock)
    stocks_zoo = na.locf(stocks_zoo)
    
    # Add it to the list and name it
    stocks_list$temp = stocks_zoo
    index = length(stocks_list)
    names(stocks_list)[index] = current_ticker
    
    # Keep only the price column
    stocks_list[[index]] = stocks_list[[index]][,-2]
    stocks_list[[index]] = stocks_list[[index]][,-1]
  }
}
stock_prices = stocks_list
datacrspall<-dplyr::bind_rows(stock_prices) %>% as.data.frame() 
data1<-datacrspall %>% lapply(as.numeric)
data2<-data1 %>% as.data.frame()

# check for NA's:
row_sums1=rowSums(data2)
col_sums1=colSums(data2)
sum(is.na(row_sums1)) 
sum(is.na(col_sums1)) 
colNA <- which(is.na(data2),arr.ind=TRUE)[,2] %>% unique() # Column 1420 displays NA
stock_prices_all<-data2[,-colNA] # no more NA
indx <- apply(stock_prices_all, 2, function(x) any(is.na(x) | is.infinite(x)))
length(indx[indx==TRUE]) # no infinite value

# GET STOCK RETURNS
# Returns start on 1997-01-03
stock_log_returns<-get_log_returns(dim(stock_prices_all)[2],dim(stock_prices_all)[1],stock_prices_all)

# check NaN in the data
which(is.na(stock_log_returns), arr.ind=TRUE) # No NA 

# Check for infinite values
indx1 <- apply(stock_log_returns, 2, function(x) any(is.na(x) | is.infinite(x)))
length(indx1[indx1==TRUE]) # no infinite value

mktcapdata<-database[,c(1,2,8)] # keep only id, date and mktcap columns
mktcapdata$mktcap<-abs(mktcapdata$mktcap)
mktcapdataall<-mktcapdata[mktcapdata$PERMNO %in% tickers[to_keep],] # keep only id that has 6300 entries
mktcapreshaped<-tidyr::spread(mktcapdataall, PERMNO, mktcap)

# n<-unique(which(is.na(mktcapreshaped),arr.ind = TRUE)[,2])
# mktcapreshaped1<-mktcapreshaped[,-c(n)]
# which(is.na(mktcapreshaped1))
# rownames(mktcapreshaped1)<-1:length(mktcapreshaped1$date)
# mktcapreshaped1$date<-as.Date(as.character(mktcapreshaped1$date),format="%Y%m%d")

lastdayvalue<-subset(mktcapreshaped,!duplicated(substr(date, 1, 6), fromLast = FALSE))
lastdayvaluend<-lastdayvalue[,-1]
lvnd<-lastdayvaluend[,-1420] %>% as.data.frame()

orderedindices<-apply(lvnd,1,order,decreasing=TRUE) %>% as.data.frame() 
colnames(orderedindices)<-lastdayvalue$date
mktposition<-data.table::transpose(orderedindices)
colnames(mktposition)<-1:dim(mktposition)[2]

mktposition_best_50<-(mktposition[218,1:50]+1) %>% t() # we add one for the date column at the beginning of the return data 
mktposition_best_250<-(mktposition[218,1:250]+1) %>% t()
mktposition_best_500<-(mktposition[218,1:500]+1) %>% t()
mktposition_best_1000<-(mktposition[218,1:1000]+1) %>% t()

colnames(stock_prices_all)<-colnames(lvnd)
colnames(stock_log_returns)<-colnames(lvnd)

mktposition %>% View()

# We take the 50; 250; 500; 1000 stocks at t=0 having the largest market cap and use this as a stock universe 
# for the ptf optimization

database$date %>% unique() %>% length()
dates_column %>% unique() %>% length()
stock_log_returns1<-stock_log_returns
stock_log_returns1$date<-mktcapreshaped$date[-1]
stock_log_returns1$date<-as.Date(as.character(stock_log_returns1$date),format="%Y%m%d")
stock_log_returns2<-stock_log_returns1 %>% dplyr::select(date,everything()) %>% as.data.frame()
stock_log_returns_monthly<-data.table::setDT(stock_log_returns1)[, lapply(.SD, sum), by = .(date = lubridate::floor_date(date, "month"))] %>% as.data.frame()

# DF of monthly stock returns for the to 50; 250; 500 and 1000 market cap (we sum log returns accross time)
stock_log_returns_monthly_50<-stock_log_returns_monthly[,c(mktposition_best_50)]
stock_log_returns_monthly_250<-stock_log_returns_monthly[,c(mktposition_best_250)]
stock_log_returns_monthly_500<-stock_log_returns_monthly[,c(mktposition_best_500)]
stock_log_returns_monthly_1000<-stock_log_returns_monthly[,c(mktposition_best_1000)]

# DF of daily stock returns for the to 50; 250; 500 and 1000 market cap
stock_log_returns_daily_50<-stock_log_returns2[,c(mktposition_best_50)]
stock_log_returns_daily_250<-stock_log_returns2[,c(mktposition_best_250)]
stock_log_returns_daily_500<-stock_log_returns2[,c(mktposition_best_500)]
stock_log_returns_daily_1000<-stock_log_returns2[,c(mktposition_best_1000)]
stock_log_returns_daily<-stock_log_returns2

# save the stock returns, stock prices and market cap positions ONLY ONCE
# save(stock_log_returns_daily, file=file.path(path, "stock_log_returns_daily.rda")) 
# save(stock_prices_all, file=file.path(path, "stock_prices_all.rda"))
# save(mktposition, file=file.path(path, "mktposition.rda")) 

# save(stock_log_returns_monthly_50,file=file.path(path, "stock_log_returns_monthly_50.rda"))
# save(stock_log_returns_monthly_250,file=file.path(path, "stock_log_returns_monthly_250.rda"))
# save(stock_log_returns_monthly_500,file=file.path(path, "stock_log_returns_monthly_500.rda"))
# save(stock_log_returns_monthly_1000,file=file.path(path, "stock_log_returns_monthly_1000.rda"))

# save(stock_log_returns_daily_50,file=file.path(path, "stock_log_returns_daily_50.rda"))
# save(stock_log_returns_daily_250,file=file.path(path, "stock_log_returns_daily_250.rda"))
# save(stock_log_returns_daily_500,file=file.path(path, "stock_log_returns_daily_500.rda"))
# save(stock_log_returns_daily_1000,file=file.path(path, "stock_log_returns_daily_1000.rda"))

















































