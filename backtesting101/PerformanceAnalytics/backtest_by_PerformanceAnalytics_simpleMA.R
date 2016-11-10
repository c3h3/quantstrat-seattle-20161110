library(quantmod)
library(PerformanceAnalytics)
library(magrittr)# HOT KEY %>% :Ctrl + Shift + M
###########################################
rm(list=ls())

##############################
# Settings
##############################
Sys.setenv(TZ="UTC")
startDate <- '2014-01-01'
endDate <- '2014-12-31'

##############################
# Data Retrieving
##############################
TW2330 <- getSymbols('2330.TW', from=startDate, to=endDate, index.class='POSIXct', adjust=TRUE, auto.assign = FALSE)

ret <- CalculateReturns(Cl(TW2330), method = 'discrete')
ret %>% head()
ret[1] <- 0
ret %>% head()

##############################
# define indicator
##############################
TW2330$SMA10 <- TTR::SMA(Cl(TW2330), 10)
head(TW2330, n=10)
TW2330 %>% head(., n=10)
TW2330 %>% head(n=10)

##############################
# define signal
##############################
signal <-ifelse(test = Cl(TW2330)>TW2330$SMA10 , yes = 1, no = 0) 
signal %>% head(30)
signal[is.na(signal)] <- 0

##############################
# Calculate equity curve
##############################
ret.strat.ma <- signal * ret
charts.PerformanceSummary(ret.strat.ma)


# Does this calculation make sense? Do you find anything strange?
