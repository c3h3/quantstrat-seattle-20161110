library(quantmod)
library(PerformanceAnalytics)
library(magrittr)# HOT KEY %>% :Ctrl + Shift + M
###########################################

##############################
# Settings
##############################
Sys.setenv(TZ="UTC")
startDate <- '2014-01-01'
endDate <- '2014-12-31'

##############################
# Data Retrieving
##############################
# index.class 必須設成 POSIX 標準時間格式
# adjust = True, 調整除權息
TW2330 <- getSymbols('2330.TW', from=startDate, to=endDate, index.class='POSIXct', adjust=TRUE, auto.assign = FALSE)

# ?PerformanceAnalytics::CalculateReturns
ret <- CalculateReturns(Ad(TW2330), method = 'discrete')#
ret %>% head()
ret[1] <- 0
ret %>% head()

##############################
# Define Signal
##############################
signal <- rep(1, nrow(TW2330))

##############################
# Calculate equity curve
##############################
ret.strat.bh <- ret * signal
equity.strat <- cumprod(1+ret.strat.bh)
plot(equity.strat)

##############################
# Reporting
##############################
chart.CumReturns(ret.strat.bh)

# performance
charts.PerformanceSummary(ret.strat.bh)
chart.RollingPerformance(ret.strat.bh)
# chart.RelativePerformance(Ra = ret.strat.bh, Rb = ret.strat.bh)

# Risk
chart.Drawdown(ret.strat.bh)
chart.BarVaR(ret.strat.bh)
chart.RiskReturnScatter(ret.strat.bh)

# distribution
chart.ACFplus(ret.strat.bh)
chart.Histogram(ret.strat.bh)

# risk metrics
SharpeRatio(ret.strat.bh)
VaR(ret.strat.bh)

# Why don't we calculate geometric average? Just do it!





##############################################
# ret2 <- CalculateReturns(Cl(TW2330), method = 'log')
# ret2[1] <- 0
# ret.strat.bh.2 <- ret2 * signal
# equity.strat.2 <- 1+cumsum(ret.strat.bh.2)
# plot(equity.strat.2)
# 
# 
# rets <- cbind(equity.strat, equity.strat.2)
# names(rets) <- c('discrete', 'log')
# plot(rets)