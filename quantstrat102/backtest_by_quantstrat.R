# remember to set working directory
library(quantmod)
library(quantstrat)
library(magrittr)


##############################
# Settings
##############################
Sys.setenv(TZ="UTC")
startDate <- '2014-01-01'
endDate <- '2016-12-31'
initEq <- 10^6
tradeSize <- initEq/10
portfolio.st <- account.st <- 'ML_strategy'# 
currency <- 'TWD'
symbol <-  "TW0050" 
symbol_yahoo <- '0050.TW'


##############################
# Define Financial Instrument
##############################
# ls(FinancialInstrument:::.instrument)
rm(list=ls(FinancialInstrument:::.instrument), envir = FinancialInstrument:::.instrument)

currency(currency)
stock(symbol, currency=currency)


##############################
# Data Retrieving
##############################
assign(symbol, getSymbols(symbol_yahoo,
                          src = 'yahoo', 
                          from=startDate, 
                          to=endDate, 
                          auto.assign = FALSE))
##############################
# INITIALIZATION
##############################
rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(name=portfolio.st, symbols = symbol, currency = currency)
initAcct(name=account.st, portfolios=portfolio.st, initDate = startDate, initEq = initEq, currency = 'TWD')
initOrders(portfolio = portfolio.st)

##############################
# Define Strategy
##############################

#[TODO]
source('BBcrossWithFilter_LongOnlyDemo.R')
stopifnot(strategy.st==portfolio.st)# remember to change the portfolio name
##############################
# BACKTESTING
##############################
applyStrategy(strategy.st , portfolios=portfolio.st)


##############################
# UPDATING
##############################
updatePortf(Portfolio = portfolio.st)
updateAcct(name = account.st)
updateEndEq(Account = account.st)

##############################
# REPORTING
##############################
a <- getAccount(account.st)
equity <-a$summary$End.Eq

returns.ns <- Return.calculate(equity,"discrete")
returns.ns[is.na(returns.ns)|returns.ns==Inf|returns.ns==-Inf] <- 0

# performance
table.AnnualizedReturns(returns.ns)
charts.PerformanceSummary(returns.ns,main=paste(symbol, strategy.st, "Performance", sep = ' '))
View(t(tradeStats(portfolio.st)))

# order book
ob <- getOrderBook(portfolio.st)
View(ob[[strategy.st]][[symbol]])

chart.Posn(portfolio.st,Symbol = symbol,TA = 'add_BBands()')
