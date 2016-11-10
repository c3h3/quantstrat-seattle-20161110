# remember to set working directory
# setwd("~/JS-BeATradeR201608/week2/")

library(quantmod)
library(quantstrat)

##############################
# Define Financial Instrument
##############################
# ls(FinancialInstrument:::.instrument)
rm(list=ls(FinancialInstrument:::.instrument), envir = FinancialInstrument:::.instrument)

currency("TWD")
stock('Xt', currency="TWD",multiplier=1)

##############################
# Settings
##############################
Sys.setenv(TZ="UTC")
startDate <- '2014-01-01'
endDate <- '2014-12-31'
initEq <- 10^6
portfolio.st <- account.st <- 'SingleMA'

##############################
# Data Retrieving
##############################
Xt <- getSymbols('2330.TW', src='yahoo', index.class=c("POSIXt","POSIXct"), from=startDate, to=endDate, adjust=TRUE, auto.assign = FALSE)
symbols <- c('Xt')


##############################
# INITIALIZATION
##############################
rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(name=portfolio.st, symbols = symbols, currency = 'TWD')
initAcct(name=account.st, portfolios=portfolio.st, initDate = startDate, initEq = initEq, currency = 'TWD')
initOrders(portfolio = portfolio.st)

##############################
# Define Strategy
##############################
source('double_SMA_strategy.R')

##############################
# BACKTESTING
##############################
applyStrategy(strategy = strategy.st, portfolios = portfolio.st)

##############################
# UPDATING
##############################
updatePortf(Portfolio = portfolio.st)
updateAcct(name = account.st)
updateEndEq(Account = account.st)


##############################
# Account object
##############################
a <- getAccount(account.st)
View(a$summary)# cash

p <- getPortfolio(portfolio.st)
View(p$summary)# equity


##############################
# Strategy object
##############################
ls(.strategy)
s <- getStrategy(strategy.st)
names(s)

ob <- getOrderBook(strategy.st)
ob

##############################
# REPORTING
##############################
# Equity Curve
equity <- a$summary$End.Eq
plot(equity,main="Consolidated TW stocks Equity Curve")

# Chart individual asset performance
chart.Posn(Portfolio=portfolio.st,Symbol=symbols)

# ret <- CalculateReturns(equity, method = 'discrete')
# ret[is.na(ret) | ret==Inf|ret==-Inf] <- 0
# # Return distribution analysis
# chart.Boxplot(ret, main = "TW stocks Returns", colorset= rich10equal)
# 
# # Annulized risk and return
# table.AnnualizedReturns(ret)


