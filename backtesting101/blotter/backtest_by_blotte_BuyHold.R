library(quantmod)
ls(.blotter)
library(blotter)
ls(.blotter)


##############################
# Define Financial Instrument
##############################
# ls(FinancialInstrument:::.instrument)
rm(list=ls(FinancialInstrument:::.instrument), envir = FinancialInstrument:::.instrument)
ls(envir=FinancialInstrument:::.instrument)

currency("TWD")
stock('Xt', currency="TWD",multiplier=1)

##############################
# Settings
##############################
Sys.setenv(TZ="UTC")
startDate <- '2014-01-01'
endDate <- '2014-12-31'
initEq <- 10^6
portfolio.st <- account.st <- 'buyHold_blotter'
# portfolio.st <- account.st <- 'buyHold_blotter_test123'

##############################
# Data Retrieving
##############################
Xt <- getSymbols('2330.TW', src='yahoo', index.class=c("POSIXt","POSIXct"), from=startDate, to=endDate, adjust=TRUE, auto.assign = FALSE)
symbols <- c('Xt')

##############################
# INITIALIZATION
##############################
initPortf(name=portfolio.st, symbols = symbols, currency = 'TWD')
initAcct(name=account.st, portfolios=portfolio.st, initDate = startDate, initEq = initEq, currency = 'TWD')
# ls(.blotter)

##############################
# Generate Transaction
##############################
# place an entry order
CurrentDate <- first(time(Xt))
ClosePrice <- coredata(Cl(Xt[1]))
UnitSize <-  trunc(initEq/ClosePrice)
addTxn(Portfolio = portfolio.st, 
       Symbol='Xt', 
       TxnDate=CurrentDate, 
       TxnPrice=ClosePrice,
       TxnQty = UnitSize)

# place an exit order
LastDate <- last(time(Xt))
LastPrice <- coredata(Cl(Xt[LastDate,]))
addTxn(Portfolio = portfolio.st, 
       Symbol='Xt', 
       TxnDate=LastDate, 
       TxnPrice=LastPrice,
       TxnQty = -UnitSize)

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
# View(a$summary)# cash
a$summary




chart.Posn(Portfolio=portfolio.st,Symbol=symbols)


# # How can I know there's "summary" and "End.Eq" attributes?
# str(a)
# names(a)
# View(a$summary)
# names(a$portfolios$buyHold_blotter)# cash account 


p <- getPortfolio(portfolio.st)
p$summary # equity
str(p)
names(p)

Tx <- getTxns(portfolio.st, symbols, index(Xt))
View(Tx)

