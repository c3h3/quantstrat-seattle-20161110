library(blotter)
library(magrittr)

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
portfolio.st <- account.st <- 'singleMA_blotter'
symbol <- 'Xt'

##############################
# Data Retrieving
##############################
Xt <- getSymbols('2330.TW', src='yahoo', index.class=c("POSIXt","POSIXct"), from=startDate, to=endDate, adjust=TRUE, auto.assign = FALSE)
symbols <- c('Xt')

##############################
# INITIALIZATION
##############################
rm(list=ls(.blotter), envir = .blotter)
# .blotter %>% rm(list=ls(.), envir = .)
initPortf(name=portfolio.st, symbols = symbols, currency = 'TWD')
initAcct(name=account.st, portfolios=portfolio.st, initDate = startDate, initEq = initEq, currency = 'TWD')

##############################
# Generate Transaction
##############################
# define indicator
Xt$SMA20 <- SMA(Cl(Xt), 20)

# define signal
signal <- ifelse(test = Cl(Xt) < Xt$SMA20 , yes = 1, no = 0) %>% lag.xts()
signal[is.na(signal)] <- 0

signal.diff = diff.xts(signal)
signal.diff[is.na(signal.diff)] <- 0

# bar-by-bar processing
for(i in 21:length(signal))
{
  currentDate <-  time(signal)[i]
  equity <-  getEndEq(Account = account.st, Date = currentDate)
  
  # must using zoo::coredata, or there's bug
  closePrice<-coredata(Cl(Xt[currentDate]))# zoo::coredata=> only retrieve data without time index
  unitSize <- trunc((equity/closePrice))# no fractional trade
  position <-  getPosQty(Portfolio = portfolio.st, Symbol='Xt', Date=currentDate)
  
  if(position==0)
  {
    if(signal.diff[i] > 0)
    {
      addTxn(portfolio.st, Symbol='Xt',  TxnDate=currentDate, TxnPrice=closePrice, TxnQty = unitSize ,verbose=T)
    } 
    
  }else
  {
    if (signal.diff[i] < 0 ){
      # [TODO] need to do something
      addTxn(portfolio.st, Symbol='Xt',  TxnDate=currentDate, TxnPrice=closePrice, TxnQty = -position ,verbose=T)
      
    }
  }
  
  updatePortf(Portfolio = portfolio.st, Dates = currentDate)
  updateAcct(name = account.st, Dates = currentDate)
  updateEndEq(Account = account.st, Dates = currentDate)
}

acct <- getAccount(Account = account.st)
port <- getPortfolio(Portfolio = portfolio.st)
equity <- acct$summary$End.Eq
plot(equity)


# Chart individual asset performance
chart.Posn(Portfolio=portfolio.st,Symbol=symbols)

ret <- CalculateReturns(equity, method = 'discrete')
ret[is.na(ret) | ret==Inf|ret==-Inf] <- 0
# Return distribution analysis
# chart.Boxplot(ret, main = "TW stocks Returns", colorset= rich10equal)

# Annulized risk and return
# table.AnnualizedReturns(ret)


