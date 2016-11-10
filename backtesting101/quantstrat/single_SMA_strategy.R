# [TEACHING]
library(quantstrat)

##############################
# Name your strategy
##############################
# ls(.strategy) # [TEACHING]

strategy.st <- 'SingleMA'
strategy(strategy.st, store=TRUE)# whether store into .strategy

# ls(.strategy) # [TEACHING]
# (s <- getStrategy(strategy.st)) # [TEACHING]
# str(s)
# names(s)
# #[PROBLEM] What is ".strategy"


##############################
# Define Indicator
##############################
# #[PROBLEM] Who is "mktdata" in add.indicator
add.indicator(strategy.st, 
              name = "SMA", 
              arguments = list(x = quote(Cl(mktdata)), n=10), 
              label="SMA10")
# (s <- getStrategy(strategy.st)) # [TEACHING]
# s$indicators
# str(s$indicators)

##############################
# Define signals
##############################
#   1. Entry Signal: when monthly price crosses over the 10-month SMA
add.signal(strategy.st,
           name="sigCrossover",
           arguments = list(columns=c("Close","SMA10"), 
                            relationship=">"), 
           label="Cl.gt.SMA")

# (s <- getStrategy(strategy.st)) # [TEACHING]
# s$signals
# str(s$signals)

#   2. Exit Signal: when the monthly price crosses under the 10-month SMA
add.signal(strategy.st,
           name="sigCrossover",
           arguments = list(columns=c("Close","SMA10"),
                            relationship="<"), 
           label="Cl.lt.SMA")

# (s <- getStrategy(strategy.st)) # [TEACHING]
# str(s$signals)


##############################
# Define rules
##############################
# FUNCTION CALL: add.rule => ruleSignal => addOrder(portfolio, symbol, timestamp, qty, price, ...)

# 1. Buy Rule: buy when the price crosses above the SMA
add.rule(strategy.st, 
         name='ruleSignal', 
         arguments = list(sigcol="Cl.gt.SMA", 
                          sigval=1, 
                          orderqty=500, 
                          ordertype='market', 
                          orderside='long', 
                          pricemethod='market'), 
         type='enter', 
         path.dep=TRUE)

# 2. Sell Rule: sell when the price crosses below the SMA
add.rule(strategy.st, 
         name='ruleSignal', 
         arguments = list(sigcol="Cl.lt.SMA", 
                          sigval=1, 
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          pricemethod='market'), 
         type='exit', 
         path.dep=TRUE)


# (s <- getStrategy(strategy.st)) # [TEACHING]
# str(s$rules)
# names(s$rules)
# s$rules$enter
# s$rules$exit
# s$rules$order

############################
# What are we doing ?
############################
# # Actually all we do so far is writing a "strategy" object. that's all!
# s <- getStrategy(strategy.st) # [TEACHING]
# str(s)
# names(s)
# 
# str(s$indicators)
# names(s$indicators)
# 
# str(s$signals)
# str(s$signals[[1]])
# names(s$signals[[1]])
# 
# str(s$rules)
# names(s$rules)
# 
# str(s$rules$enter)


print(paste0('[IMPORTANT INFO] The strategy using now is: ', strategy.st))

