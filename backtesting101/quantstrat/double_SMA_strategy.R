library(quantstrat)

strategy.st <- 'DoubleMA'
strategy(strategy.st, store=TRUE)

##############################
# Indicator
##############################
add.indicator(strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA_fast")
add.indicator(strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n=20), label="SMA_slow")

##############################
# Signal
##############################
add.signal(strategy.st,name="sigCrossover",
           arguments = list(columns=c("SMA_fast","SMA_slow"), relationship="gte"), 
           label="fast.gt.slow.SMA")

add.signal(strategy.st,
           name="sigCrossover",
           arguments = list(columns=c("SMA_fast","SMA_slow"),relationship="lt"), 
           label="fast.lt.slow.SMA")

##############################
# Rule
##############################
# 1. Buy Rule:  when the fast SMA crosses above the slow SMA
add.rule(strategy.st, 
         name='ruleSignal', 
         arguments = list(sigcol="fast.gt.slow.SMA", 
                          sigval=TRUE, 
                          orderqty=500, 
                          ordertype='market', 
                          orderside='long', 
                          pricemethod='market'), 
         type='enter', 
         path.dep=TRUE)

# 2. Sell Rule: when the fast SMA crosses below the slow SMA
add.rule(strategy.st, 
         name='ruleSignal', 
         arguments = list(sigcol="fast.lt.slow.SMA", 
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          pricemethod='market'), 
         type='exit', 
         path.dep=TRUE)


# ls(.strategy) # [TEACHING]
# (s <- getStrategy(strategy.st)) # [TEACHING]
# str(s)
# names(s)
print(paste0('[IMPORTANT INFO] The strategy using now is: ', strategy.st))
