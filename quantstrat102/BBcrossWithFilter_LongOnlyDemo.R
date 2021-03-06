library(quantstrat)
library(e1071)
##############################################
# Live Demo
##############################################
# strategy name: ML_strategyWWW

strategy.st <- 'ML_strategy'
strategy(strategy.st, store=TRUE)

##############################
# Indicator
##############################


myTTR <- function(data)
{
  myBBands = data %>% HLC %>% BBands
  volBB = myBBands$up - myBBands$dn
  colnames(volBB) <- "volBB"
  diffVolBB = diff.xts(volBB)
  colnames(diffVolBB) <- "diffVolBB"
  filterCol = diffVolBB > 0
  colnames(filterCol) <- "filterCol"
  
  result <- cbind(myBBands,volBB,diffVolBB,filterCol)
  return(result)
}


add.indicator(strategy.st, 
              name = "myTTR", 
              arguments = list(data=quote(mktdata)),
              label = 'myIndicator')

##############################
# Indicator
##############################

sigCrossover.withFilter <- function (label, 
                                     data = mktdata, 
                                     columns, relationship = c("gt", "lt", "eq", "gte", "lte"), 
                                     offset1 = 0, offset2 = 0,filterCol=F) 
{
  ret_sig = FALSE
  lng <- length(columns)
  for (i in 1:(lng - 1)) {
    ret_sig = suppressWarnings(ret_sig | diff(sigComparison(label = label, 
                                                            data = data, columns = columns[c(i, lng)], relationship = relationship, 
                                                            offset1 = offset1, offset2 = offset2)) == 1)
  }
  is.na(ret_sig) <- which(!ret_sig)
  
  if (filterCol){
    ret_sig = (data$filterCol == 1) & ret_sig
  }
  
  colnames(ret_sig) <- label
  return(ret_sig)
}

# undebug(sigCrossover.withFilter)

## Long Entry
add.signal(strategy.st,name="sigCrossover.withFilter",
           arguments=list(columns=c("Close","dn"),
                          relationship="gt",filterCol=T),# eturn TRUE only for the first observation to cross the threshold in a run
           label="crossDn.gt.withFiltter")


add.signal(strategy.st,name="sigCrossover.withFilter",
           arguments=list(columns=c("Close","mavg"),
                          relationship="gt",filterCol=T),# eturn TRUE only for the first observation to cross the threshold in a run
           label="crossMA.gt.withFiltter")

## Long Exit: Limit
add.signal(strategy.st,name="sigCrossover",
           arguments=list(columns=c("Close","up"),
                          relationship="gt"),# eturn TRUE only for the first observation to cross the threshold in a run
           label="crossUp.gt")

## Long Exit: Stop
add.signal(strategy.st,name="sigCrossover",
           arguments=list(columns=c("Close","dn"),
                          relationship="lt"),# eturn TRUE only for the first observation to cross the threshold in a run
           label="crossDn.lt")




## Short Entry

add.signal(strategy.st,name="sigCrossover.withFilter",
           arguments=list(columns=c("Close","up"),
                          relationship="lt",filterCol=T),# eturn TRUE only for the first observation to cross the threshold in a run
           label="crossUp.lt.withFiltter")


add.signal(strategy.st,name="sigCrossover.withFilter",
           arguments=list(columns=c("Close","mavg"),
                          relationship="lt",filterCol=T),# eturn TRUE only for the first observation to cross the threshold in a run
           label="crossMA.lt.withFiltter")


## Short Exit: Limit
# add.signal(strategy.st,name="sigCrossover",
#            arguments=list(columns=c("Close","dn"),
#                           relationship="lt"),# eturn TRUE only for the first observation to cross the threshold in a run
#            label="crossDn.lt")


## Short Exit: Stop
# add.signal(strategy.st,name="sigCrossover",
#            arguments=list(columns=c("Close","up"),
#                           relationship="gt"),# eturn TRUE only for the first observation to cross the threshold in a run
#            label="crossUp.gt")





##############################
# Rule
##############################

add.rule(strategy.st,name='ruleSignal',
         arguments = list(sigcol="crossDn.gt.withFiltter", 
                          sigval=TRUE,
                          replace=FALSE,
                          orderside='long',
                          ordertype='market',
                          orderqty=100,
                          orderset='ocolong'
         ),
         type='enter',
         label='LongEntry'
)

add.rule(strategy.st,name='ruleSignal',
         arguments = list(sigcol="crossUp.gt", 
                          sigval=TRUE,
                          replace=TRUE,
                          orderside='long',
                          ordertype='market',
                          orderqty='all',
                          orderset='ocolong'
         ),
         type='exit',
         label='LongExitLimit'
)

add.rule(strategy.st,name='ruleSignal',
         arguments = list(sigcol="crossDn.lt", 
                          sigval=TRUE,
                          replace=TRUE,
                          orderside='long',
                          ordertype='market',
                          orderqty='all',
                          orderset='ocolong'
         ),
         type='exit',
         label='LongExitStop'
)

##############################################

add.rule(strategy.st,name='ruleSignal',
         arguments = list(sigcol="crossUp.lt.withFiltter", 
                          sigval=TRUE,
                          replace=FALSE,
                          orderside='short',
                          ordertype='market',
                          orderqty=100,
                          orderset='ocoshort'
         ),
         type='enter',
         label='ShortEntry'
)

add.rule(strategy.st,name='ruleSignal',
         arguments = list(sigcol="crossUp.gt", 
                          sigval=TRUE,
                          replace=TRUE,
                          orderside='short',
                          ordertype='market',
                          orderqty='all',
                          orderset='ocoshort'
         ),
         type='exit',
         label='ShortExitStop'
)

add.rule(strategy.st,name='ruleSignal',
         arguments = list(sigcol="crossDn.lt", 
                          sigval=TRUE,
                          replace=TRUE,
                          orderside='short',
                          ordertype='market',
                          orderqty='all',
                          orderset='ocoshort'
         ),
         type='exit',
         label='ShortExitLimit'
)

print(paste0('[IMPORTANT INFO] The strategy using now is: ', strategy.st))






