library(randomForest)
library(xts)

### To do:
### 1. Break this down into data alignment, prediction, and finally performance calculation
### 2. Use quantmod/xts for time-series functionality
### 3. Investigate how classification is used here and try using svms or other classification algorithms
### 4. Try this code with high frequency data

data <- read.csv('gold01-09.csv', header=TRUE, stringsAsFactors=FALSE)
data <- data[-c(1)]

## Lets convert the dataset to xts format
##data$Date <- as.Date(data$Date, "%d-%B-%y")
##data <- xts(data, order.by=data$Date)

returns <- data[, 9]
target <- returns
target[target >= 0] <- 1
target[target < 0] <- -1

factormodel.tree = function(data, target=NULL, returns=NULL, verbose=TRUE, btsamples=10,
  skip=1, horizon=1, trainperiods=100, leverage='kelly', nfeatures=10, treesInBag=200) {

  ## Predictive factor model based on randomforests
  ## data must be from most recent in row 1 to oldest in the last row

  ## target as factors tell the learner to do classification rather than regression
  target = as.factor(target)

  ## make sure there's enough data for the requested number of backtest periods
  if (min(nrow(data), nrow(target)) < btsamples) {
    stop('too little data')
  }

  ## generate calendars to backtest over
  idx = rev(data.frame(target = seq(1, btsamples*skip, skip)))
  idx = data.frame(idx, data = idx$target+horizon)

  pred1pd = function(t) {
    ## Predict one single period, to be used by the backtester in sapply
    ## train the random forest
    savedEnsemble = trainEnsemble(data[(t + horizon+1):(t + trainperiods+horizon+1), ], target[(t + 1):(t + trainperiods+1)],
      verbose=TRUE, treesInBag, nfeatures)
    pred = predict(savedEnsemble, data[t + horizon, ], type='prob')
    print(pred)
  }

  ## backtest by looping over the calendar from above
  preds = sapply(idx$target, pred1pd)

  ## print output for human inspection
  print(preds)
  print(max.col(preds))
  preds = data.frame(t(rbind(mle = max.col(t(preds)), preds)))
  print(preds)
  print(paste('Accuracy:', sum((preds$mle*2-3)==target[idx$target])/length(preds$mle)))

  ## Set default leverage to Kelly fraction, assuming 5 % edge
  if(leverage == 'kelly') {
    leverage = mean(returns+.025*abs(returns))/var(returns)
  }

  equity = cumprod((returns[idx$target] * (preds$mle*2-3) * leverage)+1)
  dev.new()
  plot(1:length(equity), equity, main=paste('Equity Curve: 1$ to $', equity[length(equity)]),
       'l', xlab='Trading periods (days/weeks/etc)', ylab='Equity at day x starting with 1$')
}


trainEnsemble = function(data, target, verbose=FALSE, treesInBag, nfeatures) {
  ## Train an ensemble for feature selection
  treeBagFS = randomForest(data, target, ntree=treesInBag, importance=TRUE, keep.forest=FALSE)

  ## order the features by importance so the best can be skimmed off
  features = sort(importance(treeBagFS, type=1), index.return=TRUE, decreasing=TRUE)$ix[1:min(nfeatures,nrow(importance(treeBagFS, type=1)))]

  ## Train full ensemble using predicted best subset of the features
  treeBag = randomForest(data[ ,features], target, ntree=treesInBag, importance=TRUE, keep.forest=TRUE)

  if (verbose) {
    dev.new()
    varImpPlot(treeBagFS, pch=19, main='Variable Importance before FS') #, col=impcol)
    varImpPlot(treeBag, pch=19, main='Variable Importance')

    ## Plot error rate
    dev.new()
    par(mfrow = c(1,2))
    plot(treeBagFS, main='Error Rate during FS', log='y')
    plot(treeBag, main='Error Rate', log='y')
  }
  return(treeBag)
}


## XXX untested, possibly broken
windowData = function(data, dataperiods) {
  ## increase the data window length from 1 to 'dataperiods'
  ## i.e. 1, 2, 3, 4, 5, 6 -> 123, 234, 345, 456
   dataBak = data
   for (lag in 1:dataperiods) {
      ## name the data being appended "tX.{old names}"
      names(dataBak) = sapply(names(dataBak), function(str) paste('t', lag, '.', str, sep=''))
      if(lag == 1) {data = dataBak}
      else {data = data.frame(data[1:(end-lag+1), ], dataBak[2:(end-lag+2), ])}
   }
   return(data)
}


Main <- function() {
  factormodel.tree(data, target=target, returns=returns, verbose=TRUE, btsamples=10, skip=1, horizon=1, trainperiods=100, leverage='kelly', nfeatures=10, treesInBag=200)
  }
