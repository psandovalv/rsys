library(randomForest)

### To do:
### 1. Break this down into data alignment, prediction, and finally performance calculation
### 2. Use quantmod/xts for time-series functionality
### 3. Investigate how classification is used here and try using svms or other classification algorithms
### 4. Try this code with high frequency data

data = read.csv('gold01-09.csv', header=TRUE, stringsAsFactors=FALSE)
data = data[-c(1)]

returns = data[[9]]
targets = returns
targets[targets >= 0] = 1
targets[targets < 0] = -1

factormodel.tree = function(data, targets=NULL, returns=NULL, verbose=TRUE, btsamples=10,
  skip=1, horizon=1, dataperiods=1, trainperiods=100, leverage='kelly',
  keepNFeatures=10, treesInBag=200) {

  ## Predictive factor model based on randomforests
  ## data must be from most recent in row 1 to oldest in the last row

  ## data windowing via the dataperiods parameter is likely broken (untested)
  ## needs to cut down lengths of returns and targets accordingly
  data = windowData(data, dataperiods)

  ## targets as factors tell the learner to do classification rather than regression
  targets = as.factor(targets)

  ## make sure there's enough data for the requested number of backtest periods
  if (min(nrow(data), nrow(targets)) < btsamples) {
    stop('too little data')
  }

  ## generate calendars to backtest over
  idx = rev(data.frame(targets = seq(1, btsamples*skip, skip)))
  idx = data.frame(idx, data = idx$targets+horizon)

  pred1pd = function(t, first=FALSE) {
    ## Predict one single period, to be used by the backtester in sapply
    ## train the random forest (if statement is for speed optimization parameter)
    savedEnsemble = trainEnsemble(data[(t + horizon+1):(t + trainperiods+horizon+1), ], targets[(t + 1):(t + trainperiods+1)],
      verbose=TRUE, treesInBag, keepNFeatures)
    pred = predict(savedEnsemble, data[t + horizon, ], type='prob')
    ## print so user can estimate how much time is left
    print(pred)
  }

  ## backtest by looping over the calendar from above
  preds = sapply(idx$targets, pred1pd)

  ## print output for human inspection
  print(preds)
  print(max.col(preds))
  preds = data.frame(t(rbind(mle = max.col(t(preds)), preds)))
  print(preds)
  print(paste('Accuracy:', sum((preds$mle*2-3)==targets[idx$targets])/length(preds$mle)))

  ## Set default leverage to Kelly fraction, assuming 5 % edge
  if(leverage == 'kelly') {
    leverage = mean(returns+.025*abs(returns))/var(returns)
  }

  equity = cumprod((returns[idx$targets] * (preds$mle*2-3) * leverage)+1)
  dev.new()
  plot(1:length(equity), equity, main=paste('Equity Curve: 1$ to $', equity[length(equity)]),
       'l', xlab='Trading periods (days/weeks/etc)', ylab='Equity at day x starting with 1$')
}


trainEnsemble = function(data, targets, verbose=FALSE, treesInBag, keepNFeatures) {
  ## Train an ensemble for feature selection
  treeBagFS = randomForest(data, targets, ntree=treesInBag, importance=TRUE, keep.forest=FALSE)

  ## order the features by importance so the best can be skimmed off
  features = sort(importance(treeBagFS, type=1), index.return=TRUE, decreasing=TRUE)$ix[1:min(keepNFeatures,nrow(importance(treeBagFS, type=1)))]

  ## Train full ensemble using predicted best subset of the features
  treeBag = randomForest(data[ ,features], targets, ntree=treesInBag, importance=TRUE, keep.forest=TRUE)

  if (verbose) {
    ## Plot variable importance
    ## impcol=rep('black',min(30,nrow(importance(treeBagFS, type=1))))
    ## impcol[1:min(30,length(features))] = 'blue'

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
