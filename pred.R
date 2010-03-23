library(xts)
data <- read.csv('~/src/self/rsys/gold01-09.csv', header=TRUE, stringsAsFactors=FALSE)

## Lets convert the dataset to xts format
dates <- as.Date(data$Date, "%d-%B-%y")
data$Date <- NULL
data <- xts(data, order.by=dates)

target <- data[, 8]
target[target >= 0] <- 1
target[target < 0] <- -1

Train <- function(features, target, ntrain=30, npred=1, debug=FALSE) {
  ## Assumes that data is ordered from oldest to newest
  ## features is the input timeseries(xts) to train on
  ## target is the target timeseries(xts)
  ## ntrain is the training window size
  ## npred is the number of points ahead to predict

  iend <- nrow(features) - ntrain - npred - 1
  df <- merge(features, lag(target, -npred))
  preds = rep(0, iend)

  for (i in 1:iend) {
    window <- i:(i + ntrain - 1)
    train <- features[window, ]
    class <- target[(window + npred), ]

    if (debug) {
      cat("train ", paste(start(train), end(train), sep = " to "), "\n")
      cat("class ", paste(start(class), end(class), sep = " to "), "\n")
    }
    df <- merge(train, class)
    rownames(df) <- append(rownames(train), "class")
    mod <- lm(class ~ train$FCXr1 + train$FCXr4 + train$FCXHigh.Low. +
              train$FCXVolNorm + train$DHYr1 + train$XLFr1 + train$SPYr1 +
              train$GLDr1 + train$RTPr1)
    cat("Index ", i, "\n")
    preds[i] <- predict(mod, features[(length(window) + 1), ])
    print(preds)
  }

  return(preds)
}

Train(data, target)


