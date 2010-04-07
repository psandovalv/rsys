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
  ## Dumb linear model
  ## Assumes that data is ordered from oldest to newest
  ## features is the input timeseries(xts) to train on
  ## target is the target timeseries(xts)
  ## ntrain is the training window size
  ## npred is the number of points ahead to predict

  iend <- nrow(features) - ntrain - npred - 1
  df <- merge(features, lag(target, -npred))
  colnames(df) <- append(colnames(features), "class")

  preds <- rep(0, iend)
  actual <- rep(0, iend)

  for (i in 1:iend) {
    j <- (i + ntrain - 1)
    train <- df[i:j, ]

    if (debug)
      cat("training period ", paste(start(train), end(train), sep = " to "), "\n")

    mod <- lm(class ~ ., data=train)
    preds[i] <- predict(mod, df[(j + 1), ])
    actual[i] <- df$class[j + 1]
  }
  return(data.frame("actual"=actual, "forecast"=preds))
}


ErrorRate <- function(df, smooth.forcast=TRUE) {
    df$forecast[df$forecast > 0] <- 1
    df$forecast[df$forecast < 0] <- -1
    error <- sum(df$forecast == df$actual)/nrow(df)
    cat("Test Error is ", error, "\n")
    return(error)
  }

Main <- function() {
  res <- Train(data, target, debug=TRUE)
  ErrorRate(res)
}

Simulate <- function() {
  ## Try different training period lengths
  errors = rep(0, 100)
  for (i in 1:50) {
    res <- Train(data, target, ntrain=i, debug=FALSE)
    errors[i] <- ErrorRate(res)
  }
  return(errors)
}
