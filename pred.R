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
      cat("train ", paste(start(train), end(train), sep = " to "), "\n")

    mod <- lm(class ~ ., data=train)
    preds[i] <- predict(mod, df[(j + 1), ])
    actual[i] <- df$class[j + 1]
  }
  return(data.frame("actual"=actual, "forecast"=preds))
}

res <- Train(data, target)
res$forecast[res$forecast > 0] <- 1
res$forecast[res$forecast < 0] <- -1

cat("Test Error is ", sum(res$forecast == res$actual)/nrow(res), "\n")

