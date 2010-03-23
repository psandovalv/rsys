library(xts)
data <- read.csv('~/src/self/rsys/gold01-09.csv', header=TRUE, stringsAsFactors=FALSE)

## Lets convert the dataset to xts format
dates <- as.Date(data$Date, "%d-%B-%y")
data$Date <- NULL
data <- xts(data, order.by=dates)

target <- data[, 8]
target[target >= 0] <- 1
target[target < 0] <- -1

nperiod <- 100
nlag <- 1

lhs <- seq(1, length(data) - 2*2*nlag, nlag)
rhs <- lhs + lag
