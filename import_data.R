library(forecast)
#library(fpp2)
library(xts)
library(tseries)
library(ggfortify) 
library (ggplot2)
library(zoo)
library(sweep)

test_2m = read.csv("D:/Praca/Analityka retail/rossmann/test_2m.csv")
train_2m = read.csv("D:/Praca/Analityka retail/rossmann/train_2m.csv")

test_2m$DayOfWeek <- NULL
test_2m$Store <- NULL

train_2m$DayOfWeek <- NULL
train_2m$Store <- NULL

test_2m[,2] <-  as.numeric(as.character( test_2m[, 2] ))
test_2m[,3] <-  as.numeric(as.character( test_2m[, 3] ))
test_2m[,4] <-  as.numeric(as.character( test_2m[, 4] ))
test_2m[,5] <-  as.numeric(as.character( test_2m[, 5] ))
test_2m[,6] <-  as.numeric(as.character( test_2m[, 6] ))
test_2m[,7] <-  as.numeric(as.character( test_2m[, 7] ))

train_2m[,2] <-  as.numeric(as.character( train_2m[, 2] ))
train_2m[,3] <-  as.numeric(as.character( train_2m[, 3] ))
train_2m[,4] <-  as.numeric(as.character( train_2m[, 4] ))
train_2m[,5] <-  as.numeric(as.character( train_2m[, 5] ))
train_2m[,6] <-  as.numeric(as.character( train_2m[, 6] ))
train_2m[,7] <-  as.numeric(as.character( train_2m[, 7] ))

test_aggregated <- aggregate.data.frame(test_2m[, 2:7], by=list(Date = test_2m$Date), FUN=sum, drop = TRUE)
train_aggregated <- aggregate.data.frame(train_2m[, 2:7], by=list(Date = train_2m$Date), FUN=sum, drop = TRUE)

test_ts <- zoo(test_aggregated$Sales, order.by=as.Date(as.character(test_aggregated$Date), format='%Y-%m-%d'))
test_ts <- ts(test_ts, start=c(2015,07,23), frequency=7)

train_ts <- zoo(train_aggregated$Sales, order.by=as.Date(as.character(train_aggregated$Date), format='%Y-%m-%d'))
train_ts <- ts(train_ts,  start=c(2013,01,01), frequency=7)

fit <- auto.arima(train_ts, seasonal=TRUE)
fit
forecasted_ts <- forecast(fit,h=60)
plot(forecasted_ts)


train_components <- decompose(train_ts)
train_components$seasonal
plot(train_components)
