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
test_ts <- ts(test_ts, start=c(2015,07,23), frequency=365)

train_ts <- zoo(train_aggregated$Sales, order.by=as.Date(as.character(train_aggregated$Date), format='%Y-%m-%d'))
train_ts <- ts(train_ts,  start=c(2013,01,01), frequency=365)

fit <- auto.arima(train_ts, seasonal=TRUE)
fit
forecasted_ts <- forecast(fit,h=30)
plot(forecasted_ts)

#train_components <- decompose(train_ts)
#train_components$seasonal
#plot(train_components)



#train_hw <- HoltWinters(train_ts, beta=FALSE, gamma=FALSE)




#sweep_tmp <- sw_sweep(forecasted_ts, timekit_idx = T)


#ts(forecasted_ts$se, start=c(2015,07,23))
#ts.plot(forecasted_ts$pred)
#ts.plot(train_ts, forecasted_ts ,gpars = list(col = c("black", "green")))

#plot(forecast(fit, h = 20))

#global_temp <- subset(global_temp, select = c(LandAverageTemperature) )
#str(global_temp)
#temp_ts <-  ts(global_temp, frequency = 12, start = c(1750,1), end =   c(2015,12))

#autoplot(temp_ts) 


#fit <- auto.arima(temp_ts, D=1)
#forecast(fit,h=360)

#tiff('test.tiff', units="in", width=40, height=10, res=600)
#plot.ts(temp_ts)  
#dev.off()
#test

#global_temp = subset(global_temp, select = c(dt, LandAverageTemperature) )
#global_temp.ts = ts(global_temp, frequency=12, start=c(1750,1) )
#global_temp <- subset(global_temp, select = c(LandAverageTemperature) )
#global_temp_ts = ts(global_temp, frequency=12, start=c(1750,1) )
#plot(global_temp$LandAverageTemperature ~ global_temp$dt, xaxt = "n", type = "l")
