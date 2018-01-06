library(forecast)
library(fpp2)
library(xts)
library(tseries)
library(ggfortify) 
library (ggplot2)

global_temp = read.csv("data/GlobalTemperatures.csv")
global_temp$dt = as.Date(global_temp$dt, "%Y-%m-%d") 
global_temp <- subset(global_temp, select = c(LandAverageTemperature) )
str(global_temp)
temp_ts <-  ts(global_temp, frequency = 12, start = c(1750,1), end =   c(2015,12))

#autoplot(temp_ts) 


fit <- auto.arima(temp_ts, D=1)
forecast(fit,h=360)

#tiff('test.tiff', units="in", width=40, height=10, res=600)
#plot.ts(temp_ts)  
#dev.off()

#global_temp = subset(global_temp, select = c(dt, LandAverageTemperature) )
#global_temp.ts = ts(global_temp, frequency=12, start=c(1750,1) )
#global_temp <- subset(global_temp, select = c(LandAverageTemperature) )
# global_temp_ts = ts(global_temp, frequency=12, start=c(1750,1) )
#plot(global_temp$LandAverageTemperature ~ global_temp$dt, xaxt = "n", type = "l")
