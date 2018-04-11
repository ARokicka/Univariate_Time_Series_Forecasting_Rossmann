library(forecast)
library(xts)
library(tseries)
library(ggfortify) 
library (ggplot2)
library(zoo)
library(sweep)

test_2m = read.csv("D:/Praca/Analityka retail/rossmann/test_2m.csv")
train_2m = read.csv("D:/Praca/Analityka retail/rossmann/train_2m.csv")

prepare_data(test_2m)
prepare_data(train_2m)


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

prepare_data <- function(data_set){
  
  #sortowanie po dacie
  data_set <- data_set[order(data_set$Date),]
  
  #święta kodowane jako 0-jest; 1-nie ma
  data_set$StateHoliday <- gsub("a", "1", data_set$StateHoliday)
  data_set$StateHoliday <- gsub("b", "1", data_set$StateHoliday)
  data_set$StateHoliday <- gsub("c", "1", data_set$StateHoliday)
  
  #przekształcenie zmiennych z factor na numeric
  for(i in 4:9){
    data_set[,i] <-  as.numeric(as.character( data_set[, i] ))
  }
  
  #stworzenie zbioru unikalnych dni tygodnia dla dat
  data_set_dayOfWeek <- cbind(data_set)
  data_set_dayOfWeek[,4:9] <- NULL
  data_set_dayOfWeek[,1] <- NULL
  data_set_dayOfWeek <- data_set_dayOfWeek[!duplicated(data_set_dayOfWeek$DayOfWeek),]
  data_set_dayOfWeek <- data_set_dayOfWeek[order(data_set_dayOfWeek$Date),]
  
  #usuniecie niepotrzebnych zmiennych
  data_set$DayOfWeek <- NULL
  data_set$Store <- NULL
  
  #agreguje dane po dacie
  data_set_aggregated <- aggregate.data.frame(data_set[, 2:7], by=list(Date = data_set$Date), FUN=sum, drop = TRUE)
  
  #dostajemy dane z informacją o dniu tygodnia
  data_set <- merge(data_set_aggregated, data_set_dayOfWeek, by= "Date" )
  
  #tworze kolumne z id i ustawiam ja jako pierwsza
  data_set$Id <- seq.int(nrow(data_set))
  data_set <- data_set[,c(ncol(data_set),1:(ncol(data_set)-1))]
  
  data_set;
}