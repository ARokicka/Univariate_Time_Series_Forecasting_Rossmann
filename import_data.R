library(forecast)
library(xts)
library(tseries)
library(ggfortify) 
library (ggplot2)
library(zoo)
library(sweep)
library(lattice)

#wczytywanie danych z pliku CSV
rossmann.csv = read.csv("D:/OneDrive/Studia WNE/praca dyplomowa/dane/rossmann/rossmann.csv")

#sortowanie
rossmann <- rossmann.csv[order(rossmann.csv$Date),]


#święta kodowane jako 0-jest; 1-nie ma
rossmann$StateHoliday <- gsub("a", "1", rossmann$StateHoliday)
rossmann$StateHoliday <- gsub("b", "1", rossmann$StateHoliday)
rossmann$StateHoliday <- gsub("c", "1", rossmann$StateHoliday)

#przekształcenie zmiennych z factor na numeric
for(i in 4:9){
  rossmann[,i] <-  as.numeric(as.character( rossmann[, i] ))
}

#stworzenie zbioru unikalnych dni tygodnia dla dat
rossmann_dayOfWeek <- cbind(rossmann)
rossmann_dayOfWeek[,4:9] <- NULL
rossmann_dayOfWeek[,1] <- NULL
rossmann_dayOfWeek <- rossmann_dayOfWeek[!duplicated(rossmann_dayOfWeek$Date),]
rossmann_dayOfWeek <- rossmann_dayOfWeek[order(rossmann_dayOfWeek$Date),]

#usuniecie niepotrzebnych zmiennych
rossmann$DayOfWeek <- NULL
rossmann$Store <- NULL

#agreguje dane po dacie
rossmann <- aggregate.data.frame(rossmann[, 2:7], by=list(Date = rossmann$Date), FUN=sum, drop = TRUE)

#dostajemy dane z informacją o dniu tygodnia
rossmann <- merge(rossmann, rossmann_dayOfWeek, by= "Date" )

#tworze kolumne z id i ustawiam ja jako pierwsza
rossmann$Id <- seq.int(nrow(rossmann))
rossmann <- rossmann[,c(ncol(rossmann),1:(ncol(data_set)-1))]

#generuję ciąg dat, dzięki niemu wiem, jaki jest dzień roku
data_sequence <- seq(as.Date("2013-01-01"), as.Date("2015-07-31"), by = "day")
start_day_no <-  as.numeric(format(data_sequence[1], "%j"))

#tworzę szereg TS
#rossmann.ts <- ts(rossmann$Sales, start = c(2013, start_day_no), frequency = 365.25)
rossmann.ts <- ts(rossmann$Sales, start = 1, frequency = 7)
summary(rossmann.ts)
tail(rossmann.ts, n=30)

#podział na dane testowe i uczące
rossmann.ts.train <- window(rossmann.ts, end =   c(131,2) )
rossmann.ts.test  <- window(rossmann.ts, start = c(131,3) )

#dobierz mi model do danych uczących
rossmann.ts.fit <- auto.arima(rossmann.ts.train)
# prognozuj dane na kolejne 30 dni
rossmann.ts.fore <- forecast(rossmann.ts.test, h = 30)

#narysuj prognozę
plot(rossmann.ts.test)
lines(fitted(rossmann.ts.fore), col='red')

#narysuj cały zbiór danych podzielony na 5 wykresów, nakłada się 10% zawartości
xyplot(rossmann.ts, aspect = 1/5)
xyplot(rossmann.ts,  strip = TRUE, cut = list(number = 5, overlap = 0.1))
