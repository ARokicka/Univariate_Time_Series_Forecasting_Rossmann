library(forecast)
library(xts)
library(tseries)
library(ggfortify) 
library (ggplot2)
library(zoo)
library(sweep)
library(lattice)
library(expsmooth)

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

#sezonoWość tygodniowa i trend - brak trendu długoterminowego
par(mfrow=c(2,1))
monthplot(rossmann.ts)
boxplot(rossmann.ts ~ cycle(rossmann.ts))
#seasonplot(rossmann.ts, col = rainbow(12) )

#wykresy rozrzutu dla wartości opóźionych - autokorelacja
lag.plot(rossmann.ts, lags = 4, do.lines = FALSE)

#wykresy rozrzutu dla reszt
rossmann.ts.reszty <- decompose(rossmann.ts)$random
head(rossmann.ts.reszty)
tail(rossmann.ts.reszty)
rossmann.ts.reszty <- na.omit(rossmann.ts.reszty)

#ACF i PACF - autokorelacja
par(mfrow=c(2,1))
Acf(rossmann.ts, lag.max = 16)
Pacf(rossmann.ts, lag.max = 16)
tsdisplay(rossmann.ts)

#Transofmracja Boxa-Cox (za dużo nie daje, bo wariancja jest stała)
rossmann.ts.boxcox.sqrt <- BoxCox(rossmann.ts, lambda = 0.5)
rossmann.ts.boxcox.log <- BoxCox(rossmann.ts, lambda = 1)
par(mfrow=c(3,1))
plot(rossmann.ts, main="rossmann.ts")
grid()
plot(rossmann.ts.boxcox.sqrt, main="rossmann.ts.boxcox.sqrt")
grid()
plot(rossmann.ts.boxcox.log, main="rossmann.ts.boxcox.log")
grid()

# różnicowanie z opóźnieniem 1 i 7
rossmann.ts.diff <- diff(rossmann.ts)
rossmann.ts.diff7 <- diff(rossmann.ts, lag = 7)
tsdisplay(rossmann.ts.diff7)


# zastąpienie outlayerów i wyczyszczenie brakujacych danych
tsoutliers(rossmann.ts)
xyplot(rossmann.ts, aspect = 1/5)
xyplot(tsclean(rossmann.ts), aspect = 1/5)
rossmann.ts.train.clean <- tsclean(rossmann.ts.train)
#rossmann.ts.test <- tsclean(rossmann.ts.test)