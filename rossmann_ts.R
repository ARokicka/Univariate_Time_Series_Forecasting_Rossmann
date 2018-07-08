library(forecast)
library(xts)
library(tseries)
library(ggfortify)
library (ggplot2)
library(zoo)
library(sweep)
library(lattice)
library(expsmooth)
library(caret)
library(gridExtra)
library(grid)
library(data.table)

#wczytywanie danych z pliku CSV test 
rossmann.csv = read.csv("D:/OneDrive/Studia WNE/praca dyplomowa/dane/rossmann/rossmann.csv")

head(rossmann.csv)
tail(rossmann.csv)

#sortowanie
rossmann <- rossmann.csv[order(rossmann.csv$Date),]

#święta kodowane jako 0-jest; 1-nie ma
rossmann$StateHoliday <- gsub("a", "1", rossmann$StateHoliday)
rossmann$StateHoliday <- gsub("b", "1", rossmann$StateHoliday)
rossmann$StateHoliday <- gsub("c", "1", rossmann$StateHoliday)

#przekształcenie  zmiennych z factor na numeric
for (i in 4:9) {
  rossmann[, i] <-  as.numeric(as.character(rossmann[, i]))
}

#stworzenie zbioru unikalnych dni tygodnia dla dat
rossmann_dayOfWeek <- cbind(rossmann)
rossmann_dayOfWeek[, 4:9] <- NULL
rossmann_dayOfWeek[, 1] <- NULL
rossmann_dayOfWeek <-
  rossmann_dayOfWeek[!duplicated(rossmann_dayOfWeek$Date),]
rossmann_dayOfWeek <-
  rossmann_dayOfWeek[order(rossmann_dayOfWeek$Date),]

#usuniecie niepotrzebnych zmiennych
rossmann$DayOfWeek <- NULL
rossmann$Store <- NULL

#agreguje dane po dacie
rossmann <-
  aggregate.data.frame(
    rossmann[, 2:7],
    by = list(Date = rossmann$Date),
    FUN = sum,
    drop = TRUE
  )

#dostajemy dane z informacją o dniu tygodnia
rossmann <- merge(rossmann, rossmann_dayOfWeek, by = "Date")

#tworze kolumne z id i ustawiam ja jako pierwsza
rossmann$Id <- seq.int(nrow(rossmann))
rossmann <- rossmann[, c(ncol(rossmann), 1:(ncol(rossmann) - 1))]

#generuję ciąg dat, dzięki niemu wiem, jaki jest dzień roku
data_sequence <-
  seq(as.Date("2013-01-01"), as.Date("2015-07-31"), by = "day")
start_day_no <-  as.numeric(format(data_sequence[1], "%j"))

#tworzę szereg TS
class(rossmann.ts)
typeof(rossmann.ts)
rossmann.ts <- ts(rossmann$Sales, start = 1, frequency = 7)

#podział na dane testowe i uczące
rossmann.ts.train <- window(rossmann.ts, end =   c(131, 2))
rossmann.ts.test  <- window(rossmann.ts, start = c(131, 3))

#dobierz mi model do danych uczących
rossmann.ts.fit <- auto.arima(rossmann.ts.train)
# prognozuj dane na kolejne 30 dni
rossmann.ts.fore <- forecast(rossmann.ts.test, h = 30)

#narysuj prognozę
plot(rossmann.ts)
lines(fitted(rossmann.ts.fore), col = 'red')

#narysuj cały zbiór danych podzielony na 5 wykresów, nakłada się 0% zawartości
xyplot(rossmann.ts, aspect = 1 / 6)
xyplot(rossmann.ts,
       strip = TRUE,
       cut = list(number = 6, overlap = 0))

#sezonoWość tygodniowa i trend - brak trendu długoterminowego
par(mfrow = c(2, 1))
monthplot(rossmann.ts, labels = c("wtorek","środa","czwartek","piątek",
                                  "sobota","niedziela","poniedziałek"))
boxplot(names=c("wtorek","środa","czwartek","piątek","sobota","niedziela","poniedziałek")
        , rossmann.ts ~ cycle(rossmann.ts))

#wykresy rozrzutu dla wartości opóźionych - autokorelacja
lag.plot(rossmann.ts, lags = 8, do.lines = FALSE)

#wykresy rozrzutu dla reszt
rossmann.ts.reszty <- decompose(rossmann.ts)$random
rossmann.ts.reszty <- na.omit(rossmann.ts.reszty)

#ACF i PACF - autokorelacja
par(mfrow = c(2, 1))
Acf(rossmann.ts, lag.max = 16)
Pacf(rossmann.ts, lag.max = 16)
tsdisplay(rossmann.ts)

#Transofmracja Boxa-Cox (za dużo nie daje, bo wariancja jest stała)
rossmann.ts.boxcox.sqrt <- BoxCox(rossmann.ts, lambda = 0.5)
rossmann.ts.boxcox.log <- BoxCox(rossmann.ts, lambda = 1)
par(mfrow = c(3, 1))
plot(rossmann.ts, main = "rossmann.ts")
grid()
plot(rossmann.ts.boxcox.sqrt, main = "rossmann.ts.boxcox.sqrt")
grid()
plot(rossmann.ts.boxcox.log, main = "rossmann.ts.boxcox.log")
grid()

# różnicowanie z opóźnieniem 1 i 7
rossmann.ts.diff <- diff(rossmann.ts)
tsdisplay(rossmann.ts.diff)
rossmann.ts.diff7 <- diff(rossmann.ts, lag = 7)
tsdisplay(rossmann.ts.diff7)


# zastąpienie outlayerów i wyczyszczenie brakujacych danych
tsoutliers(rossmann.ts)
xyplot(rossmann.ts, aspect = 1 / 5)
xyplot(tsclean(rossmann.ts), aspect = 1 / 5)
rossmann.ts.train.clean <- tsclean(rossmann.ts.train)
#rossmann.ts.test <- tsclean(rossmann.ts.test)

# średnia ruchoma q=5
par(mfrow = c(2, 1))
rossmann.ts.ma5 <-
  filter(rossmann.ts, sides = 2, filter = rep(1 / 11, 11))
rossmann.ts.ma5 <- na.omit(rossmann.ts.ma5)
#plot(rossmann.ts.ma5)
xyplot(rossmann.ts.ma5,
       strip = TRUE,
       cut = list(number = 5, overlap = 0.1))


# średnia ruchoma q=10
rossmann.ts.ma10 <-
  filter(rossmann.ts, sides = 2, filter = rep(1 / 21, 21))
rossmann.ts.ma10 <- na.omit(rossmann.ts.ma10)
#plot(rossmann.ts.ma10)
xyplot(rossmann.ts.ma10,
       strip = TRUE,
       cut = list(number = 5, overlap = 0.1))


#dekompozycja addytywna
rossmann.ts.dekomp.add <- decompose(rossmann.ts, type = "additive")
rossmann.ts.dekomp.add$seasonal
#plot(rossmann.ts.dekomp.add)
decomp.plot.add <- function(x, main = NULL, ...) 
{ 
  if(is.null(main)) 
    main <- paste("Dekompozycja addytywna szeregu czasowego") 
  plot(
    cbind(szereg_czasowy = x$random + if (x$type == "additive") 
    x$trend + x$seasonal 
    else x$trend * x$seasonal, trend = x$trend, sezonowość = x$seasonal, 
    reszty = x$random), main = main, ...) 
} 

decomp.plot.add(rossmann.ts.dekomp.add)


tsdisplay(rossmann.ts.dekomp.add$random, main = "Analiza reszt (dekompozycja addytywnna): funkcja ACF i PACF")

xyplot(
  rossmann.ts.dekomp.add$random,
  strip = TRUE,
  cut = list(number = 5, overlap = 0.1)
)


# wychodzimy poza przedział ufności -> reszty nie przypominaj abialego szumu, za duzo
# wartosci poza przedzialem ufnosci
# widac tez sezonowosc

#dekompozycja multiplikatywna
rossmann.ts.dekomp.multi <-
  decompose(rossmann.ts, type = "multiplicative")


decomp.plot.multi <- function(x, main = NULL, ...) 
{ 
  if(is.null(main)) 
    main <- paste("Dekompozycja multiplikatywna szeregu czasowego") 
  plot(
    cbind(szereg_czasowy = x$random + if (x$type == "additive") 
      x$trend + x$seasonal 
      else x$trend * x$seasonal, trend = x$trend, sezonowość = x$seasonal, 
      reszty = x$random), main = main, ...) 
} 

decomp.plot.multi(rossmann.ts.dekomp.multi)


plot(rossmann.ts.dekomp.multi)
tsdisplay(rossmann.ts.dekomp.multi$random, main = "Analiza reszt (dekompozycja multiplikatywna): funkcja ACF i PACF")
xyplot(
  rossmann.ts.dekomp.multi$random,
  strip = TRUE,
  cut = list(number = 5, overlap = 0.1)
)



# wychodzimy poza przedział ufności -> reszty nie przypominaj abialego szumu, za duzo
# wartosci poza przedzialem ufnosci
# widac tez sezonowosc

# odsezonowanie danych
rossmann.ts.odsezonowane <- seasadj(rossmann.ts.dekomp.multi)
plot(rossmann.ts, col = "black", main = "Dane oryginalne i odsezonowane")
lines(rossmann.ts.odsezonowane, col = "red", lty = 2)
legend(
  "topleft",
  legend = c("oryginalny szereg", "szereg odsezonowany"),
  col = c("black", "green")
)
xyplot(
  cbind(rossmann.ts, rossmann.ts.odsezonowane),
  main = "Dane oryginalne i odsezonowane",
  strip = TRUE,
  cut = list(number = 5, overlap = 0.1),
  superpose = TRUE
)

####
#### Budowa modelu AR i MA
####

# różnicowanie z opóźnieniem 14 i 1 => najlepsze efekty

rossmann.ts.diff1 <- diff(rossmann.ts, lag = 1)
tsdisplay(rossmann.ts.diff1)
rossmann.ts.diff1.diff7 <- diff(rossmann.ts.diff1, lag = 7)
tsdisplay(rossmann.ts.diff1.diff7)


rossmann.ts.diff7 <- diff(rossmann.ts, lag = 7)
tsdisplay(rossmann.ts.diff7)
rossmann.ts.diff7.diff1 <- diff(rossmann.ts.diff7, lag = 1)
tsdisplay(rossmann.ts.diff7.diff1)

# jak widać najlepiej jest różnicować najpierw dla s=14, a pozniej s=1
# wtedy pozbywamy sie sezonowosci
# do tego wychodzi, że powinien najlepszy być model:
# AR(p=28) - chociaz trudno to stwierdzić
# MA(q=30) - chociaz trudno to stwierdzić

# a teraz test odwrotnego roznicowania => wszystko OK
#rossmann.ts.diff_14_inv <- diffinv(rossmann.ts.diff_14_1, lag = 1, xi = head(rossmann.ts.diff_14, n = 1) )
#tsdisplay(rossmann.ts.diff_14_inv)
#rossmann.ts.diff_inv <- diffinv(rossmann.ts.diff_14_inv, lag = 7, xi = head(rossmann.ts, n = 14) )
#tsdisplay(rossmann.ts.diff_inv)

# test AR(p=13)
p = 30
ar.model.yw = ar(rossmann.ts.diff14.diff1,
                 order.max = 30,
                 aic = FALSE)

#par(mfrow = c(2, 1))
#plot(ar.model.yw$resid)
Acf(ar.model.yw$resid)

ma.model.yw = ma(rossmann.ts.diff14.diff1,
                 order.max = 2,
                 aic = FALSE)

#par(mfrow = c(2, 1))
#plot(ar.model.yw$resid)
Acf(ma.model.yw$resid)
#
# print(ar.model.yw)
# ar.model.mle = ar(
#   rossmann.ts.diff14.diff1,
#   order.max = p,
#   aic = FALSE,
#   method = "mle"
# )
#

print(ar.model.mle)
ar.model.aic = ar(rossmann.ts.diff14.diff1, aic = TRUE)
print(ar.model.aic)

# teoretycznie mam teraz trzy modele ARIMA:
#     1. ARIMA(29,1,0)(0,1,0)
#     2. ARIMA(0,1,27)(0,1,0)
#     3. auto.arima
#     4. ARIMA (8,1,8)(0,1,1)
#     5. TBATS

ARIMA.model1 <-
  Arima(
    rossmann.ts.train,
    order = c(28, 1, 0),
    seasonal = list(order = c(0, 1, 0), period = 14)
  )
ARIMA.model1.pred = predict(ARIMA.model1, n.ahead = 30)

ARIMA.model2 <-
  Arima(
    rossmann.ts.train,
    order = c(0, 1, 27),
    seasonal = list(order = c(0, 1, 0), period = 14)
  )
ARIMA.model2.pred = predict(ARIMA.model2, n.ahead = 30)
ARIMA.model3 <- auto.arima(rossmann.ts.train)
ARIMA.model3.pred = predict(ARIMA.model3, n.ahead = 30)

ARIMA.model4 <-
  Arima(
    rossmann.ts.train,
    order = c(8, 1, 8),
    seasonal = list(order = c(0, 1, 1), period = 14)
  )

ARIMA.model4.pred = predict(ARIMA.model4, n.ahead = 30)
tsdisplay(ARIMA.model4$residuals)

# TBATS
TBATS.model1 <- tbats(
  rossmann.ts.train,
  use.box.cox = FALSE,
  use.trend = FALSE,
  use.damped.trend = TRUE,
  seasonal.periods = c(14),
  use.arma.errors = TRUE,
  trace = TRUE
)
TBATS.model1.pred <- forecast(TBATS.model1, h = 30)
plot(TBATS.model1.pred)
TBATS.model1.pred.forecast <-
  window(TBATS.model1.pred$mean, start = c(131, 3))
plot(TBATS.model1.pred.forecast)

SNAIVE.model <- snaive(rossmann.ts.train, h = 30)

# Holt Winters
HW.model1 <-
  hw(
    rossmann.ts.train,
    h = 30,
    seasonal = "multiplicative",
    alpha = 0.02,
    beta = 0.002
  )
HW.auto <-
  hw(rossmann.ts.train, h = 30, seasonal = "multiplicative")

#HW.model1$model
#accuracy(HW.model1$mean, rossmann.ts.test)
#accuracy(HW.auto$mean, rossmann.ts.test)
#plot(rossmann.ts.test, lwd = 3, col = 'black')
#lines(HW.model1$mean, col = 'red')
#lines(HW.auto$mean, col = 'green')

# model ETS
# AUTO = (M,N,M)
ETS.auto <- ets(rossmann.ts.train)
ETS.auto.pred <- forecast(ETS.auto, h = 30)
ETS.model1 <-
  ets(rossmann.ts.train,
      model = "MNM",
      alpha = 0.013,
      beta = 0.002)
ETS.model1.pred <- forecast(ETS.model1, h = 30)
#accuracy(ETS.auto.pred$mean, rossmann.ts.test)
#accuracy(ETS.model1.pred$mean, rossmann.ts.test)
#plot(rossmann.ts.test, lwd = 3, col = 'black')
#lines(ETS.auto.pred$mean, col = 'red')
#lines(ETS.model1.pred$mean, col = 'green')

# model TSLM
TSLM.trend.season <- tslm(rossmann.ts.train ~ trend + season)
TSLM.trend.season.pred <- forecast(TSLM.trend.season, h = 30)

TSLM.trend.season$model

#accuracy(TSLM.trend.season.pred$mean, rossmann.ts.test)
#plot(rossmann.ts.test, lwd = 3, col = 'black')
#lines(TSLM.trend.season.pred$mean, col = 'red')


# wykresy reszt
# nie widac na nich trendów i sezonowości
# ale w resztach widac pewne niewyjaśnione zalezności

par(mfrow = c(5, 2))
plot(ARIMA.model1$residuals, main = "Reszty dla modelu ARIMA(28,1,0)(0,1,0)")
Acf(ARIMA.model1$residuals, main = "ACF: Reszty dla modelu ARIMA(29,1,0)(0,1,0)")
plot(ARIMA.model2$residuals, main = "Reszty dla modelu ARIMA(0,1,27)(0,1,0)")
Acf(ARIMA.model1$residuals, main = "ACF: Reszty dla modelu ARIMA(29,1,0)(0,1,0)")
plot(ARIMA.model3$residuals, main = "Reszty dla modelu auto.arima")
Acf(ARIMA.model3$residuals, main = "ACF: Reszty dla modelu auto.arima")
plot(ARIMA.model4$residuals, main = "Reszty dla modelu ARIMA(8,1,8)(0,1,1)")
Acf(ARIMA.model4$residuals, main = "ACF: Reszty dla modelu ARIMA(8,1,8)(0,1,1)")
plot(TBATS.model1.pred$residuals, main = "Reszty dla modelu TBATS")
Acf(TBATS.model1.pred$residuals, main = "ACF: Reszty dla modelu TBATS")

par(mfrow = c(5, 2))
plot(ETS.auto.pred$residuals, main = "Reszty dla modelu ETS.auto")
Acf(ETS.auto.pred$residuals, main = "ACF: Reszty dla modelu ETS.auto")
plot(TSLM.trend.season$residuals, main = "Reszty dla modelu TSLM")
Acf(TSLM.trend.season$residuals, main = "ACF: Reszty dla modelu TSLM")
plot(SNAIVE.model$residuals, main = "Reszty dla modelu naive")
Acf(SNAIVE.model$residuals, main = "ACF: Reszty dla modelu naive")
plot(HW.auto$residuals, main = "Reszty dla modelu Holt-Winters-auto")
Acf(HW.auto$residuals, main = "ACF: Reszty dla modelu Holt-Winters-auto")
plot(HW.model1$residuals, main = "Reszty dla modelu Holt-Winters-adj")
Acf(HW.model1$residuals, main = "ACF: Reszty dla modelu Holt-Winters-adj")



# wykresy ACF
par(mfrow = c(4, 1))
Acf(ARIMA.model1$residuals, main = "ACF: Reszty dla modelu ARIMA(29,1,0)(0,1,0)")
Acf(ARIMA.model1$residuals, main = "ACF: Reszty dla modelu ARIMA(29,1,0)(0,1,0)")
Acf(ARIMA.model3$residuals, main = "ACF: Reszty dla modelu auto.arima")
Acf(ARIMA.model4$residuals, main = "ACF: Reszty dla modelu ARIMA(8,1,8)(0,1,1)")

par(mfrow = c(3, 1))
Acf(TBATS.model1.pred$residuals, main = "ACF: Reszty dla modelu TBATS")
Acf(ETS.auto.pred$residuals, main = "ACF: Reszty dla modelu ETS.auto")
Acf(TSLM.trend.season$residuals, main = "ACF: Reszty dla modelu TSLM")

par(mfrow = c(3, 1))
Acf(SNAIVE.model$residuals, main = "ACF: Reszty dla modelu naive")
Acf(HW.auto$residuals, main = "ACF: Reszty dla modelu Holt-Winters-auto")
Acf(HW.model1$residuals, main = "ACF: Reszty dla modelu Holt-Winters-adj")

# histogramy z reszt (chyba nie potrzebne za bardzo)
hist(ARIMA.model1$residuals)
hist(ARIMA.model2$residuals)
hist(ARIMA.model3$residuals)
hist(ARIMA.model4$residuals)
hist(TBATS.model1.pred$residuals)
hist(ETS.auto.pred$residuals)
hist(TSLM.trend.season$residuals)
hist(SNAIVE.model$residuals)
hist(HW.auto$residuals)
hist(HW.model1$residuals)


# nie mamy podstaw do odrzucenia hipotezy zerowej, że reszty to biały szum: ARIMA(29,1,0)(0,1,0)
Box.test(ARIMA.model1$residuals, lag = 1, type = "Ljung-Box")
Box.test(ARIMA.model1$residuals, lag = 7, type = "Ljung-Box")

Box.test(ARIMA.model2$residuals, lag = 1, type = "Ljung-Box")
Box.test(ARIMA.model2$residuals, lag = 7, type = "Ljung-Box")

Box.test(ARIMA.model3$residuals, lag = 1, type = "Ljung-Box")
Box.test(ARIMA.model3$residuals, lag = 7, type = "Ljung-Box")

Box.test(ARIMA.model4$residuals, lag = 1, type = "Ljung-Box")
Box.test(ARIMA.model4$residuals, lag = 7, type = "Ljung-Box")

Box.test(TBATS.model1.pred$residuals, lag = 1, type = "Ljung-Box")
Box.test(TBATS.model1.pred$residuals, lag = 7, type = "Ljung-Box")

Box.test(ETS.auto.pred$residuals, lag = 1, type = "Ljung-Box")
Box.test(ETS.auto.pred$residuals, lag = 7, type = "Ljung-Box")

Box.test(TSLM.trend.season$residuals, lag = 1, type = "Ljung-Box")
Box.test(TSLM.trend.season$residuals, lag = 7, type = "Ljung-Box")

Box.test(SNAIVE.model$residuals, lag = 1, type = "Ljung-Box")
Box.test(SNAIVE.model$residuals, lag = 7, type = "Ljung-Box")

Box.test(HW.auto$residuals, lag = 1, type = "Ljung-Box")
Box.test(HW.auto$residuals, lag = 7, type = "Ljung-Box")

Box.test(HW.model1$residuals, lag = 1, type = "Ljung-Box")
Box.test(HW.model1$residuals, lag = 7, type = "Ljung-Box")

# narysuj wykresy prognoz PART 1
par(mfrow = c(2, 1))
plot(rossmann.ts.test,
     lwd = 3,
     col = 'black',
     main = 'Prognozy dla danych testowych I/II')
lines(ARIMA.model1.pred$pred, col = 'chocolate4')
lines(ARIMA.model2.pred$pred, col = 'brown3')
lines(ARIMA.model3.pred$pred, col = 'forestgreen')
lines(ARIMA.model4.pred$pred, col = 'darkturquoise')
lines(TBATS.model1.pred$mean, col = 'darkorange')
grid()
legend(
  "bottomright",
  legend = c(
    "Oryginalny szereg",
    "ARIMA(29,1,0)",
    "ARIMA(0,1,27)",
    "auto.arima",
    "ARIMA(8,1,8)",
    "TBATS"
  ),
  col = c(
    "black",
    "chocolate4",
    "brown3",
    "forestgreen",
    'darkturquoise',
    "darkorange"
  ),
  lty = c(1, 1, 1, 1, 1, 1)
)
# narysuj wykresy prognoz PART 2
plot(rossmann.ts.test,
     lwd = 3,
     col = 'black',
     main = 'Prognozy dla danych testowych II/II')
lines(HW.auto$mean, col = 'chocolate4')
lines(HW.model1$mean, col = 'brown3')
lines(ETS.auto.pred$mean, col = 'forestgreen')
lines(TSLM.trend.season.pred$mean, col = 'darkturquoise')
lines(SNAIVE.model$mean, col = 'darkorange')
grid()
legend(
  "bottomright",
  legend = c(
    "Oryginalny szereg",
    "Holt-Winters auto",
    "Holt-Winters adj",
    "ETS",
    "TSLM",
    "naive"
  ),
  col = c(
    "black",
    "chocolate4",
    "brown3",
    "forestgreen",
    'darkturquoise',
    "darkorange"
  ),
  lty = c(1, 1, 1, 1, 1, 1)
)


# narysuj wykresy błędów
par(mfrow = c(2, 1))
plot(abs(rossmann.ts.test - ARIMA.model1.pred$pred),
     col = 'chocolate4',
     main = 'Błędy prognoz na danych testowych I/II')
lines(abs(rossmann.ts.test - ARIMA.model2.pred$pred), col = 'brown3')
lines(abs(rossmann.ts.test - ARIMA.model3.pred$pred), col = 'forestgreen')
lines(abs(rossmann.ts.test - ARIMA.model4.pred$pred), col = 'darkturquoise')
lines(abs(rossmann.ts.test - TBATS.model1.pred$mean), col = 'darkorange')
grid()
legend(
  "topleft",
  legend = c(
    "ARIMA(29,1,0)",
    "ARIMA(0,1,27)",
    "auto.arima",
    "ARIMA(8,1,8)",
    "TBATS"
  ),
  col = c(
    "chocolate4",
    "brown3",
    "forestgreen",
    'darkturquoise',
    'darkorange'
  ),
  lty = c(1, 1, 1, 1, 1)
)

plot(abs(rossmann.ts.test - HW.auto$mean),
     col = 'chocolate4',
     main = 'Błędy prognoz na danych testowych I/II')
lines(abs(rossmann.ts.test - HW.model1$mean), col = 'brown3')
lines(abs(rossmann.ts.test - ETS.auto.pred$mean), col = 'forestgreen')
lines(abs(rossmann.ts.test - TSLM.trend.season.pred$mean), col = 'darkturquoise')
lines(abs(rossmann.ts.test - SNAIVE.model$mean), col = 'darkorange')
grid()
legend(
  "topleft",
  legend = c("Holt-Winters auto",
             "Holt-Winters adj",
             "ETS",
             "TSLM",
             "naive"),
  col = c(
    "chocolate4",
    "brown3",
    "forestgreen",
    'darkturquoise',
    'darkorange'
  ),
  lty = c(1, 1, 1, 1, 1)
)


# policz RMSE i Rsquared
ARIMA.model1.postResample <-
  accuracy(ARIMA.model1.pred$pred, rossmann.ts.test)
ARIMA.model2.postResample <-
  accuracy(ARIMA.model2.pred$pred, rossmann.ts.test)
ARIMA.model3.postResample <-
  accuracy(ARIMA.model3.pred$pred, rossmann.ts.test)
ARIMA.model4.postResample <-
  accuracy(ARIMA.model4.pred$pred, rossmann.ts.test)
TBATS.model1.postResample <-
  accuracy(TBATS.model1.pred.forecast, rossmann.ts.test)
ETS.auto.postResample <-
  accuracy(ETS.auto.pred$mean, rossmann.ts.test)
TSLM.trend.season.postResample <-
  accuracy(TSLM.trend.season.pred$mean, rossmann.ts.test)
SNAIVE.model.postResample <-
  accuracy(SNAIVE.model$mean, rossmann.ts.test)
HW.auto.postResample <-
  accuracy(HW.auto$mean, rossmann.ts.test)
HW.model1.postResample <-
  accuracy(HW.model1$mean, rossmann.ts.test)

# diagram porównujący RMSE różnych modeli
ARIMA.compare.RMSE <-
  c(
    ARIMA.model1.postResample[2],
    ARIMA.model2.postResample[2],
    ARIMA.model3.postResample[2],
    ARIMA.model4.postResample[2],
    TBATS.model1.postResample[2],
    ETS.auto.postResample[2],
    TSLM.trend.season.postResample[2],
    SNAIVE.model.postResample[2],
    HW.auto.postResample[2],
    HW.model1.postResample[2]
  )
names(ARIMA.compare.RMSE) <- c(
  "ARIMA(29,1,0)",
  "ARIMA(0,1,27)",
  "auto.arima",
  "ARIMA(8,1,8)",
  "TBATS",
  "ETS",
  "TSLM",
  "naive",
  "Holt-Winters auto",
  "Holt-Winters adj"
)

barplot(ARIMA.compare.RMSE, col = rainbow(10), main = "Błąd: kryterium RMSE")
legend(
  "topleft",
  c(
    "ARIMA(29,1,0)",
    "ARIMA(0,1,27)",
    "auto.arima",
    "ARIMA(8,1,8)",
    "TBATS",
    "ETS",
    "TSLM",
    "naive",
    "Holt-Winters auto",
    "Holt-Winters adj"
  ),
  cex = 1.3,
  bty = "n",
  fill = rainbow(10)
)


# diagram porównujący MAPE różnych modeli
ARIMA.compare.MAPE <-
  c(
    ARIMA.model1.postResample[5],
    ARIMA.model2.postResample[5],
    ARIMA.model3.postResample[5],
    ARIMA.model4.postResample[5],
    TBATS.model1.postResample[5],
    ETS.auto.postResample[5],
    TSLM.trend.season.postResample[5],
    SNAIVE.model.postResample[5],
    HW.auto.postResample[5],
    HW.model1.postResample[5]
  )
names(ARIMA.compare.MAPE) <- c(
  "ARIMA(29,1,0)",
  "ARIMA(0,1,27)",
  "auto.arima",
  "ARIMA(8,1,8)",
  "TBATS",
  "ETS",
  "TSLM",
  "naive",
  "Holt-Winters auto",
  "Holt-Winters adj"
)

barplot(ARIMA.compare.MAPE, col = rainbow(10), main = "Błąd: kryterium MAPE")
legend(
  "topright",
  c(
    "ARIMA(29,1,0)",
    "ARIMA(0,1,27)",
    "auto.arima",
    "ARIMA(8,1,8)",
    "TBATS",
    "ETS",
    "TSLM",
    "naive",
    "Holt-Winters auto",
    "Holt-Winters adj"
  ),
  cex = 1.3,
  bty = "n",
  fill = rainbow(10)
)

# diagram porównujący MAE różnych modeli
ARIMA.compare.MAE <-
  c(
    ARIMA.model1.postResample[3],
    ARIMA.model2.postResample[3],
    ARIMA.model3.postResample[3],
    ARIMA.model4.postResample[3],
    TBATS.model1.postResample[3],
    ETS.auto.postResample[3],
    TSLM.trend.season.postResample[3],
    SNAIVE.model.postResample[3],
    HW.auto.postResample[3],
    HW.model1.postResample[3]
  )
names(ARIMA.compare.MAE) <- c(
  "ARIMA(29,1,0)",
  "ARIMA(0,1,27)",
  "auto.arima",
  "ARIMA(8,1,8)",
  "TBATS",
  "ETS",
  "TSLM",
  "naive",
  "Holt-Winters auto",
  "Holt-Winters adj"
)

barplot(ARIMA.compare.MAE, col = rainbow(10), main = "Błąd: kryterium MAE")
legend(
  "topleft",
  c(
    "ARIMA(29,1,0)",
    "ARIMA(0,1,27)",
    "auto.arima",
    "ARIMA(8,1,8)",
    "TBATS",
    "ETS",
    "TSLM",
    "naive",
    "Holt-Winters auto",
    "Holt-Winters adj"
  ),
  cex = 1.3,
  bty = "n",
  fill = rainbow(10)
)


# diagram porównujący MPE różnych modeli
ARIMA.compare.Theil <-
  c(
    ARIMA.model1.postResample[7],
    ARIMA.model2.postResample[7],
    ARIMA.model3.postResample[7],
    ARIMA.model4.postResample[7],
    TBATS.model1.postResample[7],
    ETS.auto.postResample[7],
    TSLM.trend.season.postResample[7],
    SNAIVE.model.postResample[7],
    HW.auto.postResample[7],
    HW.model1.postResample[7]
  )
names(ARIMA.compare.Theil) <- c(
  "ARIMA(29,1,0)",
  "ARIMA(0,1,27)",
  "auto.arima",
  "ARIMA(8,1,8)",
  "TBATS",
  "ETS",
  "TSLM",
  "naive",
  "Holt-Winters auto",
  "Holt-Winters adj"
)

barplot(ARIMA.compare.Theil, col = rainbow(10), main = "Błąd: kryterium Theila")
legend(
  "topleft",
  c(
    "ARIMA(29,1,0)",
    "ARIMA(0,1,27)",
    "auto.arima",
    "ARIMA(8,1,8)",
    "TBATS",
    "ETS",
    "TSLM",
    "naive",
    "Holt-Winters auto",
    "Holt-Winters adj"
  ),
  cex = 1.3,
  bty = "n",
  fill = rainbow(10)
)

# tabelka porównująca błędy
rossmann.ts.test.compare <-
  data.table(
    RMSE = c(
      prettyNum(format(round(ARIMA.model1.postResample[2], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(ARIMA.model2.postResample[2], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(ARIMA.model3.postResample[2], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(ARIMA.model4.postResample[2], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(TBATS.model1.postResample[2], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(ETS.auto.postResample[2], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(TSLM.trend.season.postResample[2], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(SNAIVE.model.postResample[2], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(HW.auto.postResample[2], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(HW.model1.postResample[2], 2), nsmall = 2),big.mark=" ",scientific=FALSE)
    ),
    
    MAE = c(
      prettyNum(format(round(ARIMA.model1.postResample[3], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(ARIMA.model2.postResample[3], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(ARIMA.model3.postResample[3], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(ARIMA.model4.postResample[3], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(TBATS.model1.postResample[3], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(ETS.auto.postResample[3], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(TSLM.trend.season.postResample[3], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(SNAIVE.model.postResample[3], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(HW.auto.postResample[3], 2), nsmall = 2),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(HW.model1.postResample[3], 2), nsmall = 2),big.mark=" ",scientific=FALSE)
    ),
    MAPE = c(
      paste(prettyNum(format(round(ARIMA.model1.postResample[5], 2), nsmall = 2),big.mark=" ",scientific=FALSE),"%"),
      paste(prettyNum(format(round(ARIMA.model2.postResample[5], 2), nsmall = 2),big.mark=" ",scientific=FALSE),"%"),
      paste(prettyNum(format(round(ARIMA.model3.postResample[5], 2), nsmall = 2),big.mark=" ",scientific=FALSE),"%"),
      paste(prettyNum(format(round(ARIMA.model4.postResample[5], 2), nsmall = 2),big.mark=" ",scientific=FALSE),"%"),
      paste(prettyNum(format(round(TBATS.model1.postResample[5], 2), nsmall = 2),big.mark=" ",scientific=FALSE),"%"),
      paste(prettyNum(format(round(ETS.auto.postResample[5], 2), nsmall = 2),big.mark=" ",scientific=FALSE),"%"),
      paste(prettyNum(format(round(TSLM.trend.season.postResample[5], 2), nsmall = 2),big.mark=" ",scientific=FALSE),"%"),
      paste(prettyNum(format(round(SNAIVE.model.postResample[5], 2), nsmall = 2),big.mark=" ",scientific=FALSE),"%"),
      paste(prettyNum(format(round(HW.auto.postResample[5], 2), nsmall = 2),big.mark=" ",scientific=FALSE),"%"),
      paste(prettyNum(format(round(HW.model1.postResample[5], 2), nsmall = 2),big.mark=" ",scientific=FALSE),"%")
    ),
    ACF1 = c(
      prettyNum(format(round(ARIMA.model1.postResample[6], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(ARIMA.model2.postResample[6], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(ARIMA.model3.postResample[6], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(ARIMA.model4.postResample[6], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(TBATS.model1.postResample[6], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(ETS.auto.postResample[6], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(TSLM.trend.season.postResample[6], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(SNAIVE.model.postResample[6], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(HW.auto.postResample[6], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(HW.model1.postResample[6], 3), nsmall = 3),big.mark=" ",scientific=FALSE)
    ),
    Theil = c(
      prettyNum(format(round(ARIMA.model1.postResample[7], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(ARIMA.model2.postResample[7], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(ARIMA.model3.postResample[7], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(ARIMA.model4.postResample[7], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(TBATS.model1.postResample[7], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(ETS.auto.postResample[7], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(TSLM.trend.season.postResample[7], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(SNAIVE.model.postResample[7], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(HW.auto.postResample[7], 3), nsmall = 3),big.mark=" ",scientific=FALSE),
      prettyNum(format(round(HW.model1.postResample[7], 3), nsmall = 3),big.mark=" ",scientific=FALSE)
    )
  )


myt <- ttheme_default(core = list(fg_params = list(hjust = 1, x = 1)))

grid.table(
  rossmann.ts.test.compare,
  theme = myt,
  rows = c(
    "ARIMA(29,1,0)",
    "ARIMA(0,1,27)",
    "auto.arima" ,
    "ARIMA(8,1,8)",
    "TBATS",
    "ETS",
    "TSLM",
    "SNAIVE",
    "Holt-Winters auto",
    "Holt-Winters adj"
  )
)