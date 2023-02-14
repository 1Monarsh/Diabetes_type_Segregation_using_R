library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)
library(remotes)
install_github("joshuaulrich/quantmod@358-getsymbols-new.session")
getSymbols('SPY', from='2021-10-01', to='2022-04-09', src='yahoo')
class(SPY)
SPY_Close_Prices = SPY[,4]
plot(SPY_Close_Prices)
class(SPY_Close_Prices)
par(mfrow=c(1,2))
Acf(SPY_Close_Prices, main='ACF for Differenced Series')
Pacf(SPY_Close_Prices, main='PACF for Differenced Series')
print(adf.test(SPY_Close_Prices))
auto.arima(SPY_Close_Prices, seasonal = FALSE)

fitA = auto.arima(SPY_Close_Prices, seasonal = FALSE)
tsdisplay(residuals(fitA), lag.max=40, main='(3,1,4) Model Residuals')

fitB = arima(SPY_Close_Prices, order=c(1,2,4))
tsdisplay(residuals(fitB), lag.max=40, main='(1,2,4) Model Residuals')

fitC = arima(SPY_Close_Prices, order=c(5,1,4))
tsdisplay(residuals(fitC), lag.max=40, main='(5,1,4) Model Residuals')

fitD = arima(SPY_Close_Prices, order=c(1,1,1))
tsdisplay(residuals(fitD), lag.max=40, main='(1,1,1) Model Residuals')


par(mfrow=c(2,2))

term <-10
fcast1 <- forecast(fitA, h=term)
plot(fcast1)

fcast2 <- forecast(fitB, h=term)
plot(fcast2)
fcast3 <- forecast(fitC, h=term)
plot(fcast3)
fcast4 <- forecast(fitD, h=term)
plot(fcast4)

accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)
accuracy(fcast4)
