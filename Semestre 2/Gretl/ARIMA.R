library(readxl)
library(quantmod)
library(timeSeries)
library(forecast)
library(xts)
library(ggplot2)
library(tseries)

setwd("D:/M1 EKAP/Semestre 2/Gretl")

mydata <- read.csv("data.csv")

WTI <- ts(mydata$WTI, start=c(2012,1,03),frequency=5)
SPGSCI <- ts(mydata$SPGSCI,start=c(2012,1,03), frequency=5)
SP500 <- ts(mydata$SP500,start=c(2012,1,03), frequency=5)

plot(WTI)

# log return and plot

diff_WTI = diff(log(WTI),lag=1)
plot(diff_WTI)
diff_SPGSCI = diff(log(SPGSCI),lag=1)
plot(diff_SPGSCI)
diff_SP500 = diff(log(SP500),lag=1)
plot(diff_SP500)

# Conduct ADF test on log returns series 
print(adf.test(diff_WTI)) #p.value < 0.05 -> stationary
print(adf.test(diff_SPGSCI)) #p.value < 0.05 -> stationary
print(adf.test(diff_SP500)) #p.value < 0.05 -> stationary

# Split the dataset in two parts - training and testing
breakpoint <- floor(length(diff_SPGSCI)*(2.9/3))

# Apply the ACF and PACF functions
par(mfrow = c(1,2))
acf.stock = acf(diff_WTI[c(1:breakpoint)], main='ACF Plot WTI', lag.max=30)
pacf.stock = pacf(diff_WTI[c(1:breakpoint)], main='PACF Plot WTI', lag.max=30)

acf.stock = acf(diff_SPGSCI[c(1:breakpoint)], main='ACF Plot SPGSCI', lag.max=30)
pacf.stock = pacf(diff_SPGSCI[c(1:breakpoint)], main='PACF Plot SPGSCI', lag.max=30)

acf.stock = acf(diff_SP500[c(1:breakpoint)], main='ACF Plot SP500', lag.max=30)
pacf.stock = pacf(diff_SP500[c(1:breakpoint)], main='PACF Plot SP500', lag.max=30)

# Summary of the ARIMA model using the determined (p,d,q) parameters
fit_WTI <- arima(WTI, order = c(1, 1, 0), seasonal=list(order=c(0,0,0), period=5))
summary(fit_WTI)

fit_SPGSCI <- arima(SPGSCI[-188], order = c(0, 1, 0), seasonal=list(order=c(1,0,0), period=5))
summary(fit_SPGSCI)

fit_SP500 <- arima(SP500, order = c(1, 1, 0), seasonal=list(order=c(0,0,0), period=5))
summary(fit_SP500)

# plotting a acf plot of the residuals
acf(fit_WTI$residuals,main="Residuals plot WTI")
acf(fit_SPGSCI$residuals,main="Residuals plot SPGSCI")
acf(fit_SP500$residuals,main="Residuals plot SP500")

# Forecasting the log returns
arima.forecast <- forecast(fit_SPGSCI, h = 3,level=99)
summary(arima.forecast)

# plotting the forecast
par(mfrow=c(1,1))
plot(arima.forecast, main = "ARIMA Forecast")

# Creating a series of forecasted returns for the forecasted period
forecasted_series = rbind(forecasted_series,arima.forecast$mean[1])
colnames(forecasted_series) = c("Forecasted")

# Creating a series of actual returns for the forecasted period
Actual_return = stock[(b+1),]
Actual_series = c(Actual_series,xts(Actual_return))
rm(Actual_return)
print(stock_prices[(b+1),])
print(stock_prices[(b+2),])
