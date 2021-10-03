# Using R code for seasonal decomposition with forecast package
# TBATS model: Exponential smoothing state space model with Box-Cox transformation, 
# ARMA errors, Trend and Seasonal components
# January 2018

library(forecast)

# les données sont dans c:\R\data
# Monthly data
y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)

******************************************
Decomposition
******************************************

# Fits a TBATS model applied to y , as described in De Livera, Hyndman & Snyder (2011). 
# Parallel processing is used by default to speed up the computations.

decomp <- tbats(yy)
show(decomp)
plot(decomp)

# Extract components of a TBATS model
comp <- tbats.components(decomp)
plot(comp)
show(comp)

# Returns seasonally adjusted data constructed by removing the seasonal component
seas <- seasadj(decomp)
plot(seas)

******************************************
Forecasting
******************************************

# Forecasting with h number of periods for forecasting with tbats
fittbats = tbats(yy)
prevtbats <- forecast(fittbats,12)
show(prevtbats)
plot(prevtbats)

# Forecasting with h number of periods for forecasting with StructTS
fitsts = StructTS(yy)
prevsts <- forecast(fitsts,12)
show(prevsts)
plot(prevsts)

# Forecasting with h number of periods for forecasting with stlm
fitstl = stlm(yy)
prevstl <- forecast(fitstl,12)
show(prevstl)
plot(prevstl)

# Forecasting with h number of periods for forecasting with stlf
yy <- ts(y[1:264,1],frequency=12)
prevstl = stlf(yy,12)
show(prevstl)
plot(prevstl)






