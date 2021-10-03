# Using R code for ETS methods with forecast package
# ETS: Error, Trend, Seasonal

library(forecast)
y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)

fitets <- ets(yy)
show(fitets)
plot(fitets)

*******************************************************
# Forecasting with h number of periods for forecasting
*******************************************************
prevets <- forecast(fitets,12)
show(prevets)


******************************************************
# Commandes
******************************************************
# Usage (options par defaut)
ets(y, model = "ZZZ", damped = NULL, alpha = NULL, beta = NULL, gamma = NULL, phi = NULL, additive.only = FALSE, lambda = NULL,
biasadj = FALSE, lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999, 3), 0.98), opt.crit = c("lik", "amse", "mse", "sigma", "mae"),
nmse = 3, bounds = c("both", "usual", "admissible"), ic = c("aicc", "aic", "bic"), restrict = TRUE,
allow.multiplicative.trend = FALSE, use.initial.values = FALSE, na.action = c("na.contiguous", "na.interp", "na.fail"), ...)

damped If TRUE, use a damped trend (either additive or multiplicative). 
If NULL, both damped and non-damped trends will be tried and the best model (according to the information criterion ic ) returned.

alpha Value of alpha. If NULL, it is estimated.

beta Value of beta. If NULL, it is estimated.

gamma Value of gamma. If NULL, it is estimated.

phi Value of phi. If NULL, it is estimated.

additive.only If TRUE, will only consider additive models. Default is FALSE.

lambda Box-Cox transformation parameter. If lambda="auto" , then a transformation is
automatically selected using BoxCox.lambda . 
The transformation is ignored if NULL. Otherwise, data transformed before model is estimated. 
When lambda is speciﬁed, additive.only is set to TRUE .
