# Using R code for SARIMA models with stats package
# June 2017
# les données sont dans c:\R\data
# Monthly data
library(seasonal)


y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)
seasX <- seas(yy)
cvs <- final(seasX)
yy <- cvs

y <- read.table("c:\\R\\data\\demandcvs.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)


# estimation d'un AR(p)
out=ar(yy,aic=TRUE,order.max=NULL)
show(out)

# if aic=TRUE: best AR model from AIC
# if aic=FALSE: the model of order order.max is fitted
# include.mean = TRUE: intercept/mean term in ARMA model (not ARIMA)
# ar.ols: with an intercept in the model

# out$coef : estimation des coefficients
# out$aic : valeur du critère AIC
# out$resid : estimation des résidus
-------------------------------------------

# Example
out=ar(yy,aic=TRUE,order.max=NULL)
show(out)

Output
Call:
ar(x = yy, aic = TRUE, order.max = 12)

Coefficients:
      1        2        3        4        5  
 1.0739   0.0597  -0.0647   0.0697  -0.1485  

Order selected 5  sigma^2 estimated as  2074
-------------------------------------------

# estimation ARMA(p,q) avec p et q fixé
out=arima(serie,order=c(p,0,q))

# estimation ARIMA(p,d,q)
out=arima(serie,order=c(p,d,q))

# estimation SARIMA(p,d,q,T) avec T la fréquence (12 for monthly data)
out=arima(serie,order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=T))

# include.mean: Should the ARMA model include a mean/intercept term? 
The default is TRUE for undifferenced series, and it is ignored for ARIMA models with differencing
-------------------------------------------

# Example 
out=arima(yy,order=c(1,1,1),seasonal=list(order=c(1,0,1),period=12))
show(out)

Output
Call:
arima(x = yy, order = c(1, 1, 1), seasonal = list(order = c(1, 0, 1), period = 12))

Coefficients:
         ar1      ma1    sar1     sma1
      0.9638  -0.6954  0.7048  -0.9389
s.e.  0.0197   0.0440  0.0958   0.0831

sigma^2 estimated as 286.8:  log likelihood = -1121.25,  aic = 2252.51

*********************************************
*********************************************

# prévision à l'horizon h
p=predict(out,h)

# p$pred: prévisions
# p$s: écart-type de l’erreur de prévision

# remarques
# command ar: no predict with h=12, only h=1
# then use arima(1,0,0) for predict h=12
--------------------------------------------

# Example
p=predict(out,12)
show(p)
$pred

# output
          Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
2018 3439.795 3429.135 3416.177 3416.183 3396.695 3396.295 3376.974 3359.444
          Sep      Oct      Nov      Dec
2018 3362.190 3356.764 3351.656 3343.851

$se
           Jan       Feb       Mar       Apr       May       Jun       Jul
2018  16.96806  27.40235  37.70806  48.26788  59.15879  70.38249  81.91716
           Aug       Sep       Oct       Nov       Dec
2018  93.73369 105.80165 118.09157 130.57584 143.22899
--------------------------------------------

plot(yy, col="orange")
lines(p$pred,col="red3")
lines(p$pred+1.96*p$se, col="blue2")
lines(p$pred-1.96*p$se, col="blue2")

