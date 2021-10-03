# Estimation of ARMA models with the tseries package

library(tseries)
library(seasonal)

y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)
seasX <- seas(yy)
cvs <- final(seasX)
yy <- cvs

dyy <- diff(yy, differences = 1)

library(tseries)
out6=arma(dyy,order=c(1,0))
show(out6)
out6$vcov

library(tseries)
out4=arma(dyy,order=c(1,1))
show(out4)
out4$vcov
