##  bayesian-structural-time-series: bsts package

y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)

ss <- AddLocalLinearTrend(list(), yy)
ss <- AddTrig(ss, yy, period = 12, frequencies = 1:3)
model <- bsts(yy, state.specification = ss, niter = 500)
plot(model)
plot(model, "components")


**************************
Specifications
**************************
# Add a local linear trend model to a state speciﬁcation as in BSM
# The local linear trend model assumes that both the mean and the slope of the trend follow random walks
ss <- AddLocalLinearTrend(list(), yy)

# Add an AR(p) state component to the state speciﬁcation
ss <- AddAr(list(),yy,lag=1)

# The semi-local linear trend model is similar to the local linear trend, 
# but more useful for long-term forecasting
ss <- AddSemilocalLinearTrend(ss, yy)

# Add a local level model to a state speciﬁcation
# The local level model assumes the trend is a random walk
ss <- AddLocalLevel(ss,yy)

# Add a trigonometric seasonal model to a state speciﬁcation as in BSM
ss <- AddTrig(ss, yy, period = 12, frequencies = 1:3)
ss <- AddTrig(ss, yy, period = 12)

# Add a seasonal model to a state speciﬁcation
# The seasonal model is as a regression on dummy variables
ss <- AddSeasonal(ss, yy, nseasons = 12)


**************************
# Residuals
**************************
burn<-SuggestBurn(0.1, model)
bsts_res<- as.numeric(residuals(model,burn=burn,mean.only=T))
acf(bsts_res)
Box.test(bsts_res)

**************************
# Forecasting
**************************
pred <- predict(model, horizon = 12, burn = 100)
plot(pred)
bsts_fore <- pred$mean


plot(model, "coefficients")
plot(model, "predictors")