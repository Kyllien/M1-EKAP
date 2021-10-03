library(readxl)
library(ggplot2)
library(forecast)
library(tsoutliers)
library(dbplyr)
library(EnvStats)
library(tseries)
library(seasonal)
library(seastests)
library(stringr)

setwd("D:/M1 EKAP/Semestre 2/Tech De Prévision")
dt <- read_excel("data.xlsx")

#Inversion de la table 
c <- 1
for (i in nrow(dt):1){
  dt[c,3] <- dt[i,2]
  c <- c + 1
}

data <- ts(dt[,3], start=c(2011,01),frequency=12)
plot(data)


#Valeur atypique
library(stargazer)
boxplot(data) #No detect

fit <- tso(data)
plot(fit)
show(fit)
#Il y a une valeur atypique
adj <- fit$yadj
plot(adj)
write(t(adj),file="ipi1984_TC.out",ncolumn=1,append=FALSE)
data <- adj
#Statistique descriptive
plot(data,main="Plot du nombre de nuités en fonction des mois", ylab="nombre de nuités totales")
mean(data)
sd(data)
skewness(data)
kurtosis(data)

#Detection de la saisonnalité
par(mfrow=c(2,1))
fried(data) #Friedman Test
seasdum(data) #Seasonnal Dummies
welch(data)



# Testing the seasonality of series
#  a boolean value is returned : TRUE or FALSE
isSeasonal(data, test="wo")

#Additive
out(seas(data))

#Decomposition
decomp <- tbats(data)
plot(decomp)

# Extract components of a TBATS model
comp <- tbats.components(decomp)
plot(comp)

# Returns seasonally adjusted data constructed by removing the seasonal component
seas <- seasadj(decomp)
plot(seas)


# Seasonal adjustment
# By default, seas calls the SEATS adjustment procedure
seasX <- seas(data)
# Final CVS
final(seasX)
plot(seasX)
summary(seasX)
#detection nouveau outlier et chgt
tsoutliers(data)
data[43,] <- 507

seasX <- seas(data)
plot(seasX)

summary(seasX)

plot(irregular(seasX))
plot(trend(seasX))

# # The X11 adjustment procedure
# seasX11 <- seas(data,  x11 = "")
# # Final CVS
# final(seasX11)
# plot(seasX11)
# summary(seasX11)
# 
# # The udg function provides access to a large number of diagnostical statistics
# udg(seasX, "x13mdl")
# 
# # The out function shows the content of the main output using the HTML version of X-13
# out(seasX)

# # The view function is a graphical tool for choosing a seasonal adjustment model
# install.packages("seasonalview")
# library(seasonalview)
# view(seasX)

#Question 3 
yy <- ts(data[1:96,], start=c(2011,01),frequency=12)
test <- ts(dt[97:108,3], start=c(2019,01),frequency=12)

# Forecasting on 3 years
library(forecast)
seasX <- seas(yy)
s <- series(seasX,"fct")
plot(s)

# Forecasting with h number of periods for forecasting with tbats
fittbats = tbats(yy)
plot(fittbats)
prevtbats <- forecast(fittbats,12)
show(prevtbats)
plot(prevtbats)
#Extract component from tbats model
comp <- tbats.components(fittbats)
plot(comp)
show(comp)

# Forecasting with h number of periods for forecasting with StructTS
fitsts = StructTS(yy)
prevsts <- forecast(fitsts,12)
show(prevsts)
plot(prevsts)

# Forecasting with h number of periods for forecasting with stl
fitstl = stlm(yy)
prevstl <- forecast(fitstl,12)
show(prevstl)
plot(prevstl)

#BSTS
library(bsts)
ss <- AddLocalLinearTrend(list(), yy)
ss <- AddTrig(ss, yy, period = 12, frequencies = 1:3)
model <- bsts(yy, state.specification = ss, niter = 500)
plot(model)
pred <- predict(model, horizon = 12, burn = 100)
plot(pred)

#Question b 
# Forecasting with h number of periods for forecasting ets
fitets <- ets(yy)
prevets <- forecast(fitets,12) #si pas pas de phi, pas de tendance linéaire, beta pente, expliquer le modele en fonction des variables
show(prevets) #il faut justifier le modele pour comprendre derriere
plot(prevets)

#Holt Winters with time series package
library(seasonal)
library(timeSeries)
seasX <- seas(yy)
cvs <- final(seasX)
# lissage
m = HoltWinters(cvs)
show(m)
summary(m)
plot(m)
plot(m$fitted[,1])
# horizon h=50 - intervals 95%
p = predict(m, 12, prediction.interval = TRUE)
plot(m, p)
show(p)
prevp = p[1]
show(prevp)

forecast(yy,12)

#NNETAR

NN <- nnetar(yy)
NNETAR <- forecast(NN,12)
plot(forecast(NN,12))

#SARIMA(p,d,q)(P,D,Q)_S
out3 <- auto.arima(yy)
show(out3)
SARIMA <- forecast(out3,12)
plot(SARIMA)



#SARIMA(0,1,1)(0,1,1)
out4 <- arima(yy,order = c(0,1,1), seasonal = list(order = c(0,1,1),period = 12))
sarima2 <- forecast(out4,12)
plot(sarima2)
show(out4)

#Prevision naive
naive <- rwf(yy,h=12)
plot(naive)
naiveS <- snaive(yy,12)
plot(naiveS)


#MSE
mse <- function(err){mean(err*err)}
erreur1 <- test - sarima2$mean
sarima2MSE <- mse(erreur1)
sarima2MSE

erreur2 <- test - SARIMA$mean
sarima1MSE <- mse(erreur2)
sarima1MSE

erreurNaive <- test - naive$mean
naiveMSE <- mse(erreurNaive)
naiveMSE

erreurNaiveS <- test - naiveS$mean
naiveSMSE <- mse(erreurNaiveS)
naiveSMSE

erreurNnetar <- test - NNETAR$mean
nnetarMSE <- mse(erreurNnetar)
nnetarMSE

erreurHW <- test - p[,1]
hwMSE <- mse(erreurHW)
hwMSE

erreurETS <- test - prevets$mean
etsMSE <- mse(erreurETS)
etsMSE

erreurBSTS <- test - pred$mean
bstsMSE <- mse(erreurBSTS)
bstsMSE

erreurSTL <- test - prevstl$mean
stlMSE <- mse(erreurSTL)
stlMSE

erreurSTS <- test - prevsts$mean
stsMSE <- mse(erreurSTS)
stsMSE

erreurTBATS <- test - prevtbats$mean
tbatsMSE <- mse(erreurTBATS)
tbatsMSE

erreurX13 <- test - s[1:12,1]
X13SMSE <- mse(erreurX13)
X13SMSE

#DM Test

#NAIVE 
dm.test(residuals(sarima2),residuals(naiveS),h=12, alternative = "l")#Moins bonne
dm.test(residuals(SARIMA),residuals(naiveS),h=12, alternative = "l")#Moins bonne
dm.test(residuals(NNETAR),residuals(naiveS),h=12, alternative = "l")#Moins bonne
dm.test(residuals(m),residuals(naiveS),h=12, alternative = "l") 
dm.test(residuals(prevets),residuals(naiveS),h=12, alternative = "l")#Moins Bonne
dm.test(residuals(model),residuals(naiveS),h=12, alternative = "l") #BSTS ne fonctionne pas
dm.test(residuals(prevstl),residuals(naiveS),h=12, alternative = "l")#stl moins bonne que naive
dm.test(residuals(prevsts),residuals(naiveS),h=12, alternative = "l") 
dm.test(residuals(prevtbats),residuals(naiveS),h=12, alternative = "l") 
dm.test(residuals(seasX),residuals(naiveS),h=12, alternative = "l")#X13 moins bonne

#SARIMA (p,d,q)
dm.test(residuals(sarima2),residuals(SARIMA),h=12, alternative = "l")
dm.test(residuals(NNETAR),residuals(SARIMA),h=12, alternative = "l")
dm.test(residuals(prevets),residuals(SARIMA),h=12, alternative = "l")
dm.test(residuals(prevstl),residuals(SARIMA),h=12, alternative = "l")
dm.test(residuals(seasX),residuals(SARIMA),h=12, alternative = "l")
dm.test(residuals(m),residuals(SARIMA),h=12, alternative = "l")
dm.test(residuals(prevsts),residuals(SARIMA),h=12, alternative = "l")
dm.test(residuals(prevtbats),residuals(SARIMA),h=12, alternative = "l")

#SARIMA (0,1,1)
dm.test(residuals(SARIMA),residuals(sarima2),h=12, alternative = "l")
dm.test(residuals(NNETAR),residuals(sarima2),h=12, alternative = "l")
dm.test(residuals(prevets),residuals(sarima2),h=12, alternative = "l")
dm.test(residuals(prevstl),residuals(sarima2),h=12, alternative = "l")
dm.test(residuals(seasX),residuals(sarima2),h=12, alternative = "l")
dm.test(residuals(m),residuals(sarima2),h=12, alternative = "l")
dm.test(residuals(prevsts),residuals(sarima2),h=12, alternative = "l")
dm.test(residuals(prevtbats),residuals(sarima2),h=12, alternative = "l")

#NNETAR
dm.test(residuals(SARIMA),residuals(NNETAR),h=12, alternative = "l")
dm.test(residuals(sarima2),residuals(NNETAR),h=12, alternative = "l")
dm.test(residuals(prevets),residuals(NNETAR),h=12, alternative = "l")
dm.test(residuals(prevstl),residuals(NNETAR),h=12, alternative = "l")
dm.test(residuals(seasX),residuals(NNETAR),h=12, alternative = "l")
dm.test(residuals(m),residuals(NNETAR),h=12, alternative = "l")
dm.test(residuals(prevsts),residuals(NNETAR),h=12, alternative = "l")
dm.test(residuals(prevtbats),residuals(NNETAR),h=12, alternative = "l")

#ETS
dm.test(residuals(SARIMA),residuals(prevets),h=12, alternative = "l")
dm.test(residuals(sarima2),residuals(prevets),h=12, alternative = "l")
dm.test(residuals(NNETAR),residuals(prevets),h=12, alternative = "l")
dm.test(residuals(prevstl),residuals(prevets),h=12, alternative = "l")
dm.test(residuals(seasX),residuals(prevets),h=12, alternative = "l")
dm.test(residuals(m),residuals(prevets),h=12, alternative = "l")
dm.test(residuals(prevsts),residuals(prevets),h=12, alternative = "l")
dm.test(residuals(prevtbats),residuals(prevets),h=12, alternative = "l")

#STL
dm.test(residuals(SARIMA),residuals(prevstl),h=12, alternative = "l")
dm.test(residuals(sarima2),residuals(prevstl),h=12, alternative = "l")
dm.test(residuals(NNETAR),residuals(prevstl),h=12, alternative = "l")
dm.test(residuals(prevets),residuals(prevstl),h=12, alternative = "l")
dm.test(residuals(seasX),residuals(prevstl),h=12, alternative = "l")
dm.test(residuals(m),residuals(prevstl),h=12, alternative = "l")
dm.test(residuals(prevsts),residuals(prevstl),h=12, alternative = "l")
dm.test(residuals(prevtbats),residuals(prevstl),h=12, alternative = "l")

#X13
dm.test(residuals(SARIMA),residuals(seasX),h=12, alternative = "l")
dm.test(residuals(sarima2),residuals(seasX),h=12, alternative = "l")
dm.test(residuals(NNETAR),residuals(seasX),h=12, alternative = "l")
dm.test(residuals(prevets),residuals(seasX),h=12, alternative = "l")
dm.test(residuals(prevstl),residuals(seasX),h=12, alternative = "l")
dm.test(residuals(m),residuals(seasX),h=12, alternative = "l")
dm.test(residuals(prevsts),residuals(seasX),h=12, alternative = "l")
dm.test(residuals(prevtbats),residuals(seasX),h=12, alternative = "l")

#HW
dm.test(residuals(SARIMA),residuals(m),h=12, alternative = "l")
dm.test(residuals(sarima2),residuals(m),h=12, alternative = "l")
dm.test(residuals(NNETAR),residuals(m),h=12, alternative = "l")
dm.test(residuals(prevets),residuals(m),h=12, alternative = "l")
dm.test(residuals(prevstl),residuals(m),h=12, alternative = "l")
dm.test(residuals(seasX),residuals(m),h=12, alternative = "l")
dm.test(residuals(prevsts),residuals(m),h=12, alternative = "l")
dm.test(residuals(prevtbats),residuals(m),h=12, alternative = "l")

#STS
dm.test(residuals(SARIMA),residuals(prevsts),h=12, alternative = "l")
dm.test(residuals(sarima2),residuals(prevsts),h=12, alternative = "l")
dm.test(residuals(NNETAR),residuals(prevsts),h=12, alternative = "l")
dm.test(residuals(prevets),residuals(prevsts),h=12, alternative = "l")
dm.test(residuals(prevstl),residuals(prevsts),h=12, alternative = "l")
dm.test(residuals(m),residuals(prevsts),h=12, alternative = "l")
dm.test(residuals(seasX),residuals(prevsts),h=12, alternative = "l")
dm.test(residuals(prevtbats),residuals(prevsts),h=12, alternative = "l")

#TBATS
dm.test(residuals(SARIMA),residuals(prevtbats),h=12, alternative = "l")
dm.test(residuals(sarima2),residuals(prevtbats),h=12, alternative = "l")
dm.test(residuals(NNETAR),residuals(prevtbats),h=12, alternative = "l")
dm.test(residuals(prevets),residuals(prevtbats),h=12, alternative = "l")
dm.test(residuals(prevstl),residuals(prevtbats),h=12, alternative = "l")
dm.test(residuals(m),residuals(prevtbats),h=12, alternative = "l")
dm.test(residuals(seasX),residuals(prevtbats),h=12, alternative = "l")
dm.test(residuals(prevsts),residuals(prevtbats),h=12, alternative = "l")

#Graphique avec toute les previsions
test2 <- cbind(test,modele="test",Date=1:12)
s2<- cbind(sarima2$mean,modele="SARIMA (0,1,1)",Date=1:12)
s1 <- cbind(SARIMA$mean,modele="SARIMA (p,d,q)",Date=1:12)
Naive <- cbind(naiveS$mean,modele="Naive",Date=1:12)
NN <- cbind(NNETAR$mean,modele="NNETAR",Date=1:12)
HW <- cbind(p[,1],modele="Holt Winter",Date=1:12)
ETS <- cbind(prevets$mean,modele="ETS",Date=1:12)
STL <- cbind(prevstl$mean,modele="STL",Date=1:12)
STS <- cbind(prevsts$mean,modele="STS",Date=1:12)
Tbats <- cbind(prevtbats$mean,modele="Tbats",Date=1:12)
X13 <- ts(cbind(s[1:12,1],modele="X13",Date=1:12),start=c(2019,01),frequency=12)

prevision <- data.frame(rbind(test2,s2,s1,Naive,NN,HW,ETS,STL,Tbats,X13))
colnames(prevision) <- c("valeur","modele","date")
prevision$valeur <- as.numeric(prevision$valeur)
prevision$date <- as.numeric(prevision$date)
library(ggplot2)
ggplot(data=prevision, aes( x=date,y=valeur, color=modele, shape=modele)) +
  geom_line(cex=0.01) +
  theme_bw() + ggtitle("Graphique des différentes prévisions, comparé à la valeur réelle") +
  xlab("Temps") + ylab("Nombre de nuité en Corse")

