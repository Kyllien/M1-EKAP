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
library(xts)
library(zoo)
library(moments)

setwd("D:/M1 EKAP/Semestre 2/Tech De Prévision")
dt <- read_excel("Projet2.xlsx")
dt$date <- as.Date(dt$date)
data <- xts(dt[,-1],order.by=dt$date)

#outliers ----

 #No detect
tsoutliers(data$BRENT) #47 48 53, 119.6133 116.4267 47.945
tsoutliers(data$STOCK) #No
tsoutliers(data$PROD) #no
tsoutliers(data$ACT_MOND) # 52 53, -9.823754 -31.930892
tsoutliers(data$MSCI_index) # 55, 822.025
tsoutliers(data$Baltic_dry_index)# 7 41 42 43 45 48 49 50 51 54 55 56 57
#4760.00 8062.25 8422.50 8782.75 8378.00 7826.60 7572.20 7317.80 7063.40 2970.80 2724.60 2478.40 2232.20

outliers <- data[c(7,41,42,43,45,47,48,49,50,51,52,53,54,55,56,57)]
boxplot(dt$BRENT, main ="BoxPlot du BRENT", col="orange")

data$BRENT[c(47,48,53)] <- c(119.6133,116.4267,47.945)
data$ACT_MOND[c(52,53)] <- c(-9.823754,-31.930892)
data$MSCI_index[55] <- 822.025
data$Baltic_dry_index[c(7,41,42,43,45,48,49,50,51,54,55,56,57)] <- c(4760.00,8062.25,8422.50,8782.75,8378.00,7826.60,7572.20,7317.80,7063.40,2970.80,2724.60,2478.40,2232.20)

#Graphique
plot(data$BRENT, main ="Brent en dollars")

#Verification de la non saisonnalité
library(seastests)
isSeasonal(data$BRENT, test="wo",freq = 12)
isSeasonal(data$STOCK, test="wo",freq = 12)
isSeasonal(data$ACT_MOND, test="wo",freq = 12)
isSeasonal(data$MSCI_index, test="wo",freq = 12)
isSeasonal(data$Baltic_dry_index, test="wo",freq = 12)

#stationnarité ----
adf.test(data$BRENT)
adf.test(data$STOCK)
adf.test(data$PROD)
adf.test(data$ACT_MOND)
adf.test(data$MSCI_index)
adf.test(data$Baltic_dry_index)
#Tout est supérieur à 0.05 donc elle ne sont pas stationnaire


#donc mise en place d'une differenciation
data$BRENT <- log(data$BRENT)
data$MSCI_index <- log(data$MSCI_index)
data$Baltic_dry_index <- log(data$Baltic_dry_index)

data_diff <- diff(data,lag=1)
data_diff <- data_diff[-1,]
plot(data_diff$BRENT, main="Rendement Mensuel du Brent", col="green")


adf.test(data_diff$BRENT)
adf.test(data_diff$STOCK)
adf.test(data_diff$PROD)
adf.test(data_diff$ACT_MOND)
adf.test(data_diff$MSCI_index)
adf.test(data_diff$Baltic_dry_index)
#Elles sont toute stationnaire au seuil de risque 1%

#Satistique Descriptive ----
tsoutliers(data_diff$BRENT) 
tsoutliers(data_diff$STOCK)
tsoutliers(data_diff$PROD)
tsoutliers(data_diff$ACT_MOND) # 50 , -30.88225
tsoutliers(data_diff$MSCI_index) #???50 -0.09945881
tsoutliers(data_diff$Baltic_dry_index) #92 0.01884715
data_diff$ACT_MOND[50,] <- -30.88225
data_diff$MSCI_index[50,] <- -0.09945881
data_diff$Baltic_dry_index[92,] <- 0.01884715

library(fBasics)
round(basicStats(data_diff),2)



#Estimation des modèles linéaires
#AR(1)
library(forecast)
library(lmtest)
out1=Arima(data_diff$BRENT,order=c(1,0,0),seasonal=list(order=c(0,0,0),period=12),lambda=1)
show(out1)
coeftest(out1)

#AR(p)
out2=ar(data_diff$BRENT,aic=TRUE,order.max=12)
show(out2)#AR(1)
coeftest(out2)


#ARIMA(p,d,q)
out3=auto.arima(data_diff$BRENT)
show(out3)
coeftest(out3)

#LED Holtwinters
modelHW <- HoltWinters(data_diff$BRENT, gamma=FALSE)
modelHW


#residus des modèles
resAR1=residuals(out1)
resARp=residuals(out2)
resARpdq=residuals(out3)
resHW=residuals(modelHW)

library(gvlma)
library(RColorBrewer)

# AR1
tsdiag(out1)     # RÃ©sume info bruit blanc
Box.test(resAR1, type = c("Ljung-Box"))       # Test portmanteau : H0=IndÃ©pendance des rÃ©sidus

par(mfrow=c(1,3))
boxplot(resAR1, main="Boxplot des residus", col="#FF9966")
hist(resAR1, xlab="Residus", ylab="Frequences", main="Histogramme des residus", freq=TRUE, col=brewer.pal(n = 8, name = "Reds"))
qqnorm(resAR1)
qqline(resAR1, col="red")
ks.test(resAR1, "pnorm", mean(resAR1), sd(resAR1))       # HO : les rÃ©sidus suivent une loi normale


# ARp
tsdiag(out2)
Box.test(resARp, type = c("Ljung-Box"))

par(mfrow=c(1,3))
boxplot(resARp, main="Boxplot des residus", col="#FF9966")
hist(resARp, xlab="Residus", ylab="Frequences", main="Histogramme des rÃ©sidus", freq=TRUE, col=brewer.pal(n = 8, name = "Reds"))
qqnorm(resARp)
qqline(resARp, col="red")
ks.test(resARp, "pnorm", mean(resARp), sd(resARp))

#ARpdq

tsdiag(out3)
Box.test(resARpdq, type = c("Ljung-Box"))

par(mfrow=c(1,3))
boxplot(resARpdq, main="Boxplot des rÃ©sidus", col="#FF9966")
hist(resARpdq, xlab="Residus", ylab="Frequences", main="Histogramme des residus", freq=TRUE, col=brewer.pal(n = 8, name = "Reds"))
qqnorm(resARpdq)
qqline(resARpdq, col="red")
ks.test(resARpdq, "pnorm", mean(resARpdq), sd(resARpdq))


# HW
par(mfrow=c(2,1))
acf(resHW)
pacf(resHW)
Box.test(resHW, type = c("Ljung-Box"))

par(mfrow=c(1,3))
boxplot(resHW, main="Boxplot des residus", col="#FF9966")
hist(resHW, xlab="Residus", ylab="Frequences", main="Histogramme des residus", freq=TRUE, col=brewer.pal(n = 8, name = "Reds"))
qqnorm(resHW)
qqline(resHW, col="red")
ks.test(resHW, "pnorm", mean(resHW), sd(resHW))

#Prediction à 1 mois sur l'année 2019
#on crée 2 tables les valeurs test et les valeurs à predire (2019)

test <- ts(data=data_diff$BRENT[-c(173:184),],start=c(2004,09), frequency=12)
reel <- ts(data=data_diff$BRENT[c(173:184),],start=c(2019,01), frequency=12)

#AR1
library(forecast)
AR1 <- test
for (i in 1:12){
  modelAR1 <- arima(AR1,order=c(1,0,0))
  prev <- forecast(model,1)
  p <- rbind(data.frame(Valeur=as.numeric(AR1)),data.frame(Valeur=as.numeric(prev$mean)))
  AR1 <- ts(p, start=c(2004,09),frequency=12)
}

#ARp
ARp <- test
for (i in 1:12){
  modelARp <- ar(ARp,AIC=TRUE)
  prev <- forecast(model,1)
  p <- rbind(data.frame(Valeur=as.numeric(ARp)),data.frame(Valeur=as.numeric(prev$mean)))
  ARp <- ts(p, start=c(2004,09),frequency=12)
}

#ARIMA(p,d,q)
ARIMAp <- test
for (i in 1:12){
  modelARIMA <- auto.arima(ARIMAp)
  prev <- forecast(model,1)
  p <- rbind(data.frame(Valeur=as.numeric(ARIMAp)),data.frame(Valeur=as.numeric(prev$mean)))
  ARIMAp <- ts(p, start=c(2004,09),frequency=12)
}

#Holt-Winters
HW <- test
for (i in 1:12){
  modelHW <- HoltWinters(HW,gamma =FALSE)
  prev <- forecast(model,1)
  p <- rbind(data.frame(Valeur=as.numeric(HW)),data.frame(Valeur=as.numeric(prev$mean)))
  HW <- ts(p, start=c(2004,09),frequency=12)
}


#Prevision avec variable explicative
#lm
model1<-lm(BRENT~STOCK+PROD+ACT_MOND+Baltic_dry_index+MSCI_index, data=data_diff[-c(173:184)])
summary(model1)

model2<-lm(BRENT~PROD+ACT_MOND+MSCI_index, data=data_diff[-c(173:184)])
summary(model2)

model3<-lm(BRENT~ACT_MOND+MSCI_index, data=data_diff[-c(173:184)])
summary(model3)

#MASS
library(MASS)
step1 <- stepAIC(model1, direction="both")
step2 <- stepAIC(model2, direction="both")
step3 <- stepAIC(model3, direction = "both")

#leaps
library(leaps)
leaps1 <- regsubsets(BRENT ~STOCK+PROD+ACT_MOND+Baltic_dry_index+MSCI_index, data= data_diff[-c(173:184)], nbest=1, nvmax=5, method=c("exhaustive"))
summary(leaps1)

# Choosing the optimal model
res.sum <- summary(leaps1)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)
plot(leaps1, scale="bic", main = "BIC")

#glmulti
dataglmulti <- data.frame(data_diff[-c(173:184),])
library(rJava)
library(glmulti)
multi <- glmulti(BRENT ~STOCK+PROD+ACT_MOND+Baltic_dry_index+MSCI_index, data= dataglmulti,
                 level = 1, method = "h", crit = "aic", confsetsize = 2, plotty = F, report = F, fitfunction = "lm")      
summary(multi)
show(multi)


#ARX avec AR(1)
library(tidyverse)
library(lgarch)
library(gets)
training <- data.frame(data_diff[-c(173:184),])
class(training[,2:6])
mX = data.matrix(training[,2:6])
modelArX <- arx(training$BRENT, mc = T, ar = 1, mxreg = mX[, 1:5], vcov.type = "white")

getsm <- getsm(modelArX)
getsm

#Meilleuir modele avec seulement MSCI et sans la constante
modelArX <- arx(training$BRENT, mc = F, ar = 1, mxreg = mX[, c(4)], vcov.type = "white")
getsm <- getsm(modelArX)
getsm

#Prevision du modele lineaire avec act monde et msci faite à la main à laide des coefficients calcule par le model de regression
summary(model3)
prevLM <- data.frame(Date=1:12,Valeur=1:12,Ticker="LM")
for (i in 1:12){
  prevLM[i,2] <- (-0.0013247) + (data_diff$ACT_MOND[172 + i]*0.0006551) + (data_diff$MSCI_index[172+i]*0.7696694) + 0.004694
}

#Prevision avec Arx

Y <- data_diff[,1]
X <- data.frame(data_diff[,5])

prevARx <- data.frame(Date=1:12,Valeur=1:12,Ticker = "ARx")
for (i in 1:12) {
  mX <- data.matrix(X[1:(171+i),])
  y <- data.matrix(Y[1:(171+i)])
  modelARX <- arx(y, mc = T, ar = 1, mxreg = mX, vcov.type = "white")
  prevARx[i,2] <- predict(ARX, n.ahead = 1, newmxreg = data.matrix(X[172+i,]))
}
prevARx

#Graphique
prevAR1 <- data.frame(Date=1:12,Valeur=AR1[c(173:184),],Ticker="AR1")
prevARp <- data.frame(Date=1:12,Valeur=ARp[c(173:184),],Ticker="ARp")
prevARIMA <- data.frame(Date=1:12,Valeur=ARIMAp[c(173:184),],Ticker="ARIMA")
prevHW <- data.frame(Date=1:12,Valeur=HW[c(173:184),],Ticker="HW")
valeurR <- data.frame(Date=1:12,Valeur=as.numeric(data_diff[c(173:184),1]),Ticker="ValeurR")

graphPrev <- rbind(prevAR1,prevARp,prevARIMA,prevHW,prevARx,prevLM,valeurR)

library(ggplot2)
ggplot(data=graphPrev, aes( x=Date,y=Valeur, color=Ticker, shape=Ticker)) +
  geom_line(cex=0.01) +
  theme_bw() + ggtitle("Graphique des différentes prévisions, comparé à la valeur réelle") +
  xlab("Temps") + ylab("Rendement")


#Erreur de prevision
#Prevision naive
naive <- rwf(data_diff[1:172,1],h=12)
plot(naive)

#MSE
mse <- function(err){mean(err*err)}
erreur1 <- valeurR[,2] - prevAR1[,2]
AR1MSE <- mse(erreur1)
AR1MSE

erreur1 <- valeurR[,2] - prevARp[,2]
ARpMSE <- mse(erreur1)
ARpMSE

erreur1 <- valeurR[,2] - prevARIMA[,2]
ARIMAMSE <- mse(erreur1)
ARIMAMSE

erreur1 <- valeurR[,2] - prevHW[,2]
HWMSE <- mse(erreur1)
HWMSE

erreur1 <- valeurR[,2] - prevARx[,2]
ARxMSE <- mse(erreur1)
ARxMSE

erreur1 <- valeurR[,2] - prevLM[,2]
LmMSE <- mse(erreur1)
LmMSE

erreur1 <- valeurR[,2] - naive$mean
naiveMSE <- mse(erreur1)
naiveMSE

MSE <- cbind(AR1MSE,ARpMSE,ARIMAMSE,HWMSE,ARxMSE,LmMSE,naiveMSE)
MSE

#Test de precision : dm test
data.frame(residuals(modelAR1))
resNaive <- residuals(naive)
resHW <- residuals(modelHW)[c(1:172)]
resArX <- residuals(modelArX)
resArP <- residuals(modelARp)[c(1:172)]
resArima <- residuals(modelARIMA)[c(1:172)]
resAr1 <- residuals(modelAR1)[c(1:172)]
resLM <- residuals(model3)[c(1:172)]

#Par rapport a la naive
dm.test(resAr1,resNaive,h=12, alternative = "l")
dm.test(resArP,resNaive,h=12, alternative = "l")
dm.test(resArima,resNaive,h=12, alternative = "l")
dm.test(resHW,resNaive,h=12, alternative = "l")
dm.test(resLM,resNaive,h=12, alternative = "l")
dm.test(resArX,resNaive,h=12, alternative = "l")

#AR1
dm.test(resArP,resAr1,h=12, alternative = "l")
dm.test(resArima,resAr1,h=12, alternative = "l")
dm.test(resHW,resAr1,h=12, alternative = "l")
dm.test(resLM,resAr1,h=12, alternative = "l")
dm.test(resArX,resAr1,h=12, alternative = "l")

#ArP
dm.test(resAr1,resArP,h=12, alternative = "l")
dm.test(resArima,resArP,h=12, alternative = "l")
dm.test(resHW,resArP,h=12, alternative = "l")
dm.test(resLM,resArP,h=12, alternative = "l")
dm.test(resArX,resArP,h=12, alternative = "l")

#Arima
dm.test(resAr1,resArima,h=12, alternative = "l")
dm.test(resArP,resArima,h=12, alternative = "l")
dm.test(resHW,resArima,h=12, alternative = "l")
dm.test(resLM,resArima,h=12, alternative = "l")
dm.test(resArX,resArima,h=12, alternative = "l")

#HW
dm.test(resAr1,resHW,h=12, alternative = "l")
dm.test(resArP,resHW,h=12, alternative = "l")
dm.test(resArima,resHW,h=12, alternative = "l")
dm.test(resLM,resHW,h=12, alternative = "l")
dm.test(resArX,resHW,h=12, alternative = "l")

#LM
dm.test(resAr1,resLM,h=12, alternative = "l")
dm.test(resArP,resLM,h=12, alternative = "l")
dm.test(resArima,resLM,h=12, alternative = "l")
dm.test(resHW,resLM,h=12, alternative = "l")
dm.test(resArX,resLM,h=12, alternative = "l")

#ArX
dm.test(resAr1,resArX,h=12, alternative = "l")
dm.test(resArP,resArX,h=12, alternative = "l")
dm.test(resArima,resArX,h=12, alternative = "l")
dm.test(resHW,resArX,h=12, alternative = "l")
dm.test(resLM,resArX,h=12, alternative = "l")
