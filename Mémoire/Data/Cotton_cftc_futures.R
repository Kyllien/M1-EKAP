library(readxl)
library(tidyverse)
library(ggplot2)
library(tseries)
library(zoo)
library(xts)
library(rugarch)
library(rmgarch)
library(stats)
library(fBasics)
library(PerformanceAnalytics)
library(quantmod)
library(MASS)
library(mvtnorm)
library(mnormt) 


setwd("D:/M1 EKAP/Mémoire/Data")
BDI <- read.csv("Variables Controles/Transfo/BDI.csv")
BDI <- BDI[,-1]
colnames(BDI) <- c("Date","Close","Rd_BDI")
MSCI <- read.csv("Variables Controles/Transfo/MSCI.csv")
MSCI <- MSCI[,-1]
colnames(MSCI) <- c("Date","Close","Rd_MSCI")

MSCI$Date <- as.Date(MSCI$Date)
BDI$Date <- as.Date(BDI$Date)

dataP <- left_join(MSCI[,c(1,3),BDI[,c(1,3)]])
#-- cotton ----

cottonCFTC <- read.csv("CFTC/Transfo2/Cotton.csv")
cottonCFTC <- cottonCFTC[,-1]
colnames(cottonCFTC) <- c("Date","Rd_Long_Tot","Rd_Long_NoProd","Rd_Short_Tot","Rd_Short_NoProd","Rd_Net","Rd_Net_NoProd","Rd_Merc_Long","Rd_Merc_Short","Rd_Swap_Long","Rd_Swap_Short","Rd_Money_Long","Rd_Money_Short","Rd_Other_Long","Rd_Other_Short")  
cottonContract <- read_excel("Futures/ct_returns.xlsx",4)
cottonContract <- cottonContract[-1,]
cottonCFTC$Date <- as.Date(cottonCFTC$Date)
cottonContract$Date <- as.Date(cottonContract$Date)

#On supprime les données manquantes et qui ne correspondent pas

cotton <- left_join(cottonCFTC[],cottonContract[,c(1,3)])

cotton <- left_join(cotton,BDI[,c(1,3)])
cotton <- left_join(cotton,MSCI[,c(1,3)])
cotton <- na.omit(cotton)

#Plot

ggplot(cotton) + geom_line(aes(x=Date,y=ct_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Long_Tot),color="lightblue")

ggplot(cotton) + geom_line(aes(x=Date,y=ct_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Long_NoProd),color="lightblue")

ggplot(cotton) + geom_line(aes(x=Date,y=ct_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Short_Tot),color="lightblue")

ggplot(cotton) + geom_line(aes(x=Date,y=ct_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Short_NoProd),color="lightblue")

ggplot(cotton) + geom_line(aes(x=Date,y=ct_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Net),color="lightblue")

ggplot(cotton) + geom_line(aes(x=Date,y=ct_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Net_NoProd),color="lightblue")

#Basic Stats
basicStats(cotton[,-1])

#ADF TEST
adf.test(cotton$Rd_Long_Tot)
adf.test(cotton$Rd_Long_NoProd)
adf.test(cotton$Rd_Short_Tot)
adf.test(cotton$Rd_Short_NoProd)
adf.test(cotton$Rd_Net)
adf.test(cotton$Rd_Net_NoProd)
adf.test(cotton$ct_weekly_return)
adf.test(cotton$Rd_MSCI)
adf.test(cotton$Rd_BDI)
adf.test(cotton$Rd_Merc_Long)
adf.test(cotton$Rd_Merc_Short)
adf.test(cotton$Rd_Swap_Long)
adf.test(cotton$Rd_Swap_Short)
adf.test(cotton$Rd_Money_Long)
adf.test(cotton$Rd_Money_Short)
adf.test(cotton$Rd_Other_Long)
adf.test(cotton$Rd_Other_Short)

#Test Ljung
Box.test(cotton$Rd_Long_Tot^2, type = "Ljung")
Box.test(cotton$Rd_Long_NoProd^2, type = "Ljung")
Box.test(cotton$Rd_Short_Tot^2, type = "Ljung")
Box.test(cotton$Rd_Short_NoProd^2, type = "Ljung")
Box.test(cotton$Rd_Net^2, type = "Ljung") #Not
Box.test(cotton$Rd_Net_NoProd^2, type = "Ljung") #Not
Box.test(cotton$ct_weekly_return^2, type = "Ljung")
Box.test(cotton$Rd_MSCI^2, type = "Ljung")
Box.test(cotton$Rd_BDI^2, type = "Ljung")
Box.test(cotton$Rd_Merc_Long^2, type = "Ljung")
Box.test(cotton$Rd_Merc_Short^2, type = "Ljung")
Box.test(cotton$Rd_Swap_Long^2, type = "Ljung")
Box.test(cotton$Rd_Swap_Short^2, type = "Ljung")
Box.test(cotton$Rd_Money_Long^2, type = "Ljung")
Box.test(cotton$Rd_Money_Short^2, type = "Ljung")
Box.test(cotton$Rd_Other_Long^2, type = "Ljung")
Box.test(cotton$Rd_Other_Short^2, type = "Ljung")

#Test Normalite Jarque
jarque.bera.test(cotton$Rd_Long_Tot)
jarque.bera.test(cotton$Rd_Long_NoProd)
jarque.bera.test(cotton$Rd_Short_Tot)
jarque.bera.test(cotton$Rd_Short_NoProd)
jarque.bera.test(cotton$Rd_Net)
jarque.bera.test(cotton$Rd_Net_NoProd)
jarque.bera.test(cotton$ct_weekly_return)
jarque.bera.test(cotton$Rd_MSCI)
jarque.bera.test(cotton$Rd_BDI)
jarque.bera.test(cotton$Rd_Merc_Long)
jarque.bera.test(cotton$Rd_Merc_Short)
jarque.bera.test(cotton$Rd_Swap_Long)
jarque.bera.test(cotton$Rd_Swap_Short)
jarque.bera.test(cotton$Rd_Money_Long)
jarque.bera.test(cotton$Rd_Money_Short)
jarque.bera.test(cotton$Rd_Other_Long)
jarque.bera.test(cotton$Rd_Other_Short)

#ACF/PACF pour creer le garch univariate
par(mfrow=c(1,1))
acf(cotton$Rd_Long_Tot)
pacf(cotton$Rd_Long_Tot)# (1,1)

acf(cotton$Rd_Long_NoProd)
pacf(cotton$Rd_Long_NoProd) #(1,1)

acf(cotton$Rd_Short_Tot)
pacf(cotton$Rd_Short_Tot) #(1,1)

acf(cotton$Rd_Short_NoProd)
pacf(cotton$Rd_Short_NoProd) #(1,1)

acf(cotton$Rd_Net)
pacf(cotton$Rd_Net)#(1,0)

acf(cotton$Rd_Net_NoProd)
pacf(cotton$Rd_Net_NoProd)#(1,1)

acf(cotton$ct_weekly_return)
pacf(cotton$ct_weekly_return)#(1,0)

acf(cotton$Rd_BDI)
pacf(cotton$Rd_BDI)#(1,1)

acf(cotton$Rd_MSCI)
pacf(cotton$Rd_MSCI)#(1,0)

acf(cotton$Rd_Merc_Long)
pacf(cotton$Rd_Merc_Long)#(1,1) a verifier

acf(cotton$Rd_Merc_Short)
pacf(cotton$Rd_Merc_Short)#(1,1)

acf(cotton$Rd_Swap_Long)
pacf(cotton$Rd_Swap_Long)#(1,1)

acf(cotton$Rd_Swap_Short)
pacf(cotton$Rd_Swap_Short)#(1,1) a verifier

acf(cotton$Rd_Money_Long)
pacf(cotton$Rd_Money_Long)#(1,1)

acf(cotton$Rd_Money_Short)
pacf(cotton$Rd_Money_Short)#(1,1) a verifier

acf(cotton$Rd_Other_Long)
pacf(cotton$Rd_Other_Long) #(1,1)

acf(cotton$Rd_Other_Short)
pacf(cotton$Rd_Other_Short)#(1,1) a verifier


#Xts
Rd_cotton <- xts(cotton,order.by=cotton$Date)

#---- ModelDCC Pour Long Tot ----
uspec.n <- multispec(list(ugarchspec(mean.model = list(armaOrder = c(1,0))),ugarchspec(mean.model = list(armaOrder = c(1,1)))))
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_cotton[,c(2,16)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cotton[,c(2,16)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble à un bruit blanc, les modele semblent etre correct

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,3]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,4]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])
jarque.bera.test(fit1@mfit$stdresid[,3])
jarque.bera.test(fit1@mfit$stdresid[,4])

#Test ARCH
ArchTest(fit1@mfit$stdresid)

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

cor_cotton_Long_Tot_Contract <- as.xts(cor1[2,1,])
plot(cor_cotton_Long_Tot_Contract,main="Correlation Dynamique Coton Long/Contrat")
cor_cotton_Long_Tot_BDI <- as.xts(cor1[3,1,])
plot(cor_cotton_Long_Tot_BDI,main="Correlation Dynamique Coton Long/BDI")
cor_cotton_Long_Tot_MSCI <- as.xts(cor1[4,1,])
plot(cor_cotton_Long_Tot_MSCI,main="Correlation Dynamique Coton Long/MSCI")

cor_cotton_Contract_BDI <- as.xts(cor1[2,3,])
plot(cor_cotton_Contract_BDI,main="Correlation Dynamique Contrat/BDI")
cor_cotton_Contract_MSCI <- as.xts(cor1[2,4,])
plot(cor_cotton_Contract_MSCI,main="Correlation Dynamique Contrat/MSCI")

#---- ModelDCC Pour Long Tot No Prod ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_cotton[,c(3,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cotton[,c(3,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble à un bruit blanc, les modele semblent etre correct

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,3]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,4]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])
jarque.bera.test(fit1@mfit$stdresid[,3])
jarque.bera.test(fit1@mfit$stdresid[,4])

#Test ARCH

MarchTest(fit1@mfit$stdresid)
MarchTest(fit1@mfit$stdresid[,c(1,2)])

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

par(mfrow=c(2,1))
cor_cotton_Long_Tot_NoProd_Contract <- as.xts(cor1[2,1,])
plot(cor_cotton_Long_Tot_NoProd_Contract,main="Correlation Dynamique Coton LongNoProd/Contrat")
cor_cotton_Long_Tot_NoProd_BDI <- as.xts(cor1[3,1,])
plot(cor_cotton_Long_Tot_NoProd_BDI,main="Correlation Dynamique Coton LongNoProd/BDI")
cor_cotton_Long_Tot_NoProd_MSCI <- as.xts(cor1[4,1,])
plot(cor_cotton_Long_Tot_NoProd_MSCI,main="Correlation Dynamique Coton LongNoProd/MSCI")

#---- ModelDCC Pour Short Tot ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_cotton[,c(4,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cotton[,c(4,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble à un bruit blanc, les modele semblent etre correct

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,3]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,4]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])
jarque.bera.test(fit1@mfit$stdresid[,3])
jarque.bera.test(fit1@mfit$stdresid[,4])

#Test ARCH

MarchTest(fit1@mfit$stdresid)
MarchTest(fit1@mfit$stdresid[,c(1,2)])

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

par(mfrow=c(2,1))
cor_cotton_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_cotton_Short_Contract,main="Correlation Dynamique Coton ShortContrat")
cor_cotton_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_cotton_Short_BDI,main="Correlation Dynamique Coton Short/BDI")
cor_cotton_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_cotton_Short_MSCI,main="Correlation Dynamique Coton Short/MSCI")

#---- ModelDCC Pour Short NoProd ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_cotton[,c(5,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cotton[,c(5,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble à un bruit blanc, les modele semblent etre correct

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,3]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,4]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])
jarque.bera.test(fit1@mfit$stdresid[,3])
jarque.bera.test(fit1@mfit$stdresid[,4])

#Test ARCH

MarchTest(fit1@mfit$stdresid)
MarchTest(fit1@mfit$stdresid[,c(1,2)])

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

par(mfrow=c(2,1))
cor_cotton_Short_NoProd_Contract <- as.xts(cor1[2,1,])
plot(cor_cotton_Short_NoProd_Contract,main="Correlation Dynamique Coton Short NoProd/Contrat")
cor_cotton_Short_NoProd_BDI <- as.xts(cor1[3,1,])
plot(cor_cotton_Short_NoProd_BDI,main="Correlation Dynamique Coton Short NoProd/BDI")
cor_cotton_Short_NoProd_MSCI <- as.xts(cor1[4,1,])
plot(cor_cotton_Short_NoProd_MSCI,main="Correlation Dynamique Coton Short NoProd/MSCI")


#---- ModelDCC Pour Merc Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_cotton[,c(8,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cotton[,c(8,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble à un bruit blanc, les modele semblent etre correct
#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,3]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,4]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])
jarque.bera.test(fit1@mfit$stdresid[,3])
jarque.bera.test(fit1@mfit$stdresid[,4])

#Test ARCH

MarchTest(fit1@mfit$stdresid)
MarchTest(fit1@mfit$stdresid[,c(1,2)])
#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

par(mfrow=c(2,1))
cor_cotton_Merc_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_cotton_Merc_Long_Contract,main="Correlation Dynamique Coton Merc Long/Contrat")
cor_cotton_Merc_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_cotton_Merc_Long_BDI,main="Correlation Dynamique Coton Merc Long/BDI")
cor_cotton_Merc_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_cotton_Merc_Long_MSCI,main="Correlation Dynamique Coton Merc Long/MSCI")

#---- ModelDCC Pour Merc Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_cotton[,c(9,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cotton[,c(9,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble à un bruit blanc, les modele semblent etre correct

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,3]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,4]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])
jarque.bera.test(fit1@mfit$stdresid[,3])
jarque.bera.test(fit1@mfit$stdresid[,4])

#Test ARCH

MarchTest(fit1@mfit$stdresid)
MarchTest(fit1@mfit$stdresid[,c(1,2)])
#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

par(mfrow=c(2,1))
cor_cotton_Merc_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_cotton_Merc_Short_Contract,main="Correlation Dynamique Coton Merc Short/Contrat")
cor_cotton_Merc_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_cotton_Merc_Short_BDI,main="Correlation Dynamique Coton Merc Short/BDI")
cor_cotton_Merc_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_cotton_Merc_Short_MSCI,main="Correlation Dynamique Coton Merc Short/MSCI")

#---- ModelDCC Pour SWAP Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_cotton[,c(10,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cotton[,c(10,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble à un bruit blanc, les modele semblent etre correct

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,3]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,4]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])
jarque.bera.test(fit1@mfit$stdresid[,3])
jarque.bera.test(fit1@mfit$stdresid[,4])

#Test ARCH

MarchTest(fit1@mfit$stdresid)
MarchTest(fit1@mfit$stdresid[,c(1,2)])

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

par(mfrow=c(2,1))
cor_cotton_Swap_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_cotton_Swap_Long_Contract,main="Correlation Dynamique Coton Swap Long/Contrat")
cor_cotton_Swap_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_cotton_Swap_Long_BDI,main="Correlation Dynamique Coton Swap Long/BDI")
cor_cotton_Swap_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_cotton_Swap_Long_MSCI,main="Correlation Dynamique Coton Swap Long/MSCI")

#---- ModelDCC Pour Swap Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_cotton[,c(11,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cotton[,c(11,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble à un bruit blanc, les modele semblent etre correct

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,3]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,4]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])
jarque.bera.test(fit1@mfit$stdresid[,3])
jarque.bera.test(fit1@mfit$stdresid[,4])

#Test ARCH

MarchTest(fit1@mfit$stdresid)
MarchTest(fit1@mfit$stdresid[,c(1,2)])

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

par(mfrow=c(2,1))
cor_cotton_Swap_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_cotton_Swap_Short_Contract,main="Correlation Dynamique Coton Swap Short/Contrat")
cor_cotton_Swap_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_cotton_Swap_Short_BDI,main="Correlation Dynamique Coton Swap Short/BDI")
cor_cotton_Swap_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_cotton_Swap_Short_MSCI,main="Correlation Dynamique Coton Swap Short/MSCI")

#---- ModelDCC Pour Money Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_cotton[,c(12,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cotton[,c(12,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble à un bruit blanc, les modele semblent etre correct

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,3]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,4]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])
jarque.bera.test(fit1@mfit$stdresid[,3])
jarque.bera.test(fit1@mfit$stdresid[,4])

#Test ARCH

MarchTest(fit1@mfit$stdresid)
MarchTest(fit1@mfit$stdresid[,c(1,2)])

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

par(mfrow=c(2,1))
cor_cotton_Money_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_cotton_Money_Long_Contract,main="Correlation Dynamique Coton Money Long/Contrat")
cor_cotton_Money_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_cotton_Money_Long_BDI,main="Correlation Dynamique Coton Money Long/BDI")
cor_cotton_Money_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_cotton_Money_Long_MSCI,main="Correlation Dynamique Coton Money Long/MSCI")

#---- ModelDCC Pour Money Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_cotton[,c(13,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cotton[,c(13,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble à un bruit blanc, les modele semblent etre correct

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,3]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,4]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])
jarque.bera.test(fit1@mfit$stdresid[,3])
jarque.bera.test(fit1@mfit$stdresid[,4])

#Test ARCH

MarchTest(fit1@mfit$stdresid)
MarchTest(fit1@mfit$stdresid[,c(1,2)])

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

par(mfrow=c(2,1))
cor_cotton_Money_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_cotton_Money_Short_Contract,main="Correlation Dynamique Coton Money Short/Contrat")
cor_cotton_Money_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_cotton_Money_Short_BDI,main="Correlation Dynamique Coton Money Short/BDI")
cor_cotton_Money_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_cotton_Money_Short_MSCI,main="Correlation Dynamique Coton Money Short/MSCI")

#---- ModelDCC Pour Other Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_cotton[,c(14,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cotton[,c(14,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble à un bruit blanc, les modele semblent etre correct

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,3]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,4]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])
jarque.bera.test(fit1@mfit$stdresid[,3])
jarque.bera.test(fit1@mfit$stdresid[,4])

#Test ARCH

MarchTest(fit1@mfit$stdresid)
MarchTest(fit1@mfit$stdresid[,c(1,2)])

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

par(mfrow=c(2,1))
cor_cotton_Other_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_cotton_Other_Long_Contract,main="Correlation Dynamique Coton Other Long/Contrat")
cor_cotton_Other_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_cotton_Other_Long_BDI,main="Correlation Dynamique Coton Other Long/BDI")
cor_cotton_Other_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_cotton_Other_Long_MSCI,main="Correlation Dynamique Coton Other Long/MSCI")

#---- ModelDCC Pour Other Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_cotton[,c(15,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cotton[,c(15,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble à un bruit blanc, les modele semblent etre correct

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,3]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,4]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])
jarque.bera.test(fit1@mfit$stdresid[,3])
jarque.bera.test(fit1@mfit$stdresid[,4])

#Test ARCH

MarchTest(fit1@mfit$stdresid)
MarchTest(fit1@mfit$stdresid[,c(1,2)])

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

par(mfrow=c(2,1))
cor_cotton_Other_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_cotton_Other_Short_Contract,main="Correlation Dynamique Coton Other Short/Contrat")
cor_cotton_Other_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_cotton_Other_Short_BDI,main="Correlation Dynamique Coton Other Short/BDI")
cor_cotton_Other_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_cotton_Other_Short_MSCI,main="Correlation Dynamique Coton Other Short/MSCI")

