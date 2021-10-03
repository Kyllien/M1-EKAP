library(readxl)
library(tidyverse)
library(ggplot2)
library(tseries)
library(zoo)
library(xts)
library(rugarch)
library(rmgarch)
library(stats)
library(MTS)

setwd("D:/M1 EKAP/Mémoire/Data")
BDI <- read.csv("Variables Controles/Transfo/BDI.csv")
BDI <- BDI[,-1]
colnames(BDI) <- c("Date","Close","Rd_BDI")
MSCI <- read.csv("Variables Controles/Transfo/MSCI.csv")
MSCI <- MSCI[,-1]
colnames(MSCI) <- c("Date","Close","Rd_MSCI")

MSCI$Date <- as.Date(MSCI$Date)
BDI$Date <- as.Date(BDI$Date)

#-- wheat_srw ----

wheat_srwCFTC <- read.csv("CFTC/Transfo2/wheat_srw.csv")
wheat_srwCFTC <- wheat_srwCFTC[,-1]
colnames(wheat_srwCFTC) <- c("Date","Rd_Long_Tot","Rd_Long_NoProd","Rd_Short_Tot","Rd_Short_NoProd","Rd_Net","Rd_Net_NoProd","Rd_Merc_Long","Rd_Merc_Short","Rd_Swap_Long","Rd_Swap_Short","Rd_Money_Long","Rd_Money_Short","Rd_Other_Long","Rd_Other_Short")  
wheat_srwContract <- read_excel("Futures/w_returns.xlsx",4)
wheat_srwContract <- wheat_srwContract[-1,]
wheat_srwCFTC$Date <- as.Date(wheat_srwCFTC$Date)
wheat_srwContract$Date <- as.Date(wheat_srwContract$Date)

#On supprime les données manquantes et qui ne correspondent pas

wheat_srw <- left_join(wheat_srwCFTC[],wheat_srwContract[,c(1,3)])

wheat_srw <- left_join(wheat_srw,BDI[,c(1,3)])
wheat_srw <- left_join(wheat_srw,MSCI[,c(1,3)])
wheat_srw <- na.omit(wheat_srw)

#Plot

ggplot(wheat_srw) + geom_line(aes(x=Date,y=w_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Long_Tot),color="lightblue")

ggplot(wheat_srw) + geom_line(aes(x=Date,y=w_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Long_NoProd),color="lightblue")

ggplot(wheat_srw) + geom_line(aes(x=Date,y=w_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Short_Tot),color="lightblue")

ggplot(wheat_srw) + geom_line(aes(x=Date,y=w_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Short_NoProd),color="lightblue")

ggplot(wheat_srw) + geom_line(aes(x=Date,y=w_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Net),color="lightblue")

ggplot(wheat_srw) + geom_line(aes(x=Date,y=w_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Net_NoProd),color="lightblue")


#ADF TEST
adf.test(wheat_srw$Rd_Long_Tot)
adf.test(wheat_srw$Rd_Long_NoProd)
adf.test(wheat_srw$Rd_Short_Tot)
adf.test(wheat_srw$Rd_Short_NoProd)
adf.test(wheat_srw$Rd_Net)
adf.test(wheat_srw$Rd_Net_NoProd)
adf.test(wheat_srw$w_weekly_return)
adf.test(wheat_srw$Rd_MSCI)
adf.test(wheat_srw$Rd_BDI)
adf.test(wheat_srw$Rd_Merc_Long)
adf.test(wheat_srw$Rd_Merc_Short)
adf.test(wheat_srw$Rd_Swap_Long)
adf.test(wheat_srw$Rd_Swap_Short)
adf.test(wheat_srw$Rd_Money_Long)
adf.test(wheat_srw$Rd_Money_Short)
adf.test(wheat_srw$Rd_Other_Long)
adf.test(wheat_srw$Rd_Other_Short)

#Test Ljung
Box.test(wheat_srw$Rd_Long_Tot^2, type = "Ljung")
Box.test(wheat_srw$Rd_Long_NoProd^2, type = "Ljung")
Box.test(wheat_srw$Rd_Short_Tot^2, type = "Ljung")
Box.test(wheat_srw$Rd_Short_NoProd^2, type = "Ljung")
Box.test(wheat_srw$Rd_Net^2, type = "Ljung")
Box.test(wheat_srw$Rd_Net_NoProd^2, type = "Ljung")
Box.test(wheat_srw$w_weekly_return^2, type = "Ljung")
Box.test(wheat_srw$Rd_MSCI^2, type = "Ljung")
Box.test(wheat_srw$Rd_BDI^2, type = "Ljung")
Box.test(wheat_srw$Rd_Merc_Long^2, type = "Ljung")
Box.test(wheat_srw$Rd_Merc_Short^2, type = "Ljung")
Box.test(wheat_srw$Rd_Swap_Long^2, type = "Ljung")
Box.test(wheat_srw$Rd_Swap_Short^2, type = "Ljung")
Box.test(wheat_srw$Rd_Money_Long^2, type = "Ljung")
Box.test(wheat_srw$Rd_Money_Short^2, type = "Ljung")
Box.test(wheat_srw$Rd_Other_Long^2, type = "Ljung")
Box.test(wheat_srw$Rd_Other_Short^2, type = "Ljung")

#Test Normalite Jarque
jarque.bera.test(wheat_srw$Rd_Long_Tot)
jarque.bera.test(wheat_srw$Rd_Long_NoProd)
jarque.bera.test(wheat_srw$Rd_Short_Tot)
jarque.bera.test(wheat_srw$Rd_Short_NoProd)
jarque.bera.test(wheat_srw$Rd_Net)
jarque.bera.test(wheat_srw$Rd_Net_NoProd)
jarque.bera.test(wheat_srw$w_weekly_return)
jarque.bera.test(wheat_srw$Rd_MSCI)
jarque.bera.test(wheat_srw$Rd_BDI)
jarque.bera.test(wheat_srw$Rd_Merc_Long)
jarque.bera.test(wheat_srw$Rd_Merc_Short)
jarque.bera.test(wheat_srw$Rd_Swap_Long)
jarque.bera.test(wheat_srw$Rd_Swap_Short)
jarque.bera.test(wheat_srw$Rd_Money_Long)
jarque.bera.test(wheat_srw$Rd_Money_Short)
jarque.bera.test(wheat_srw$Rd_Other_Long)
jarque.bera.test(wheat_srw$Rd_Other_Short)

#ACF/PACF pour creer le garch univariate
par(mfrow=c(1,1))
acf(wheat_srw$Rd_Long_Tot)
pacf(wheat_srw$Rd_Long_Tot)# (1,1)

acf(wheat_srw$Rd_Long_NoProd)
pacf(wheat_srw$Rd_Long_NoProd) #(1,1)

acf(wheat_srw$Rd_Short_Tot)
pacf(wheat_srw$Rd_Short_Tot) #(1,1)

acf(wheat_srw$Rd_Short_NoProd)
pacf(wheat_srw$Rd_Short_NoProd) #(1,1)

acf(wheat_srw$Rd_Net)
pacf(wheat_srw$Rd_Net)#(1,0)

acf(wheat_srw$Rd_Net_NoProd)
pacf(wheat_srw$Rd_Net_NoProd)#(1,1)

acf(wheat_srw$w_weekly_return)
pacf(wheat_srw$w_weekly_return)#(1,0)

acf(wheat_srw$Rd_BDI)
pacf(wheat_srw$Rd_BDI)#(1,1)

acf(wheat_srw$Rd_MSCI)
pacf(wheat_srw$Rd_MSCI)#(1,0)

acf(wheat_srw$Rd_Merc_Long)
pacf(wheat_srw$Rd_Merc_Long)#(1,1) a verifier

acf(wheat_srw$Rd_Merc_Short)
pacf(wheat_srw$Rd_Merc_Short)#(1,1)

acf(wheat_srw$Rd_Swap_Long)
pacf(wheat_srw$Rd_Swap_Long)#(1,0)

acf(wheat_srw$Rd_Swap_Short)
pacf(wheat_srw$Rd_Swap_Short)#(1,0) a verifier

acf(wheat_srw$Rd_Money_Long)
pacf(wheat_srw$Rd_Money_Long)#(1,1)

acf(wheat_srw$Rd_Money_Short)
pacf(wheat_srw$Rd_Money_Short)#(1,1) a verifier

acf(wheat_srw$Rd_Other_Long)
pacf(wheat_srw$Rd_Other_Long) #(1,0)

acf(wheat_srw$Rd_Other_Short)
pacf(wheat_srw$Rd_Other_Short)#(1,1) a verifier


#Xts
Rd_wheat_srw <- xts(wheat_srw,order.by=wheat_srw$Date)

#---- ModelDCC Pour Long Tot ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wheat_srw[,c(2,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_srw[,c(2,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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

cor_wheat_srw_Long_Tot_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_srw_Long_Tot_Contract,main="Correlation Dynamique Blé srw Long/Contrat")
cor_wheat_srw_Long_Tot_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_srw_Long_Tot_BDI,main="Correlation Dynamique Blé srw Long/BDI")
cor_wheat_srw_Long_Tot_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_srw_Long_Tot_MSCI,main="Correlation Dynamique Blé srw Long/MSCI")

cor_wheat_srw_Contract_BDI <- as.xts(cor1[2,3,])
plot(cor_wheat_srw_Contract_BDI,main="Correlation Dynamique Contrat/BDI")
cor_wheat_srw_Contract_MSCI <- as.xts(cor1[2,4,])
plot(cor_wheat_srw_Contract_MSCI,main="Correlation Dynamique Contrat/MSCI")

#---- ModelDCC Pour Long Tot No Prod ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wheat_srw[,c(3,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_srw[,c(3,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
MarchTest(fit1@mfit$stdresid[,c(1,3)])

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

par(mfrow=c(2,1))
cor_wheat_srw_Long_Tot_NoProd_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_srw_Long_Tot_NoProd_Contract,main="Correlation Dynamique Blé srw LongNoProd/Contrat")
cor_wheat_srw_Long_Tot_NoProd_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_srw_Long_Tot_NoProd_BDI,main="Correlation Dynamique Blé srw LongNoProd/BDI")
cor_wheat_srw_Long_Tot_NoProd_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_srw_Long_Tot_NoProd_MSCI,main="Correlation Dynamique Blé srw LongNoProd/MSCI")

#---- ModelDCC Pour Short Tot ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wheat_srw[,c(4,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_srw[,c(4,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_wheat_srw_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_srw_Short_Contract,main="Correlation Dynamique Blé srw ShortContrat")
cor_wheat_srw_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_srw_Short_BDI,main="Correlation Dynamique Blé srw Short/BDI")
cor_wheat_srw_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_srw_Short_MSCI,main="Correlation Dynamique Blé srw Short/MSCI")

#---- ModelDCC Pour Short NoProd ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(0,1)))))
multf <- multifit(uspec.n,Rd_wheat_srw[,c(5,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_srw[,c(5,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_wheat_srw_Short_NoProd_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_srw_Short_NoProd_Contract,main="Correlation Dynamique Blé srw Short NoProd/Contrat")
cor_wheat_srw_Short_NoProd_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_srw_Short_NoProd_BDI,main="Correlation Dynamique Blé srw Short NoProd/BDI")
cor_wheat_srw_Short_NoProd_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_srw_Short_NoProd_MSCI,main="Correlation Dynamique Blé srw Short NoProd/MSCI")


#---- ModelDCC Pour Merc Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wheat_srw[,c(8,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_srw[,c(8,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_wheat_srw_Merc_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_srw_Merc_Long_Contract,main="Correlation Dynamique Blé srw Merc Long/Contrat")
cor_wheat_srw_Merc_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_srw_Merc_Long_BDI,main="Correlation Dynamique Blé srw Merc Long/BDI")
cor_wheat_srw_Merc_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_srw_Merc_Long_MSCI,main="Correlation Dynamique Blé srw Merc Long/MSCI")

#---- ModelDCC Pour Merc Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wheat_srw[,c(9,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_srw[,c(9,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_wheat_srw_Merc_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_srw_Merc_Short_Contract,main="Correlation Dynamique Blé srw Merc Short/Contrat")
cor_wheat_srw_Merc_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_srw_Merc_Short_BDI,main="Correlation Dynamique Blé srw Merc Short/BDI")
cor_wheat_srw_Merc_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_srw_Merc_Short_MSCI,main="Correlation Dynamique Blé srw Merc Short/MSCI")

#---- ModelDCC Pour SWAP Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wheat_srw[,c(10,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_srw[,c(10,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_wheat_srw_Swap_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_srw_Swap_Long_Contract,main="Correlation Dynamique Blé srw Swap Long/Contrat")
cor_wheat_srw_Swap_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_srw_Swap_Long_BDI,main="Correlation Dynamique Blé srw Swap Long/BDI")
cor_wheat_srw_Swap_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_srw_Swap_Long_MSCI,main="Correlation Dynamique Blé srw Swap Long/MSCI")

#---- ModelDCC Pour Swap Short ----
uspec.n <- multispec(replicate(3, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wheat_srw[,c(11,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_srw[,c(11,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_wheat_srw_Swap_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_srw_Swap_Short_Contract,main="Correlation Dynamique Blé srw Swap Short/Contrat")
cor_wheat_srw_Swap_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_srw_Swap_Short_BDI,main="Correlation Dynamique Blé srw Swap Short/BDI")
cor_wheat_srw_Swap_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_srw_Swap_Short_MSCI,main="Correlation Dynamique Blé srw Swap Short/MSCI")

#---- ModelDCC Pour Money Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wheat_srw[,c(12,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_srw[,c(12,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_wheat_srw_Money_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_srw_Money_Long_Contract,main="Correlation Dynamique Blé srw Money Long/Contrat")
cor_wheat_srw_Money_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_srw_Money_Long_BDI,main="Correlation Dynamique Blé srw Money Long/BDI")
cor_wheat_srw_Money_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_srw_Money_Long_MSCI,main="Correlation Dynamique Blé srw Money Long/MSCI")

#---- ModelDCC Pour Money Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wheat_srw[,c(13,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_srw[,c(13,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_wheat_srw_Money_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_srw_Money_Short_Contract,main="Correlation Dynamique Blé srw Money Short/Contrat")
cor_wheat_srw_Money_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_srw_Money_Short_BDI,main="Correlation Dynamique Blé srw Money Short/BDI")
cor_wheat_srw_Money_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_srw_Money_Short_MSCI,main="Correlation Dynamique Blé srw Money Short/MSCI")

#---- ModelDCC Pour Other Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wheat_srw[,c(14,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_srw[,c(14,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_wheat_srw_Other_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_srw_Other_Long_Contract,main="Correlation Dynamique Blé srw Other Long/Contrat")
cor_wheat_srw_Other_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_srw_Other_Long_BDI,main="Correlation Dynamique Blé srw Other Long/BDI")
cor_wheat_srw_Other_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_srw_Other_Long_MSCI,main="Correlation Dynamique Blé srw Other Long/MSCI")

#---- ModelDCC Pour Other Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wheat_srw[,c(15,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_srw[,c(15,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_wheat_srw_Other_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_srw_Other_Short_Contract,main="Correlation Dynamique Blé srw Other Short/Contrat")
cor_wheat_srw_Other_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_srw_Other_Short_BDI,main="Correlation Dynamique Blé srw Other Short/BDI")
cor_wheat_srw_Other_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_srw_Other_Short_MSCI,main="Correlation Dynamique Blé srw Other Short/MSCI")

