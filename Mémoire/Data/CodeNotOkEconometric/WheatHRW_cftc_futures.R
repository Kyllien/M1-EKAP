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

#-- wheat_hrw ----

wheat_hrwCFTC <- read.csv("CFTC/Transfo2/wheat_hrw.csv")
wheat_hrwCFTC <- wheat_hrwCFTC[,-1]
colnames(wheat_hrwCFTC) <- c("Date","Rd_Long_Tot","Rd_Long_NoProd","Rd_Short_Tot","Rd_Short_NoProd","Rd_Net","Rd_Net_NoProd","Rd_Merc_Long","Rd_Merc_Short","Rd_Swap_Long","Rd_Swap_Short","Rd_Money_Long","Rd_Money_Short","Rd_Other_Long","Rd_Other_Short")  
wheat_hrwContract <- read_excel("Futures/w_returns.xlsx",4)
wheat_hrwContract <- wheat_hrwContract[-1,]
wheat_hrwCFTC$Date <- as.Date(wheat_hrwCFTC$Date)
wheat_hrwContract$Date <- as.Date(wheat_hrwContract$Date)

#On supprime les données manquantes et qui ne correspondent pas

wheat_hrw <- left_join(wheat_hrwCFTC[],wheat_hrwContract[,c(1,3)])

wheat_hrw <- left_join(wheat_hrw,BDI[,c(1,3)])
wheat_hrw <- left_join(wheat_hrw,MSCI[,c(1,3)])
wheat_hrw <- na.omit(wheat_hrw)

#Plot

ggplot(wheat_hrw) + geom_line(aes(x=Date,y=w_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Long_Tot),color="lightblue")

ggplot(wheat_hrw) + geom_line(aes(x=Date,y=w_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Long_NoProd),color="lightblue")

ggplot(wheat_hrw) + geom_line(aes(x=Date,y=w_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Short_Tot),color="lightblue")

ggplot(wheat_hrw) + geom_line(aes(x=Date,y=w_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Short_NoProd),color="lightblue")

ggplot(wheat_hrw) + geom_line(aes(x=Date,y=w_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Net),color="lightblue")

ggplot(wheat_hrw) + geom_line(aes(x=Date,y=w_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Net_NoProd),color="lightblue")


#ADF TEST
adf.test(wheat_hrw$Rd_Long_Tot)
adf.test(wheat_hrw$Rd_Long_NoProd)
adf.test(wheat_hrw$Rd_Short_Tot)
adf.test(wheat_hrw$Rd_Short_NoProd)
adf.test(wheat_hrw$Rd_Net)
adf.test(wheat_hrw$Rd_Net_NoProd)
adf.test(wheat_hrw$w_weekly_return)
adf.test(wheat_hrw$Rd_MSCI)
adf.test(wheat_hrw$Rd_BDI)
adf.test(wheat_hrw$Rd_Merc_Long)
adf.test(wheat_hrw$Rd_Merc_Short)
adf.test(wheat_hrw$Rd_Swap_Long)
adf.test(wheat_hrw$Rd_Swap_Short)
adf.test(wheat_hrw$Rd_Money_Long)
adf.test(wheat_hrw$Rd_Money_Short)
adf.test(wheat_hrw$Rd_Other_Long)
adf.test(wheat_hrw$Rd_Other_Short)

#Test Ljung
Box.test(wheat_hrw$Rd_Long_Tot^2, type = "Ljung")
Box.test(wheat_hrw$Rd_Long_NoProd^2, type = "Ljung")
Box.test(wheat_hrw$Rd_Short_Tot^2, type = "Ljung")
Box.test(wheat_hrw$Rd_Short_NoProd^2, type = "Ljung")
Box.test(wheat_hrw$Rd_Net^2, type = "Ljung")
Box.test(wheat_hrw$Rd_Net_NoProd^2, type = "Ljung")
Box.test(wheat_hrw$w_weekly_return^2, type = "Ljung")
Box.test(wheat_hrw$Rd_MSCI^2, type = "Ljung")
Box.test(wheat_hrw$Rd_BDI^2, type = "Ljung")
Box.test(wheat_hrw$Rd_Merc_Long^2, type = "Ljung")
Box.test(wheat_hrw$Rd_Merc_Short^2, type = "Ljung")
Box.test(wheat_hrw$Rd_Swap_Long^2, type = "Ljung")
Box.test(wheat_hrw$Rd_Swap_Short^2, type = "Ljung")
Box.test(wheat_hrw$Rd_Money_Long^2, type = "Ljung")
Box.test(wheat_hrw$Rd_Money_Short^2, type = "Ljung")
Box.test(wheat_hrw$Rd_Other_Long^2, type = "Ljung")
Box.test(wheat_hrw$Rd_Other_Short^2, type = "Ljung")

#Test Normalite Jarque
jarque.bera.test(wheat_hrw$Rd_Long_Tot)
jarque.bera.test(wheat_hrw$Rd_Long_NoProd)
jarque.bera.test(wheat_hrw$Rd_Short_Tot)
jarque.bera.test(wheat_hrw$Rd_Short_NoProd)
jarque.bera.test(wheat_hrw$Rd_Net)
jarque.bera.test(wheat_hrw$Rd_Net_NoProd)
jarque.bera.test(wheat_hrw$w_weekly_return)
jarque.bera.test(wheat_hrw$Rd_MSCI)
jarque.bera.test(wheat_hrw$Rd_BDI)
jarque.bera.test(wheat_hrw$Rd_Merc_Long)
jarque.bera.test(wheat_hrw$Rd_Merc_Short)
jarque.bera.test(wheat_hrw$Rd_Swap_Long)
jarque.bera.test(wheat_hrw$Rd_Swap_Short)
jarque.bera.test(wheat_hrw$Rd_Money_Long)
jarque.bera.test(wheat_hrw$Rd_Money_Short)
jarque.bera.test(wheat_hrw$Rd_Other_Long)
jarque.bera.test(wheat_hrw$Rd_Other_Short)

#ACF/PACF pour creer le garch univariate
par(mfrow=c(1,1))
acf(wheat_hrw$Rd_Long_Tot)
pacf(wheat_hrw$Rd_Long_Tot)# (1,1)

acf(wheat_hrw$Rd_Long_NoProd)
pacf(wheat_hrw$Rd_Long_NoProd) #(1,1)

acf(wheat_hrw$Rd_Short_Tot)
pacf(wheat_hrw$Rd_Short_Tot) #(1,1)

acf(wheat_hrw$Rd_Short_NoProd)
pacf(wheat_hrw$Rd_Short_NoProd) #(1,1)

acf(wheat_hrw$Rd_Net)
pacf(wheat_hrw$Rd_Net)#(1,0)

acf(wheat_hrw$Rd_Net_NoProd)
pacf(wheat_hrw$Rd_Net_NoProd)#(1,1)

acf(wheat_hrw$w_weekly_return)
pacf(wheat_hrw$w_weekly_return)#(1,0)

acf(wheat_hrw$Rd_BDI)
pacf(wheat_hrw$Rd_BDI)#(1,1)

acf(wheat_hrw$Rd_MSCI)
pacf(wheat_hrw$Rd_MSCI)#(1,0)

acf(wheat_hrw$Rd_Merc_Long)
pacf(wheat_hrw$Rd_Merc_Long)#(1,1) a verifier

acf(wheat_hrw$Rd_Merc_Short)
pacf(wheat_hrw$Rd_Merc_Short)#(1,1)

acf(wheat_hrw$Rd_Swap_Long)
pacf(wheat_hrw$Rd_Swap_Long)#(1,0)

acf(wheat_hrw$Rd_Swap_Short)
pacf(wheat_hrw$Rd_Swap_Short)#(1,0) a verifier

acf(wheat_hrw$Rd_Money_Long)
pacf(wheat_hrw$Rd_Money_Long)#(1,1)

acf(wheat_hrw$Rd_Money_Short)
pacf(wheat_hrw$Rd_Money_Short)#(1,1) a verifier

acf(wheat_hrw$Rd_Other_Long)
pacf(wheat_hrw$Rd_Other_Long) #(1,0)

acf(wheat_hrw$Rd_Other_Short)
pacf(wheat_hrw$Rd_Other_Short)#(1,1) a verifier


#Xts
Rd_wheat_hrw <- xts(wheat_hrw,order.by=wheat_hrw$Date)

#---- ModelDCC Pour Long Tot ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wheat_hrw[,c(2,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_hrw[,c(2,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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

cor_wheat_hrw_Long_Tot_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_hrw_Long_Tot_Contract,main="Correlation Dynamique Blé Hrw Long/Contrat")
cor_wheat_hrw_Long_Tot_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_hrw_Long_Tot_BDI,main="Correlation Dynamique Blé Hrw Long/BDI")
cor_wheat_hrw_Long_Tot_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_hrw_Long_Tot_MSCI,main="Correlation Dynamique Blé Hrw Long/MSCI")

cor_wheat_hrw_Contract_BDI <- as.xts(cor1[2,3,])
plot(cor_wheat_hrw_Contract_BDI,main="Correlation Dynamique Contrat/BDI")
cor_wheat_hrw_Contract_MSCI <- as.xts(cor1[2,4,])
plot(cor_wheat_hrw_Contract_MSCI,main="Correlation Dynamique Contrat/MSCI")

#---- ModelDCC Pour Long Tot No Prod ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wheat_hrw[,c(3,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_hrw[,c(3,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wheat_hrw_Long_Tot_NoProd_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_hrw_Long_Tot_NoProd_Contract,main="Correlation Dynamique Blé Hrw LongNoProd/Contrat")
cor_wheat_hrw_Long_Tot_NoProd_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_hrw_Long_Tot_NoProd_BDI,main="Correlation Dynamique Blé Hrw LongNoProd/BDI")
cor_wheat_hrw_Long_Tot_NoProd_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_hrw_Long_Tot_NoProd_MSCI,main="Correlation Dynamique Blé Hrw LongNoProd/MSCI")

#---- ModelDCC Pour Short Tot ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wheat_hrw[,c(4,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_hrw[,c(4,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wheat_hrw_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_hrw_Short_Contract,main="Correlation Dynamique Blé Hrw ShortContrat")
cor_wheat_hrw_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_hrw_Short_BDI,main="Correlation Dynamique Blé Hrw Short/BDI")
cor_wheat_hrw_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_hrw_Short_MSCI,main="Correlation Dynamique Blé Hrw Short/MSCI")

#---- ModelDCC Pour Short NoProd ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wheat_hrw[,c(5,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_hrw[,c(5,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wheat_hrw_Short_NoProd_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_hrw_Short_NoProd_Contract,main="Correlation Dynamique Blé Hrw Short NoProd/Contrat")
cor_wheat_hrw_Short_NoProd_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_hrw_Short_NoProd_BDI,main="Correlation Dynamique Blé Hrw Short NoProd/BDI")
cor_wheat_hrw_Short_NoProd_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_hrw_Short_NoProd_MSCI,main="Correlation Dynamique Blé Hrw Short NoProd/MSCI")


#---- ModelDCC Pour Merc Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wheat_hrw[,c(8,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_hrw[,c(8,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wheat_hrw_Merc_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_hrw_Merc_Long_Contract,main="Correlation Dynamique Blé Hrw Merc Long/Contrat")
cor_wheat_hrw_Merc_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_hrw_Merc_Long_BDI,main="Correlation Dynamique Blé Hrw Merc Long/BDI")
cor_wheat_hrw_Merc_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_hrw_Merc_Long_MSCI,main="Correlation Dynamique Blé Hrw Merc Long/MSCI")

#---- ModelDCC Pour Merc Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wheat_hrw[,c(9,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_hrw[,c(9,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wheat_hrw_Merc_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_hrw_Merc_Short_Contract,main="Correlation Dynamique Blé Hrw Merc Short/Contrat")
cor_wheat_hrw_Merc_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_hrw_Merc_Short_BDI,main="Correlation Dynamique Blé Hrw Merc Short/BDI")
cor_wheat_hrw_Merc_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_hrw_Merc_Short_MSCI,main="Correlation Dynamique Blé Hrw Merc Short/MSCI")

#---- ModelDCC Pour SWAP Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wheat_hrw[,c(10,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_hrw[,c(10,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wheat_hrw_Swap_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_hrw_Swap_Long_Contract,main="Correlation Dynamique Blé Hrw Swap Long/Contrat")
cor_wheat_hrw_Swap_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_hrw_Swap_Long_BDI,main="Correlation Dynamique Blé Hrw Swap Long/BDI")
cor_wheat_hrw_Swap_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_hrw_Swap_Long_MSCI,main="Correlation Dynamique Blé Hrw Swap Long/MSCI")

#---- ModelDCC Pour Swap Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wheat_hrw[,c(11,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_hrw[,c(11,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wheat_hrw_Swap_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_hrw_Swap_Short_Contract,main="Correlation Dynamique Blé Hrw Swap Short/Contrat")
cor_wheat_hrw_Swap_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_hrw_Swap_Short_BDI,main="Correlation Dynamique Blé Hrw Swap Short/BDI")
cor_wheat_hrw_Swap_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_hrw_Swap_Short_MSCI,main="Correlation Dynamique Blé Hrw Swap Short/MSCI")

#---- ModelDCC Pour Money Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wheat_hrw[,c(12,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_hrw[,c(12,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wheat_hrw_Money_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_hrw_Money_Long_Contract,main="Correlation Dynamique Blé Hrw Money Long/Contrat")
cor_wheat_hrw_Money_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_hrw_Money_Long_BDI,main="Correlation Dynamique Blé Hrw Money Long/BDI")
cor_wheat_hrw_Money_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_hrw_Money_Long_MSCI,main="Correlation Dynamique Blé Hrw Money Long/MSCI")

#---- ModelDCC Pour Money Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wheat_hrw[,c(13,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_hrw[,c(13,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wheat_hrw_Money_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_hrw_Money_Short_Contract,main="Correlation Dynamique Blé Hrw Money Short/Contrat")
cor_wheat_hrw_Money_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_hrw_Money_Short_BDI,main="Correlation Dynamique Blé Hrw Money Short/BDI")
cor_wheat_hrw_Money_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_hrw_Money_Short_MSCI,main="Correlation Dynamique Blé Hrw Money Short/MSCI")

#---- ModelDCC Pour Other Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wheat_hrw[,c(14,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_hrw[,c(14,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble à un bruit blanc, les modele semblent etre correct

#Test de Portmanteau
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
cor_wheat_hrw_Other_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_hrw_Other_Long_Contract,main="Correlation Dynamique Blé Hrw Other Long/Contrat")
cor_wheat_hrw_Other_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_hrw_Other_Long_BDI,main="Correlation Dynamique Blé Hrw Other Long/BDI")
cor_wheat_hrw_Other_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_hrw_Other_Long_MSCI,main="Correlation Dynamique Blé Hrw Other Long/MSCI")

#---- ModelDCC Pour Other Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wheat_hrw[,c(15,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat_hrw[,c(15,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wheat_hrw_Other_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_wheat_hrw_Other_Short_Contract,main="Correlation Dynamique Blé Hrw Other Short/Contrat")
cor_wheat_hrw_Other_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_wheat_hrw_Other_Short_BDI,main="Correlation Dynamique Blé Hrw Other Short/BDI")
cor_wheat_hrw_Other_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_wheat_hrw_Other_Short_MSCI,main="Correlation Dynamique Blé Hrw Other Short/MSCI")

