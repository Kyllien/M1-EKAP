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

#-- CORN ----

cornCFTC <- read.csv("CFTC/Transfo2/Corn.csv")
cornCFTC <- cornCFTC[,-1]
colnames(cornCFTC) <- c("Date","Rd_Long_Tot","Rd_Long_NoProd","Rd_Short_Tot","Rd_Short_NoProd","Rd_Net","Rd_Net_NoProd","Rd_Merc_Long","Rd_Merc_Short","Rd_Swap_Long","Rd_Swap_Short","Rd_Money_Long","Rd_Money_Short","Rd_Other_Long","Rd_Other_Short")  
cornContract <- read_excel("Futures/c_returns.xlsx",4)
cornContract <- cornContract[-1,]
cornCFTC$Date <- as.Date(cornCFTC$Date)
cornContract$Date <- as.Date(cornContract$Date)

#On supprime les données manquantes et qui ne correspondent pas

CORN <- left_join(cornCFTC[],cornContract[,c(1,3)])

CORN <- left_join(CORN,BDI[,c(1,3)])
CORN <- left_join(CORN,MSCI[,c(1,3)])
CORN <- na.omit(CORN)

#Plot

ggplot(CORN) + geom_line(aes(x=Date,y=c_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Long_Tot),color="lightblue")

ggplot(CORN) + geom_line(aes(x=Date,y=c_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Long_NoProd),color="lightblue")

ggplot(CORN) + geom_line(aes(x=Date,y=c_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Short_Tot),color="lightblue")

ggplot(CORN) + geom_line(aes(x=Date,y=c_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Short_NoProd),color="lightblue")

ggplot(CORN) + geom_line(aes(x=Date,y=c_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Net),color="lightblue")

ggplot(CORN) + geom_line(aes(x=Date,y=c_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Net_NoProd),color="lightblue")


#ADF TEST
adf.test(CORN$Rd_Long_Tot)
adf.test(CORN$Rd_Long_NoProd)
adf.test(CORN$Rd_Short_Tot)
adf.test(CORN$Rd_Short_NoProd)
adf.test(CORN$Rd_Net)
adf.test(CORN$Rd_Net_NoProd)
adf.test(CORN$c_weekly_return)
adf.test(CORN$Rd_MSCI)
adf.test(CORN$Rd_BDI)
adf.test(CORN$Rd_Merc_Long)
adf.test(CORN$Rd_Merc_Short)
adf.test(CORN$Rd_Swap_Long)
adf.test(CORN$Rd_Swap_Short)
adf.test(CORN$Rd_Money_Long)
adf.test(CORN$Rd_Money_Short)
adf.test(CORN$Rd_Other_Long)
adf.test(CORN$Rd_Other_Short)

#Test Ljung
Box.test(CORN$Rd_Long_Tot^2, type = "Ljung")
Box.test(CORN$Rd_Long_NoProd^2, type = "Ljung")
Box.test(CORN$Rd_Short_Tot^2, type = "Ljung")
Box.test(CORN$Rd_Short_NoProd^2, type = "Ljung")
Box.test(CORN$Rd_Net^2, type = "Ljung")
Box.test(CORN$Rd_Net_NoProd^2, type = "Ljung")
Box.test(CORN$c_weekly_return^2, type = "Ljung")
Box.test(CORN$Rd_MSCI^2, type = "Ljung")
Box.test(CORN$Rd_BDI^2, type = "Ljung")
Box.test(CORN$Rd_Merc_Long^2, type = "Ljung")
Box.test(CORN$Rd_Merc_Short^2, type = "Ljung")
Box.test(CORN$Rd_Swap_Long^2, type = "Ljung")
Box.test(CORN$Rd_Swap_Short^2, type = "Ljung")
Box.test(CORN$Rd_Money_Long^2, type = "Ljung")
Box.test(CORN$Rd_Money_Short^2, type = "Ljung")
Box.test(CORN$Rd_Other_Long^2, type = "Ljung")
Box.test(CORN$Rd_Other_Short^2, type = "Ljung")

#Test Normalite Jarque
jarque.bera.test(CORN$Rd_Long_Tot)
jarque.bera.test(CORN$Rd_Long_NoProd)
jarque.bera.test(CORN$Rd_Short_Tot)
jarque.bera.test(CORN$Rd_Short_NoProd)
jarque.bera.test(CORN$Rd_Net)
jarque.bera.test(CORN$Rd_Net_NoProd)
jarque.bera.test(CORN$c_weekly_return)
jarque.bera.test(CORN$Rd_MSCI)
jarque.bera.test(CORN$Rd_BDI)
jarque.bera.test(CORN$Rd_Merc_Long)
jarque.bera.test(CORN$Rd_Merc_Short)
jarque.bera.test(CORN$Rd_Swap_Long)
jarque.bera.test(CORN$Rd_Swap_Short)
jarque.bera.test(CORN$Rd_Money_Long)
jarque.bera.test(CORN$Rd_Money_Short)
jarque.bera.test(CORN$Rd_Other_Long)
jarque.bera.test(CORN$Rd_Other_Short)

#ACF/PACF pour creer le garch univariate
par(mfrow=c(1,1))
acf(CORN$Rd_Long_Tot)
pacf(CORN$Rd_Long_Tot)# (1,1)

acf(CORN$Rd_Long_NoProd)
pacf(CORN$Rd_Long_NoProd) #(1,1)

acf(CORN$Rd_Short_Tot)
pacf(CORN$Rd_Short_Tot) #(1,1)

acf(CORN$Rd_Short_NoProd)
pacf(CORN$Rd_Short_NoProd) #(1,1)

acf(CORN$Rd_Net)
pacf(CORN$Rd_Net)#(1,0)

acf(CORN$Rd_Net_NoProd)
pacf(CORN$Rd_Net_NoProd)#(1,1)

acf(CORN$c_weekly_return)
pacf(CORN$c_weekly_return)#(1,0)

acf(CORN$Rd_BDI)
pacf(CORN$Rd_BDI)#(1,1)

acf(CORN$Rd_MSCI)
pacf(CORN$Rd_MSCI)#(1,0)

acf(CORN$Rd_Merc_Long)
pacf(CORN$Rd_Merc_Long)#(1,1) a verifier

acf(CORN$Rd_Merc_Short)
pacf(CORN$Rd_Merc_Short)#(1,1)

acf(CORN$Rd_Swap_Long)
pacf(CORN$Rd_Swap_Long)#(1,1)

acf(CORN$Rd_Swap_Short)
pacf(CORN$Rd_Swap_Short)#(1,1) a verifier

acf(CORN$Rd_Money_Long)
pacf(CORN$Rd_Money_Long)#(1,1)

acf(CORN$Rd_Money_Short)
pacf(CORN$Rd_Money_Short)#(1,1) a verifier

acf(CORN$Rd_Other_Long)
pacf(CORN$Rd_Other_Long) #(1,1)

acf(CORN$Rd_Other_Short)
pacf(CORN$Rd_Other_Short)#(1,1) a verifier


#Xts
Rd_CORN <- xts(CORN,order.by=CORN$Date)

#---- ModelDCC Pour Long Tot ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_CORN[,c(2,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_CORN[,c(2,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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

cor_CORN_Long_Tot_Contract <- as.xts(cor1[2,1,])
plot(cor_CORN_Long_Tot_Contract,main="Correlation Dynamique Maïs Long/Contrat")
cor_CORN_Long_Tot_BDI <- as.xts(cor1[3,1,])
plot(cor_CORN_Long_Tot_BDI,main="Correlation Dynamique Maïs Long/BDI")
cor_CORN_Long_Tot_MSCI <- as.xts(cor1[4,1,])
plot(cor_CORN_Long_Tot_MSCI,main="Correlation Dynamique Maïs Long/MSCI")

cor_CORN_Contract_BDI <- as.xts(cor1[2,3,])
plot(cor_CORN_Contract_BDI,main="Correlation Dynamique Contrat/BDI")
cor_CORN_Contract_MSCI <- as.xts(cor1[2,4,])
plot(cor_CORN_Contract_MSCI,main="Correlation Dynamique Contrat/MSCI")

#---- ModelDCC Pour Long Tot No Prod ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_CORN[,c(3,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_CORN[,c(3,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_CORN_Long_Tot_NoProd_Contract <- as.xts(cor1[2,1,])
plot(cor_CORN_Long_Tot_NoProd_Contract,main="Correlation Dynamique Maïs LongNoProd/Contrat")
cor_CORN_Long_Tot_NoProd_BDI <- as.xts(cor1[3,1,])
plot(cor_CORN_Long_Tot_NoProd_BDI,main="Correlation Dynamique Maïs LongNoProd/BDI")
cor_CORN_Long_Tot_NoProd_MSCI <- as.xts(cor1[4,1,])
plot(cor_CORN_Long_Tot_NoProd_MSCI,main="Correlation Dynamique Maïs LongNoProd/MSCI")

#---- ModelDCC Pour Short Tot ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_CORN[,c(4,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_CORN[,c(4,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_CORN_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_CORN_Short_Contract,main="Correlation Dynamique Maïs ShortContrat")
cor_CORN_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_CORN_Short_BDI,main="Correlation Dynamique Maïs Short/BDI")
cor_CORN_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_CORN_Short_MSCI,main="Correlation Dynamique Maïs Short/MSCI")

#---- ModelDCC Pour Short NoProd ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_CORN[,c(5,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_CORN[,c(5,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_CORN_Short_NoProd_Contract <- as.xts(cor1[2,1,])
plot(cor_CORN_Short_NoProd_Contract,main="Correlation Dynamique Maïs Short NoProd/Contrat")
cor_CORN_Short_NoProd_BDI <- as.xts(cor1[3,1,])
plot(cor_CORN_Short_NoProd_BDI,main="Correlation Dynamique Maïs Short NoProd/BDI")
cor_CORN_Short_NoProd_MSCI <- as.xts(cor1[4,1,])
plot(cor_CORN_Short_NoProd_MSCI,main="Correlation Dynamique Maïs Short NoProd/MSCI")


#---- ModelDCC Pour Merc Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_CORN[,c(8,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_CORN[,c(8,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_CORN_Merc_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_CORN_Merc_Long_Contract,main="Correlation Dynamique Maïs Merc Long/Contrat")
cor_CORN_Merc_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_CORN_Merc_Long_BDI,main="Correlation Dynamique Maïs Merc Long/BDI")
cor_CORN_Merc_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_CORN_Merc_Long_MSCI,main="Correlation Dynamique Maïs Merc Long/MSCI")

#---- ModelDCC Pour Merc Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_CORN[,c(9,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_CORN[,c(9,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_CORN_Merc_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_CORN_Merc_Short_Contract,main="Correlation Dynamique Maïs Merc Short/Contrat")
cor_CORN_Merc_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_CORN_Merc_Short_BDI,main="Correlation Dynamique Maïs Merc Short/BDI")
cor_CORN_Merc_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_CORN_Merc_Short_MSCI,main="Correlation Dynamique Maïs Merc Short/MSCI")

#---- ModelDCC Pour SWAP Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_CORN[,c(10,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_CORN[,c(10,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_CORN_Swap_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_CORN_Swap_Long_Contract,main="Correlation Dynamique Maïs Swap Long/Contrat")
cor_CORN_Swap_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_CORN_Swap_Long_BDI,main="Correlation Dynamique Maïs Swap Long/BDI")
cor_CORN_Swap_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_CORN_Swap_Long_MSCI,main="Correlation Dynamique Maïs Swap Long/MSCI")

#---- ModelDCC Pour Swap Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_CORN[,c(11,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_CORN[,c(11,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_CORN_Swap_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_CORN_Swap_Short_Contract,main="Correlation Dynamique Maïs Swap Short/Contrat")
cor_CORN_Swap_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_CORN_Swap_Short_BDI,main="Correlation Dynamique Maïs Swap Short/BDI")
cor_CORN_Swap_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_CORN_Swap_Short_MSCI,main="Correlation Dynamique Maïs Swap Short/MSCI")

#---- ModelDCC Pour Money Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_CORN[,c(12,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_CORN[,c(12,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_CORN_Money_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_CORN_Money_Long_Contract,main="Correlation Dynamique Maïs Money Long/Contrat")
cor_CORN_Money_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_CORN_Money_Long_BDI,main="Correlation Dynamique Maïs Money Long/BDI")
cor_CORN_Money_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_CORN_Money_Long_MSCI,main="Correlation Dynamique Maïs Money Long/MSCI")

#---- ModelDCC Pour Money Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_CORN[,c(13,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_CORN[,c(13,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_CORN_Money_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_CORN_Money_Short_Contract,main="Correlation Dynamique Maïs Money Short/Contrat")
cor_CORN_Money_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_CORN_Money_Short_BDI,main="Correlation Dynamique Maïs Money Short/BDI")
cor_CORN_Money_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_CORN_Money_Short_MSCI,main="Correlation Dynamique Maïs Money Short/MSCI")

#---- ModelDCC Pour Other Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_CORN[,c(14,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_CORN[,c(14,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_CORN_Other_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_CORN_Other_Long_Contract,main="Correlation Dynamique Maïs Other Long/Contrat")
cor_CORN_Other_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_CORN_Other_Long_BDI,main="Correlation Dynamique Maïs Other Long/BDI")
cor_CORN_Other_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_CORN_Other_Long_MSCI,main="Correlation Dynamique Maïs Other Long/MSCI")

#---- ModelDCC Pour Other Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_CORN[,c(15,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_CORN[,c(15,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_CORN_Other_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_CORN_Other_Short_Contract,main="Correlation Dynamique Maïs Other Short/Contrat")
cor_CORN_Other_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_CORN_Other_Short_BDI,main="Correlation Dynamique Maïs Other Short/BDI")
cor_CORN_Other_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_CORN_Other_Short_MSCI,main="Correlation Dynamique Maïs Other Short/MSCI")

