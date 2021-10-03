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

#-- wti ----

wtiCFTC <- read.csv("CFTC/Transfo2/wti.csv")
wtiCFTC <- wtiCFTC[,-1]
colnames(wtiCFTC) <- c("Date","Rd_Long_Tot","Rd_Long_NoProd","Rd_Short_Tot","Rd_Short_NoProd","Rd_Net","Rd_Net_NoProd","Rd_Merc_Long","Rd_Merc_Short","Rd_Swap_Long","Rd_Swap_Short","Rd_Money_Long","Rd_Money_Short","Rd_Other_Long","Rd_Other_Short")  
wtiContract <- read_excel("Futures/cl_returns.xlsx",3)
wtiContract <- wtiContract[-1,]
wtiCFTC$Date <- as.Date(wtiCFTC$Date)
wtiContract$Date <- as.Date(wtiContract$Date)

#On supprime les données manquantes et qui ne correspondent pas

wti <- left_join(wtiCFTC[],wtiContract[,c(1,3)])

wti <- left_join(wti,BDI[,c(1,3)])
wti <- left_join(wti,MSCI[,c(1,3)])
wti <- na.omit(wti)

#Plot

ggplot(wti) + geom_line(aes(x=Date,y=cl_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Long_Tot),color="lightblue")

ggplot(wti) + geom_line(aes(x=Date,y=cl_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Long_NoProd),color="lightblue")

ggplot(wti) + geom_line(aes(x=Date,y=cl_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Short_Tot),color="lightblue")

ggplot(wti) + geom_line(aes(x=Date,y=cl_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Short_NoProd),color="lightblue")

ggplot(wti) + geom_line(aes(x=Date,y=cl_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Net),color="lightblue")

ggplot(wti) + geom_line(aes(x=Date,y=cl_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Net_NoProd),color="lightblue")

basicStats(wti[,-1])
#ADF TEST
adf.test(wti$Rd_Long_Tot)
adf.test(wti$Rd_Long_NoProd)
adf.test(wti$Rd_Short_Tot)
adf.test(wti$Rd_Short_NoProd)
adf.test(wti$Rd_Net)
adf.test(wti$Rd_Net_NoProd)
adf.test(wti$cl_weekly_return)
adf.test(wti$Rd_MSCI)
adf.test(wti$Rd_BDI)
adf.test(wti$Rd_Merc_Long)
adf.test(wti$Rd_Merc_Short)
adf.test(wti$Rd_Swap_Long)
adf.test(wti$Rd_Swap_Short)
adf.test(wti$Rd_Money_Long)
adf.test(wti$Rd_Money_Short)
adf.test(wti$Rd_Other_Long)
adf.test(wti$Rd_Other_Short)

#Test Ljung
Box.test(wti$Rd_Long_Tot^2, type = "Ljung")
Box.test(wti$Rd_Long_NoProd^2, type = "Ljung")
Box.test(wti$Rd_Short_Tot^2, type = "Ljung")
Box.test(wti$Rd_Short_NoProd^2, type = "Ljung")
Box.test(wti$Rd_Net^2, type = "Ljung")
Box.test(wti$Rd_Net_NoProd^2, type = "Ljung")
Box.test(wti$cl_weekly_return^2, type = "Ljung")
Box.test(wti$Rd_MSCI^2, type = "Ljung")
Box.test(wti$Rd_BDI^2, type = "Ljung")
Box.test(wti$Rd_Merc_Long^2, type = "Ljung")
Box.test(wti$Rd_Merc_Short^2, type = "Ljung")
Box.test(wti$Rd_Swap_Long^2, type = "Ljung")
Box.test(wti$Rd_Swap_Short^2, type = "Ljung")
Box.test(wti$Rd_Money_Long^2, type = "Ljung")
Box.test(wti$Rd_Money_Short^2, type = "Ljung")
Box.test(wti$Rd_Other_Long^2, type = "Ljung")
Box.test(wti$Rd_Other_Short^2, type = "Ljung")

#Test Normalite Jarque
jarque.bera.test(wti$Rd_Long_Tot)
jarque.bera.test(wti$Rd_Long_NoProd)
jarque.bera.test(wti$Rd_Short_Tot)
jarque.bera.test(wti$Rd_Short_NoProd)
jarque.bera.test(wti$Rd_Net)
jarque.bera.test(wti$Rd_Net_NoProd)
jarque.bera.test(wti$cl_weekly_return)
jarque.bera.test(wti$Rd_MSCI)
jarque.bera.test(wti$Rd_BDI)
jarque.bera.test(wti$Rd_Merc_Long)
jarque.bera.test(wti$Rd_Merc_Short)
jarque.bera.test(wti$Rd_Swap_Long)
jarque.bera.test(wti$Rd_Swap_Short)
jarque.bera.test(wti$Rd_Money_Long)
jarque.bera.test(wti$Rd_Money_Short)
jarque.bera.test(wti$Rd_Other_Long)
jarque.bera.test(wti$Rd_Other_Short)

#ACF/PACF pour creer le garch univariate
par(mfrow=c(1,1))
acf(wti$Rd_Long_Tot)
pacf(wti$Rd_Long_Tot)# (1,1)

acf(wti$Rd_Long_NoProd)
pacf(wti$Rd_Long_NoProd) #(1,0)

acf(wti$Rd_Short_Tot)
pacf(wti$Rd_Short_Tot) #(1,1)

acf(wti$Rd_Short_NoProd)
pacf(wti$Rd_Short_NoProd) #(1,0)

acf(wti$Rd_Net)
pacf(wti$Rd_Net)#(1,0)

acf(wti$Rd_Net_NoProd)
pacf(wti$Rd_Net_NoProd)#(1,1)

acf(wti$cl_weekly_return)
pacf(wti$cl_weekly_return)#(1,0)

acf(wti$Rd_BDI)
pacf(wti$Rd_BDI)#(1,1)

acf(wti$Rd_MSCI)
pacf(wti$Rd_MSCI)#(1,0)

acf(wti$Rd_Merc_Long)
pacf(wti$Rd_Merc_Long)#(1,1) a verifier

acf(wti$Rd_Merc_Short)
pacf(wti$Rd_Merc_Short)#(1,1)

acf(wti$Rd_Swap_Long)
pacf(wti$Rd_Swap_Long)#(1,1)

acf(wti$Rd_Swap_Short)
pacf(wti$Rd_Swap_Short)#(1,1) a verifier

acf(wti$Rd_Money_Long)
pacf(wti$Rd_Money_Long)#(1,1)

acf(wti$Rd_Money_Short)
pacf(wti$Rd_Money_Short)#(1,1) a verifier

acf(wti$Rd_Other_Long)
pacf(wti$Rd_Other_Long) #(1,1)

acf(wti$Rd_Other_Short)
pacf(wti$Rd_Other_Short)#(1,1) a verifier


#Xts
Rd_wti <- xts(wti,order.by=wti$Date)

#---- ModelDCC Pour Long Tot ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wti[,c(2,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wti[,c(2,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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

cor_wti_Long_Tot_Contract <- as.xts(cor1[2,1,])
plot(cor_wti_Long_Tot_Contract,main="Correlation Dynamique WTI Long/Contrat")
cor_wti_Long_Tot_BDI <- as.xts(cor1[3,1,])
plot(cor_wti_Long_Tot_BDI,main="Correlation Dynamique WTI Long/BDI")
cor_wti_Long_Tot_MSCI <- as.xts(cor1[4,1,])
plot(cor_wti_Long_Tot_MSCI,main="Correlation Dynamique WTI Long/MSCI")

cor_wti_Contract_BDI <- as.xts(cor1[2,3,])
plot(cor_wti_Contract_BDI,main="Correlation Dynamique WTI Contrat/BDI")
cor_wti_Contract_MSCI <- as.xts(cor1[2,4,])
plot(cor_wti_Contract_MSCI,main="Correlation Dynamique  WTI Contrat/MSCI")

#---- ModelDCC Pour Long Tot No Prod ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wti[,c(3,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wti[,c(3,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wti_Long_Tot_NoProd_Contract <- as.xts(cor1[2,1,])
plot(cor_wti_Long_Tot_NoProd_Contract,main="Correlation Dynamique WTI LongNoProd/Contrat")
cor_wti_Long_Tot_NoProd_BDI <- as.xts(cor1[3,1,])
plot(cor_wti_Long_Tot_NoProd_BDI,main="Correlation Dynamique WTI LongNoProd/BDI")
cor_wti_Long_Tot_NoProd_MSCI <- as.xts(cor1[4,1,])
plot(cor_wti_Long_Tot_NoProd_MSCI,main="Correlation Dynamique WTI LongNoProd/MSCI")

#---- ModelDCC Pour Short Tot ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wti[,c(4,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wti[,c(4,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wti_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_wti_Short_Contract,main="Correlation Dynamique WTI ShortContrat")
cor_wti_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_wti_Short_BDI,main="Correlation Dynamique WTI Short/BDI")
cor_wti_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_wti_Short_MSCI,main="Correlation Dynamique WTI Short/MSCI")

#---- ModelDCC Pour Short NoProd ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wti[,c(5,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wti[,c(5,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wti_Short_NoProd_Contract <- as.xts(cor1[2,1,])
plot(cor_wti_Short_NoProd_Contract,main="Correlation Dynamique WTI Short NoProd/Contrat")
cor_wti_Short_NoProd_BDI <- as.xts(cor1[3,1,])
plot(cor_wti_Short_NoProd_BDI,main="Correlation Dynamique WTI Short NoProd/BDI")
cor_wti_Short_NoProd_MSCI <- as.xts(cor1[4,1,])
plot(cor_wti_Short_NoProd_MSCI,main="Correlation Dynamique WTI Short NoProd/MSCI")


#---- ModelDCC Pour Merc Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wti[,c(8,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wti[,c(8,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wti_Merc_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_wti_Merc_Long_Contract,main="Correlation Dynamique WTI Merc Long/Contrat")
cor_wti_Merc_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_wti_Merc_Long_BDI,main="Correlation Dynamique WTI Merc Long/BDI")
cor_wti_Merc_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_wti_Merc_Long_MSCI,main="Correlation Dynamique WTI Merc Long/MSCI")

#---- ModelDCC Pour Merc Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wti[,c(9,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wti[,c(9,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wti_Merc_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_wti_Merc_Short_Contract,main="Correlation Dynamique WTI Merc Short/Contrat")
cor_wti_Merc_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_wti_Merc_Short_BDI,main="Correlation Dynamique WTI Merc Short/BDI")
cor_wti_Merc_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_wti_Merc_Short_MSCI,main="Correlation Dynamique WTI Merc Short/MSCI")

#---- ModelDCC Pour SWAP Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wti[,c(10,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wti[,c(10,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wti_Swap_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_wti_Swap_Long_Contract,main="Correlation Dynamique WTI Swap Long/Contrat")
cor_wti_Swap_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_wti_Swap_Long_BDI,main="Correlation Dynamique WTI Swap Long/BDI")
cor_wti_Swap_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_wti_Swap_Long_MSCI,main="Correlation Dynamique WTI Swap Long/MSCI")

#---- ModelDCC Pour Swap Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wti[,c(11,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wti[,c(11,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wti_Swap_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_wti_Swap_Short_Contract,main="Correlation Dynamique WTI Swap Short/Contrat")
cor_wti_Swap_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_wti_Swap_Short_BDI,main="Correlation Dynamique WTI Swap Short/BDI")
cor_wti_Swap_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_wti_Swap_Short_MSCI,main="Correlation Dynamique WTI Swap Short/MSCI")

#---- ModelDCC Pour Money Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wti[,c(12,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wti[,c(12,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wti_Money_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_wti_Money_Long_Contract,main="Correlation Dynamique WTI Money Long/Contrat")
cor_wti_Money_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_wti_Money_Long_BDI,main="Correlation Dynamique WTI Money Long/BDI")
cor_wti_Money_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_wti_Money_Long_MSCI,main="Correlation Dynamique WTI Money Long/MSCI")

#---- ModelDCC Pour Money Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wti[,c(13,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wti[,c(13,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_wti_Money_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_wti_Money_Short_Contract,main="Correlation Dynamique WTI Money Short/Contrat")
cor_wti_Money_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_wti_Money_Short_BDI,main="Correlation Dynamique WTI Money Short/BDI")
cor_wti_Money_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_wti_Money_Short_MSCI,main="Correlation Dynamique WTI Money Short/MSCI")

#---- ModelDCC Pour Other Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wti[,c(14,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wti[,c(14,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_wti_Other_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_wti_Other_Long_Contract,main="Correlation Dynamique WTI Other Long/Contrat")
cor_wti_Other_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_wti_Other_Long_BDI,main="Correlation Dynamique WTI Other Long/BDI")
cor_wti_Other_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_wti_Other_Long_MSCI,main="Correlation Dynamique WTI Other Long/MSCI")

#---- ModelDCC Pour Other Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wti[,c(15,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wti[,c(15,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_wti_Other_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_wti_Other_Short_Contract,main="Correlation Dynamique WTI Other Short/Contrat")
cor_wti_Other_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_wti_Other_Short_BDI,main="Correlation Dynamique WTI Other Short/BDI")
cor_wti_Other_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_wti_Other_Short_MSCI,main="Correlation Dynamique WTI Other Short/MSCI")

