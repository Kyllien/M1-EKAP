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

#-- soybeans ----

soybeansCFTC <- read.csv("CFTC/Transfo2/Soybeans.csv")
soybeansCFTC <- soybeansCFTC[,-1]
colnames(soybeansCFTC) <- c("Date","Rd_Long_Tot","Rd_Long_NoProd","Rd_Short_Tot","Rd_Short_NoProd","Rd_Net","Rd_Net_NoProd","Rd_Merc_Long","Rd_Merc_Short","Rd_Swap_Long","Rd_Swap_Short","Rd_Money_Long","Rd_Money_Short","Rd_Other_Long","Rd_Other_Short")  
soybeansContract <- read_excel("Futures/s_returns.xlsx",4)
soybeansContract <- soybeansContract[-1,]
soybeansCFTC$Date <- as.Date(soybeansCFTC$Date)
soybeansContract$Date <- as.Date(soybeansContract$Date)

#On supprime les données manquantes et qui ne correspondent pas

soybeans <- left_join(soybeansCFTC[],soybeansContract[,c(1,3)])

soybeans <- left_join(soybeans,BDI[,c(1,3)])
soybeans <- left_join(soybeans,MSCI[,c(1,3)])
soybeans <- na.omit(soybeans)

#Plot

ggplot(soybeans) + geom_line(aes(x=Date,y=s_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Long_Tot),color="lightblue")

ggplot(soybeans) + geom_line(aes(x=Date,y=s_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Long_NoProd),color="lightblue")

ggplot(soybeans) + geom_line(aes(x=Date,y=s_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Short_Tot),color="lightblue")

ggplot(soybeans) + geom_line(aes(x=Date,y=s_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Short_NoProd),color="lightblue")

ggplot(soybeans) + geom_line(aes(x=Date,y=s_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Net),color="lightblue")

ggplot(soybeans) + geom_line(aes(x=Date,y=s_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Net_NoProd),color="lightblue")


#ADF TEST
adf.test(soybeans$Rd_Long_Tot)
adf.test(soybeans$Rd_Long_NoProd)
adf.test(soybeans$Rd_Short_Tot)
adf.test(soybeans$Rd_Short_NoProd)
adf.test(soybeans$Rd_Net)
adf.test(soybeans$Rd_Net_NoProd)
adf.test(soybeans$s_weekly_return)
adf.test(soybeans$Rd_MSCI)
adf.test(soybeans$Rd_BDI)
adf.test(soybeans$Rd_Merc_Long)
adf.test(soybeans$Rd_Merc_Short)
adf.test(soybeans$Rd_Swap_Long)
adf.test(soybeans$Rd_Swap_Short)
adf.test(soybeans$Rd_Money_Long)
adf.test(soybeans$Rd_Money_Short)
adf.test(soybeans$Rd_Other_Long)
adf.test(soybeans$Rd_Other_Short)



#ACF/PACF pour creer le garch univariate
par(mfrow=c(1,1))
acf(soybeans$Rd_Long_Tot)
pacf(soybeans$Rd_Long_Tot)# (1,1)

acf(soybeans$Rd_Long_NoProd)
pacf(soybeans$Rd_Long_NoProd) #(1,1)

acf(soybeans$Rd_Short_Tot)
pacf(soybeans$Rd_Short_Tot) #(1,1)

acf(soybeans$Rd_Short_NoProd)
pacf(soybeans$Rd_Short_NoProd) #(1,1)

acf(soybeans$Rd_Net)
pacf(soybeans$Rd_Net)#(1,0)

acf(soybeans$Rd_Net_NoProd)
pacf(soybeans$Rd_Net_NoProd)#(1,1)

acf(soybeans$s_weekly_return)
pacf(soybeans$s_weekly_return)#(1,0)

acf(soybeans$Rd_BDI)
pacf(soybeans$Rd_BDI)#(1,1)

acf(soybeans$Rd_MSCI)
pacf(soybeans$Rd_MSCI)#(1,0)

acf(soybeans$Rd_Merc_Long)
pacf(soybeans$Rd_Merc_Long)#(1,1) a verifier

acf(soybeans$Rd_Merc_Short)
pacf(soybeans$Rd_Merc_Short)#(1,1)

acf(soybeans$Rd_Swap_Long)
pacf(soybeans$Rd_Swap_Long)#(1,1)

acf(soybeans$Rd_Swap_Short)
pacf(soybeans$Rd_Swap_Short)#(1,1) a verifier

acf(soybeans$Rd_Money_Long)
pacf(soybeans$Rd_Money_Long)#(1,1)

acf(soybeans$Rd_Money_Short)
pacf(soybeans$Rd_Money_Short)#(1,1) a verifier

acf(soybeans$Rd_Other_Long)
pacf(soybeans$Rd_Other_Long) #(1,1)

acf(soybeans$Rd_Other_Short)
pacf(soybeans$Rd_Other_Short)#(1,1) a verifier

#Test Ljung
Box.test(soybeans$Rd_Long_Tot^2, type = "Ljung")
Box.test(soybeans$Rd_Long_NoProd^2, type = "Ljung")
Box.test(soybeans$Rd_Short_Tot^2, type = "Ljung")
Box.test(soybeans$Rd_Short_NoProd^2, type = "Ljung")
Box.test(soybeans$Rd_Net^2, type = "Ljung")
Box.test(soybeans$Rd_Net_NoProd^2, type = "Ljung")
Box.test(soybeans$s_weekly_return^2, type = "Ljung")
Box.test(soybeans$Rd_MSCI^2, type = "Ljung")
Box.test(soybeans$Rd_BDI^2, type = "Ljung")
Box.test(soybeans$Rd_Merc_Long^2, type = "Ljung")
Box.test(soybeans$Rd_Merc_Short^2, type = "Ljung")
Box.test(soybeans$Rd_Swap_Long^2, type = "Ljung")
Box.test(soybeans$Rd_Swap_Short^2, type = "Ljung")
Box.test(soybeans$Rd_Money_Long^2, type = "Ljung")
Box.test(soybeans$Rd_Money_Short^2, type = "Ljung")
Box.test(soybeans$Rd_Other_Long^2, type = "Ljung")
Box.test(soybeans$Rd_Other_Short^2, type = "Ljung")

#Test Normalite Jarque
jarque.bera.test(soybeans$Rd_Long_Tot)
jarque.bera.test(soybeans$Rd_Long_NoProd)
jarque.bera.test(soybeans$Rd_Short_Tot)
jarque.bera.test(soybeans$Rd_Short_NoProd)
jarque.bera.test(soybeans$Rd_Net)
jarque.bera.test(soybeans$Rd_Net_NoProd)
jarque.bera.test(soybeans$s_weekly_return)
jarque.bera.test(soybeans$Rd_MSCI)
jarque.bera.test(soybeans$Rd_BDI)
jarque.bera.test(soybeans$Rd_Merc_Long)
jarque.bera.test(soybeans$Rd_Merc_Short)
jarque.bera.test(soybeans$Rd_Swap_Long)
jarque.bera.test(soybeans$Rd_Swap_Short)
jarque.bera.test(soybeans$Rd_Money_Long)
jarque.bera.test(soybeans$Rd_Money_Short)
jarque.bera.test(soybeans$Rd_Other_Long)
jarque.bera.test(soybeans$Rd_Other_Short)

#Xts
Rd_soybeans <- xts(soybeans,order.by=soybeans$Date)

#---- ModelDCC Pour Long Tot ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_soybeans[,c(2,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_soybeans[,c(2,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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

cor_soybeans_Long_Tot_Contract <- as.xts(cor1[2,1,])
plot(cor_soybeans_Long_Tot_Contract,main="Correlation Dynamique Soja Long/Contrat")
cor_soybeans_Long_Tot_BDI <- as.xts(cor1[3,1,])
plot(cor_soybeans_Long_Tot_BDI,main="Correlation Dynamique Soja Long/BDI")
cor_soybeans_Long_Tot_MSCI <- as.xts(cor1[4,1,])
plot(cor_soybeans_Long_Tot_MSCI,main="Correlation Dynamique Soja Long/MSCI")

cor_soybeans_Contract_BDI <- as.xts(cor1[2,3,])
plot(cor_soybeans_Contract_BDI,main="Correlation Dynamique Contrat/BDI")
cor_soybeans_Contract_MSCI <- as.xts(cor1[2,4,])
plot(cor_soybeans_Contract_MSCI,main="Correlation Dynamique Contrat/MSCI")

#---- ModelDCC Pour Long Tot No Prod ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_soybeans[,c(3,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_soybeans[,c(3,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_soybeans_Long_Tot_NoProd_Contract <- as.xts(cor1[2,1,])
plot(cor_soybeans_Long_Tot_NoProd_Contract,main="Correlation Dynamique Soja LongNoProd/Contrat")
cor_soybeans_Long_Tot_NoProd_BDI <- as.xts(cor1[3,1,])
plot(cor_soybeans_Long_Tot_NoProd_BDI,main="Correlation Dynamique Soja LongNoProd/BDI")
cor_soybeans_Long_Tot_NoProd_MSCI <- as.xts(cor1[4,1,])
plot(cor_soybeans_Long_Tot_NoProd_MSCI,main="Correlation Dynamique Soja LongNoProd/MSCI")

#---- ModelDCC Pour Short Tot ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_soybeans[,c(4,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_soybeans[,c(4,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_soybeans_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_soybeans_Short_Contract,main="Correlation Dynamique Soja ShortContrat")
cor_soybeans_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_soybeans_Short_BDI,main="Correlation Dynamique Soja Short/BDI")
cor_soybeans_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_soybeans_Short_MSCI,main="Correlation Dynamique Soja Short/MSCI")

#---- ModelDCC Pour Short NoProd ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_soybeans[,c(5,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_soybeans[,c(5,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_soybeans_Short_NoProd_Contract <- as.xts(cor1[2,1,])
plot(cor_soybeans_Short_NoProd_Contract,main="Correlation Dynamique Soja Short NoProd/Contrat")
cor_soybeans_Short_NoProd_BDI <- as.xts(cor1[3,1,])
plot(cor_soybeans_Short_NoProd_BDI,main="Correlation Dynamique Soja Short NoProd/BDI")
cor_soybeans_Short_NoProd_MSCI <- as.xts(cor1[4,1,])
plot(cor_soybeans_Short_NoProd_MSCI,main="Correlation Dynamique Soja Short NoProd/MSCI")


#---- ModelDCC Pour Merc Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_soybeans[,c(8,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_soybeans[,c(8,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_soybeans_Merc_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_soybeans_Merc_Long_Contract,main="Correlation Dynamique Soja Merc Long/Contrat")
cor_soybeans_Merc_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_soybeans_Merc_Long_BDI,main="Correlation Dynamique Soja Merc Long/BDI")
cor_soybeans_Merc_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_soybeans_Merc_Long_MSCI,main="Correlation Dynamique Soja Merc Long/MSCI")

#---- ModelDCC Pour Merc Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_soybeans[,c(9,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_soybeans[,c(9,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_soybeans_Merc_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_soybeans_Merc_Short_Contract,main="Correlation Dynamique Soja Merc Short/Contrat")
cor_soybeans_Merc_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_soybeans_Merc_Short_BDI,main="Correlation Dynamique Soja Merc Short/BDI")
cor_soybeans_Merc_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_soybeans_Merc_Short_MSCI,main="Correlation Dynamique Soja Merc Short/MSCI")

#---- ModelDCC Pour SWAP Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_soybeans[,c(10,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_soybeans[,c(10,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_soybeans_Swap_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_soybeans_Swap_Long_Contract,main="Correlation Dynamique Soja Swap Long/Contrat")
cor_soybeans_Swap_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_soybeans_Swap_Long_BDI,main="Correlation Dynamique Soja Swap Long/BDI")
cor_soybeans_Swap_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_soybeans_Swap_Long_MSCI,main="Correlation Dynamique Soja Swap Long/MSCI")

#---- ModelDCC Pour Swap Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_soybeans[,c(11,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_soybeans[,c(11,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_soybeans_Swap_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_soybeans_Swap_Short_Contract,main="Correlation Dynamique Soja Swap Short/Contrat")
cor_soybeans_Swap_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_soybeans_Swap_Short_BDI,main="Correlation Dynamique Soja Swap Short/BDI")
cor_soybeans_Swap_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_soybeans_Swap_Short_MSCI,main="Correlation Dynamique Soja Swap Short/MSCI")

#---- ModelDCC Pour Money Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_soybeans[,c(12,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_soybeans[,c(12,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_soybeans_Money_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_soybeans_Money_Long_Contract,main="Correlation Dynamique Soja Money Long/Contrat")
cor_soybeans_Money_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_soybeans_Money_Long_BDI,main="Correlation Dynamique Soja Money Long/BDI")
cor_soybeans_Money_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_soybeans_Money_Long_MSCI,main="Correlation Dynamique Soja Money Long/MSCI")

#---- ModelDCC Pour Money Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_soybeans[,c(13,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_soybeans[,c(13,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_soybeans_Money_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_soybeans_Money_Short_Contract,main="Correlation Dynamique Soja Money Short/Contrat")
cor_soybeans_Money_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_soybeans_Money_Short_BDI,main="Correlation Dynamique Soja Money Short/BDI")
cor_soybeans_Money_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_soybeans_Money_Short_MSCI,main="Correlation Dynamique Soja Money Short/MSCI")

#---- ModelDCC Pour Other Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_soybeans[,c(14,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_soybeans[,c(14,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_soybeans_Other_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_soybeans_Other_Long_Contract,main="Correlation Dynamique Soja Other Long/Contrat")
cor_soybeans_Other_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_soybeans_Other_Long_BDI,main="Correlation Dynamique Soja Other Long/BDI")
cor_soybeans_Other_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_soybeans_Other_Long_MSCI,main="Correlation Dynamique Soja Other Long/MSCI")

#---- ModelDCC Pour Other Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_soybeans[,c(15,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_soybeans[,c(15,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_soybeans_Other_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_soybeans_Other_Short_Contract,main="Correlation Dynamique Soja Other Short/Contrat")
cor_soybeans_Other_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_soybeans_Other_Short_BDI,main="Correlation Dynamique Soja Other Short/BDI")
cor_soybeans_Other_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_soybeans_Other_Short_MSCI,main="Correlation Dynamique Soja Other Short/MSCI")

