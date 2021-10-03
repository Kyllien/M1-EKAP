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

#-- sugar ----

sugarCFTC <- read.csv("CFTC/Transfo2/Sugar.csv")
sugarCFTC <- sugarCFTC[,-1]
colnames(sugarCFTC) <- c("Date","Rd_Long_Tot","Rd_Long_NoProd","Rd_Short_Tot","Rd_Short_NoProd","Rd_Net","Rd_Net_NoProd","Rd_Merc_Long","Rd_Merc_Short","Rd_Swap_Long","Rd_Swap_Short","Rd_Money_Long","Rd_Money_Short","Rd_Other_Long","Rd_Other_Short")  
sugarContract <- read_excel("Futures/sb_returns.xlsx",4)
sugarContract <- sugarContract[-1,]
sugarCFTC$Date <- as.Date(sugarCFTC$Date)
sugarContract$Date <- as.Date(sugarContract$Date)

#On supprime les données manquantes et qui ne correspondent pas

sugar <- left_join(sugarCFTC[],sugarContract[,c(1,3)])

sugar <- left_join(sugar,BDI[,c(1,3)])
sugar <- left_join(sugar,MSCI[,c(1,3)])
sugar <- na.omit(sugar)

#Plot

ggplot(sugar) + geom_line(aes(x=Date,y=sb_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Long_Tot),color="lightblue")

ggplot(sugar) + geom_line(aes(x=Date,y=sb_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Long_NoProd),color="lightblue")

ggplot(sugar) + geom_line(aes(x=Date,y=sb_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Short_Tot),color="lightblue")

ggplot(sugar) + geom_line(aes(x=Date,y=sb_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Short_NoProd),color="lightblue")

ggplot(sugar) + geom_line(aes(x=Date,y=sb_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Net),color="lightblue")

ggplot(sugar) + geom_line(aes(x=Date,y=sb_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Net_NoProd),color="lightblue")

#Stats
basicStats(sugar[,-1])

#ADF TEST
adf.test(sugar$Rd_Long_Tot)
adf.test(sugar$Rd_Long_NoProd)
adf.test(sugar$Rd_Short_Tot)
adf.test(sugar$Rd_Short_NoProd)
adf.test(sugar$Rd_Net)
adf.test(sugar$Rd_Net_NoProd)
adf.test(sugar$sb_weekly_return)
adf.test(sugar$Rd_MSCI)
adf.test(sugar$Rd_BDI)
adf.test(sugar$Rd_Merc_Long)
adf.test(sugar$Rd_Merc_Short)
adf.test(sugar$Rd_Swap_Long)
adf.test(sugar$Rd_Swap_Short)
adf.test(sugar$Rd_Money_Long)
adf.test(sugar$Rd_Money_Short)
adf.test(sugar$Rd_Other_Long)
adf.test(sugar$Rd_Other_Short)

#Test Ljung
Box.test(sugar$Rd_Long_Tot^2, type = "Ljung")
Box.test(sugar$Rd_Long_NoProd^2, type = "Ljung")
Box.test(sugar$Rd_Short_Tot^2, type = "Ljung")
Box.test(sugar$Rd_Short_NoProd^2, type = "Ljung")
Box.test(sugar$Rd_Net^2, type = "Ljung")
Box.test(sugar$Rd_Net_NoProd^2, type = "Ljung")
Box.test(sugar$sb_weekly_return^2, type = "Ljung")
Box.test(sugar$Rd_MSCI^2, type = "Ljung")
Box.test(sugar$Rd_BDI^2, type = "Ljung")
Box.test(sugar$Rd_Merc_Long^2, type = "Ljung")
Box.test(sugar$Rd_Merc_Short^2, type = "Ljung")
Box.test(sugar$Rd_Swap_Long^2, type = "Ljung")
Box.test(sugar$Rd_Swap_Short^2, type = "Ljung")
Box.test(sugar$Rd_Money_Long^2, type = "Ljung")
Box.test(sugar$Rd_Money_Short^2, type = "Ljung")
Box.test(sugar$Rd_Other_Long^2, type = "Ljung")
Box.test(sugar$Rd_Other_Short^2, type = "Ljung")

#Test Normalite Jarque
jarque.bera.test(sugar$Rd_Long_Tot)
jarque.bera.test(sugar$Rd_Long_NoProd)
jarque.bera.test(sugar$Rd_Short_Tot)
jarque.bera.test(sugar$Rd_Short_NoProd)
jarque.bera.test(sugar$Rd_Net)
jarque.bera.test(sugar$Rd_Net_NoProd)
jarque.bera.test(sugar$sb_weekly_return)
jarque.bera.test(sugar$Rd_MSCI)
jarque.bera.test(sugar$Rd_BDI)
jarque.bera.test(sugar$Rd_Merc_Long)
jarque.bera.test(sugar$Rd_Merc_Short)
jarque.bera.test(sugar$Rd_Swap_Long)
jarque.bera.test(sugar$Rd_Swap_Short)
jarque.bera.test(sugar$Rd_Money_Long)
jarque.bera.test(sugar$Rd_Money_Short)
jarque.bera.test(sugar$Rd_Other_Long)
jarque.bera.test(sugar$Rd_Other_Short)

#ACF/PACF pour creer le garch univariate
par(mfrow=c(1,1))
acf(sugar$Rd_Long_Tot)
pacf(sugar$Rd_Long_Tot)# (1,1)

acf(sugar$Rd_Long_NoProd)
pacf(sugar$Rd_Long_NoProd) #(1,1)

acf(sugar$Rd_Short_Tot)
pacf(sugar$Rd_Short_Tot) #(1,1)

acf(sugar$Rd_Short_NoProd)
pacf(sugar$Rd_Short_NoProd) #(1,1)

acf(sugar$Rd_Net)
pacf(sugar$Rd_Net)#(1,0)

acf(sugar$Rd_Net_NoProd)
pacf(sugar$Rd_Net_NoProd)#(1,1)

acf(sugar$sb_weekly_return)
pacf(sugar$sb_weekly_return)#(1,0)

acf(sugar$Rd_BDI)
pacf(sugar$Rd_BDI)#(1,1)

acf(sugar$Rd_MSCI)
pacf(sugar$Rd_MSCI)#(1,0)

acf(sugar$Rd_Merc_Long)
pacf(sugar$Rd_Merc_Long)#(1,1) a verifier

acf(sugar$Rd_Merc_Short)
pacf(sugar$Rd_Merc_Short)#(1,1)

acf(sugar$Rd_Swap_Long)
pacf(sugar$Rd_Swap_Long)#(1,1)

acf(sugar$Rd_Swap_Short)
pacf(sugar$Rd_Swap_Short)#(1,1) a verifier

acf(sugar$Rd_Money_Long)
pacf(sugar$Rd_Money_Long)#(1,1)

acf(sugar$Rd_Money_Short)
pacf(sugar$Rd_Money_Short)#(1,1) a verifier

acf(sugar$Rd_Other_Long)
pacf(sugar$Rd_Other_Long) #(1,1)

acf(sugar$Rd_Other_Short)
pacf(sugar$Rd_Other_Short)#(1,1) a verifier


#Xts
Rd_sugar <- xts(sugar,order.by=sugar$Date)

#---- ModelDCC Pour Long Tot ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_sugar[,c(2,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_sugar[,c(2,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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

cor_sugar_Long_Tot_Contract <- as.xts(cor1[2,1,])
plot(cor_sugar_Long_Tot_Contract,main="Correlation Dynamique Sucre Long/Contrat")
cor_sugar_Long_Tot_BDI <- as.xts(cor1[3,1,])
plot(cor_sugar_Long_Tot_BDI,main="Correlation Dynamique Sucre Long/BDI")
cor_sugar_Long_Tot_MSCI <- as.xts(cor1[4,1,])
plot(cor_sugar_Long_Tot_MSCI,main="Correlation Dynamique Sucre Long/MSCI")

cor_sugar_Contract_BDI <- as.xts(cor1[2,3,])
plot(cor_sugar_Contract_BDI,main="Correlation Dynamique Contrat/BDI")
cor_sugar_Contract_MSCI <- as.xts(cor1[2,4,])
plot(cor_sugar_Contract_MSCI,main="Correlation Dynamique Contrat/MSCI")

#---- ModelDCC Pour Long Tot No Prod ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_sugar[,c(3,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_sugar[,c(3,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_sugar_Long_Tot_NoProd_Contract <- as.xts(cor1[2,1,])
plot(cor_sugar_Long_Tot_NoProd_Contract,main="Correlation Dynamique Sucre LongNoProd/Contrat")
cor_sugar_Long_Tot_NoProd_BDI <- as.xts(cor1[3,1,])
plot(cor_sugar_Long_Tot_NoProd_BDI,main="Correlation Dynamique Sucre LongNoProd/BDI")
cor_sugar_Long_Tot_NoProd_MSCI <- as.xts(cor1[4,1,])
plot(cor_sugar_Long_Tot_NoProd_MSCI,main="Correlation Dynamique Sucre LongNoProd/MSCI")

#---- ModelDCC Pour Short Tot ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_sugar[,c(4,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_sugar[,c(4,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_sugar_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_sugar_Short_Contract,main="Correlation Dynamique Sucre ShortContrat")
cor_sugar_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_sugar_Short_BDI,main="Correlation Dynamique Sucre Short/BDI")
cor_sugar_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_sugar_Short_MSCI,main="Correlation Dynamique Sucre Short/MSCI")

#---- ModelDCC Pour Short NoProd ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_sugar[,c(5,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_sugar[,c(5,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_sugar_Short_NoProd_Contract <- as.xts(cor1[2,1,])
plot(cor_sugar_Short_NoProd_Contract,main="Correlation Dynamique Sucre Short NoProd/Contrat")
cor_sugar_Short_NoProd_BDI <- as.xts(cor1[3,1,])
plot(cor_sugar_Short_NoProd_BDI,main="Correlation Dynamique Sucre Short NoProd/BDI")
cor_sugar_Short_NoProd_MSCI <- as.xts(cor1[4,1,])
plot(cor_sugar_Short_NoProd_MSCI,main="Correlation Dynamique Sucre Short NoProd/MSCI")


#---- ModelDCC Pour Merc Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_sugar[,c(8,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_sugar[,c(8,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_sugar_Merc_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_sugar_Merc_Long_Contract,main="Correlation Dynamique Sucre Merc Long/Contrat")
cor_sugar_Merc_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_sugar_Merc_Long_BDI,main="Correlation Dynamique Sucre Merc Long/BDI")
cor_sugar_Merc_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_sugar_Merc_Long_MSCI,main="Correlation Dynamique Sucre Merc Long/MSCI")

#---- ModelDCC Pour Merc Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_sugar[,c(9,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_sugar[,c(9,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_sugar_Merc_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_sugar_Merc_Short_Contract,main="Correlation Dynamique Sucre Merc Short/Contrat")
cor_sugar_Merc_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_sugar_Merc_Short_BDI,main="Correlation Dynamique Sucre Merc Short/BDI")
cor_sugar_Merc_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_sugar_Merc_Short_MSCI,main="Correlation Dynamique Sucre Merc Short/MSCI")

#---- ModelDCC Pour SWAP Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_sugar[,c(10,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_sugar[,c(10,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_sugar_Swap_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_sugar_Swap_Long_Contract,main="Correlation Dynamique Sucre Swap Long/Contrat")
cor_sugar_Swap_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_sugar_Swap_Long_BDI,main="Correlation Dynamique Sucre Swap Long/BDI")
cor_sugar_Swap_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_sugar_Swap_Long_MSCI,main="Correlation Dynamique Sucre Swap Long/MSCI")

#---- ModelDCC Pour Swap Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_sugar[,c(11,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_sugar[,c(11,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_sugar_Swap_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_sugar_Swap_Short_Contract,main="Correlation Dynamique Sucre Swap Short/Contrat")
cor_sugar_Swap_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_sugar_Swap_Short_BDI,main="Correlation Dynamique Sucre Swap Short/BDI")
cor_sugar_Swap_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_sugar_Swap_Short_MSCI,main="Correlation Dynamique Sucre Swap Short/MSCI")

#---- ModelDCC Pour Money Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_sugar[,c(12,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_sugar[,c(12,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_sugar_Money_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_sugar_Money_Long_Contract,main="Correlation Dynamique Sucre Money Long/Contrat")
cor_sugar_Money_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_sugar_Money_Long_BDI,main="Correlation Dynamique Sucre Money Long/BDI")
cor_sugar_Money_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_sugar_Money_Long_MSCI,main="Correlation Dynamique Sucre Money Long/MSCI")

#---- ModelDCC Pour Money Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_sugar[,c(13,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_sugar[,c(13,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
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
cor_sugar_Money_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_sugar_Money_Short_Contract,main="Correlation Dynamique Sucre Money Short/Contrat")
cor_sugar_Money_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_sugar_Money_Short_BDI,main="Correlation Dynamique Sucre Money Short/BDI")
cor_sugar_Money_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_sugar_Money_Short_MSCI,main="Correlation Dynamique Sucre Money Short/MSCI")

#---- ModelDCC Pour Other Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_sugar[,c(14,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_sugar[,c(14,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_sugar_Other_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_sugar_Other_Long_Contract,main="Correlation Dynamique Sucre Other Long/Contrat")
cor_sugar_Other_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_sugar_Other_Long_BDI,main="Correlation Dynamique Sucre Other Long/BDI")
cor_sugar_Other_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_sugar_Other_Long_MSCI,main="Correlation Dynamique Sucre Other Long/MSCI")

#---- ModelDCC Pour Other Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_sugar[,c(15,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_sugar[,c(15,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)

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
cor_sugar_Other_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_sugar_Other_Short_Contract,main="Correlation Dynamique Sucre Other Short/Contrat")
cor_sugar_Other_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_sugar_Other_Short_BDI,main="Correlation Dynamique Sucre Other Short/BDI")
cor_sugar_Other_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_sugar_Other_Short_MSCI,main="Correlation Dynamique Sucre Other Short/MSCI")

