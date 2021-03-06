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
library(fBasics)

setwd("D:/M1 EKAP/M�moire/Data")
BDI <- read.csv("Variables Controles/Transfo/BDI.csv")
BDI <- BDI[,-1]
colnames(BDI) <- c("Date","Close","Rd_BDI")
MSCI <- read.csv("Variables Controles/Transfo/MSCI.csv")
MSCI <- MSCI[,-1]
colnames(MSCI) <- c("Date","Close","Rd_MSCI")

MSCI$Date <- as.Date(MSCI$Date)
BDI$Date <- as.Date(BDI$Date)

#-- cocoa ----

cocoaCFTC <- read.csv("CFTC/Transfo2/Cocoa.csv")
cocoaCFTC <- cocoaCFTC[,-1]
colnames(cocoaCFTC) <- c("Date","Rd_Long_Tot","Rd_Long_NoProd","Rd_Short_Tot","Rd_Short_NoProd","Rd_Net","Rd_Net_NoProd","Rd_Merc_Long","Rd_Merc_Short","Rd_Swap_Long","Rd_Swap_Short","Rd_Money_Long","Rd_Money_Short","Rd_Other_Long","Rd_Other_Short")  
cocoaContract <- read_excel("Futures/cc_returns.xlsx",4)
cocoaContract <- cocoaContract[-1,]
cocoaCFTC$Date <- as.Date(cocoaCFTC$Date)
cocoaContract$Date <- as.Date(cocoaContract$Date)

#On supprime les donn�es manquantes et qui ne correspondent pas

cocoa <- left_join(cocoaCFTC[],cocoaContract[,c(1,3)])

cocoa <- left_join(cocoa,BDI[,c(1,3)])
cocoa <- left_join(cocoa,MSCI[,c(1,3)])
cocoa <- na.omit(cocoa)

#Plot

ggplot(cocoa) + geom_line(aes(x=Date,y=cc_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Long_Tot),color="lightblue")

ggplot(cocoa) + geom_line(aes(x=Date,y=cc_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Long_NoProd),color="lightblue")

ggplot(cocoa) + geom_line(aes(x=Date,y=cc_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Short_Tot),color="lightblue")

ggplot(cocoa) + geom_line(aes(x=Date,y=cc_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Short_NoProd),color="lightblue")

ggplot(cocoa) + geom_line(aes(x=Date,y=cc_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Net),color="lightblue")

ggplot(cocoa) + geom_line(aes(x=Date,y=cc_weekly_return),color="darkgreen") + geom_line(aes(x=Date,y=Rd_Net_NoProd),color="lightblue")

#Stats des
basicStats(cocoa[,-1])

#ADF TEST
adf.test(cocoa$Rd_Long_Tot)
adf.test(cocoa$Rd_Long_NoProd)
adf.test(cocoa$Rd_Short_Tot)
adf.test(cocoa$Rd_Short_NoProd)
adf.test(cocoa$Rd_Net)
adf.test(cocoa$Rd_Net_NoProd)
adf.test(cocoa$cc_weekly_return)
adf.test(cocoa$Rd_MSCI)
adf.test(cocoa$Rd_BDI)
adf.test(cocoa$Rd_Merc_Long)
adf.test(cocoa$Rd_Merc_Short)
adf.test(cocoa$Rd_Swap_Long)
adf.test(cocoa$Rd_Swap_Short)
adf.test(cocoa$Rd_Money_Long)
adf.test(cocoa$Rd_Money_Short)
adf.test(cocoa$Rd_Other_Long)
adf.test(cocoa$Rd_Other_Short)


#Test Ljung
Box.test(cocoa$Rd_Long_Tot^2, type = "Ljung")
Box.test(cocoa$Rd_Long_NoProd^2, type = "Ljung")
Box.test(cocoa$Rd_Short_Tot^2, type = "Ljung")
Box.test(cocoa$Rd_Short_NoProd^2, type = "Ljung")
Box.test(cocoa$Rd_Net^2, type = "Ljung")
Box.test(cocoa$Rd_Net_NoProd^2, type = "Ljung")
Box.test(cocoa$cc_weekly_return^2, type = "Ljung")
Box.test(cocoa$Rd_MSCI^2, type = "Ljung")
Box.test(cocoa$Rd_BDI^2, type = "Ljung")
Box.test(cocoa$Rd_Merc_Long^2, type = "Ljung")
Box.test(cocoa$Rd_Merc_Short^2, type = "Ljung")
Box.test(cocoa$Rd_Swap_Long^2, type = "Ljung")
Box.test(cocoa$Rd_Swap_Short^2, type = "Ljung")
Box.test(cocoa$Rd_Money_Long^2, type = "Ljung")
Box.test(cocoa$Rd_Money_Short^2, type = "Ljung")
Box.test(cocoa$Rd_Other_Long^2, type = "Ljung")
Box.test(cocoa$Rd_Other_Short^2, type = "Ljung")

#Test Normalite Jarque
jarque.bera.test(cocoa$Rd_Long_Tot)
jarque.bera.test(cocoa$Rd_Long_NoProd)
jarque.bera.test(cocoa$Rd_Short_Tot)
jarque.bera.test(cocoa$Rd_Short_NoProd)
jarque.bera.test(cocoa$Rd_Net)
jarque.bera.test(cocoa$Rd_Net_NoProd)
jarque.bera.test(cocoa$cc_weekly_return)
jarque.bera.test(cocoa$Rd_MSCI)
jarque.bera.test(cocoa$Rd_BDI)
jarque.bera.test(cocoa$Rd_Merc_Long)
jarque.bera.test(cocoa$Rd_Merc_Short)
jarque.bera.test(cocoa$Rd_Swap_Long)
jarque.bera.test(cocoa$Rd_Swap_Short)
jarque.bera.test(cocoa$Rd_Money_Long)
jarque.bera.test(cocoa$Rd_Money_Short)
jarque.bera.test(cocoa$Rd_Other_Long)
jarque.bera.test(cocoa$Rd_Other_Short)

#ACF/PACF pour creer le garch univariate
par(mfrow=c(1,1))
acf(cocoa$Rd_Long_Tot)
pacf(cocoa$Rd_Long_Tot)# (1,1)

acf(cocoa$Rd_Long_NoProd)
pacf(cocoa$Rd_Long_NoProd) #(1,1)

acf(cocoa$Rd_Short_Tot)
pacf(cocoa$Rd_Short_Tot) #(1,1)

acf(cocoa$Rd_Short_NoProd)
pacf(cocoa$Rd_Short_NoProd) #(1,1)

acf(cocoa$Rd_Net)
pacf(cocoa$Rd_Net)#(1,0)

acf(cocoa$Rd_Net_NoProd)
pacf(cocoa$Rd_Net_NoProd)#(1,1)

acf(cocoa$cc_weekly_return)
pacf(cocoa$cc_weekly_return)#(1,0)

acf(cocoa$Rd_BDI)
pacf(cocoa$Rd_BDI)#(1,1)

acf(cocoa$Rd_MSCI)
pacf(cocoa$Rd_MSCI)#(1,0)

acf(cocoa$Rd_Merc_Long)
pacf(cocoa$Rd_Merc_Long)#(1,1) a verifier

acf(cocoa$Rd_Merc_Short)
pacf(cocoa$Rd_Merc_Short)#(1,1)

acf(cocoa$Rd_Swap_Long)
pacf(cocoa$Rd_Swap_Long)#(1,1)

acf(cocoa$Rd_Swap_Short)
pacf(cocoa$Rd_Swap_Short)#(1,1) a verifier

acf(cocoa$Rd_Money_Long)
pacf(cocoa$Rd_Money_Long)#(1,1)

acf(cocoa$Rd_Money_Short)
pacf(cocoa$Rd_Money_Short)#(1,1) a verifier

acf(cocoa$Rd_Other_Long)
pacf(cocoa$Rd_Other_Long) #(1,1)

acf(cocoa$Rd_Other_Short)
pacf(cocoa$Rd_Other_Short)#(1,1) a verifier


#Xts
Rd_cocoa <- xts(cocoa,order.by=cocoa$Date)

#---- ModelDCC Pour Long Tot ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_cocoa[,c(2,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cocoa[,c(2,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble � un bruit blanc, les modele semblent etre correct

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

cor_cocoa_Long_Tot_Contract <- as.xts(cor1[2,1,])
plot(cor_cocoa_Long_Tot_Contract,main="Correlation Dynamique Cacao Long/Contrat")
cor_cocoa_Long_Tot_BDI <- as.xts(cor1[3,1,])
plot(cor_cocoa_Long_Tot_BDI,main="Correlation Dynamique Cacao Long/BDI")
cor_cocoa_Long_Tot_MSCI <- as.xts(cor1[4,1,])
plot(cor_cocoa_Long_Tot_MSCI,main="Correlation Dynamique Cacao Long/MSCI")

cor_cocoa_Contract_BDI <- as.xts(cor1[2,3,])
plot(cor_cocoa_Contract_BDI,main="Correlation Dynamique Cacao Contrat/BDI")
cor_cocoa_Contract_MSCI <- as.xts(cor1[2,4,])
plot(cor_cocoa_Contract_MSCI,main="Correlation Dynamique Cacao Contrat/MSCI")

#---- ModelDCC Pour Long Tot No Prod ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_cocoa[,c(3,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cocoa[,c(3,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble � un bruit blanc, les modele semblent etre correct

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
cor_cocoa_Long_Tot_NoProd_Contract <- as.xts(cor1[2,1,])
plot(cor_cocoa_Long_Tot_NoProd_Contract,main="Correlation Dynamique Cacao LongNoProd/Contrat")
cor_cocoa_Long_Tot_NoProd_BDI <- as.xts(cor1[3,1,])
plot(cor_cocoa_Long_Tot_NoProd_BDI,main="Correlation Dynamique Cacao LongNoProd/BDI")
cor_cocoa_Long_Tot_NoProd_MSCI <- as.xts(cor1[4,1,])
plot(cor_cocoa_Long_Tot_NoProd_MSCI,main="Correlation Dynamique Cacao LongNoProd/MSCI")

#---- ModelDCC Pour Short Tot ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_cocoa[,c(4,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cocoa[,c(4,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble � un bruit blanc, les modele semblent etre correct

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
cor_cocoa_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_cocoa_Short_Contract,main="Correlation Dynamique Cacao ShortContrat")
cor_cocoa_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_cocoa_Short_BDI,main="Correlation Dynamique Cacao Short/BDI")
cor_cocoa_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_cocoa_Short_MSCI,main="Correlation Dynamique Cacao Short/MSCI")

#---- ModelDCC Pour Short NoProd ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_cocoa[,c(5,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cocoa[,c(5,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble � un bruit blanc, les modele semblent etre correct

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
cor_cocoa_Short_NoProd_Contract <- as.xts(cor1[2,1,])
plot(cor_cocoa_Short_NoProd_Contract,main="Correlation Dynamique Cacao Short NoProd/Contrat")
cor_cocoa_Short_NoProd_BDI <- as.xts(cor1[3,1,])
plot(cor_cocoa_Short_NoProd_BDI,main="Correlation Dynamique Cacao Short NoProd/BDI")
cor_cocoa_Short_NoProd_MSCI <- as.xts(cor1[4,1,])
plot(cor_cocoa_Short_NoProd_MSCI,main="Correlation Dynamique Cacao Short NoProd/MSCI")


#---- ModelDCC Pour Merc Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_cocoa[,c(8,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cocoa[,c(8,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble � un bruit blanc, les modele semblent etre correct
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
cor_cocoa_Merc_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_cocoa_Merc_Long_Contract,main="Correlation Dynamique Cacao Merc Long/Contrat")
cor_cocoa_Merc_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_cocoa_Merc_Long_BDI,main="Correlation Dynamique Cacao Merc Long/BDI")
cor_cocoa_Merc_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_cocoa_Merc_Long_MSCI,main="Correlation Dynamique Cacao Merc Long/MSCI")

#---- ModelDCC Pour Merc Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_cocoa[,c(9,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cocoa[,c(9,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble � un bruit blanc, les modele semblent etre correct

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
cor_cocoa_Merc_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_cocoa_Merc_Short_Contract,main="Correlation Dynamique Cacao Merc Short/Contrat")
cor_cocoa_Merc_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_cocoa_Merc_Short_BDI,main="Correlation Dynamique Cacao Merc Short/BDI")
cor_cocoa_Merc_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_cocoa_Merc_Short_MSCI,main="Correlation Dynamique Cacao Merc Short/MSCI")

#---- ModelDCC Pour SWAP Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_cocoa[,c(10,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cocoa[,c(10,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble � un bruit blanc, les modele semblent etre correct

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
cor_cocoa_Swap_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_cocoa_Swap_Long_Contract,main="Correlation Dynamique Cacao Swap Long/Contrat")
cor_cocoa_Swap_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_cocoa_Swap_Long_BDI,main="Correlation Dynamique Cacao Swap Long/BDI")
cor_cocoa_Swap_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_cocoa_Swap_Long_MSCI,main="Correlation Dynamique Cacao Swap Long/MSCI")

#---- ModelDCC Pour Swap Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_cocoa[,c(11,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cocoa[,c(11,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble � un bruit blanc, les modele semblent etre correct

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
cor_cocoa_Swap_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_cocoa_Swap_Short_Contract,main="Correlation Dynamique Cacao Swap Short/Contrat")
cor_cocoa_Swap_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_cocoa_Swap_Short_BDI,main="Correlation Dynamique Cacao Swap Short/BDI")
cor_cocoa_Swap_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_cocoa_Swap_Short_MSCI,main="Correlation Dynamique Cacao Swap Short/MSCI")

#---- ModelDCC Pour Money Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_cocoa[,c(12,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cocoa[,c(12,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble � un bruit blanc, les modele semblent etre correct

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
cor_cocoa_Money_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_cocoa_Money_Long_Contract,main="Correlation Dynamique Cacao Money Long/Contrat")
cor_cocoa_Money_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_cocoa_Money_Long_BDI,main="Correlation Dynamique Cacao Money Long/BDI")
cor_cocoa_Money_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_cocoa_Money_Long_MSCI,main="Correlation Dynamique Cacao Money Long/MSCI")

#---- ModelDCC Pour Money Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_cocoa[,c(13,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cocoa[,c(13,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble � un bruit blanc, les modele semblent etre correct

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
cor_cocoa_Money_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_cocoa_Money_Short_Contract,main="Correlation Dynamique Cacao Money Short/Contrat")
cor_cocoa_Money_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_cocoa_Money_Short_BDI,main="Correlation Dynamique Cacao Money Short/BDI")
cor_cocoa_Money_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_cocoa_Money_Short_MSCI,main="Correlation Dynamique Cacao Money Short/MSCI")

#---- ModelDCC Pour Other Long ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_cocoa[,c(14,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cocoa[,c(14,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble � un bruit blanc, les modele semblent etre correct

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
cor_cocoa_Other_Long_Contract <- as.xts(cor1[2,1,])
plot(cor_cocoa_Other_Long_Contract,main="Correlation Dynamique Cacao Other Long/Contrat")
cor_cocoa_Other_Long_BDI <- as.xts(cor1[3,1,])
plot(cor_cocoa_Other_Long_BDI,main="Correlation Dynamique Cacao Other Long/BDI")
cor_cocoa_Other_Long_MSCI <- as.xts(cor1[4,1,])
plot(cor_cocoa_Other_Long_MSCI,main="Correlation Dynamique Cacao Other Long/MSCI")

#---- ModelDCC Pour Other Short ----
uspec.n <- multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_cocoa[,c(15,16,17,18)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cocoa[,c(15,16,17,18)],fit.control=list(eval.se = TRUE), fit=multf)
fit1
#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble � un bruit blanc, les modele semblent etre correct

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
cor_cocoa_Other_Short_Contract <- as.xts(cor1[2,1,])
plot(cor_cocoa_Other_Short_Contract,main="Correlation Dynamique Cacao Other Short/Contrat")
cor_cocoa_Other_Short_BDI <- as.xts(cor1[3,1,])
plot(cor_cocoa_Other_Short_BDI,main="Correlation Dynamique Cacao Other Short/BDI")
cor_cocoa_Other_Short_MSCI <- as.xts(cor1[4,1,])
plot(cor_cocoa_Other_Short_MSCI,main="Correlation Dynamique Cacao Other Short/MSCI")

