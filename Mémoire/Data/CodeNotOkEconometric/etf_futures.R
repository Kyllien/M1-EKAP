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

#-- CORN ----

cornETF <- read.csv("ETF/CORN.csv")
cornC <- read_excel("Futures/c_returns.xlsx",3)
cornETF$Date <- as.Date(cornETF$Date)
cornC$Date <- as.Date(cornC$Date)

#On supprime les données manquantes et qui ne correspondent pas

CORN <- left_join(cornC[,c(1,2)],cornETF[,c(1,5)])
CORN <- na.omit(CORN)

#Calcul des rendements avec log et diff

Rd_CORN <- CORN[-1,1]
Rd_CORN$ETF <- diff(log(CORN$Close))
Rd_CORN$Contract <- diff(log(CORN$c_spgsci_not_adj))

#Plot

ggplot(Rd_CORN) + geom_line(aes(x=Date,y=ETF),color="darkgreen") + geom_line(aes(x=Date,y=Contract),color="lightblue")

basicStats(Rd_CORN[,-1])
#ADF TEST
adf.test(Rd_CORN$ETF)
adf.test(Rd_CORN$Contract)

#ACF/PACF pour creer le garch univariate
acf(Rd_CORN$ETF)
pacf(Rd_CORN$ETF)

acf(Rd_CORN$Contract)
pacf(Rd_CORN$Contract)

Box.test(as.numeric(Rd_CORN$ETF))
Box.test(as.numeric(Rd_CORN$Contract))

#Xts
Rd_CORN <- xts(Rd_CORN,order.by=Rd_CORN$Date)

#ModelDCC
#On utilise le meme model univariate pour les deux variables
uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_CORN[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_CORN[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

#test des residus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus ressemble à un bruit blanc, le modele semble correct

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])

#Test ARCH
MarchTest(fit1@mfit$stdresid)

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

cor_CORN <- as.xts(cor1[2,1,])
plot(cor_CORN,main="Correlation Conditionnelle Dynamique Maïs")


#-- Natural Gaz ----
ngETF <- read.csv("ETF/UNG.csv")
ngC <- read_excel("Futures/ng_returns.xlsx",3)
ngETF$Date <- as.Date(ngETF$Date)
ngC$Date <- as.Date(ngC$Date)


#On supprime les données manquantes et qui ne correspondent pas

NG <- left_join(ngC[,c(1,2)],ngETF[,c(1,5)])
NG <- na.omit(NG)

#Calcul des rendements avec log et diff

Rd_NG <- NG[-1,1]
Rd_NG$ETF <- diff(log(NG$Close))
Rd_NG$Contract <- diff(log(NG$ng_spgsci_not_adj))

#ADF TEST
adf.test(Rd_NG$ETF)
adf.test(Rd_NG$Contract)

basicStats(Rd_NG[,-1])
#Plot

ggplot(Rd_NG) + geom_line(aes(x=Date,y=ETF),color="darkgreen") + geom_line(aes(x=Date,y=Contract),color="lightblue")

#ACF/PACF
acf(Rd_NG$ETF)
pacf(Rd_NG$ETF)

acf(Rd_NG$Contract)
pacf(Rd_NG$Contract)
#model ARMA(1,1) choisis

#Xts
Rd_NG <- xts(Rd_NG,order.by=Rd_NG$Date)

#ModelDCC
#On utilise le meme model univariate pour les deux variables
uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_NG[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_NG[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

#Test des résidus
acf(residuals(fit1))
pacf(residuals(fit1))
#les residus semble ressemble à un bruit blanc, le modele semble correct

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])

#Test ARCH

MarchTest(fit1@mfit$stdresid)


#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

cor_NG <- as.xts(cor1[2,1,])
plot(cor_NG,main="Correlation Dynamique Natural Gaz")

#-- Cacao ----

ccETF <- read.csv("ETF/NIB.csv")
ccC <- read_excel("Futures/cc_returns.xlsx",3)
ccETF$Date <- as.Date(ccETF$Date)
ccC$Date <- as.Date(ccC$Date)

#On supprime les données manquantes et qui ne correspondent pas

CC <- left_join(ccC[,c(1,2)],ccETF[,c(1,5)])
CC <- na.omit(CC)

#Calcul des rendements avec log et diff

Rd_CC <- CC[-1,1]
Rd_CC$ETF <- diff(log(CC$Close))
Rd_CC$Contract <- diff(log(CC$cc_spgsci_not_adj))

basicStats(Rd_CC[,-1])
#ADF TEST
adf.test(Rd_CC$ETF)
adf.test(Rd_CC$Contract)

#Plot
ggplot(Rd_CC) + geom_line(aes(x=Date,y=ETF),color="darkgreen") + geom_line(aes(x=Date,y=Contract),color="lightblue")

#ACF/PACF
acf(Rd_CC$ETF)
pacf(Rd_CC$ETF)

acf(Rd_CC$Contract)
pacf(Rd_CC$Contract)
#model AR(1) choisis

#Xts Object
Rd_CC <- xts(Rd_CC,order.by=Rd_CC$Date)

#ModelDCC
#On utilise le meme model univariate pour les deux variables
uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_CC[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_CC[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

#residus
acf(residuals(fit1))
pacf(residuals(fit1))
#malgré certain pic on considere que les residus ressemblent à un bruit blanc, le modele choisis semble correct

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])

#Test ARCH

MarchTest(fit1@mfit$stdresid)

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

cor_CC <- as.xts(cor1[2,1,])
plot(cor_CC,main="Correlation Dynamique Cacao")


#-- SOYBEAN ----

soybETF <- read.csv("ETF/SOYB.csv")
soybC <- read_excel("Futures/s_returns.xlsx",3)
soybETF$Date <- as.Date(soybETF$Date)
soybC$Date <- as.Date(soybC$Date)

#On supprime les données manquantes et qui ne correspondent pas

soyb <- left_join(soybC[,c(1,2)],soybETF[,c(1,5)])
soyb <- na.omit(soyb)

#Calcul des rendements avec log et diff

Rd_soyb <- soyb[-1,1]
Rd_soyb$ETF <- diff(log(soyb$Close))
Rd_soyb$Contract <- diff(log(soyb$s_spgsci_not_adj))

#ADF TEST
adf.test(Rd_soyb$ETF)
adf.test(Rd_soyb$Contract)

#ACF/PACF
acf(Rd_soyb$ETF)
pacf(Rd_soyb$ETF)

acf(Rd_soyb$Contract)
pacf(Rd_soyb$Contract)
#on choisis un modele ARMA(1,1)

#Plot
ggplot(Rd_soyb) + geom_line(aes(x=Date,y=ETF),color="darkgreen") + geom_line(aes(x=Date,y=Contract),color="lightblue")

#Xts Object
Rd_soyb <- xts(Rd_soyb,order.by=Rd_soyb$Date)

#ModelDCC
#On utilise le meme model univariate pour les deux variables
uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_soyb[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_soyb[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

#Residus
acf(residuals(fit1))
pacf(residuals(fit1))
#le modele semble acceptable

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])

#Test ARCH

MarchTest(fit1@mfit$stdresid)

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

cor_soyb <- as.xts(cor1[2,1,])
plot(cor_soyb,main="Correlation Dynamique Soja")


#-- WTI/UCO ----

wti_ucoETF <- read.csv("ETF/UCO.csv")
wti_ucoC <- read_excel("Futures/cl_returns.xlsx",2)
wti_ucoETF$Date <- as.Date(wti_ucoETF$Date)
wti_ucoC$Date <- as.Date(wti_ucoC$Date)


#On supprime les données manquantes et qui ne correspondent pas

wti_uco <- left_join(wti_ucoC[,c(1,2)],wti_ucoETF[,c(1,5)])
wti_uco <- na.omit(wti_uco)

#Calcul des rendements avec log et diff

Rd_wti_uco <- wti_uco[-1,1]
Rd_wti_uco$ETF <- diff(log(wti_uco$Close))
Rd_wti_uco$Contract <- diff(log(wti_uco$cl_spgsci_not_adj))

#ADF TEST
adf.test(Rd_wti_uco$ETF)
adf.test(Rd_wti_uco$Contract)

#Plot
ggplot(Rd_wti_uco) + geom_line(aes(x=Date,y=ETF),color="darkgreen") + geom_line(aes(x=Date,y=Contract),color="lightblue")

#ACF/PACF
acf(Rd_wti_uco$ETF)
pacf(Rd_wti_uco$ETF)

acf(Rd_wti_uco$Contract)
pacf(Rd_wti_uco$Contract)
#model Ar(1) choisis

#Xts Object
Rd_wti_uco <- xts(Rd_wti_uco,order.by=Rd_wti_uco$Date)

#ModelDCC
#On utilise le meme model univariate pour les deux variables
uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wti_uco[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wti_uco[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

#residus
acf(residuals(fit1))
pacf(residuals(fit1))
#cela ne semble pas suivre un bruit blanc

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])

#Test ARCH

MarchTest(fit1@mfit$stdresid)

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

cor_wti_uco <- as.xts(cor1[2,1,])
plot(cor_wti_uco,main="Correlation Dynamique du WTI avec ETF UCO")

#-- WTI/USO ----

wti_usoETF <- read.csv("ETF/USO.csv")
wti_usoC <- read_excel("Futures/cl_returns.xlsx",2)
wti_usoETF$Date <- as.Date(wti_usoETF$Date)
wti_usoC$Date <- as.Date(wti_usoC$Date)


#On supprime les données manquantes et qui ne correspondent pas

wti_uso <- left_join(wti_usoC[,c(1,2)],wti_usoETF[,c(1,5)])
wti_uso <- na.omit(wti_uso)

#Calcul des rendements avec log et diff

Rd_wti_uso <- wti_uso[-1,1]
Rd_wti_uso$ETF <- diff(log(wti_uso$Close))
Rd_wti_uso$Contract <- diff(log(wti_uso$cl_spgsci_not_adj))

#ADF TEST
adf.test(Rd_wti_uso$ETF)
adf.test(Rd_wti_uso$Contract)

#Plot
ggplot(Rd_wti_uso) + geom_line(aes(x=Date,y=ETF),color="darkgreen") + geom_line(aes(x=Date,y=Contract),color="lightblue")

#ACF/PACF
acf(Rd_wti_uso$ETF)
pacf(Rd_wti_uso$ETF)

acf(Rd_wti_uso$Contract)
pacf(Rd_wti_uso$Contract)
#model ARMA(1,1) choisis

#Xts Object
Rd_wti_uso <- xts(Rd_wti_uso,order.by=Rd_wti_uso$Date)

#ModelDCC
#On utilise le meme model univariate pour les deux variables
uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_wti_uso[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wti_uso[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

#residus
acf(residuals(fit1))
pacf(residuals(fit1))
#cela semble un bruit blanc

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])

#Test ARCH

MarchTest(fit1@mfit$stdresid)

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

cor_wti_uso <- as.xts(cor1[2,1,])
plot(cor_wti_uso,main="Correlation Dynamique du WTI avec ETF USO")

#-- Sugar/CANE ----

caneETF <- read.csv("ETF/CANE.csv")
caneC <- read_excel("Futures/sb_returns.xlsx",3)
caneETF$Date <- as.Date(caneETF$Date)


#On supprime les données manquantes et qui ne correspondent pas

cane <- left_join(caneC[,c(1,2)],caneETF[,c(1,5)])
cane <- na.omit(cane)

#Calcul des rendements avec log et diff

Rd_cane <- cane[-1,1]
Rd_cane$ETF <- diff(log(cane$Close))
Rd_cane$Contract <- diff(log(cane$sb_spgsci_not_adj))

#ADF TEST
adf.test(Rd_cane$ETF)
adf.test(Rd_cane$Contract)

#ACF/PACF
acf(Rd_cane$ETF)
pacf(Rd_cane$ETF)

acf(Rd_cane$Contract)
pacf(Rd_cane$Contract)
#on choisis un modele ARMA(1,1) a cause de l'etf

#Plot
ggplot(Rd_cane) + geom_line(aes(x=Date,y=ETF),color="darkgreen") + geom_line(aes(x=Date,y=Contract),color="lightblue")

#Xts Object
Rd_cane <- xts(Rd_cane,order.by=Rd_cane$Date)

#ModelDCC
#On utilise le meme model univariate pour les deux variables
uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_cane[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_cane[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

#Residus
acf(residuals(fit1))
pacf(residuals(fit1))
#le modele semble acceptable

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])

#Test ARCH

MarchTest(fit1@mfit$stdresid)

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

cor_cane <- as.xts(cor1[2,1,])
plot(cor_cane,main="Correlation Dynamique du Sucre avec ETF CANE")


#-- Sugar/SGG ----

sggETF <- read.csv("ETF/SGG.csv")
sggC <- read_excel("Futures/sb_returns.xlsx",3)
sggETF$Date <- as.Date(sggETF$Date)
sggC$Date <- as.Date(sggC$Date)


#On supprime les données manquantes et qui ne correspondent pas

sgg <- left_join(sggC[,c(1,2)],sggETF[,c(1,5)])
sgg <- na.omit(sgg)

#Calcul des rendements avec log et diff

Rd_sgg <- sgg[-1,1]
Rd_sgg$ETF <- diff(log(sgg$Close))
Rd_sgg$Contract <- diff(log(sgg$sb_spgsci_not_adj))

#ADF TEST
adf.test(Rd_sgg$ETF)
adf.test(Rd_sgg$Contract)

#ACF/PACF
acf(Rd_sgg$ETF)
pacf(Rd_sgg$ETF)

acf(Rd_sgg$Contract)
pacf(Rd_sgg$Contract)
#on choisis un modele AR(1)

#Plot
ggplot(Rd_sgg) + geom_line(aes(x=Date,y=ETF),color="darkgreen") + geom_line(aes(x=Date,y=Contract),color="lightblue")

#Xts Object
Rd_sgg <- xts(Rd_sgg,order.by=Rd_sgg$Date)

#ModelDCC
#On utilise le meme model univariate pour les deux variables
uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(0,0)))))
multf <- multifit(uspec.n,Rd_sgg[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_sgg[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

#Residus
acf(residuals(fit1))
pacf(residuals(fit1))
#le modele semble acceptable

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])

#Test ARCH

MarchTest(fit1@mfit$stdresid)

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

cor_sgg <- as.xts(cor1[2,1,])
plot(cor_sgg,main="Correlation Dynamique du Sucre avec ETF SGG")

#-- WHEAT ----

wheatETF <- read.csv("ETF/WEAT.csv")
wheatC <- read_excel("Futures/w_returns.xlsx",3)
wheatETF$Date <- as.Date(wheatETF$Date)
wheatC$Date <- as.Date(wheatC$Date)

#On supprime les données manquantes et qui ne correspondent pas

wheat <- left_join(wheatC[,c(1,2)],wheatETF[,c(1,5)])
wheat <- na.omit(wheat)

#Calcul des rendements avec log et diff

Rd_wheat <- wheat[-1,1]
Rd_wheat$ETF <- diff(log(wheat$Close))
Rd_wheat$Contract <- diff(log(wheat$w_spgsci_not_adj))

#ADF TEST
adf.test(Rd_wheat$ETF)
adf.test(Rd_wheat$Contract)

#ACF/PACF
acf(Rd_wheat$ETF)
pacf(Rd_wheat$ETF)

acf(Rd_wheat$Contract)
pacf(Rd_wheat$Contract)
#on choisis un modele AR(1)

#Plot
ggplot(Rd_wheat) + geom_line(aes(x=Date,y=ETF),color="darkgreen") + geom_line(aes(x=Date,y=Contract),color="lightblue")

#Xts Object
Rd_wheat <- xts(Rd_wheat,order.by=Rd_wheat$Date)

#ModelDCC
#On utilise le meme model univariate pour les deux variables
uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multf <- multifit(uspec.n,Rd_wheat[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_wheat[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

#Residus
acf(residuals(fit1))
pacf(residuals(fit1))
#le modele semble acceptable

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])

#Test ARCH

MarchTest(fit1@mfit$stdresid)


#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

cor_wheat <- as.xts(cor1[2,1,])
plot(cor_wheat,main="Correlation Dynamique du Blé")

#-- COFFEE ----

coffeeETF <- read.csv("ETF/JO.csv")
coffeeC <- read_excel("Futures/kc_returns.xlsx",3)
coffeeETF$Date <- as.Date(coffeeETF$Date)
coffeeC$Date <- as.Date(coffeeC$Date)


#On supprime les données manquantes et qui ne correspondent pas

coffee <- left_join(coffeeC[,c(1,2)],coffeeETF[,c(1,5)])
coffee <- na.omit(coffee)

#Calcul des rendements avec log et diff

Rd_coffee <- coffee[-1,1]
Rd_coffee$ETF <- diff(log(coffee$Close))
Rd_coffee$Contract <- diff(log(coffee$kc_spgsci_not_adj))

#ADF TEST
adf.test(Rd_coffee$ETF)
adf.test(Rd_coffee$Contract)

#ACF/PACF
acf(Rd_coffee$ETF)
pacf(Rd_coffee$ETF)

acf(Rd_coffee$Contract)
pacf(Rd_coffee$Contract)
#on choisis un modele ARMA(1,1)

#Plot
ggplot(Rd_coffee) + geom_line(aes(x=Date,y=ETF),color="darkgreen") + geom_line(aes(x=Date,y=Contract),color="lightblue")

#Xts Object
Rd_coffee <- xts(Rd_coffee,order.by=Rd_coffee$Date)

#ModelDCC
#On utilise le meme model univariate pour les deux variables
uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_coffee[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_coffee[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

#Residus
acf(residuals(fit1))
pacf(residuals(fit1))
#le modele semble acceptable

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])

#Test ARCH

MarchTest(fit1@mfit$stdresid)

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

cor_coffee <- as.xts(cor1[2,1,])
plot(cor_coffee,main="Correlation Dynamique Cafe")


