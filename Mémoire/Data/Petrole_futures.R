library(PerformanceAnalytics)
library(quantmod)
library(MASS)
library(mvtnorm)
library(mnormt) 
library(fBasics)
library(readxl)
library(tidyverse)
library(ggplot2)
library(tseries)
library(zoo)
library(xts)
library(rugarch)
library(rmgarch)
library(stats)
library("lmtest")
library("FinTS")
library(forecast)
library(car)

setwd("D:/M1 EKAP/Mémoire/Data")

Petrole <- read_excel("Futures/cl_returns.xlsx",2)
Wheat <- read_excel("Futures/w_returns.xlsx",3)
Corn <- read_excel("Futures/c_returns.xlsx",3)
Soybeans <- read_excel("Futures/s_returns.xlsx",3)
Cocoa <- read_excel("Futures/cc_returns.xlsx",3)
Coffee <- read_excel("Futures/kc_returns.xlsx",3)
Cotton <- read_excel("Futures/ct_returns.xlsx",3)
Sugar <- read_excel("Futures/sb_returns.xlsx",3)


#Plot 
PetroleP <- Petrole[,c(1,2)]
colnames(PetroleP) <- c("Date","Prix")
PetroleP$Ticker <- "Petrole"
PetroleP$Log <- log(PetroleP$Prix)

CornP <- Corn[,c(1,2)]
colnames(CornP) <- c("Date","Prix")
CornP$Ticker <- "Corn"
CornP$Log <- log(CornP$Prix)

WheatP <- Wheat[,c(1,2)]
colnames(WheatP) <- c("Date","Prix")
WheatP$Ticker <- "Wheat"
WheatP$Log <- log(WheatP$Prix)

SoybeansP <- Soybeans[,c(1,2)]
colnames(SoybeansP) <- c("Date","Prix")
SoybeansP$Ticker <- "Soybeans"
SoybeansP$Log <- log(SoybeansP$Prix)

CocoaP <- Cocoa[,c(1,2)]
colnames(CocoaP) <- c("Date","Prix")
CocoaP$Ticker <- "Cocoa"
CocoaP$Log <- log(CocoaP$Prix)

CoffeeP <- Coffee[,c(1,2)]
colnames(CoffeeP) <- c("Date","Prix")
CoffeeP$Ticker <- "Coffee"
CoffeeP$Log <- log(CoffeeP$Prix)

CottonP <- Cotton[,c(1,2)]
colnames(CottonP) <- c("Date","Prix")
CottonP$Ticker <- "Cotton"
CottonP$Log <- log(CottonP$Prix)

SugarP <- Sugar[,c(1,2)]
colnames(SugarP) <- c("Date","Prix")
SugarP$Ticker <- "Sugar"
SugarP$Log <- log(SugarP$Prix)

dataPlot <- rbind(PetroleP,WheatP,CottonP,SugarP,SoybeansP,CoffeeP,CocoaP,WheatP)

ggplot(dataPlot) + geom_line(aes(x=Date,y=Log,color=Ticker)) + xlab("Date") + ylab("Prix en Log")
  
#Calcul Rd

Petrole2 <- Petrole[-1,1]
Petrole2$Rd_P <- diff(log(Petrole$cl_spgsci_not_adj))

Wheat2 <- Wheat[-1,1]
Wheat2$Rd_W <- diff(log(Wheat$w_spgsci_not_adj))

Corn2 <- Corn[-1,1]
Corn2$Rd_C <- diff(log(Corn$c_spgsci_not_adj))

Soybeans2 <- Soybeans[-1,1]
Soybeans2$Rd_S <- diff(log(Soybeans$s_spgsci_not_adj))

Cocoa2 <- Cocoa[-1,1]
Cocoa2$Rd_CC <- diff(log(Cocoa$cc_spgsci_not_adj))

Coffee2 <- Coffee[-1,1]
Coffee2$Rd_CF <- diff(log(Coffee$kc_spgsci_not_adj))

Cotton2 <- Cotton[-1,1]
Cotton2$Rd_CT <- diff(log(Cotton$ct_spgsci_not_adj))

Sugar2 <- Sugar[-1,1]
Sugar2$Rd_Sb <- diff(log(Sugar$sb_spgsci_not_adj))

Petrole2$Date <- as.Date(Petrole2$Date)
Sugar2$Date <- as.Date(Sugar2$Date)
Wheat2$Date <- as.Date(Wheat2$Date)
Corn2$Date <- as.Date(Corn2$Date)
Soybeans2$Date <- as.Date(Soybeans2$Date)
Cocoa2$Date <- as.Date(Cocoa2$Date)
Coffee2$Date <- as.Date(Coffee2$Date)
Cotton2$Date <- as.Date(Cotton2$Date)

data1 <- left_join(Petrole2,Sugar2)
data1 <- left_join(data1,Wheat2)
data1 <- left_join(data1,Corn2)
data1 <- left_join(data1,Soybeans2)
data1 <- left_join(data1,Cocoa2)
data1 <- left_join(data1,Coffee2)
data1 <- left_join(data1,Cotton2)

data1 <- na.omit(data1)

basicStats(data1[,-1])

#Join

Petrole_Wheat <- left_join(Petrole2,Wheat2)
Petrole_Corn <- left_join(Petrole2,Corn2)
Petrole_Cotton <- left_join(Petrole2,Cotton2)
Petrole_Cocoa <- left_join(Petrole2,Cocoa2)
Petrole_Soybeans <- left_join(Petrole2,Soybeans2)
Petrole_Coffee <- left_join(Petrole2,Coffee2)
Petrole_Sugar <- left_join(Petrole2,Sugar2)

Petrole_Corn <- na.omit(Petrole_Corn)
Petrole_Wheat <- na.omit(Petrole_Wheat)
Petrole_Cotton <- na.omit(Petrole_Cotton)
Petrole_Soybeans <- na.omit(Petrole_Soybeans)
Petrole_Cocoa <- na.omit(Petrole_Cocoa)
Petrole_Coffee <- na.omit(Petrole_Coffee)
Petrole_Sugar <- na.omit(Petrole_Sugar)

#ACF PACF
par(mfrow=c(1,1))

acf(Petrole2$Rd_P)
pacf(Petrole2$Rd_P) #(1,1)

acf(Corn2$Rd_C)
pacf(Corn2$Rd_C) #(1,0)

acf(Wheat2$Rd_W)
pacf(Wheat2$Rd_W) #(1,0)

acf(Soybeans2$Rd_S)
pacf(Soybeans2$Rd_S) #(1,0)

acf(Cocoa2$Rd_CC)
pacf(Cocoa2$Rd_CC) #(1,0)

acf(Coffee2$Rd_CF)
pacf(Coffee2$Rd_CF) #(1,1)

acf(Cotton2$Rd_CT)
pacf(Cotton2$Rd_CT) #(1,1) a verif

acf(Sugar2$Rd_Sb)
pacf(Sugar2$Rd_Sb) #(1,0)

#ADF Test et jarque bera test

adf.test(Petrole_Cocoa$Rd_P)
adf.test(Petrole_Cocoa$Rd_CC)
adf.test(Petrole_Coffee$Rd_CF)
adf.test(Petrole_Cotton$Rd_CT)
adf.test(Petrole_Corn$Rd_C)
adf.test(Petrole_Wheat$Rd_W)
adf.test(Petrole_Soybeans$Rd_S)
adf.test(Petrole_Sugar$Rd_Sb)

jarque.bera.test(Petrole_Cocoa$Rd_P)
jarque.bera.test(Petrole_Cocoa$Rd_CC)
jarque.bera.test((Petrole_Coffee$Rd_CF))
jarque.bera.test((Petrole_Cotton$Rd_CT))
jarque.bera.test(Petrole_Corn$Rd_C)
jarque.bera.test(Petrole_Wheat$Rd_W)
jarque.bera.test(Petrole_Soybeans$Rd_S)
jarque.bera.test(Petrole_Sugar$Rd_Sb)

#Ljung test


#Plot

ggplot(Petrole_Cocoa) + geom_line(aes(x=Date,y=Rd_P)) + ylab("Prix Logarithmique Petrole Differencie")

ggplot(Petrole_Cocoa) + geom_line(aes(x=Date,y=Rd_CC)) + ylab("Prix Logarithmique Cacao Differencie")

ggplot(Petrole_Coffee) + geom_line(aes(x=Date,y=Rd_CF)) + ylab("Prix Logarithmique Café Differencie")

ggplot(Petrole_Cotton) + geom_line(aes(x=Date,y=Rd_CT)) + ylab("Prix Logarithmique Coton Differencie")

ggplot(Petrole_Sugar) + geom_line(aes(x=Date,y=Rd_Sb)) + ylab("Prix Logarithmique Sucre Differencie")

ggplot(Petrole_Soybeans) + geom_line(aes(x=Date,y=Rd_S)) + ylab("Prix Logarithmique Soja Differencie")

ggplot(Petrole_Wheat) + geom_line(aes(x=Date,y=Rd_W)) + ylab("Prix Logarithmique Blé Differencie")

ggplot(Petrole_Corn) + geom_line(aes(x=Date,y=Rd_C)) + ylab("Prix Logarithmique Blé Differencie")



#Box.test : autocorrélation

Box.test(data1$Rd_P^2, type="Ljung")
Box.test(data1$Rd_Sb^2, type="Ljung")
Box.test(data1$Rd_W^2, type="Ljung")
Box.test(data1$Rd_S^2, type="Ljung")
Box.test(Coffee2$Rd_CF*Coffee2$Rd_CF, type="Ljung")
Box.test(Corn2$Rd_C*Corn2$Rd_C, type="Ljung")
Box.test(Soybeans2$Rd_S*Soybeans2$Rd_S, type="Ljung")
Box.test(Cotton2$Rd_CT*Cotton2$Rd_CT, type="Ljung")

#Test
Rd_data <- na.omit(dataStat)
Rd_data <- as.xts(Rd_data, order.by = Rd_data$Date)

uspec.n <- multispec(replicate(8, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_data[,c(2:9)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_data[,c(2:9)],fit.control=list(eval.se = TRUE), fit=multf)

#residus
acf(residuals(fit1))
pacf(residuals(fit1))
#cela ne semble pas suivre un bruit blanc

#Test Portmanteau

Box.test(fit1@mfit$stdresid[,1]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,2]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,3]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,4]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,5]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,6]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,7]^2, type="Ljung")
Box.test(fit1@mfit$stdresid[,8]^2, type="Ljung")

#Normalite
jarque.bera.test(fit1@mfit$stdresid[,1])
jarque.bera.test(fit1@mfit$stdresid[,2])
jarque.bera.test(fit1@mfit$stdresid[,3])
jarque.bera.test(fit1@mfit$stdresid[,4])
jarque.bera.test(fit1@mfit$stdresid[,5])
jarque.bera.test(fit1@mfit$stdresid[,6])
jarque.bera.test(fit1@mfit$stdresid[,7])
jarque.bera.test(fit1@mfit$stdresid[,8])

#Test ARCH

MarchTest(fit1@mfit$stdresid)

#Model based on time varying covariance and correlation matrix
cov1 <- rcov(fit1)
cor1 <- rcor(fit1)

cor_cocoa <- as.xts(cor1[2,1,])
plot(cor_cocoa,main="Correlation Dynamique du Petrole-Cacao")


#Xts

Rd_Petrole_Cocoa <- as.xts(Petrole_Cocoa, order.by = Petrole_Cocoa$Date)
Rd_Petrole_Sugar <- as.xts(Petrole_Sugar, order.by = Petrole_Sugar$Date)
Rd_Petrole_Coffee <- as.xts(Petrole_Coffee, order.by = Petrole_Coffee$Date)
Rd_Petrole_Cotton <- as.xts(Petrole_Cotton, order.by = Petrole_Cotton$Date)
Rd_Petrole_Corn <- as.xts(Petrole_Corn, order.by = Petrole_Corn$Date)
Rd_Petrole_Wheat <- as.xts(Petrole_Wheat, order.by = Petrole_Wheat$Date)
Rd_Petrole_Soybeans <- as.xts(Petrole_Soybeans, order.by = Petrole_Soybeans$Date)

#-- Model pour Cocoa ----
uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_Petrole_Cocoa[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_Petrole_Cocoa[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

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

cor_cocoa <- as.xts(cor1[2,1,])
plot(cor_cocoa,main="Correlation Dynamique du Petrole-Cacao")

#-- Model pour Wheat ----

uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_Petrole_Wheat[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_Petrole_Wheat[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

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

cor_wheat <- as.xts(cor1[2,1,])
plot(cor_wheat,main="Correlation Dynamique du Petrole-Blé")

#-- Model pour Corn ----

uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_Petrole_Corn[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_Petrole_Corn[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

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

cor_corn <- as.xts(cor1[2,1,])
plot(cor_corn,main="Correlation Dynamique du Petrole-Maïs")

#-- Model pour Coffee ----

uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_Petrole_Coffee[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_Petrole_Coffee[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

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

cor_Coffee <- as.xts(cor1[2,1,])
plot(cor_Coffee,main="Correlation Dynamique du Petrole-Café")

#-- Model pour Cotton ----

uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_Petrole_Cotton[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_Petrole_Cotton[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

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

cor_Cotton <- as.xts(cor1[2,1,])
plot(cor_Cotton,main="Correlation Dynamique du Petrole-Coton")

#-- Model pour Soybeans ----

uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_Petrole_Soybeans[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_Petrole_Soybeans[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

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

cor_Soybeans <- as.xts(cor1[2,1,])
plot(cor_Soybeans,main="Correlation Dynamique du Petrole-Soja")

#-- Model pour Sugar ----

uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,1)))))
multf <- multifit(uspec.n,Rd_Petrole_Sugar[,c(2,3)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_Petrole_Sugar[,c(2,3)],fit.control=list(eval.se = TRUE), fit=multf)

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

cor_Sugar <- as.xts(cor1[2,1,])
plot(cor_Sugar,main="Correlation Dynamique du Petrole-Sucre")
