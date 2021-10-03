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
Cattle_Feeder <- read_excel("Futures/fc_returns.xlsx",3)
Cattle_Live <- read_excel("Futures/lc_returns.xlsx",3)
Hogs <- read_excel("Futures/lh_returns.xlsx",3)
Natural_Gas <- read_excel("Futures/ng_returns.xlsx",3)
Copper<- read_excel("Futures/hg_returns.xlsx",3)
Gold <- read_excel("Futures/gc_returns.xlsx",2)
Silver <- read_excel("Futures/si_returns.xlsx",3)

Petrole2 <- Petrole[,c(1,2)]
Wheat2 <- Wheat[,c(1,2)]
Corn2 <- Corn[,c(1,2)]
Soybeans2 <- Soybeans[,c(1,2)]
Cocoa2 <- Cocoa[,c(1,2)]
Coffee2 <- Coffee[,c(1,2)]
Cotton2 <- Cotton[,c(1,2)]
Sugar2 <- Sugar[,c(1,2)]
Cattle_Feeder2 <- Cattle_Feeder[,c(1,2)]
Cattle_Live2 <- Cattle_Live[,c(1,2)]
Hogs2 <- Hogs[,c(1,2)]
Natural_Gas2 <- Natural_Gas[,c(1,2)]
Copper2 <- Copper[,c(1,2)]
Gold2 <- Gold[,c(1,2)]
Silver2 <- Silver[,c(1,2)]

#Plot 
Petrole3 <- Petrole2
colnames(Petrole3) <- c("Date","Prix")
Petrole3$Ticker <- "Petrole"
Wheat3 <- Wheat2
colnames(Wheat3) <- c("Date","Prix")
Wheat3$Ticker <- "Wheat"
Corn3 <- Corn2
colnames(Corn3) <- c("Date","Prix")
Corn3$Ticker <- "Corn"
Soybeans3 <- Soybeans2
colnames(Soybeans3) <- c("Date","Prix")
Soybeans3$Ticker <- "Soybeans"
Cocoa3 <- Cocoa2
colnames(Cocoa3) <- c("Date","Prix")
Cocoa3$Ticker <- "Cocoa"
Cotton3 <- Cotton2
colnames(Cotton3) <- c("Date","Prix")
Cotton3$Ticker <- "Cotton"
Coffee3 <- Coffee2
colnames(Coffee3) <- c("Date","Prix")
Coffee3$Ticker <- "Coffee"
Sugar3 <- Sugar2
colnames(Sugar3) <- c("Date","Prix")
Sugar3$Ticker <- "Sugar"
Cattle_Feeder3 <- Cattle_Feeder2
colnames(Cattle_Feeder3) <- c("Date","Prix")
Cattle_Feeder3$Ticker <- "Cattle_Feeder"
Cattle_Live3 <- Cattle_Live2
colnames(Cattle_Live3) <- c("Date","Prix")
Cattle_Live3$Ticker <- "Cattle_Live"
Hogs3 <- Hogs2
colnames(Hogs3) <- c("Date","Prix")
Hogs3$Ticker <- "Hogs"
Gold3 <- Gold2
colnames(Gold3) <- c("Date","Prix")
Gold3$Ticker <- "Gold"
Silver3 <- Silver2
colnames(Silver3) <- c("Date","Prix")
Silver3$Ticker <- "Silver"
Copper3 <- Copper2
colnames(Copper3) <- c("Date","Prix")
Copper3$Ticker <- "Copper"
Natural_Gas3 <- Natural_Gas2
colnames(Natural_Gas3) <- c("Date","Prix")
Natural_Gas3$Ticker <- "Natural_Gas"

dataPlot <- rbind(Petrole3,Wheat3,Cotton3,Corn3,Coffee3)
dataPlot2 <- rbind(Cocoa3,Gold3,Copper3,Silver3,Soybeans3)
dataPlot3 <- rbind(Cattle_Live3,Cattle_Feeder3,Hogs3,Natural_Gas3)
ggplot(dataPlot) + geom_line(aes(x=Date,y=log(Prix),color=Ticker)) + ylab("Prix en Log")
ggplot(dataPlot2) + geom_line(aes(x=Date,y=log(Prix),color=Ticker)) + ylab("Prix en Log")
ggplot(dataPlot3) + geom_line(aes(x=Date,y=log(Prix),color=Ticker)) + ylab("Prix en Log")


#Join

Data <- left_join(Petrole2,Wheat2)
Data <- left_join(Data,Corn2)
Data <- left_join(Data,Soybeans2)
Data <- left_join(Data,Cocoa2)
Data <- left_join(Data,Cotton2)
Data <- left_join(Data,Coffee2)
Data <- left_join(Data,Sugar2)
Data <- left_join(Data,Cattle_Feeder2)
Data <- left_join(Data,Cattle_Live2)
Data <- left_join(Data,Hogs2)
Data <- left_join(Data,Gold2)
Data <- left_join(Data,Silver2)
Data <- left_join(Data,Copper2)
Data <- left_join(Data,Natural_Gas2)

Data <- na.omit(Data)


#Calcul des Rd
Rd_Data <- Data[-1,1]
Rd_Data$Petrole <- diff(log(Data$cl_spgsci_not_adj))
Rd_Data$Corn <- diff(log(Data$c_spgsci_not_adj))
Rd_Data$Cotton <- diff(log(Data$ct_spgsci_not_adj))
Rd_Data$Cacao <- diff(log(Data$cc_spgsci_not_adj))
Rd_Data$Coffee <- diff(log(Data$kc_spgsci_not_adj))
Rd_Data$Wheat <- diff(log(Data$w_spgsci_not_adj))
Rd_Data$Soybeans <- diff(log(Data$s_spgsci_not_adj))
Rd_Data$Sugar <- diff(log(Data$sb_spgsci_not_adj))
Rd_Data$Cattle_Feeder <- diff(log(Data$fc_spgsci_not_adj))
Rd_Data$Cattle_Live <- diff(log(Data$lc_spgsci_not_adj))
Rd_Data$Hogs <- diff(log(Data$lh_spgsci_not_adj))
Rd_Data$Natural_Gas <- diff(log(Data$ng_spgsci_not_adj))
Rd_Data$Copper <- diff(log(Data$hg_spgsci_not_adj))
Rd_Data$Gold <- diff(log(Data$gc_spgsci_not_adj))
Rd_Data$Silver <- diff(log(Data$si_spgsci_not_adj))

#Stats des
basicStats(Rd_Data[,-1])

#Petrole, Corn, Soybeans, Sugar, Cattle_Feeder, Cattle_Live, Copper, Gold, Silver

#ADF Test et jarque bera test
adf.test(Rd_Data$Petrole)
adf.test(Rd_Data$Corn)
adf.test(Rd_Data$Soybeans)
adf.test(Rd_Data$Sugar)
adf.test(Rd_Data$Cattle_Feeder)
adf.test(Rd_Data$Cattle_Live)
adf.test(Rd_Data$Copper)
adf.test(Rd_Data$Gold)
adf.test(Rd_Data$Silver)

jarque.bera.test(Rd_Data$Petrole)
jarque.bera.test(Rd_Data$Corn)
jarque.bera.test(Rd_Data$Soybeans)
jarque.bera.test(Rd_Data$Sugar)
jarque.bera.test(Rd_Data$Cattle_Feeder)
jarque.bera.test(Rd_Data$Cattle_Live)
jarque.bera.test(Rd_Data$Copper)
jarque.bera.test(Rd_Data$Gold)
jarque.bera.test(Rd_Data$Silver)

#Ljung test

Box.test(Rd_Data$Petrole, type="Ljung", lag=50)
Box.test(Rd_Data$Corn, type="Ljung", lag=40) #Autocorrele
Box.test(Rd_Data$Soybeans, type="Ljung", lag=42) #Autocorrele
Box.test(Rd_Data$Sugar, type="Ljung", lag=35) #Autocorrele
Box.test(Rd_Data$Cattle_Feeder, type="Ljung")
Box.test(Rd_Data$Cattle_Live, type="Ljung", lag=1) #Autocorrele
Box.test(Rd_Data$Copper, type="Ljung")
Box.test(Rd_Data$Gold, type="Ljung", lag=20) #Autocorrele
Box.test(Rd_Data$Silver, type="Ljung", lag=35) #Autocorrele

Box.test(Rd_Data$Petrole^2, type="Ljung")
Box.test(Rd_Data$Corn^2, type="Ljung")
Box.test(Rd_Data$Soybeans^2, type="Ljung", lag=5) #Autocorrele
Box.test(Rd_Data$Sugar^2, type="Ljung")
Box.test(Rd_Data$Cattle_Feeder^2, type="Ljung")
Box.test(Rd_Data$Cattle_Live^2, type="Ljung", lag=80) #Autocorrele
Box.test(Rd_Data$Copper^2, type="Ljung")
Box.test(Rd_Data$Gold^2, type="Ljung")
Box.test(Rd_Data$Silver^2, type="Ljung")

#Plot Rendement

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Petrole)) + ylab("Rendement du Petrole")

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Corn)) + ylab("Rendement du Maïs")

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Soybeans)) + ylab("Rendement du Soja")

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Sugar)) + ylab("Rendement du Sucre")

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Cattle_Feeder)) + ylab("Rendement du Cattle_Feeder")

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Cattle_Live)) + ylab("Rendement du Cattle_Live")

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Copper)) + ylab("Rendement du Cuivre")

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Gold)) + ylab("Rendement de l'Or")

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Silver)) + ylab("Rendement de l'Argent")

#Plot volatilite

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Petrole^2)) + ylab("Volatilite du Petrole")

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Corn^2)) + ylab("Volatilite du Maïs")

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Soybeans^2)) + ylab("Volatilite du Soja")

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Sugar^2)) + ylab("Volatilite du Sucre")

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Cattle_Feeder^2)) + ylab("Volatilite du Cattle_Feeder")

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Cattle_Live^2)) + ylab("Volatilite du Cattle_Live")

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Copper^2)) + ylab("Volatilite du Cuivre")

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Gold^2)) + ylab("Volatilite de l'Or")

ggplot(Rd_Data) + geom_line(aes(x=Date,y=Silver^2)) + ylab("Volatilite de l'Argent")



#Modele a effectuer pour le petrole, cafe, blé, sucre et coton

#-- Petrole ----
#Arima
fit<- auto.arima(Rd_Data$Petrole, seasonal = F)
fit
coeftest(fit)
Box.test(fit$residuals,lag=12) 
Box.test((fit$residuals)^2,lag=25) #Residus autocorrele
ArchTest(fit$residuals, 12)#arch effect
#ARCH
specA_Petrole <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                           c(10, 0)), 
                   mean.model = list(armaOrder = c(0,1), 
                                     external.regressors = NULL), 
                   distribution.model = "norm") 
Arch1 <- ugarchfit(spec=specA_Petrole, data = Rd_Data$Petrole)
Arch1
# garch diagnostics
AutocorTest(residuals(Arch1,standardize=T),lag=12)
AutocorTest(residuals(Arch1,standardize=T)^2,lag=45) #Residus std au carré autocorreles justif pour passer au GARCH
ArchTest(residuals(Arch1,standardize=T),lag=40)

#GARCH
specG_Petrole <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                           c(1,1)), 
                   mean.model = list(armaOrder = c(0,1), 
                                     external.regressors = NULL), 
                   distribution.model = "norm") 
GArch <- ugarchfit(spec=specG_Petrole, data = Rd_Data$Petrole)
GArch
# garch diagnostics
AutocorTest(residuals(GArch,standardize=T),lag=12)
AutocorTest(residuals(GArch,standardize=T)^2,lag=80) #:Residus Autocorrele au seuil de risque 10%
ArchTest(residuals(GArch,standardize=T),lag=80)

#--Corn ----
#Arima
fit<- auto.arima(Rd_Data$Corn, seasonal = F)
#ARMA (0,0)

#ARCH
specA_Corn <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                           c(12, 0)), 
                   mean.model = list(armaOrder = c(0,0), 
                                     external.regressors = NULL), 
                   distribution.model = "norm") 
Arch1 <- ugarchfit(spec=specA_Corn, data = Rd_Data$Corn)
Arch1
# garch diagnostics
AutocorTest(residuals(Arch1,standardize=T),lag=12)
AutocorTest(residuals(Arch1,standardize=T)^2,lag=80) #Residus std au carré autocorreles justif pour passer au GARCH
ArchTest(residuals(Arch1,standardize=T),lag=80)

#GARCH
specG_Corn <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                           c(1,2)), 
                   mean.model = list(armaOrder = c(0,0), 
                                     external.regressors = NULL), 
                   distribution.model = "norm") 
GArch <- ugarchfit(spec=specG_Corn, data = Rd_Data$Corn)
GArch
# garch diagnostics
AutocorTest(residuals(GArch,standardize=T),lag=12)
AutocorTest(residuals(GArch,standardize=T)^2,lag=61) 
ArchTest(residuals(GArch,standardize=T),lag=70)

#--Soybeans ----
#Arima
fit<- auto.arima(Rd_Data$Soybeans, seasonal = F)
fit
#ARMA (0,0)

#ARCH
specA_Soybeans <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                 c(8, 0)), 
                         mean.model = list(armaOrder = c(0,0), 
                                           external.regressors = NULL), 
                         distribution.model = "norm") 
Arch1 <- ugarchfit(spec=specA_Soybeans, data = Rd_Data$Soybeans)
Arch1
# garch diagnostics
AutocorTest(residuals(Arch1,standardize=T),lag=100)
AutocorTest(residuals(Arch1,standardize=T)^2,lag=300) 
ArchTest(residuals(Arch1,standardize=T),lag=300)
#pas de pb d autocorrelation mais le modele est vraiment pourris

#GARCH
specG_Soybeans <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                 c(1,1)), 
                         mean.model = list(armaOrder = c(0,0), 
                                           external.regressors = NULL), 
                         distribution.model = "norm") 
GArch <- ugarchfit(spec=specG_Soybeans, data = Rd_Data$Soybeans)
GArch
# garch diagnostics
AutocorTest(residuals(GArch,standardize=T),lag=12)
AutocorTest(residuals(GArch,standardize=T)^2,lag=50) #:Residus non autocorrele
ArchTest(residuals(GArch,standardize=T),lag=100)

#--Sugar ----
#Arima
fit<- auto.arima(Rd_Data$Sugar, seasonal = F)
fit
#ARMA (0,0)

#ARCH
specA_Sugar <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                     c(15, 0)), 
                             mean.model = list(armaOrder = c(0,0), 
                                               external.regressors = NULL), 
                             distribution.model = "norm") 
Arch1 <- ugarchfit(spec=specA_Sugar, data = Rd_Data$Sugar)
Arch1
# garch diagnostics
AutocorTest(residuals(Arch1,standardize=T),lag=12)
AutocorTest(residuals(Arch1,standardize=T)^2,lag=100) #Autocorrelation on passe au garch
ArchTest(residuals(Arch1,standardize=T),lag=100)


#GARCH
specG_Sugar <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                     c(1,1)), 
                             mean.model = list(armaOrder = c(0,0), 
                                               external.regressors = NULL), 
                             distribution.model = "norm") 
GArch <- ugarchfit(spec=specG_Sugar, data = Rd_Data$Sugar)
GArch
# garch diagnostics
AutocorTest(residuals(GArch,standardize=T),lag=100)
AutocorTest(residuals(GArch,standardize=T)^2,lag=100) #:Residus non autocorrele
ArchTest(residuals(GArch,standardize=T),lag=20)

#--Cattle_Feeder ----
#Arima
fit<- auto.arima(Rd_Data$Cattle_Feeder, seasonal = F)
fit
#ARMA (0,1)
coeftest(fit)
Box.test(fit$residuals,lag=12) 
Box.test((fit$residuals)^2,lag=25) #Residus autocorrele
ArchTest(fit$residuals, 12)#Arch effect

#ARCH
specA_Cattle_Feeder <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                     c(15, 0)), 
                             mean.model = list(armaOrder = c(0,1), 
                                               external.regressors = NULL), 
                             distribution.model = "norm") 
Arch1 <- ugarchfit(spec=specA_Cattle_Feeder, data = Rd_Data$Cattle_Feeder)
Arch1
# garch diagnostics
AutocorTest(residuals(Arch1,standardize=T),lag=12)
AutocorTest(residuals(Arch1,standardize=T)^2,lag=400) 
ArchTest(residuals(Arch1,standardize=T),lag=300)
#pas de pb d autocorrelation mais le modele est vraiment pourris

#GARCH
specG_Cattle_Feeder <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                     c(1,2)), 
                             mean.model = list(armaOrder = c(0,1), 
                                               external.regressors = NULL), 
                             distribution.model = "norm") 
GArch <- ugarchfit(spec=specG_Cattle_Feeder, data = Rd_Data$Cattle_Feeder)
GArch
# garch diagnostics
AutocorTest(residuals(GArch,standardize=T),lag=12)
AutocorTest(residuals(GArch,standardize=T)^2,lag=20) #:Residus non autocorrele
ArchTest(residuals(GArch,standardize=T),lag=10)

#--Cattle_Live ----
#Arima
fit<- auto.arima(Rd_Data$Cattle_Live, seasonal = F)
fit
#ARMA (0,0)

#ARCH
specA_Cattle_Live <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                          c(15, 0)), 
                                  mean.model = list(armaOrder = c(0,0), 
                                                    external.regressors = NULL), 
                                  distribution.model = "norm") 
Arch1 <- ugarchfit(spec=specA_Cattle_Live, data = Rd_Data$Cattle_Live)
Arch1
# garch diagnostics
AutocorTest(residuals(Arch1,standardize=T),lag=12)
AutocorTest(residuals(Arch1,standardize=T)^2,lag=60) 
ArchTest(residuals(Arch1,standardize=T),lag=50)
#pb d autocorrelation et d effet ARCH

#GARCH
specG_Cattle_Live <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                          c(1,2)), 
                                  mean.model = list(armaOrder = c(0,0), 
                                                    external.regressors = NULL), 
                                  distribution.model = "norm") 
GArch <- ugarchfit(spec=specG_Cattle_Live, data = Rd_Data$Cattle_Live)
GArch
# garch diagnostics
AutocorTest(residuals(GArch,standardize=T),lag=12)
AutocorTest(residuals(GArch,standardize=T)^2,lag=43) #:Residus correle au seuil de risque 10%
ArchTest(residuals(GArch,standardize=T),lag=12)

#--Copper ----
#Arima
fit<- auto.arima(Rd_Data$Copper, seasonal = F)
fit
#ARMA (1,0)
coeftest(fit)
Box.test(fit$residuals,lag=12) 
Box.test((fit$residuals)^2,lag=25) #Residus autocorrele
ArchTest(fit$residuals, 12)#Arch effect

#ARCH
specA_Copper <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                          c(12, 0)), 
                                  mean.model = list(armaOrder = c(1,0), 
                                                    external.regressors = NULL), 
                                  distribution.model = "norm") 
Arch1 <- ugarchfit(spec=specA_Copper, data = Rd_Data$Copper)
Arch1
# garch diagnostics
AutocorTest(residuals(Arch1,standardize=T),lag=12)
AutocorTest(residuals(Arch1,standardize=T)^2,lag=60) 
ArchTest(residuals(Arch1,standardize=T),lag=50)
#pb d autocorrelation et arch effect

#GARCH
specG_Copper <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                          c(1,1)), 
                                  mean.model = list(armaOrder = c(1,0), 
                                                    external.regressors = NULL), 
                                  distribution.model = "norm") 
GArch <- ugarchfit(spec=specG_Copper, data = Rd_Data$Copper)
GArch
# garch diagnostics
AutocorTest(residuals(GArch,standardize=T),lag=45)
AutocorTest(residuals(GArch,standardize=T)^2,lag=1) #:Residus autoocorrele au seuil de risque 10%
ArchTest(residuals(GArch,standardize=T),lag=1) #Arch effect au seuil de risque 10%

#--Gold ----
#Arima
fit<- auto.arima(Rd_Data$Gold, seasonal = F)
fit
fit=arima(Rd_Data$Gold,order=c(1,0,0),include.mean=T)
#ARMA (2,0)
coeftest(fit) #Coefficient non significatif
Box.test(fit$residuals,lag=12) 
Box.test((fit$residuals)^2,lag=25) #Residus autocorrele
ArchTest(fit$residuals, 12)#Arch effect

#ARCH
specA_Gold <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                          c(12, 0)), 
                                  mean.model = list(armaOrder = c(0,0), 
                                                    external.regressors = NULL), 
                                  distribution.model = "norm") 
Arch1 <- ugarchfit(spec=specA_Gold, data = Rd_Data$Gold)
Arch1
# garch diagnostics
AutocorTest(residuals(Arch1,standardize=T),lag=12)
AutocorTest(residuals(Arch1,standardize=T)^2,lag=60) 
ArchTest(residuals(Arch1,standardize=T),lag=50)
#pb d autocorrelation et arch effect

#GARCH
specG_Gold <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                          c(1,2)), 
                                  mean.model = list(armaOrder = c(0,0), 
                                                    external.regressors = NULL), 
                                  distribution.model = "norm") 
GArch <- ugarchfit(spec=specG_Gold, data = Rd_Data$Gold)
GArch
# garch diagnostics
AutocorTest(residuals(GArch,standardize=T),lag=12)
AutocorTest(residuals(GArch,standardize=T)^2,lag=4) #:Residus autocorrele
ArchTest(residuals(GArch,standardize=T),lag=4)#pb arch effect

#--Silver ----
#Arima
fit<- auto.arima(Rd_Data$Silver, seasonal = F)
fit
#ARMA (1,0)
coeftest(fit)
Box.test(fit$residuals,lag=12) 
Box.test((fit$residuals)^2,lag=25) #Residus autocorrele
ArchTest(fit$residuals, 12)#Arch effect

#ARCH
specA_Silver <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                          c(15, 0)), 
                                  mean.model = list(armaOrder = c(1,0), 
                                                    external.regressors = NULL), 
                                  distribution.model = "norm") 
Arch1 <- ugarchfit(spec=specA_Silver, data = Rd_Data$Silver)
Arch1
# garch diagnostics
AutocorTest(residuals(Arch1,standardize=T),lag=12)
AutocorTest(residuals(Arch1,standardize=T)^2,lag=300) #Residus au carre autocorreles au seuil de risque 10% 
ArchTest(residuals(Arch1,standardize=T),lag=70)
#arch effect au sueil de risque 5%

#GARCH
specG_Silver <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = 
                                                          c(1,2)), 
                                  mean.model = list(armaOrder = c(0,0), 
                                                    external.regressors = NULL), 
                                  distribution.model = "norm") 
GArch <- ugarchfit(spec=specG_Silver, data = Rd_Data$Silver)
GArch
# garch diagnostics
AutocorTest(residuals(GArch,standardize=T),lag=12)
AutocorTest(residuals(GArch,standardize=T)^2,lag=3) #:Residus autocorrele
ArchTest(residuals(GArch,standardize=T),lag=3)#Pb ARCH EFFECT

#Xts
Rd_Data <- xts(Rd_Data,order.by = Rd_Data$Date)
summary(Rd_Data)

#-- Creation modele DCC ----
uspec.n <- multispec(list(specG_Petrole,specG_Corn,specG_Soybeans,specG_Sugar,specG_Cattle_Feeder,specG_Cattle_Live,specG_Copper,specG_Gold,specG_Silver))
multf <- multifit(uspec.n,Rd_Data[,c(2,3,8,9,10,11,14,15,16)])
spec1 <- dccspec(uspec=uspec.n,dccOrder = c(1,1), distribution = 'mvnorm')

#Model Estimation
fit1 <- dccfit(spec1,data=Rd_Data[,c(2,3,8,9,10,11,14,15,16)],fit.control=list(eval.se = TRUE), fit=multf)

fit1

# garch diagnostics
AutocorTest(fit1@mfit$stdresid[,1]^2,lag=10) #:Residus non autocorrele
AutocorTest(fit1@mfit$stdresid[,2]^2,lag=10) #:Residus non autocorrele
AutocorTest(fit1@mfit$stdresid[,3]^2,lag=10) #:Residus non autocorrele
AutocorTest(fit1@mfit$stdresid[,4]^2,lag=10) #:Residus non autocorrele
AutocorTest(fit1@mfit$stdresid[,5]^2,lag=10) #:Residus non autocorrele
AutocorTest(fit1@mfit$stdresid[,6]^2,lag=10) #:Residus non autocorrele
AutocorTest(fit1@mfit$stdresid[,7]^2,lag=10) #:Residus non autocorrele
AutocorTest(fit1@mfit$stdresid[,8]^2,lag=50) #:Residus non autocorrele
AutocorTest(fit1@mfit$stdresid[,9]^2,lag=50) #:Residus non autocorrele

ArchTest(fit1@mfit$stdresid,lag=10)#Pas d ARCH EFFECT sur ensemble du modele
#On test arch effect sur chaque relation
ArchTest(fit1@mfit$stdresid[,c(1,2)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(1,3)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(1,4)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(1,5)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(1,6)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(1,7)],lag=20)
ArchTest(fit1@mfit$stdresid[,c(1,8)],lag=50)
ArchTest(fit1@mfit$stdresid[,c(1,9)],lag=50)
#Aucun probleme pour relation avec le petrole

ArchTest(fit1@mfit$stdresid[,c(2,3)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(2,4)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(2,5)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(2,6)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(2,7)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(2,8)],lag=50)
ArchTest(fit1@mfit$stdresid[,c(2,9)],lag=40)
#Aucun prob avec les relations liées au mais

ArchTest(fit1@mfit$stdresid[,c(3,4)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(3,5)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(3,6)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(3,7)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(3,8)],lag=50)
ArchTest(fit1@mfit$stdresid[,c(3,9)],lag=40)
#Aucun prob avec les relations liées au soja

ArchTest(fit1@mfit$stdresid[,c(4,5)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(4,6)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(4,7)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(4,8)],lag=50)
ArchTest(fit1@mfit$stdresid[,c(4,9)],lag=40)
#Auncun prob avec les relations liées au Sucre

ArchTest(fit1@mfit$stdresid[,c(5,6)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(5,7)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(5,8)],lag=50)
ArchTest(fit1@mfit$stdresid[,c(5,9)],lag=50)
#aucun arch effect avec cattle feeder

ArchTest(fit1@mfit$stdresid[,c(6,7)],lag=10)
ArchTest(fit1@mfit$stdresid[,c(6,8)],lag=60)
ArchTest(fit1@mfit$stdresid[,c(6,9)],lag=40)
#Aucun pb avec cattle live

ArchTest(fit1@mfit$stdresid[,c(7,8)],lag=50)
ArchTest(fit1@mfit$stdresid[,c(7,9)],lag=50)
#ok
ArchTest(fit1@mfit$stdresid[,c(8,9)],lag=80)

#Correlation COnditionnelle Dynamique
cor1 <- rcor(fit1)

cor_Petrole_Corn <- as.xts(cor1[1,2,])
cor_Petrole_Soybeans <- as.xts(cor1[1,3,])
cor_Petrole_Sugar <- as.xts(cor1[1,4,])
cor_Petrole_CattleFeeder <- as.xts(cor1[1,5,])
cor_Petrole_CattleLive <- as.xts(cor1[1,6,])
cor_Petrole_Copper <- as.xts(cor1[1,7,])
cor_Petrole_Gold <- as.xts(cor1[1,8,])
cor_Petrole_Silver <- as.xts(cor1[1,9,])

cor_Corn_Soybeans <- as.xts(cor1[2,3,])
cor_Corn_Sugar <- as.xts(cor1[2,4,])
cor_Corn_CattleFeeder <- as.xts(cor1[2,5,])
cor_Corn_CattleLive <- as.xts(cor1[2,6,])
cor_Corn_Copper <- as.xts(cor1[2,7,])
cor_Corn_Gold <- as.xts(cor1[2,8,])
cor_Corn_Silver <- as.xts(cor1[2,9,])

cor_Soybeans_Sugar <- as.xts(cor1[3,4,])
cor_Soybeans_CattleFeeder <- as.xts(cor1[3,5,])
cor_Soybeans_CattleLive <- as.xts(cor1[3,6,])
cor_Soybeans_Copper <- as.xts(cor1[3,7,])
cor_Soybeans_Gold <- as.xts(cor1[3,8,])
cor_Soybeans_Silver <- as.xts(cor1[3,9,])

cor_Sugar_CattleFeeder <- as.xts(cor1[4,5,])
cor_Sugar_CattleLive <- as.xts(cor1[4,6,])
cor_Sugar_Copper <- as.xts(cor1[4,7,])
cor_Sugar_Gold <- as.xts(cor1[4,8,])
cor_Sugar_Silver <- as.xts(cor1[4,9,])

cor_CattleFeeder_CattleLive <- as.xts(cor1[5,6,])
cor_CattleFeeder_Copper <- as.xts(cor1[5,7,])
cor_CattleFeeder_Gold <- as.xts(cor1[5,8,])
cor_CattleFeeder_Silver <- as.xts(cor1[5,9,])

cor_CattleLive_Copper <- as.xts(cor1[6,7,])
cor_CattleLive_Gold <- as.xts(cor1[6,8,])
cor_CattleLive_Silver <- as.xts(cor1[6,9,])

cor_Copper_Gold <- as.xts(cor1[7,8,])
cor_Copper_Silver <- as.xts(cor1[7,9,])

cor_Gold_Silver <- as.xts(cor1[8,9,])


plot(cor_Petrole_Corn,main="Correlation Conditionnelle Dynamique du Petrole-Corn")
plot(cor_Petrole_Soybeans,main="Correlation Conditionnelle Dynamique du Petrole-Soybeans")
plot(cor_Petrole_Sugar,main="Correlation Conditionnelle Dynamique du Petrole-Sugar")
plot(cor_Petrole_CattleFeeder,main="Correlation Conditionnelle Dynamique du Petrole-Cattle_Feeder")
plot(cor_Petrole_CattleLive,main="Correlation Conditionnelle Dynamique du Petrole-Cattle_Live")
plot(cor_Petrole_Copper,main="Correlation Conditionnelle Dynamique du Petrole-Copper")
plot(cor_Petrole_Gold,main="Correlation Conditionnelle Dynamique du Petrole-Gold")
plot(cor_Petrole_Silver,main="Correlation Conditionnelle Dynamique du Petrole-Silver")

plot(cor_Corn_Soybeans,main="Correlation Conditionnelle Dynamique du Corn-Soybeans")
plot(cor_Corn_Sugar,main="Correlation Conditionnelle Dynamique du Corn-Sugar")
plot(cor_Corn_CattleFeeder,main="Correlation Conditionnelle Dynamique du Corn-Cattle_Feeder")
plot(cor_Corn_CattleLive,main="Correlation Conditionnelle Dynamique du Corn-Cattle_Live")
plot(cor_Corn_Copper,main="Correlation Conditionnelle Dynamique du Corn-Copper")
plot(cor_Corn_Gold,main="Correlation Conditionnelle Dynamique du Corn-Gold")
plot(cor_Corn_Silver,main="Correlation Conditionnelle Dynamique du Corn-Silver")

plot(cor_Soybeans_Sugar,main="Correlation Conditionnelle Dynamique du Soybeans-Sugar")
plot(cor_Soybeans_CattleFeeder,main="Correlation Conditionnelle Dynamique du Soybeans-Cattle_Feeder")
plot(cor_Soybeans_CattleLive,main="Correlation Conditionnelle Dynamique du Soybeans-Cattle_Live")
plot(cor_Soybeans_Copper,main="Correlation Conditionnelle Dynamique du Soybeans-Copper")
plot(cor_Soybeans_Gold,main="Correlation Conditionnelle Dynamique du Soybeans-Gold")
plot(cor_Soybeans_Silver,main="Correlation Conditionnelle Dynamique du Soybeans-Silver")

plot(cor_Sugar_CattleFeeder,main="Correlation Conditionnelle Dynamique du Sugar-Cattle_Feeder")
plot(cor_Sugar_CattleLive,main="Correlation Conditionnelle Dynamique du Sugar-Cattle_Live")
plot(cor_Sugar_Copper,main="Correlation Conditionnelle Dynamique du Sugar-Copper")
plot(cor_Sugar_Gold,main="Correlation Conditionnelle Dynamique du Sugar-Gold")
plot(cor_Sugar_Silver,main="Correlation Conditionnelle Dynamique du Sugar-Silver")

plot(cor_CattleFeeder_CattleLive,main="Correlation Conditionnelle Dynamique du CattleFeeder-Cattle_Live")
plot(cor_CattleFeeder_Copper,main="Correlation Conditionnelle Dynamique du CattleFeeder-Copper")
plot(cor_CattleFeeder_Gold,main="Correlation Conditionnelle Dynamique du CattleFeeder-Gold")
plot(cor_CattleFeeder_Silver,main="Correlation Conditionnelle Dynamique du CattleFeeder-Silver")

plot(cor_CattleLive_Copper,main="Correlation Conditionnelle Dynamique du CattleLive-Copper")
plot(cor_CattleLive_Gold,main="Correlation Conditionnelle Dynamique du CattleLive-Gold")
plot(cor_CattleLive_Silver,main="Correlation Conditionnelle Dynamique du CattleLive-Silver")

plot(cor_Copper_Gold,main="Correlation Conditionnelle Dynamique du Copper-Gold")
plot(cor_Copper_Silver,main="Correlation Conditionnelle Dynamique du Copper-Silver")

plot(cor_Gold_Silver,main="Correlation Conditionnelle Dynamique du Gold-Silver")

