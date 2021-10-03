library(PerformanceAnalytics)
library(psych)
View(managers)

####Moment d'ordre 1-4, kurtosis, skewness, cov et corr.
data(managers)
#preview the data
head(managers)
#summary period statistics
summary(managers)
sd(managers[,1:10],na.rm=TRUE)
#cumulative returns
tail(cumprod(1+managers),1)
data(managers)
data(managers)
# Coskewness : en statistique, mesure le degré de modification simultanée de trois variables aléatoires et est utilisé en finance pour analyser le risque de sécurité et le risque de portefeuille.
#Cokurtosis : 
data(managers)
kurtosis(managers[,1:8])
data(portfolio_bacon)
print(kurtosis(portfolio_bacon[,1], method="sample")) #expected 3.03
print(kurtosis(portfolio_bacon[,1], method="sample_excess")) #expected

data(managers)
apply(managers[,1:6], 2, sd, na.rm=TRUE)
DownsideDeviation(managers[,1:6]) # MAR 0%
DownsideDeviation(managers[,1:6], MAR = .04/12) #MAR 4%
SemiDeviation(managers[,1,drop=FALSE])
SemiDeviation(managers[,1:6])
SemiVariance (managers[,1,drop=FALSE])
SemiVariance (managers[,1:6]) #calculated using method="subset"
chart.Correlation(managers[,1:8], histogram=TRUE, pch="+")



CoVariance(managers[, "HAM2", drop=FALSE], managers[, "SP500 TR", drop=FALSE])
CoSkewness(managers[, "HAM2", drop=FALSE], managers[, "SP500 TR", drop=FALSE])
CoKurtosis(managers[, "HAM2", drop=FALSE], managers[, "SP500 TR", drop=FALSE])


describe(managers)
SemiVariance(managers)
DownsideDeviation(managers)

#A faire pour chaque titre
CoSkewnessMatrix( managers,managers[,8])
CoKurtosisMatrix(managers,managers[,8])
CoVariance(managers[,8],managers)

cor(managers)

#Exercice 2

BetaCoVariance(managers$HAM2,managers$`SP500 TR`)

CAPM.alpha(managers$HAM1, managers$`SP500 TR`, Rf = 0.035/12)
CAPM.beta(managers$HAM1, managers$`SP500 TR`, Rf = 0.035/12)

for(i in c(1:6)){
  beta = CAPM.beta(managers[, i], managers$`SP500 TR`, Rf = 0.035/12)
  alpha = CAPM.alpha(managers[, i], managers$`SP500 TR`, Rf = 0.035/12)
  print(names(managers[, i]))
  print(beta)
  print(alpha)
}

CAPM.alpha(managers$HAM1, managers$`SP500 TR`, Rf = managers$'US 3m TR')
CAPM.beta(managers$HAM1, managers$`SP500 TR`, Rf = managers$'US 3m TR')

#Exercice 3
base<-managers
SharpeRatio(base$HAM1, base$`SP500 TR`)
TreynorRatio(base$HAM1, base$`SP500 TR`)
CAPM.jensenAlpha(base$HAM1, base$`SP500 TR`)

SharpeRatio(base['2005', 1], base['2005', 8])

SharpeRatio(base$HAM1, base$`SP500 TR`)
CAPM.jensenAlpha(base$HAM1, base$`SP500 TR`)
CAPM.jensenAlpha(base$HAM1, base$`SP500 TR`)

for(i in c(1:5)){
  a=SharpeRatio(base[, i], base$`SP500 TR`)
  b=TreynorRatio(base[, i], base$`SP500 TR`)
  c=CAPM.jensenAlpha(base[, i], base$`SP500 TR`)
  print(a)
  print(b)
  print(c)
}

#Exercice 4

MarketTiming(base$HAM1, base$`SP500 TR`, Rf = base$'US 3m TR', method = "TM")
MarketTiming(base$HAM1, base$`SP500 TR`, Rf = base$'US 3m TR', method = "HM")

#Exercice 5
library(readxl)
WMT <- read.csv("C:/Users/UTILISATEUR/Downloads/WMT.csv")
AMZN <- read.csv("C:/Users/UTILISATEUR/Downloads/AMZN.csv")
TSLA <- read.csv("C:/Users/UTILISATEUR/Downloads/TSLA.csv")

Prix <-cbind(WMT[,6],AMZN[,6],TSLA[,6])



