library(psych)
library(tseries)
library()

DataEvalActif <- read.csv("https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/mult_assets.csv", header = TRUE, sep= ",")
View(DataEvalActif)

summary(DataEvalActif)

describe(DataEvalActif)
#X : moy = 0.07 sd=0.05
#Y : moy= 0.03 sd=0.02
#Z : moy = 0.04 sd=0.03

#Creation du vecteur

w<-c(seq(0,1,0.001))
w

plot(DataEvalActif)
