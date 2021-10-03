#question 1
completset2 <- read.csv2("~/M1/IntroR/base excel/completset1.csv")
View(completset2)

#question 2
library(dplyr)
age2<-completset2$age*completset2$age
inc2<-completset2$inc*completset2$inc

completset2<-cbind(completset2,age2)
completset2<-cbind(completset2,inc2)
View(completset2)

#question 3
modele<-lm(nettfa~inc+age,data=completset2)
summary(modele)
confint(modele)

#medianne pas roche de zéro donc les résidus ne suivent pas une loi normale
#la p-value<0,05 ce qui signifie que l'on rejette HO, donc qu'il y a un moins 
#un variable significative
#conjointement les varaibles permettent d'expliquer en partie la variable 
#endogène au seuil de 1%
#Elles sont toutes significatives au seuil de 1%, HO est refusée au seuil de 1%
#Les trois étoiles permettent de dire que c'est significatif au seuil de 1%
#la fonction confint nous donne les intervalles de confinace pour les estimateurs
#pour utiliser les intervalles, l'échantillon doit être représentatif, aléatoirement distribué


#question 4
plot(modele)
residus<-residuals(modele)
completset2<-cbind(completset2,residus)
qqnorm(completset2$residus)

abline(h=0,col="red") #permet de rajouter une ligne sur un graphique
#il faudrait que les pointillés suivent la ligne au nv de 0

hist(completset2$residus)

library(normtest)
jb.norm.test(rnorm(100))
jb.norm.test(completset2$residus)

#la p-value est à chier, ca ne suit pas une loi normale
#p-value<0,01, rejet de normalité au seuil de 1%, les résidus
#ne suivent donc pas une loi normale
#Tt les estimateurs sont biaisé, il faut utiliser un autre modèle,
#changement de la forme fonctionnelle, ou application d'un filtre

#question 5
completset2$nettfadev<-completset2$nettfa*1000
str(completset2)
modele1<-lm(nettfadev~inc+age,data=completset2)

#question6
summary(modele1)

#tt les coeff ont été multiplié par 1000

#question 7
modele2<-lm(nettfa~inc+age+age2+e401k,data=completset2)
summary(modele2)

#regarder la significativité


#question 8

completset3<-completset2[order(completset2$fsize),]
library(strucchange)
sctest(completset3$nettfa~completset3$inc+completset3$age+completset3$age2+completset3$e401k,type="Chow",point=2018)

#la p-value<0,01 ==> ce qui signifie qu'il y a rejet de H0 (stabilité des coeff)
#acceptation de H1 ==> il y a donc pas de stabilité des coeff

#question 9
library(lmtest)
onepers<-completset2[!completset2$fsize>1,]
#! ecreser tt les obs qd fsize>1
bptest(nettfa~inc+age+age2+e401k,data=onepers)

#rejet de HO(il y a donc homoscédasticté des résidus au seuil de 1%)
#nos résidus sont hétéroscedastique.