library(readxl)
library(EnvStats)
library(mlogit)
library(VGAM)
library(lmtest)

#???question 1 ------------------------------------
base<-read_excel("C:/Users/delch/Documents/M1/Semestre  2/Econométrie des variables qualitatives/base excel/Marque.xls")
View(base)
base<-as.data.frame(base)

#question 2 ----------------------------------------
boxplot(base$Age)
#Il y a 5 valeurs atypiques pour la varibale Age
rosnerTest(base$Age,  k = 7, alpha = 0.05)
#Après un rosner test, nous voyons qu'il n'y a pas d'outliers

#question 3 ---------------------------------------
base$Femme<-as.factor(base$Femme)
summary(base)

#question 4 -------------------------------
#les variables aux choix sont marque 1, marque 2 et marque 3 et la variable liée aux individus sont le prix

#question 5 --------------------------------------------
base1<-mlogit.data(base, shape="wide", varying=3:5, choice="Choice")
head(base1,8)
#10 lignes vont apparaitre

ml<-mlogit(Choice~0|Femme+Age, data=base1)
summary(ml)
ml1<-vglm(Choice~Femme+Age,multinomial(refLevel=1),data=base)
summary(ml1)
#vglm prendre la base de début

#question 6---------------------
ml2<-mlogit(Choice~Prix|Femme+Age, data=base1)
summary(ml2)

lrtest(ml,ml2)
#p-value de 0.57, le prix n'a pas d'impact

#question 7 ------------------------
exp(coef(ml1))
#la marque 1 a servi de ref 
#le fait d'être une femme va vaoir un impact significatif de choisir la marquez 2 par rapport à1 et 3 par rapport à 1

#question 8-------------------------

mycol <- c("red", "blue")
ooo <- with(base, order(Age))
with(base, matplot(Age[ooo], fitted(ml1)[ooo,], ylim = c(0,1),
                   xlab = "Age", ylab = "Probabilité estimée",
                   main = " Effet de la variable Age ", type = "l", lwd = 2, col = c(mycol[1],
                                                                                     "black", mycol[-1])))
with(base, rug(Age))
legend("topright", col = c(mycol[1], "black", mycol[-1]), lty=1:3
       ,legend=colnames(ml1@y))

#question 9 -----------------------

ml3<-mlogit(Choice~0|Femme+Age,heterosc=TRUE, data=base1)
summary(ml3)

lrtest(ml,ml3)

#Question 10 ----------------------

