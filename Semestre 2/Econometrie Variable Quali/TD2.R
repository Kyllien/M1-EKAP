library(readxl)
library(outliers)
library(mlogit)
Marque <- read_excel("D:/M1 EKAP/Semestre 2/Econometrie Variable Quali/Marque.xls")
View(Marque)

#Question 1 : pas de outlier
boxplot(Marque$Age)
grubbs.test(Marque$Age)

#Question 3
Marque$Femme <- as.factor(Marque$Femme)

#Question 4 
head(Marqueb)
Marqueb<-Marque[,-1]
Marqueb<-mlogit.data(Marqueb, shape= "long",varying=2 :6,choice= "Choice")

import