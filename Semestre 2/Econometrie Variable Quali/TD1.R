library(outliers)
library(EnvStats)
library(ggplot2)
library(car)

press <- Base_pression_arterielle

#Question 2
cor(press[,c("Age","IMC")])



#Question 3

press$Press_arter<-as.factor(press$Press_arter)
press$Genre<-as.factor(press$Genre)
press$Fumer<-as.factor(press$Fumer)
press$Pression<-as.factor(press$Pression)
press$Alcool<-as.factor(press$Alcool)
press$Stress<-as.factor(press$Stress)
press$Sel<-as.factor(press$Sel)


#Question 4

#lien Quali quali (in/dependance)
chisq.test(press$Genre,press$Pression)
chisq.test(press)

for (i in names(press))
{
  if(is.factor(press[ , i]) == TRUE){
    for (j in names(press))
    {
      if(is.factor(press[ , j]) == TRUE){
        print(i)
        print(j)
        chisq.test(press[,i],press[,j])
      }
    }
  }
}

#lien quanti quali in/dependance (ordre important)
t.test(press$Age~press$Fumer)

#BoxPlot
par(mfrow=c(2,2))
boxplot(press$Age)
boxplot(press$IMC)

#Outlier quanti
grubbs.test(press$Age)
grubbs.test((press$IMC))#No outliers for 2

#Histo
hist(press$Age, col="red", main="Histogram de l'Age des patients", xlab="Age",ylab="Frequence")
hist(press$IMC, col="blue", main="Histogram de l'IMC des patients", xlab="IMC",ylab="Frequence")
#ILs ne suivent pas une loi normal
shapiro.test(press$IMC)
shapiro.test(press$Age)

#Question 5

cor(press[,-c(1,2)]) #Pas de prob de multicolinearite

#Modele LOGIT
modele<-
  glm(Pression~Age+IMC+Sel+Fumer+Genre+Sport+Stress+Alcool,data=press,family=
        binomial(link="logit"))
summary(modele)

#VIF
vif(modele)

#Interet du  modele
chi2<- (modele$null.deviance-modele$deviance)
ddl<-modele$df.null-modele$df.residual
pvalue<-pchisq(chi2,ddl,lower.tail=F)
print(pvalue) #On refuse H0: on refuse l hypothese de nullite de l'ensemble des coefficients de variables explicatives du modèle, il y a interet à estimer ce modèle

#Calcul des effets marginaux
mean(dlogis(predict(modele,type="link")))*coef(modele)

#Calcul des ODD-Ratios
exp(coef(modele))

#Calcul de proba estimée pour chaque observation de la bdd
pred.proba<-predict(modele,type="response")
print(pred.proba)

plot(press$Pression,col="red")
par(new=TRUE)
plot(pred.proba,col="blue")


