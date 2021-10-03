#question 1

library(wooldridge)
data("ceosal2")

#question 2

summary(ceosal2)
View(ceosal2)
ceosal2$college<-as.factor(ceosal2$college)
ceosal2$grad<-as.factor(ceosal2$grad)
summary(ceosal2)

#question 3

modele<-lm(lsalary~lsales,data=ceosal2)
modele
summary(modele)

#Pour ce modele, il y a un R² de 0,28 et la varibale lsales est statistiquement signficatif au seuil de 1%
#les varibles sont en logarithme, une augmentation de 1% du chiffre d'affaire, le salaire du PDG augmentara de 0,22%
#Pour les résidus, la moyenne est très proche de zéro, ce que signifie que les résidus suivent une loi normale


#question 4

modele1<-lm(lsalary~lsales+lmktval,data=ceosal2)
modele1
summary(modele1)

#Pour ce modèle, les variables lsales et lmktval sont statistiquemet significative respectivement au seuil de 1% et au seuil de 5%
#Le R² est de 0,29 il est donc meilleur que celui du modele précédent
#Au niveau des résidus, leur moyenne est très proche de zéro, ce qui signife qu'il suivent une loi normale
#le CA augmente de 1%, salaire PDG augmente de 0,16%

#question 5

#il a changé pcq il y a une varibale de plus dans l'équation mais il reste significatif au même seuil pour les deux modèles
#l'ajout de la variable, 
#dès que les vraribles sont corréls, l'estimation sera biaisé à la hausse ou à la baisse, ici biais positif'
#explication fausse le poids donné à la varaibles explicative sera trop grand
#l'effet de la variable lsales plus une partie du poids de la variable lmhtval

#question 6

library(psych)
attach(ceosal2)
cor(lsales,lmktval,use="complete.obs",method="pearson")
plot(ceosal2$lsales,ceosal2$lmktval)
#il y a une corrélation posotive entre les deux de 0,736
library(car)
vif(modele1)


#biais possitif, surrestimer l'effet d'une varaible

#question 7

modele2<-lm(lsalary~lsales+lmktval+profits,data=ceosal2)
summary(modele2)
cor(ceosal2$profits,ceosal2$lmktval)
plot(ceosal2$profits,ceosal2$lmktval)
vif(modele2)

#la varible profit n'a pas d'effet sur la variable salary
#profits n'a pas été ajouté en log pcq des valeurs de profits sont négative
#Une relation très frote entre profits et lmktval, pas mettre ces deux variables
#le R² ajusté a diminué en rajoutant une variable, de très peu
#le vif nous donne rien 

#question 8

modele3<-lm(lsalary~lsales+lmktval+ceoten,data=ceosal2)
summary(modele3)
vif(modele3)

#Tte les varibles sont significaif au seuil de 5%
#Le R² ajusté a augmenté, donc c'est bien
#qd mon ancienneté augmente de 1 an, le salaire du PDG augmente de 0,01 milliers de $
#l'ancienneté explique pas énormement la salaire la valeur du beta est assez faible
#

#question 9

modele4<-lm(lsalary~lsales+lmktval+ceoten+ceotensq,data=ceosal2)
summary(modele4)

#l'effet est très faible mais il est négatif
#variation du salire plus élevé mais plus mon ancienneté est longue et plus l'effet sur le salaire est faible
#tt les paramètres sont staitiquement signifcativement 
#le R² a augmenté

#question 10

#critère AIC sert à minimiser
AIC(modele1)
AIC(modele2)
AIC(modele3)
AIC(modele4)

#la stat la plus faible pour le AIC est sur le modèle 4

#critère BIC  
BIC(modele1)
BIC(modele2)
BIC(modele3)
BIC(modele4)

#la stat la plus faible est sur le modèle 4
#Pour le R² est ajusté c'est aussi pour le modèle 4
#on choisit le modèle 5

#question 11
library(stargazer)
stargazer(modele4,out="resulat.tex")
stargazer(modele4,modele3,title="résultat des régression")
formtex<-stargazer(modele4,modele3,modele2,type="text",out="table.txt")

#question 12
plot(modele4,1)
plot(modele4,2)
library(lmtest)
res<-resid(modele4)
hist(res)
#les residus paraisent bien distribué en loi normale
shapiro.test(res)
library(normtest)
jb.norm.test(res)
#les résidus ne sont pas normalement distribués
#rejet de H0 p-value<0,05

#question 13

#test de Breush-pagan, homoscédacticité
bptest(modele4)
#rejet de HO, donc il y bien un pb d'homoscédasticité

#test de White
library(skedastic)
white_lm(modele4, interactions=TRUE)
#rejet de HO, les résidus sont hétéroscédastique au seuil de 1%

#autre méthde pour white mais plus relou
#bptest(modele5, varformula=)

#test de goldfield-Quandt
gqtest(modele4)
#meme conclusion

#question 14

#test de Durbin-Watson
dwtest(modele4)
#p-value >0,05 ==> H0 est accpeté, les résidus ne sont pas auto-corrélés

#test de Breush-Godfrey
bgtest(modele4,order=1)
#même conclusion, on rejette pas l'hypothèse nulle d'auto corrélation entre les résidus
#les résidus sont donc intépendant



