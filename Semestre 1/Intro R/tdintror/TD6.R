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

#Pour ce modele, il y a un R� de 0,28 et la varibale lsales est statistiquement signficatif au seuil de 1%
#les varibles sont en logarithme, une augmentation de 1% du chiffre d'affaire, le salaire du PDG augmentara de 0,22%
#Pour les r�sidus, la moyenne est tr�s proche de z�ro, ce que signifie que les r�sidus suivent une loi normale


#question 4

modele1<-lm(lsalary~lsales+lmktval,data=ceosal2)
modele1
summary(modele1)

#Pour ce mod�le, les variables lsales et lmktval sont statistiquemet significative respectivement au seuil de 1% et au seuil de 5%
#Le R� est de 0,29 il est donc meilleur que celui du modele pr�c�dent
#Au niveau des r�sidus, leur moyenne est tr�s proche de z�ro, ce qui signife qu'il suivent une loi normale
#le CA augmente de 1%, salaire PDG augmente de 0,16%

#question 5

#il a chang� pcq il y a une varibale de plus dans l'�quation mais il reste significatif au m�me seuil pour les deux mod�les
#l'ajout de la variable, 
#d�s que les vraribles sont corr�ls, l'estimation sera biais� � la hausse ou � la baisse, ici biais positif'
#explication fausse le poids donn� � la varaibles explicative sera trop grand
#l'effet de la variable lsales plus une partie du poids de la variable lmhtval

#question 6

library(psych)
attach(ceosal2)
cor(lsales,lmktval,use="complete.obs",method="pearson")
plot(ceosal2$lsales,ceosal2$lmktval)
#il y a une corr�lation posotive entre les deux de 0,736
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
#profits n'a pas �t� ajout� en log pcq des valeurs de profits sont n�gative
#Une relation tr�s frote entre profits et lmktval, pas mettre ces deux variables
#le R� ajust� a diminu� en rajoutant une variable, de tr�s peu
#le vif nous donne rien 

#question 8

modele3<-lm(lsalary~lsales+lmktval+ceoten,data=ceosal2)
summary(modele3)
vif(modele3)

#Tte les varibles sont significaif au seuil de 5%
#Le R� ajust� a augment�, donc c'est bien
#qd mon anciennet� augmente de 1 an, le salaire du PDG augmente de 0,01 milliers de $
#l'anciennet� explique pas �normement la salaire la valeur du beta est assez faible
#

#question 9

modele4<-lm(lsalary~lsales+lmktval+ceoten+ceotensq,data=ceosal2)
summary(modele4)

#l'effet est tr�s faible mais il est n�gatif
#variation du salire plus �lev� mais plus mon anciennet� est longue et plus l'effet sur le salaire est faible
#tt les param�tres sont staitiquement signifcativement 
#le R� a augment�

#question 10

#crit�re AIC sert � minimiser
AIC(modele1)
AIC(modele2)
AIC(modele3)
AIC(modele4)

#la stat la plus faible pour le AIC est sur le mod�le 4

#crit�re BIC  
BIC(modele1)
BIC(modele2)
BIC(modele3)
BIC(modele4)

#la stat la plus faible est sur le mod�le 4
#Pour le R� est ajust� c'est aussi pour le mod�le 4
#on choisit le mod�le 5

#question 11
library(stargazer)
stargazer(modele4,out="resulat.tex")
stargazer(modele4,modele3,title="r�sultat des r�gression")
formtex<-stargazer(modele4,modele3,modele2,type="text",out="table.txt")

#question 12
plot(modele4,1)
plot(modele4,2)
library(lmtest)
res<-resid(modele4)
hist(res)
#les residus paraisent bien distribu� en loi normale
shapiro.test(res)
library(normtest)
jb.norm.test(res)
#les r�sidus ne sont pas normalement distribu�s
#rejet de H0 p-value<0,05

#question 13

#test de Breush-pagan, homosc�dacticit�
bptest(modele4)
#rejet de HO, donc il y bien un pb d'homosc�dasticit�

#test de White
library(skedastic)
white_lm(modele4, interactions=TRUE)
#rejet de HO, les r�sidus sont h�t�rosc�dastique au seuil de 1%

#autre m�thde pour white mais plus relou
#bptest(modele5, varformula=)

#test de goldfield-Quandt
gqtest(modele4)
#meme conclusion

#question 14

#test de Durbin-Watson
dwtest(modele4)
#p-value >0,05 ==> H0 est accpet�, les r�sidus ne sont pas auto-corr�l�s

#test de Breush-Godfrey
bgtest(modele4,order=1)
#m�me conclusion, on rejette pas l'hypoth�se nulle d'auto corr�lation entre les r�sidus
#les r�sidus sont donc int�pendant



