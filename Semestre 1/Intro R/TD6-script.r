#Question 1

library(wooldridge)

data('ceosal2')
ceosal2
View(ceosal2)

#Statistique descriptives

summary(ceosal2)

#Question 2

str(ceosal2)

#Question 3

lsal <- lm(ceosal2$lsalary ~ ceosal2$lsales)
summary(lsal)

#la P-value est inférieur à 0,05 pour student alors la variable est significatif et on rejete H0

#la P-value est inférieur à 0,01 pour la statistique de ficher et donc le modèle est significatif

#une augmentation du chiffre d'affaire de 1 % entraine une augmentation du salaire des PDG de 0,22%

#On peut voir que la median est proche de zero donc les residus suivent une loi normale
                                                                                                
#Question 4

lsal2 <- lm(ceosal2$lsalary ~ ceosal2$lsales + ceosal2$lmktval)
summary(lsal2)

#Quand le chiffre d'affaire augmenter de 1% le salaire du PDG augmente de 0,16 % et la valeur du marche augmnte de 0,11%

#Question 5 pourquoi le B1 à t'il changé ?

#En mettant une variable en plus le B1 à diminuer parce que la variable qu'on à ajouter est significative et parce que cette variable est corrélés avec la précédente
#les estimateurs peuvent donc être biaisé si il manque des variables explicatives qui peut expliquer notre variable à expliquer et encore plus quand ces variables sont corrélés avec notre variable explicatives principales
#si la variable qu'on ajoute dans notre modèle n'est donc pas autant significatif il y'a donc aucun interêt à ajouter cette variable dans notre modèle

#Question 6

cor.test(ceosal2$lsales, ceosal2$lmktval)

#Pour voir la rélation entre nos deux variables
plot(ceosal2$lsales, ceosal2$lmktval)
abline(h=0, col="red")#pour pouvoir tracer une droite de régression à travers le nuage

#Pour utiliser le VIF on installer la library car
library(car)
vif(lsal2)
help("vif")

#Question 7

lsal3 <- lm(ceosal2$lsalary ~ ceosal2$lsales + ceosal2$lmktval + ceosal2$profits)
lsal3
summary(lsal3)

#On appliquer pas le log aux profits parce qu'il possède des valeurs négatives
#La variable profit n'a pas d'éffet sur les salaires des PDG

  #On fait un test de corrélation pour voir 
cor.test(ceosal2$profits, ceosal2$lmktval)
plot(ceosal2$profits,ceosal2$lmktval)

  #On remarque y'a un problème de multicolinéarité, on décidé donc soit d'ajouter profit ou lmktval dans le modèle mais pas les deux en mêmes temps
cor.test(ceosal2$sales,ceosal2$lmktval)

  #Test du vif

vif(lsal3)

  #

#avec profit en log

lsal4 <- lm(ceosal2$lsalary ~ ceosal2$lsales + ceosal2$lmktval + log(ceosal2$profits))
lsal4
summary(lsal4)
#Question 8

lsal5 <- lm(ceosal2$lsalary ~ ceosal2$lsales + ceosal2$lmktval + ceosal2$ceoten)
lsal5
summary(lsal5)

  #Le R2 à augmenter donc la qualité du modèle s'est améliorer

#Si mon expérience dans la société augmente de 1année alors le salaire du PDG augmente de 0,10 milliers de dollars

#Question 9

lsal6 <- lm(ceosal2$lsalary ~ ceosal2$lsales + ceosal2$lmktval + ceosal2$ceoten + ceosal2$ceotensq)
summary(lsal6)

#la variable ceoten expliquer une anciennété plus élévé et plus longtemps je suis dans la société plus mon salaire est faible

#Question 10
#Comparaison critère AIC

#Vif
library(car)
vif(lsal)
vif(lsal2)
vif(lsal3)
vif(lsal4)
vif(lsal5)
vif(lsal6)

AIC(lsal)
AIC(lsal2)
AIC(lsal3)
AIC(lsal4)
AIC(lsal5)
AIC(lsal6)


#Critère BIC
#le critère BIC minimiser les residus, plus il est faible plus notre modèle peut minimiser les erreurs
BIC(lsal)
BIC(lsal2)
BIC(lsal3)
BIC(lsal4)
BIC(lsal5)
BIC(lsal6)

#Question 11
library(stargazer)
stargazer(lsal6) #par défaut stargazer nous donnes des lignes de code qui nous permet de faire des tableaux sur la text
  #Les lignes de code afficher sur la console on les copies et on les colles sur la text
stargazer(lsal6, type = "text", title="Resultats des regressions pour les variables", out="table1.txt")
formtex <- stargazer(lsal6, lslal5, lsal3, lsal2, lsal, type="text", out="table2.txt")

#Question 12

#Normal Q - Q plot
plot(lsal5, main="residus") #on fais un graphique avec nos residus
plot(lsal5,2) #2 permet le qqplot

library(lmtest)
res<-resid(lsal5)
hist(res)
shapiro.test(res) #On rejtte lhypothese nulle de normalite des individus

library(normtest)
jb.norm.test(res) #meme que shapiro

#Question 13

#test de Breush-Pagan
bptest(lsal5)
#Rejet H0 car p value inf a 0.05, donc probleme d hetreroscadasticite

#test de white
library(skedastic)
white_lm(lsal5, interactions=TRUE)
#probleme d hetroscadiscité, rejet H0 au seuil de risque de 0.05

#Test de Golfeld Quandf : test d autocorrelation des erreurs
gqtest(lsal5) #Meme conclusion que precedemement

#Question 14
#ho : autocorrelation des erreurs
#???test de durbin-watson
dwtest(lsal5)
#test de breush godfrey
bgtest(lsal5, order=1) #order permet de detecter l autocorrelation a un ordre sup
#rejet H0, pas de probleme d autocorrelation des erreurs
bgtest(lsal5, order=2) 
bgtest(lsal5, order=3)
bgtest(lsal5, order=4) 

