#question 1

completset<-read.csv2("C:/Users/delch/Documents/M1/IntroR/base excel/completset1.csv")
View(completset)
#question 2
completsetpira<-factor(completset$pira,levels=c(0,1))
table(completsetpira)
tab<-table(completset$gender)
tab
prop.table(tab,1)

#question 3
summary(completset)
hist(completset$age,ylim=c(0,3500),xlim=c(20,70),breaks=5)
#surreprésentation des 30-40 ans au nv des épargne alors que les 60-70 n'épargne plus sur un compte épargne retraite

#question 4
completset$femme<-as.factor(completset$femme)
completset$homme<-as.factor(completset$homme)
summary(completset$inc)
#le revenu médian est de 33,29K$ par an mais en moyenne il dispose de 39K$ par an


#question 5
table(completset$fsize)
#il y a 2017 ménages d'une personne

#question 6
onepers<-completset[!completset$fsize>1,] #ca va effacer tte la ligne 
str(onepers)

#question 7
mco<-lm(nettfa~inc+age+homme+femme,data=onepers)
summary(mco)
#avec une médianne de -3,36, ca ne va pas suivre une loi normale, ca devrait être proche de 0
#NA pour les femmes pcq il y a un pb de colinéarité donc il a choisit de sup la variables femmes

#question 8
mco1<-lm(nettfa~inc+age+homme,data=onepers)
summary(mco1)
#c'est le même modèle qu'au dessus pca R avait deja fait le travail sur le modèle de dessus
#inc et age ont une p-value<0,05, il y a donc une relation entre nette fa avec age et inc
#le coeff a estimé de inc, lorsque la valeur de inc aumente d'une unité (1000$), tte choses égales par ailleurs, le revenu patrimoniale augmente de 793$

#question 9
mco2<-lm(nettfa~inc+age,data=onepers)
summary(mco2)

#question 10
plot(onepers$age,onepers$inc,xlab="age",ylab="inc",main="nuage de points entre age et inc")
#tres homogéne, pas de relation entre inc et age

#question 11
cor.test(onepers$inc,onepers$age)
#coeff de corrélation tres faible et pas significatif au seuil de 5% mais a 10%

#question 12
mco4<-lm(nettfa~inc,data=onepers)
summary(mco4)
#inc a augmenté un peu, en RLS il est de 0,82 alors qu'avec l'age il est de 0,79, il y a bien un biais positif, mais extremement faible
