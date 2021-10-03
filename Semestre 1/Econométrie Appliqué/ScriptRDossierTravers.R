library(car)
library(MASS)
library(psych)
library(EnvStats)
library(ggplot2)

data <- DataEmploi

str(data)
names(data[1])

summary(data)
describe(data)
colnames(data)


fenetre=par(mfrow=c(2,2))
attach(data)

#Faire hist pour tous (automatise)
for(k in 3: 26){
  hist(names(data[k]), main=names(data[k]))
}
data$Taux_Chomage
data[,4]
hist(data[,4],xlab="PRIX",main="Répartition des    prix    des    maisons", col="blue")
hist(Taux_Chomage, xlab="Superficie(en m2)",main="Répartition des superficies des maisons", col="green")

#Plot et cor A FAIRE

plot(data$Taux_Chomage~data$Impot_Autres)
abline(lm(data$Taux_Chomage~data$Impot_Autres))
cor(data$Taux_Chomage,data$Impot_Autres)

#Automatisation plot et cor pour tx de chomage
fenetre
for(k in 5: 26){
  plot(data$Taux_Chomage~data[,k], main=names(data[k]), ylab="Taux de chomage en %")
  abline(lm(data$Taux_Chomage~data[,k]))
  print("Correlation pour le taux de chomage et ")
  print(names(data[k]))
  print(cor(data$Taux_Chomage,data[,k]))
}

#Plot toute les variables
for(k in 3: 26){
  for(i in 3: 26){
    plot(data[,k]~data[,i], main=names(data[k]), ylab=names(data[k]), xlab=names(data[i]))
    abline(lm(data[,k]~data[,i]))
    print(names(data[k]))
    print("Correlation de :")
    print(names(data[i]))
    print(cor(data[,k],data[,i]))
  }
}
plot(data[4]~data[3])
ncol(data)

#BoxPlot et Outliers A Faire / Valeur Atypique

#Automatisation 
for(k in 3: 26){
  boxplot(data[,k], main=names(data[k]))
}


#Detection et Suppression Outliers 
order(data$Taux_Chomage)
rosnerTest(data$Taux_Chomage, k = 8, alpha = 0.05) #83eme variable = outliers
data<-data[-c(83),]

rosnerTest(data$Taux_Activité, k = 8, alpha = 0.05) #67 eme variable = outliers
data<-data[-c(67),]


#Automatisation rosnertEST
for(i in 3: 26){
  print("RosnerTest pour la colonne nommé :")
  print(names(data[i]))
  print(rosnerTest(data[,i], k = 10, alpha = 0.05))
}
data[,3]
data$Taux_Activité
data
#Tableau des corrélation 

cor(data[,-c(1,2,3,4)], use="complete.obs")

#Automatiser shapiro test

for(k in 3: 26){
  print("Shapiro test pour la colonne nommé :")
  print(names(data[k]))
  print(k)
  print(shapiro.test(data[,k]))
}

#Tableau correlation de Spearman

cor(data[,-c(1,2,3,4)], use="complete.obs", method=c("spearman"))


#Regression

reg<-lm(data$Taux_Chomage ~ data$Taux_Epargne + data$PIB_Hab + data$Taux_Rural + data$Agriculture + data$Impot_Benef + data$Impot_Autres + data$Impot_Travail + data$Impot_Paiement + data$Investissement_E + data$Emploi_Vul + data$Chef_Entreprise + data$Salarie + data$Emploi_Industrie + data$Emploi_Service + data$Emploi_Agriculture + data$Technologie_Exportation + data$Masse_Monétaire + data$Credit_Prive + data$Entreprise_Procedure + data$Entreprise_Temps)
summary(reg)

reg1<-lm(data$Taux_Chomage ~ data$Taux_Rural + data$Impot_Travail + data$Technologie_Exportation )
summary(reg1)

#Divisions de la base en regroupement de pays

View(data)
