data <- BddProjetF
library(car)
library(MASS)
library(psych)
library(factoextra)
library(FactoMineR)
library(lmtest)
library(stargazer)
library(EnvStats)
library(outliers)
library(gvlma)

## I- Introductuction

attach(data)
summary(data)
describe(data)

# Suppression des outliers
plot(data$Taux_Imposition, xlab="Pays", ylab="Taux d'Imposition sur les bénéfices commerciaux en %")
order(data$Taux_Imposition)
data<-data[-c(35,5),]


## II- Analyse détaillée du sujet
## A- LA variable 


#Graphiques des differentes variables
describe(Taux_Epargne)
plot(data$Taux_Epargne, xlab="Pays", ylab="Taux d'épargne en %")
plot(data$Espérance_Vie, xlab="Pays", ylab="Espérance de vie en années")
plot(data$Taux_Imposition, xlab="Pays", ylab="Taux d'Imposition sur les bénéfices commerciaux en %")

order(data$Taux_Imposition)



order(data$Espérance_Vie)
View(data)

plot(data$PIB_hab, xlab="Pays", ylab="PIB par habitant en $US")
order(data$PIB_hab)

plot(data$Croissance_PIB, xlab="Pays", ylab="Croissance du PIB en %")
order(data$Croissance_PIB)

plot(data$Inflation, xlab="Pays", ylab="Inflation en %")
order(data$Inflation)

plot(data$Taux_Chomage, xlab="Pays", ylab="Taux de chomage en %")
order(data$Taux_Chomage)

plot(data$Taux_Mortalité, xlab="Pays", ylab="Taux de mortalité pour 1000 personnes")
order(data$Taux_Mortalité)

plot(data$Taux_Demographique, xlab="Pays", ylab="Taux démographique en %")
order(data$Taux_Demographique)

plot(data$Emploi_Vul_H, xlab="Pays", ylab="Taux d'emploi vulnérable homme en %")
plot(data$Emploi_Vul_F, xlab="Pays", ylab="Taux d'emploi vulnérable femme en %")


## C- Corrélation entre la variable illustrative et les variables explicatives
#Correlation variable explique et variables explicatives

plot(data$Taux_Epargne~data$Espérance_Vie, xlab="Esperance de Vie en années", ylab="Taux d'Epargne en % du PIB")
abline(lm(data$Taux_Epargne~data$Espérance_Vie), col="red")
cor(data$Taux_Epargne,data$Espérance_Vie)

plot(data$Taux_Epargne~data$Taux_Imposition, xlab="Taux d'Imposition des sociétés en % des bénéfices commerciaux", ylab="Taux d'Epargne en % du PIB")
abline(lm(data$Taux_Epargne~data$Taux_Imposition), col="red")
cor(data$Taux_Epargne,data$Taux_Imposition)

plot(data$Taux_Epargne~data$PIB_hab, xlab="PIB par habitants en $US", ylab="Taux d'Epargne en % du PIB")
abline(lm(data$Taux_Epargne~data$PIB_hab), col="red")
cor(data$Taux_Epargne,data$PIB_hab)

plot(data$Taux_Epargne~data$Croissance_PIB, xlab="Taux de croissance du PIB en %", ylab="Taux d'Epargne en % du PIB")
abline(lm(data$Taux_Epargne~data$Croissance_PIB), col="red")
cor(data$Taux_Epargne,data$Croissance_PIB)

plot(data$Taux_Epargne~data$Inflation, xlab="Taux d'inflation en %", ylab="Taux d'Epargne en % du PIB")
abline(lm(data$Taux_Epargne~data$Inflation), col="red")
cor(data$Taux_Epargne,data$Inflation)

plot(data$Taux_Epargne~data$Taux_Chomage, xlab="Taux de chômage en % de la population active", ylab="Taux d'Epargne en % du PIB")
abline(lm(data$Taux_Epargne~data$Taux_Chomage), col="red")
cor(data$Taux_Epargne,data$Taux_Chomage)

plot(data$Taux_Epargne~data$Taux_Mortalité, xlab="Taux de mortalité pour 1000 personnes", ylab="Taux d'Epargne en % du PIB")
abline(lm(data$Taux_Epargne~data$Taux_Mortalité), col="red")
cor(data$Taux_Epargne,data$Taux_Mortalité)

plot(data$Taux_Epargne~data$Taux_Demographique, xlab="Taux démographique (croissance de la population en %)", ylab="Taux d'Epargne en % du PIB")
abline(lm(data$Taux_Epargne~data$Taux_Demographique), col="red")
cor(data$Taux_Epargne,data$Taux_Demographique)

plot(data$Taux_Epargne~data$Emploi_Vul_F, xlab="Proportion d'emploi vulnérable femmes en % de l'emploi total des femmes", ylab="Taux d'Epargne en % du PIB")
abline(lm(data$Taux_Epargne~data$Emploi_Vul_F), col="red")
cor(data$Taux_Epargne,data$Emploi_Vul_F)

plot(data$Taux_Epargne~data$Emploi_Vul_H, xlab="Proportion d'emploi vulnérable fommes en % de l'emploi total des hommes", ylab="Taux d'Epargne en % du PIB")
abline(lm(data$Taux_Epargne~data$Emploi_Vul_H), col="red")
cor(data$Taux_Epargne,data$Emploi_Vul_H)

plot(data$Taux_Epargne~data$Part_14, xlab="Proportion de la population ayant 14 ans ou moins", ylab="Taux d'Epargne en % du PIB")
abline(lm(data$Taux_Epargne~data$Part_14), col="red")
cor(data$Taux_Epargne,data$Part_14)

plot(data$Taux_Epargne~data$Part_15_64, xlab="Proportion de la population ayant entre 15 et 64 ans", ylab="Taux d'Epargne en % du PIB")
abline(lm(data$Taux_Epargne~data$Part_15_64), col="red")
cor(data$Taux_Epargne,data$Part_15_64)

plot(data$Taux_Epargne~data$Part_65, xlab="Proportion de la population ayant plus de 65 ans", ylab="Taux d'Epargne en % du PIB")
abline(lm(data$Taux_Epargne~data$Part_65), col="red")
cor(data$Taux_Epargne,data$Part_65)
cor(data[,-1])
base<-BddProjetF

## III- ACP

res.pca=PCA(base[,2:14])
dim(base)
str(base)



fviz_pca_ind(res.pca)
fviz_pca_var(res.pca,axes=c(1,3))
## A- Valeurs propre et le nombre d'axes
res.pca$eig
round(res.pca$eig,2)
# ==> on garde3 axes car le cumul en % de la variance est 70
## B-
#. Variables
res.pca
#.Contribution
round(res.pca$var$contrib,2)

#.Corrélations
dimdesc(res.pca)

#.Cosinus
round(res.pca$var$cos2,2)

## C- Cercle de corrélation
#. Cercle de corrélation par rapport à l'axe 1 et 2
plot.PCA(res.pca,tilte="Graph des obs",xlim=c(-7,6),axes=c(1,2),choix="var",cex=.8)


fviz_pca_var(res.pca, col.var="contrib", c(1,2), title="Cercle de corrélation selon l'axe 1 & 2")+
  scale_color_gradient2(low="Green", mid="blue",
                        high="Red", midpoint=6, space ="Lab") + theme_minimal()




#. Cercle de corrélation par rapport à l'axe 2 et 3
plot.PCA(res.pca,tilte="Graph des obs",xlim=c(-7,6),axes=c(2,3),choix="var",cex=.8)

fviz_pca_var(res.pca, col.var="contrib", c(2,3), title="Cercle de corrélation selon l'axe 2 & 3")+
  scale_color_gradient2(low="Green", mid="blue",
                        high="red", midpoint=7.5, space ="Lab") + theme_minimal()


## D- Projection des individus sur le plan factoriel
plot(res.pca,title="50 pays les plus contributifs",select="contrib 45")
plot(res.pca,title="50 pays les plus contributifs",select="contrib 181")


fviz_pca_ind(res.pca,col.ind="contrib", c(1,2),select.ind = list(contrib = 40), title="Projection des 40 individus les plus contributifs") +
  scale_color_gradient2(low="black", mid="blue", high="red", midpoint=2, space ="Lab") + theme_minimal()

summary(base)
base<-data
## IV- Corrélation entre le variable illustrative et les variables latentes
## A - Régression linéaire multiples
#.Définir la variable active de la base

base.actifs=base[,2:14]
base.illus=base[,15]
#.Indiquer le nombre des lignes pour les variables actives
n=nrow(base.actifs)
print(n)
#. Renommer le fichier <<base.actifs>>
baseA = base.actifs

## ACP sur le fichier baseA

res.pca= PCA(baseA)

## Régression entre Taux_Epargne et les deux variables lattentes

model1 <- lm(base$Taux_Epargne~res.pca$ind$coord[,1]+res.pca$ind$coord[,2])
summary(model1)



model1a <- lm(base$Taux_Epargne~res.pca$ind$coord[,2]+res.pca$ind$coord[,3])
summary(model1a)


formatex <- stargazer(model1,model1a, type = "text", out="table")

## B- Relation entre le Taux d'épargne observé et prédit par "lm"
res.pca=PCA(base[,2:14])
lm <- lm(base$Taux_Epargne~res.pca$ind$coord[,1]+res.pca$ind$coord[,2])
summary(lm)
# Valeur du Taux d'epargne prédict par "lm"
base$predict=predict(lm)
print(base$predict)
# Les valeurs du Taux d'épargne
base$Taux_Epargne
print(base$Taux_Epargne)
# Graphique comparant le Taux d'epargne observé et celui prédit par "lm"

rownames(base)<-base$Pays
plot(base$predict,base$Taux_Epargne,col=0,main = "Taux-Epargne prédit par le modèle et le Taux_Epargne réel")
text(base$predict,base$Taux_Epargne,row.names(base),cex=.6)
abline(lm(base$Taux_Epargne~base$predict), col="red")

##C - Test de Normalité des résidus
residus <- residuals(model1)
#Detection et suppression outliers
boxplot(residus,horizontal =T,col = "blue",main= "Dispersion des résidus du modèle")
rosnerTest(residus, k = 8, alpha = 0.05) #83eme variable = outliers
grubbs.test(residus)
residus
order(residus)
#Evaluation globale de la qualité
gvlma(model1)
hist(residus,freq = T,main= "Histogramme de répartion des résidus",col ="blue")
skewness(residus)
kurtosis(residus)
qqnorm(residus)
qqline(residus)
ks.test (residus,"pnorm",mean(residus),sd(residus))
shapiro.test(residus)

## D- Régression logistique binaire
#. Codage de la variable Taux_Epargne Elele / Moyen / Faible
summary(base)
boxplot(base$Taux_Epargne)
summary(base$Taux_Epargne)
base$Taux_Epargne_Eleve[base$Taux_Epargne>28.619]="1"
base$Taux_Epargne_Eleve[base$Taux_Epargne<=28.619]="0"
base$Taux_Epargne_Moyen[base$Taux_Epargne<=28.619 & base$Taux_Epargne>=16.657]="1"
base$Taux_Epargne_Moyen[base$Taux_Epargne>28.619 | base$Taux_Epargne<16.657]="0"
base$Taux_Epargne_Faible[base$Taux_Epargne<=16.657]="1"
base$Taux_Epargne_Faible[base$Taux_Epargne>16.657]="0"
#. transformation de caractère de la série en numérics
base$Taux_Epargne_Eleve <- as.numeric(base$Taux_Epargne_Eleve)
base$Taux_Epargne_Moyen <- as.numeric(base$Taux_Epargne_Moyen)
base$Taux_Epargne_Faible <- as.numeric(base$Taux_Epargne_Faible)
#. on effectue l'analyse logit premier modele
model=lm(base$Taux_Epargne_Eleve~res.pca$ind$coord[,1]+res.pca$ind$coord[,2])
summary(model)
model=lm(base$Taux_Epargne_Moyen~res.pca$ind$coord[,1]+res.pca$ind$coord[,2])
summary(model)
model=lm(base$Taux_Epargne_Faible~res.pca$ind$coord[,1]+res.pca$ind$coord[,2])
summary(model)
#. on effectue l'analyse logit du second modele
model=lm(base$Taux_Epargne_Eleve~res.pca$ind$coord[,2]+res.pca$ind$coord[,3])
summary(model)
model=lm(base$Taux_Epargne_Moyen~res.pca$ind$coord[,2]+res.pca$ind$coord[,3])
summary(model)
model=lm(base$Taux_Epargne_Faible~res.pca$ind$coord[,2]+res.pca$ind$coord[,3])
summary(model)


