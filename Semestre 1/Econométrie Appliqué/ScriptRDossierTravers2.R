#--- Intro -----
library(car)
library(MASS)
library(psych)
library(EnvStats)
library(ggplot2)
library(dplyr)
library(outliers)
library(lmtest)
library(AER)
library(stargazer)
library(factoextra)
library(FactoMineR)
library(missMDA)
library(readxl)
DataEmploi <- read_excel("M1 EKAP/Semestre 1/Econométrie Appliqué/DataEmploi.xlsx")
data <- DataEmploi
#On regarde le nombre de NA
summary(data)
#Taux syndic et taux couverture : 130 et 137 / 3 variables Loi : 75 / Tech : 49 / Masse Monétaire : 35 / Taux_Epargne : 27

#--- Fais les moyennes, medianne, ecart type etc ----
describe(data)

#--- Outliers en fonction du PIB ------
par(mfrow=c(1,1))
boxplot(data$PIB_Hab) #Doute sur quelques variables
rosnerTest(data$PIB_Hab, k = 8, alpha = 0.05) #3 Outliers
data<-data[-c(95,149,132),]

#--- Mise en place des variables quantitatives ----
str(data)
data$Conv_Prio_122<-as.factor(data$Conv_Prio_122)
data$Conv_Prio_129<-as.factor(data$Conv_Prio_129)
data$Conv_Syndic_087<-as.factor(data$Conv_Syndic_087)
data$Conv_Syndic_098<-as.factor(data$Conv_Syndic_098)
data$Conv_TravEnfant182<-as.factor(data$Conv_TravEnfant182)
data$Conv_TravEnfant_138<-as.factor(data$Conv_TravEnfant_138)
data$Conv_TravForce_029<-as.factor(data$Conv_TravForce_029)
data$Conv_TravForce_105<-as.factor(data$Conv_TravForce_105)
str(data)

#--- Creation des base "pays en developpement" et "pays developpé" et loi -----
PaysDev<-subset(data, data$PIB_Hab>=mean(data$PIB_Hab))
PaysEnDev<-subset(data, data$PIB_Hab<mean(data$PIB_Hab))
summary(PaysEnDev)
summary(PaysDev)
PaysLoi <- subset(data, !is.na(data$Loi_Indemnite)) #Permet de travailler avec les coefficients fournis par le OIT
summary(PaysLoi)
#--- Creation des data en fonction du territoire et du type d'économie ----
Afrique <- data[c(2,4,6,20,24,28,29,30,32,40,41,42,45,48,49,57,55),]
MoyenOrient <- data[c(1,7,9,12,14,38,51),]
Europe <- data[c(4,5,11,17,18,23,27,37,46,47,53,54),]
Asie <- data[c(1,7,12,14,15,21,26,31,35,36,38,43),]
AmeriqueOceanie <- data[c(8,10,13,16,19,22,25,33,34,39,44,50,52,56),]
EconomieAncienne <- data[c(5,10,11,18,33,36,37,47,53,54,56),]
EconomieJeune <- data[c(4,7,8,13,14,16,19,22,25,26,27,34,35,39,43,44,46,52),]
EconomieFragile <- data[c(2,3,1,6,9,15,17,20,21,23,24,28,29,30,32,31,38,40,42,41,45,48,49,50,57),]
Petrolier <- data[c(4,6,7,8,10,12,14,17,25,26,33,35,39,41,49,51,55,),]
PaysService <- subset(data, data$Emploi_Service>=60)
PaysAgricole <- subset(data, data$Emploi_Agriculture>=37)
PaysIndustrie <- subset(data, data$Emploi_Agriculture>20)


#--- ACP sur totalité data -----
nb = estim_ncpPCA(data[,4:23],ncp.max=5) #estime le nombre de dimension necessaire
res.comp = imputePCA(data[,4:23],ncp=2) #COmplete le jeu de donnée (na remplace par les valeurs estimes) 
res.pca=PCA(res.comp)#acp
fviz_pca_ind(res.pca)
fviz_pca_var(res.pca,axes=c(1,3))
#Valeurs propre et le nombre d'axes
res.pca$eig
round(res.pca$eig,2)
# on garde 3 axe car le cumul en % de la variance est de 78%
# Variables
res.pca
#Contribution
round(res.pca$var$contrib,2)
#Corrélations
dimdesc(res.pca)
#Cosinus
round(res.pca$var$cos2,2)

#Cercle de corrélation
plot.PCA(res.pca,tilte="Graph des obs",xlim=c(-7,6),axes=c(1,2),choix="var",cex=.8)
fviz_pca_var(res.pca, col.var="contrib", c(1,2), title="Cercle de corrélation selon l'axe 1 & 2")+
  scale_color_gradient2(low="Grey", mid="blue",
                        high="Red", midpoint=6, space ="Lab") + theme_minimal()


#--- DATA PaysDev -----

summary(PaysDev) #On ne va pas utiliser Masse Monétaire ; Taux Interet ; Taux_Syndic ; Taux_Couverture Taux Epargne | Coeff_Salaire | Loi car trop de NA
#On verra pour l'utilisation des 3 variables lois

#------ Test des Outliers et BoxPlot  ----
par(mfrow=c(1,1))
boxplot(PaysDev$Taux_Chomage)
rosnerTest(PaysDev$Taux_Chomage, k = 8, alpha = 0.05)

PaysDev<-PaysDev[-c(26,20,42),]
par(mfrow=c(1,1))
for(k in 3: 23){
  boxplot(PaysDev[,k], main=names(PaysDev[k]))
}
for(k in 32: 37){
  boxplot(PaysDev[,k], main=names(PaysDev[k]))
}
par(mfrow=c(1,2))
boxplot(PaysDev$Taux_Chomage, main="BoxPlot Taux Chomage")
boxplot(PaysDev$Investissement_E, main="BoxPlot Investissement Education")
boxplot(PaysDev$Impot_Paiement, main="BoxPlot Impot Paiement")
boxplot(PaysDev$Impot_Autres, main="BoxPlot Impot Autres")
boxplot(PaysDev$Technologie_Exportation,  main="BoxPlot Technologie Exportation")
boxplot(PaysDev$Taux_Epargne, main="BoxPlot Taux Epargne")
#On voit apparaitre des possibles valeurs atypique pour : Taux-Epargne ; Impot_Autre ; Agriculture ; Impot_Travail 
# Emploi_Indus ; Emploi_Agriculture ; Technologie_Exportation ; Coef_Salaire ; les 3 lois
#Rosner Test et Grubbs
summary(PaysDev)
order(PaysDev$Taux_Epargne)#na : 55, 46, 25, 19
order(PaysDev$Investissement_E)#na : 46
order(PaysDev$Technologie_Exportation)#na : 6 55 46 43 25
order(PaysDev$Taux_Jeune_DescBIT)#na : 37 30
PaysDevTech <- PaysDev[-c(55,19,46,6,43,25,37,30),] #Suppression des lignes contenant des NA
summary(PaysDevTech)
PaysDevTech <- PaysDevTech[,-c(7,19,20,22,23,32,33,34,35,36)] #On supprime les variables que l'on ne va pas utliser

rosnerTest(PaysDevTech$Taux_Epargne, k = 8, alpha = 0.05) # 1 Outlier : n 8
grubbs.test(PaysDevTech$Taux_Epargne,type=10, two.sided = TRUE)

rosnerTest(PaysDevTech$Impot_Autres, k = 8, alpha = 0.05) # 4 outliers : n 3 / 22 / 15 / 19
rosnerTest(PaysDevTech$Impot_Benef, k = 8, alpha = 0.05) # 
grubbs.test(PaysDevTech$Impot_Benef,type=10, two.sided = TRUE) #1 outlier 36
rosnerTest(PaysDevTech$Impot_Travail, k = 8, alpha = 0.05) # 0 outlier : 
grubbs.test(PaysDevTech$Impot_Travail,type=10, two.sided = TRUE)#pas d outlier
rosnerTest(PaysDevTech$Impot_Paiement, k = 8, alpha = 0.05) # 6 outlier : 26 13 25 49 27 47

rosnerTest(PaysDevTech$Investissement_E, k = 10, alpha = 0.05) # 8 Outliers : 23 38 12 13 24 47 7 5
rosnerTest(PaysDevTech$Salarie, k = 10, alpha = 0.05) 
grubbs.test(PaysDevTech$Salarie,type=10, two.sided = TRUE) #0  outlier

rosnerTest(PaysDevTech$Emploi_Industrie, k = 8, alpha = 0.05) # 0 Outlier
grubbs.test(PaysDevTech$Emploi_Industrie,type=10, two.sided = TRUE)
rosnerTest(PaysDevTech$Emploi_Service, k = 8, alpha = 0.05) 
grubbs.test(PaysDevTech$Emploi_Service,type=10, two.sided = TRUE) # 0  outlie
rosnerTest(PaysDevTech$Emploi_Agriculture, k = 8, alpha = 0.05) # 4 Outliers : 43 48 28

rosnerTest(PaysDevTech$Technologie_Exportation, k = 8, alpha = 0.05) # 2 Outliers : 12 / 32
rosnerTest(PaysDevTech$Taux_Rural, k = 8, alpha = 0.05) # 0 outlier
grubbs.test(PaysDevTech$Taux_Rural,type=10, two.sided = TRUE)

rosnerTest(PaysDevTech$Taux_Jeune_DescBIT, k = 8, alpha = 0.05) # 0 outlier
grubbs.test(PaysDevTech$Taux_Jeune_DescBIT,type=10, two.sided = TRUE)
rosnerTest(PaysDevTech$Entreprise_Procedure, k = 8, alpha = 0.05) # 0 outlier
grubbs.test(PaysDevTech$Entreprise_Procedure,type=10, two.sided = TRUE)
rosnerTest(PaysDevTech$Emploi_Vul, k = 8, alpha = 0.05) # 0 outlier


#En vue de travailler avec un maximum de variables et d observations nous allons proceder a la creation de 2 bnvx dataframes afin de travailler dun cote avec investissement education et de l autre les impots et nous ne travaillerons pas avec emploi agriculture ni impot paiement
PaysDevTech<-PaysDevTech[,-c(16,9)]
PaysDevTechImpot<-PaysDevTech[-c(12,32,8,3,22,15,19),]
PaysDevTechImpot<-PaysDevTechImpot[,-c(10)]
PaysDevTechEduc<-PaysDevTech[-c(12,32,8,23,38,12,13,24,47,7,5),]
PaysDevTechEduc<-PaysDevTechEduc[,-c(7,8,9)]

#------ Plot ----
par(mfrow=c(2,2))
plot(PaysDevTechImpot$Taux_Chomage~PaysDevTechImpot$Salarie, main="Taux chomage ~ Salarie")
abline(lm(PaysDevTechImpot$Taux_Chomage~PaysDevTechImpot$Salarie))
plot(PaysDevTechImpot$Taux_Chomage~PaysDevTechImpot$Impot_Benef, main="Taux chomage ~ Impot Benef")
abline(lm(PaysDevTechImpot$Taux_Chomage~PaysDevTechImpot$Impot_Benef))
plot(PaysDevTechImpot$Taux_Chomage~PaysDevTechImpot$Impot_Travail,main="Taux chomage ~ Impot Travail")
abline(lm(PaysDevTechImpot$Taux_Chomage~PaysDevTechImpot$Impot_Travail))
plot(PaysDevTechImpot$Taux_Chomage~PaysDevTechImpot$Taux_Rural, main="Taux chomage ~ Taux Rural")
abline(lm(PaysDevTechImpot$Taux_Chomage~PaysDevTechImpot$Taux_Rural))

plot(PaysDevTechEduc$Taux_Chomage~PaysDevTechEduc$Taux_Rural, main="Taux chomage ~ Taux Rural")
abline(lm(PaysDevTechEduc$Taux_Chomage~PaysDevTechEduc$Taux_Rural))
plot(PaysDevTechEduc$Taux_Chomage~PaysDevTechEduc$Taux_Jeune_DescBIT, main="Taux chomage ~ Taux chomage \n jeune")
abline(lm(PaysDevTechEduc$Taux_Chomage~PaysDevTechEduc$Taux_Jeune_DescBIT))
plot(PaysDevTechEduc$Taux_Chomage~PaysDevTechEduc$Entreprise_Procedure, main="Taux chomage ~ Entreprise \n Procedure")
abline(lm(PaysDevTechEduc$Taux_Chomage~PaysDevTechEduc$Entreprise_Procedure))
plot(PaysDevTechEduc$Taux_Chomage~PaysDevTechEduc$PIB_Hab, main="Taux chomage ~ PIB Habitants")
abline(lm(PaysDevTechEduc$Taux_Chomage~PaysDevTechEduc$PIB_Hab))

plot(PaysDevTechImpot$Taux_Chomage~PaysDevTechImpot$Emploi_Industrie, main="Emploi Industrie,\n data Impot")
abline(lm(PaysDevTechImpot$Taux_Chomage~PaysDevTechImpot$Technologie_Exportation))
plot(PaysDevTechEduc$Taux_Chomage~PaysDevTechEduc$Emploi_Industrie, main="Emploi Industrie,\n data Educ")
abline(lm(PaysDevTechEduc$Taux_Chomage~PaysDevTechEduc$Emploi_Industrie))
plot(PaysDevTechImpot$Taux_Chomage~PaysDevTechImpot$Technologie_Exportation, main="Technologie Exportation,\n data Impot")
abline(lm(PaysDevTechImpot$Taux_Chomage~PaysDevTechImpot$Technologie_Exportation))
plot(PaysDevTechEduc$Taux_Chomage~PaysDevTechEduc$Technologie_Exportation, main="Technologie Exportation,\n data Educ")
abline(lm(PaysDevTechEduc$Taux_Chomage~PaysDevTechEduc$Technologie_Exportation))




#------ ACP sur PaysDev -----
nb = estim_ncpPCA(PaysDev[,4:23],ncp.max=5)
res.comp = imputePCA(PaysDev[,4:23],ncp=1)
res.pca=PCA(res.comp)
fviz_pca_ind(res.pca)
fviz_pca_var(res.pca,axes=c(1,3))
#Valeurs propre et le nombre d'axes
res.pca$eig
round(res.pca$eig,2)
# on garde 2 axe car le cumul en % de la variance est de 77.37%
# Variables
res.pca
#Contribution
round(res.pca$var$contrib,2)
#Corrélations
dimdesc(res.pca)
#Cosinus
round(res.pca$var$cos2,2)
#  Cercle de corrélation
plot.PCA(res.pca,tilte="Graph des obs",xlim=c(-7,6),axes=c(1,2),choix="var",cex=.8)
fviz_pca_var(res.pca, col.var="contrib", c(1,2), title="Cercle de corrélation selon l'axe 1 & 2")+
  scale_color_gradient2(low="grey", mid="blue",
                        high="Red", midpoint=6, space ="Lab") + theme_minimal()
plot.PCA(res.pca,tilte="Graph des obs",xlim=c(-7,6),axes=c(1,2),choix="var",cex=.8)
fviz_pca_var(res.pca, col.var="contrib", c(1,3), title="Cercle de corrélation selon l'axe 1 & 3")+
  scale_color_gradient2(low="grey", mid="blue",
                        high="Red", midpoint=6, space ="Lab") + theme_minimal()


#------ Moindres Carrés ------
#---------- Normalité des résidus : Shapiro et Corrélation -----
#-------------- DATA EDUC -----------
stargazer(cor(PaysDevTechEduc[,-c(1,2,14,15,16,17,18,19,20,21)],method="spearman"),type="text",out="CorrPaysDevImpot.txt")
stargazer(cor(PaysDevTechEduc[,-c(1,2,14,15,16,17,18,19,20,21)],use="complete.obs"),type="text",out="CorrPaysDevImpot2.txt")
stargazer(cor(PaysDevTechEduc[,-c(1,2,3,4,7,12,13,14,15,16,17,18,19,20,21,22)],method="spearman"),type="text",out="CorrPaysDevImpot.txt")

shapiro.test(PaysDevTechEduc$Taux_Rural)#rejet H0 : p-value : 0.0344 donc elle ne suit pas une loi normale au seuil de risque 5%
shapiro.test(PaysDevTechEduc$Investissement_E)#accepte H0
shapiro.test(PaysDevTechEduc$Salarie)#accepte H0
shapiro.test(PaysDevTechEduc$Emploi_Service)#accepte H0
shapiro.test(PaysDevTechEduc$Technologie_Exportation)#accepte H0
shapiro.test(PaysDevTechEduc$Taux_Epargne)#accepte H0
shapiro.test(PaysDevTechEduc$PIB_Hab)#accepte H0
shapiro.test(PaysDevTechEduc$Taux_Rural)#rejet H0 : p-value : 0.0344 donc elle ne suit pas une loi normale au seuil de risque 5%

shapiro.test(PaysDevTechEduc$Emploi_Vul)#refuse H0
shapiro.test(PaysDevTechEduc$Emploi_Industrie)#refuse H0
shapiro.test(PaysDevTechEduc$Entreprise_Procedure)#accepte H0 au seuil de risque 5%
shapiro.test(PaysDevTechEduc$Taux_Jeune_DescBIT)#accepte H0


#-------------- DATA IMPOT ------------
stargazer(cor(PaysDevTechImpot[,-c(1,2,16,17,18,19,20,21,22,23)],method="spearman"),type="text",out="CorrPaysDevImpot.txt")
stargazer(cor(PaysDevTechImpot[,-c(1,2,16,17,18,19,20,21,22,23)],use="complete.obs"),type="text",out="CorrPaysDevImpot2.txt")
stargazer(cor(PaysDevTechImpot[,-c(1,2,3,7,9,14,15,16,17,18,19,20,21,22,23,24)],method="spearman"),type="text",out="CorrPaysDevImpot2.txt")

shapiro.test(PaysDevTechImpot$Taux_Rural)#accepte H0  donc elle  suit une loi normale au seuil de risque 5%
shapiro.test(PaysDevTechImpot$Salarie)#acccepte H0 donc elle suit une loi normale au seuil de risque 5%
shapiro.test(PaysDevTechImpot$Emploi_Service)#accepte H0,  donc elle suit une loi normale au seuil de risque de 5%
shapiro.test(PaysDevTechImpot$Impot_Benef)#on accepte H0 donc elle suit une loi normale au seuil de risque de 5%
shapiro.test(PaysDevTechImpot$Impot_Travail)#accepte H0 donc elle suit une loi normale au seuil de risque 5%
shapiro.test((PaysDevTechImpot$Impot_Autres))#on accepte H0 au seuil de risque 5%
shapiro.test(PaysDevTechImpot$Taux_Epargne)#on accepte H0 
shapiro.test(PaysDevTechImpot$PIB_Hab)#On accepte H0 au seuil de risque 5%
shapiro.test(PaysDevTechImpot$Emploi_Vul)#on rejette H0 
shapiro.test(PaysDevTechImpot$Emploi_Industrie)#on accepte H0 au seuil de risque 5%
shapiro.test(PaysDevTechImpot$Entreprise_Procedure)#on accepte H0 
shapiro.test(PaysDevTechImpot$Taux_Jeune_DescBIT)#on accepte H0
shapiro.test(PaysDevTechImpot$Technologie_Exportation)#On accepte H0




#---------- Corrélation des variables explicatives et reduction de la base avec les variables que l'ont veux utliser -------
cor(PaysDevTechImpot[,-c(1,2,3,2,16,17,18,19,20,21,22,23)],use="complete.obs")
cor(PaysDevTechImpot[,-c(1,2,3,2,16,17,18,19,20,21,22,23)],method=c("spearman")) 
#On remarque une correlation entre emploi vul et salarie (0.98), or emploi vul ne suit pas une loi normale0
summary(PaysDevTechImpot)
cor(PaysDevTechEduc[,-c(1,2,3,14,15,16,17,18,19,20,21)],use="complete.obs") #Correlation de (emploi industrie, pib hab, taux rural) avec emploi service / salarie et emploi vul / 
cor(PaysDevTechEduc[,-c(1,2,3,14,15,16,17,18,19,20,21)],method=c("spearman"))

#---------- Modèle PaysDev -----
#-------------- DATA IMPOT ---------
reg1 <- lm(PaysDevTechImpot$Taux_Chomage ~
    PaysDevTechImpot$Salarie +
    PaysDevTechImpot$Impot_Benef +
    PaysDevTechImpot$Taux_Rural +
    PaysDevTechImpot$Emploi_Industrie +
    PaysDevTechImpot$Technologie_Exportation)
summary(reg1)

reg2 <- lm(PaysDevTechImpot$Taux_Chomage ~
            PaysDevTechImpot$Impot_Travail  + 
            PaysDevTechImpot$Salarie + 
            PaysDevTechImpot$Taux_Rural +
            PaysDevTechImpot$Emploi_Industrie +  
            PaysDevTechImpot$Technologie_Exportation )
summary(reg2)


#-------------------------- Modele : Reg2 toutes les variables sont exogènes car les variables explicatives suivent une lois normales et ne sont pas correles entre elles ----------
#Test RAMSEY
reset(reg2, data=PaysDevTech) #Dapres la p value , on accepte H0 donc la forme fonctionnelle linéaire du modèle est  la bonne

#VIF
vif(reg2) #vif correct puisque <10 donc il n'y pâs de risque de multicolinéarité

#Test de Breush-Pagan
bptest(reg2) #on refuse H0 au seuil de risque 5% : pb d heteroscedascité
shapiro.test(residuals(reg2))#Les residus suivent une loi normale

#Detection de la source d hetero
residualPlots(reg2)#Taux Rural est la source on utlisera la correctrion de white sinon
reg2 <- lm(log(PaysDevTechImpot$Taux_Chomage) ~
             PaysDevTechImpot$Impot_Travail  + 
             PaysDevTechImpot$Salarie + 
             PaysDevTechImpot$Taux_Rural +
             PaysDevTechImpot$Emploi_Industrie +  
             PaysDevTechImpot$Technologie_Exportation )
residus2=resid(reg2)
regLnresid2<-lm(log(residus2^2)~PaysDevTechImpot$Taux_Rural)
expo2=exp(fitted(regLnresid2))
regFGLS2 <- lm(PaysDevTechImpot$Taux_Chomage ~
                PaysDevTechImpot$Impot_Travail  + 
                PaysDevTechImpot$Salarie + 
                PaysDevTechImpot$Taux_Rural +
                PaysDevTechImpot$Emploi_Industrie +  
                PaysDevTechImpot$Technologie_Exportation,weights=1/expo2 )
summary(regFGLS2) #On garde ce modele : plus petit ecart type etplus grand R²
summary(reg2)
stargazer(reg2,regFGLS2,type="text",out="MCQGreg2.txt")

#Endogeneite, verification sur salarie  et taux rural
reg2_DMC<-ivreg(formula = PaysDevTechImpot$Taux_Chomage ~
                  PaysDevTechImpot$Impot_Travail  + 
                  PaysDevTechImpot$Salarie +   
                  PaysDevTechImpot$Taux_Rural +
                  PaysDevTechImpot$Emploi_Industrie + 
                  PaysDevTechImpot$Technologie_Exportation  | 
                  PaysDevTechImpot$Emploi_Industrie + 
                  PaysDevTechImpot$Impot_Travail  +  
                  PaysDevTechImpot$Technologie_Exportation + PaysDevTechImpot$Emploi_Vul + PaysDevTechImpot$Taux_Jeune_DescBIT + PaysDevTechImpot$PIB_Hab + PaysDevTechImpot$Emploi_Service, weights=1/expo2 )
summary(reg2_DMC,vcov=sandwich, diagnostics = TRUE)

#Test avec emploi industrie
reg2_DMC<-ivreg(formula = PaysDevTechImpot$Taux_Chomage ~
                  PaysDevTechImpot$Impot_Travail  + 
                  PaysDevTechImpot$Salarie +   
                  PaysDevTechImpot$Taux_Rural +
                  PaysDevTechImpot$Emploi_Industrie + 
                  PaysDevTechImpot$Technologie_Exportation  | 
                  PaysDevTechImpot$Impot_Travail  +  
                  PaysDevTechImpot$Technologie_Exportation + PaysDevTechImpot$Emploi_Vul + PaysDevTechImpot$Taux_Jeune_DescBIT + PaysDevTechImpot$Impot_Benef + PaysDevTechImpot$Emploi_Service, weights=1/expo2 )
summary(reg2_DMC,vcov=sandwich, diagnostics = TRUE)
#On garde le modele reg2 


#-------------------------- Modèle : Reg1, on peut supposer une endo avec salarie ou emploi indus ou taux rural -----
#Test RAMSEY
reset(reg1, data=PaysDevTech)#D'apres la p_value : 0.72, au seuil de risque 5% on accepte H0 donc la forme fonctionnelle linéaire pour ce modèle est la bonne

#VIF
vif(reg1) #Pas de problkeme de multicolinéarité

#Test de Breush-Pagan
bptest(reg1)#P;Value = 0.324, donc on accepte HHHH0 au seuil de risque 5% donc l'hypothsese d'homoscedasticité est vérifiée

#Detection de la source d hetero
residualPlots(reg1)#Taux_rural est la source
reg1 <- lm(PaysDevTechImpot$Taux_Chomage ~
             PaysDevTechImpot$Impot_Benef  + 
             PaysDevTechImpot$Salarie + 
             PaysDevTechImpot$Taux_Rural +
             PaysDevTechImpot$Emploi_Industrie +  
             PaysDevTechImpot$Technologie_Exportation )
residus1=resid(reg1)
regLnresid1<-lm(log(residus1^2)~PaysDevTechImpot$Taux_Rural)
expo1=exp(fitted(regLnresid1))
regFGLS1 <- lm(PaysDevTechImpot$Taux_Chomage ~
                 PaysDevTechImpot$Impot_Benef  + 
                 PaysDevTechImpot$Salarie + 
                 PaysDevTechImpot$Taux_Rural +
                 PaysDevTechImpot$Emploi_Industrie +  
                 PaysDevTechImpot$Technologie_Exportation,weights=1/expo1 )
summary(regFGLS1)#On garde ce modele ecart type plus reduit
summary(reg1)

#Endogénéité : verification sur salarie et emploi indus et taux rural
reg1_DMC<-ivreg(formula = PaysDevTechImpot$Taux_Chomage ~
                  PaysDevTechImpot$Impot_Benef  + 
                  PaysDevTechImpot$Salarie +   
                  PaysDevTechImpot$Taux_Rural +
                  PaysDevTechImpot$Emploi_Industrie + 
                  PaysDevTechImpot$Technologie_Exportation  | 
                  PaysDevTechImpot$Impot_Benef  +  
                  PaysDevTechImpot$Technologie_Exportation + PaysDevTechImpot$Emploi_Vul + PaysDevTechImpot$Taux_Jeune_DescBIT + PaysDevTechImpot$Impot_Travail + PaysDevTechImpot$Emploi_Service)
summary(reg1_DMC,vcov=sandwich, diagnostics = TRUE)
#On garde le modele precedent 
summary(reg1)

#-------------- DATA EDUC --------
summary(PaysDevTechEduc)
regE1 <- lm(PaysDevTechEduc$Taux_Chomage ~
             PaysDevTechEduc$Investissement_E  +  PaysDevTechEduc$Taux_Jeune_DescBIT +
             PaysDevTechEduc$Salarie + PaysDevTechEduc$Entreprise_Procedure +
             PaysDevTechEduc$PIB_Hab + PaysDevTechEduc$Taux_Rural + 
             PaysDevTechEduc$Emploi_Industrie + PaysDevTechEduc$Technologie_Exportation +
             PaysDevTechEduc$Conv_Prio_122 + PaysDevTechEduc$Conv_Prio_129 +
             PaysDevTechEduc$Conv_Syndic_087 + PaysDevTechEduc$Conv_Syndic_098 +
             PaysDevTechEduc$Conv_TravEnfant_138 +
             PaysDevTechEduc$Conv_TravForce_105)
summary(regE1)

regE2 <- lm(PaysDevTechEduc$Taux_Chomage ~
              PaysDevTechEduc$Taux_Jeune_DescBIT +
              PaysDevTechEduc$Entreprise_Procedure +
              PaysDevTechEduc$PIB_Hab + 
              PaysDevTechEduc$Emploi_Industrie + PaysDevTechEduc$Technologie_Exportation +
              PaysDevTechEduc$Conv_Prio_122 + PaysDevTechEduc$Conv_Prio_129 +
              PaysDevTechEduc$Conv_TravEnfant_138 +
              PaysDevTechEduc$Conv_TravForce_105)
summary(regE2)

regE3 <- lm(PaysDevTechEduc$Taux_Chomage ~
              PaysDevTechEduc$Entreprise_Procedure +
              PaysDevTechEduc$PIB_Hab + 
              PaysDevTechEduc$Emploi_Industrie + PaysDevTechEduc$Technologie_Exportation +
              PaysDevTechEduc$Conv_Prio_122 + 
              PaysDevTechEduc$Conv_TravForce_105)
summary(regE3)

regE4 <- lm(PaysDevTechEduc$Taux_Chomage ~
              PaysDevTechEduc$Entreprise_Procedure +
              PaysDevTechEduc$PIB_Hab + 
              PaysDevTechEduc$Emploi_Service + PaysDevTechEduc$Technologie_Exportation +
              PaysDevTechEduc$Conv_Prio_122 + 
              PaysDevTechEduc$Conv_TravForce_105)
summary(regE4)
#-------------------------- Modèle : RegE2, on peut supposer une endo avec Taux jeune descbit ou emploi indus ou pib hab ---------
#Test RAMSEY
reset(regE2, data=PaysDevTechEduc)#D'apres la p_value : 0.8147, au seuil de risque 5% on accepte H0 donc la forme fonctionnelle linéaire pour ce modèle est la bonne

#VIF
vif(regE2) #Pas de problkeme de multicolinéarité

#Test de Breush-Pagan
bptest(regE2)#P;Value = 0.7269, donc on accepte HHHH0 au seuil de risque 5% donc l'hypothsese d'homoscedasticité est vérifiée

#Detection de la source d hetero
residualPlots(regE2)#On ne connait pas la source d heteroscedaticite 
ncvTest(lm(PaysDevTechEduc$Taux_Chomage ~
             PaysDevTechEduc$Taux_Jeune_DescBIT +
             PaysDevTechEduc$Entreprise_Procedure +
             PaysDevTechEduc$PIB_Hab + 
             PaysDevTechEduc$Emploi_Industrie + PaysDevTechEduc$Technologie_Exportation +
             PaysDevTechEduc$Conv_Prio_122 + PaysDevTechEduc$Conv_Prio_129 +
             PaysDevTechEduc$Conv_TravEnfant_138 +
             PaysDevTechEduc$Conv_TravForce_105),~PaysDevTechEduc$Conv_Prio_122)
ncvTest(lm(PaysDevTechEduc$Taux_Chomage ~
             PaysDevTechEduc$Taux_Jeune_DescBIT +
             PaysDevTechEduc$Entreprise_Procedure +
             PaysDevTechEduc$PIB_Hab + 
             PaysDevTechEduc$Emploi_Industrie + PaysDevTechEduc$Technologie_Exportation +
             PaysDevTechEduc$Conv_Prio_122 + PaysDevTechEduc$Conv_Prio_129 +
             PaysDevTechEduc$Conv_TravEnfant_138 +
             PaysDevTechEduc$Conv_TravForce_105),~PaysDevTechEduc$Conv_Prio_129)

ncvTest(lm(PaysDevTechEduc$Taux_Chomage ~
             PaysDevTechEduc$Taux_Jeune_DescBIT +
             PaysDevTechEduc$Entreprise_Procedure +
             PaysDevTechEduc$PIB_Hab + 
             PaysDevTechEduc$Emploi_Industrie + PaysDevTechEduc$Technologie_Exportation +
             PaysDevTechEduc$Conv_Prio_122 + PaysDevTechEduc$Conv_Prio_129 +
             PaysDevTechEduc$Conv_TravEnfant_138 +
             PaysDevTechEduc$Conv_TravForce_105),~PaysDevTechEduc$Conv_TravEnfant_138)

ncvTest(lm(PaysDevTechEduc$Taux_Chomage ~
             PaysDevTechEduc$Taux_Jeune_DescBIT +
             PaysDevTechEduc$Entreprise_Procedure +
             PaysDevTechEduc$PIB_Hab + 
             PaysDevTechEduc$Emploi_Industrie + PaysDevTechEduc$Technologie_Exportation +
             PaysDevTechEduc$Conv_Prio_122 + PaysDevTechEduc$Conv_Prio_129 +
             PaysDevTechEduc$Conv_TravEnfant_138 +
             PaysDevTechEduc$Conv_TravForce_105),~PaysDevTechEduc$Conv_TravForce_105)


#On arrive pas a determiner la source d'heteroscedasticité application de la correction de white
coeftest(regE2,vcov=vcovHC(regE2,type="HC0"))
waldtest(regE2, vcov = vcovHC(regE2,type="HC0"))
summary(regE2) #On garde le modele corrige car les ecart types sont plus petits et la significativite des variables a augmente

#Endogénéité : verification sur PIB_hab et Emploi Industrie et Taux jeune
regE2_DMC<-ivreg(formula = PaysDevTechEduc$Taux_Chomage ~
                  PaysDevTechEduc$Taux_Jeune_DescBIT +
                  PaysDevTechEduc$Entreprise_Procedure + PaysDevTechEduc$PIB_Hab + 
                  PaysDevTechEduc$Emploi_Industrie + PaysDevTechEduc$Technologie_Exportation +
                  PaysDevTechEduc$Conv_Prio_122 + PaysDevTechEduc$Conv_Prio_129 +
                  PaysDevTechEduc$Conv_TravEnfant_138 +
                  PaysDevTechEduc$Conv_TravForce_105  | PaysDevTechEduc$Entreprise_Procedure +
                  PaysDevTechEduc$Technologie_Exportation +
                  PaysDevTechEduc$Conv_Prio_122 + PaysDevTechEduc$Conv_Prio_129 +
                  PaysDevTechEduc$Conv_TravEnfant_138 + PaysDevTechEduc$Conv_TravForce_105 +
                  PaysDevTechEduc$Emploi_Service + PaysDevTechEduc$Emploi_Vul + PaysDevTechEduc$Investissement_E +
                  PaysDevTechEduc$Taux_Epargne + PaysDevTechEduc$Salarie +  PaysDevTechEduc$Conv_Syndic_087)
summary(regE2_DMC,vcov=sandwich, diagnostics = TRUE)#L'endo se trouvais avec le taux de chomlage jeune (ce qui est logique avec la theorie)
#en vue des tests nous avons de bons intruments (Sargan) cependant on va preferer le MCO (Haussman)
stargazer(regE2_DMC,type="text",out="table.txt")
#Nous utiliserons donc la correction de white afin de parler des effets et de la significativte de nos variables
stargazer(coeftest(regE2,vcov=vcovHC(regE2,type="HC0")),regE2,type="text",out="table.txt") 

#-------------------------- Modèle : RegE3, on peut supposer une endo avec emploi indus ou pib hab ------
#Test RAMSEY
reset(regE3, data=PaysDevTechEduc)#D'apres la p_value : 0.9522, au seuil de risque 5% on accepte H0 donc la forme fonctionnelle linéaire pour ce modèle est la bonne

#VIF
vif(regE3) #Pas de problkeme de multicolinéarité

#Test de Breush-Pagan
bptest(regE3)#P;Value = 0.8762, donc on accepte HHHH0 au seuil de risque 5% donc l'hypothsese d'homoscedasticité est vérifiée

#Detection de la source d hetero
residualPlots(regE3)#On ne connait pas la source d heteroscedaticite 

ncvTest(lm(PaysDevTechEduc$Taux_Chomage ~
             PaysDevTechEduc$Entreprise_Procedure +
             PaysDevTechEduc$PIB_Hab + 
             PaysDevTechEduc$Emploi_Industrie + PaysDevTechEduc$Technologie_Exportation +
             PaysDevTechEduc$Conv_Prio_122 + 
             PaysDevTechEduc$Conv_TravForce_105),~PaysDevTechEduc$Conv_Prio_122)

ncvTest(lm(PaysDevTechEduc$Taux_Chomage ~
             PaysDevTechEduc$Entreprise_Procedure +
             PaysDevTechEduc$PIB_Hab + 
             PaysDevTechEduc$Emploi_Industrie + PaysDevTechEduc$Technologie_Exportation +
             PaysDevTechEduc$Conv_Prio_122 + 
             PaysDevTechEduc$Conv_TravForce_105),~PaysDevTechEduc$Conv_Prio_122)
#On ne trouve pas la source : utilisation de la correction de white
coeftest(regE3,vcov=vcovHC(regE3,type="HC0"))
waldtest(regE3, vcov = vcovHC(regE3,type="HC0"))
summary(regE3) #On garde le modele corrige car plus petit ecart type d erreur et significativite plus importante
stargazer(coeftest(regE3,vcov=vcovHC(regE3,type="HC0")),regE3,type="text",out="table.txt")

#Endogénéité : verification sur PIB hab et emploi industrie
regE3_DMC<-ivreg(formula = PaysDevTechEduc$Taux_Chomage ~
                  PaysDevTechEduc$Entreprise_Procedure +
                  PaysDevTechEduc$PIB_Hab + 
                  PaysDevTechEduc$Emploi_Industrie + PaysDevTechEduc$Technologie_Exportation +
                  PaysDevTechEduc$Conv_Prio_122 + 
                  PaysDevTechEduc$Conv_TravForce_105 | 
                  PaysDevTechEduc$Entreprise_Procedure + PaysDevTechEduc$Technologie_Exportation +
                  PaysDevTechEduc$Conv_Prio_122 + PaysDevTechEduc$Conv_TravForce_105 +
                  PaysDevTechEduc$Emploi_Vul + PaysDevTechEduc$Salarie + PaysDevTechEduc$Emploi_Service + PaysDevTechEduc$Taux_Epargne)
                 
summary(regE3_DMC,vcov=sandwich, diagnostics = TRUE)
#Les instruments sont pertinents et bon (weak et sargan) cependant on va preferer les mco (Haussman)

#-------------------------- Modèle : RegE4, on peut supposer une endo avec emploi indus ou pib hab ------
#Test RAMSEY
reset(regE4, data=PaysDevTechEduc)#D'apres la p_value : 0.6589, au seuil de risque 5% on accepte H0 donc la forme fonctionnelle linéaire pour ce modèle est la bonne

#VIF
vif(regE4) #Pas de problkeme de multicolinéarité

#Test de Breush-Pagan
bptest(regE4)#P;Value = 0.9349, donc on accepte HHHH0 au seuil de risque 5% donc l'hypothsese d'homoscedasticité est vérifiée

#Detection de la source d hetero
residualPlots(regE4)#On ne connait pas la source d heteroscedaticite 

ncvTest(lm(PaysDevTechEduc$Taux_Chomage ~
             PaysDevTechEduc$Entreprise_Procedure +
             PaysDevTechEduc$PIB_Hab + 
             PaysDevTechEduc$Emploi_Service + PaysDevTechEduc$Technologie_Exportation +
             PaysDevTechEduc$Conv_Prio_122 + 
             PaysDevTechEduc$Conv_TravForce_105),~PaysDevTechEduc$Conv_Prio_122)

ncvTest(lm(PaysDevTechEduc$Taux_Chomage ~
             PaysDevTechEduc$Entreprise_Procedure +
             PaysDevTechEduc$PIB_Hab + 
             PaysDevTechEduc$Emploi_Service + PaysDevTechEduc$Technologie_Exportation +
             PaysDevTechEduc$Conv_Prio_122 + 
             PaysDevTechEduc$Conv_TravForce_105),~PaysDevTechEduc$Conv_TravForce_105)


#On ne trouve pas la source : utilisation de la correction de white
stargazer(regE4,coeftest(regE4,vcov=vcovHC(regE4,type="HC0")),type="text",out="table.txt")
waldtest(regE4, vcov = vcovHC(regE4,type="HC0"))
summary(regE4) #On garde le modele corrige car plus petit ecart type d erreur et significativite plus importante


#Endogénéité : verification sur PIB hab et emploi service
regE4_DMC<-ivreg(formula = PaysDevTechEduc$Taux_Chomage ~
                  PaysDevTechEduc$Entreprise_Procedure +
                  PaysDevTechEduc$PIB_Hab + 
                  PaysDevTechEduc$Emploi_Service + PaysDevTechEduc$Technologie_Exportation +
                  PaysDevTechEduc$Conv_Prio_122 + 
                  PaysDevTechEduc$Conv_TravForce_105
                  | PaysDevTechEduc$Entreprise_Procedure +
                  PaysDevTechEduc$Technologie_Exportation +
                    PaysDevTechEduc$Conv_Prio_122 + 
                    PaysDevTechEduc$Conv_TravForce_105 +
                  PaysDevTechEduc$Emploi_Vul + PaysDevTechEduc$Salarie + PaysDevTechEduc$Emploi_Service + PaysDevTechEduc$Taux_Epargne)

regE4_DMC<-ivreg(formula = PaysDevTechEduc$Taux_Chomage ~
                   PaysDevTechEduc$Entreprise_Procedure +
                   PaysDevTechEduc$PIB_Hab + 
                   PaysDevTechEduc$Emploi_Service + PaysDevTechEduc$Technologie_Exportation +
                   PaysDevTechEduc$Conv_Prio_122 + 
                   PaysDevTechEduc$Conv_TravForce_105
                 | PaysDevTechEduc$Entreprise_Procedure +
                   PaysDevTechEduc$Technologie_Exportation +
                   PaysDevTechEduc$Conv_Prio_122 + 
                   PaysDevTechEduc$Conv_TravForce_105 +
                   PaysDevTechEduc$Emploi_Vul + PaysDevTechEduc$Salarie +  PaysDevTechEduc$Taux_Epargne)

summary(regE4_DMC,vcov=sandwich, diagnostics = TRUE)
#Les instruments sont pertinents et bon (weak et sargan) cependant on va preferer les mco (Haussman)



#--- DATA PaysEnDev ----
summary(PaysEnDev) #On ne va pas utiliser Technologie_Exportation ; Taux Interet ; Taux_Syndic ; Taux_Couverture | Taux Epargne | Coeff_Salaire | Loi car trop de NA
#On verra pour l'utilisation des 3 variables lois
#Nettoyage de la base des variables NA
order(PaysEnDev$Agriculture)
order(PaysEnDev$Impot_Autres)
order(PaysEnDev$Masse_Monétaire)
order(PaysEnDev$Entreprise_Procedure)
order(PaysEnDevBis$Taux_Jeune_DescBIT)
PaysEnDev<-PaysEnDev[-c(110,108,106,101,59,49,24,9,89,86,62,57,36,113,112,94,87,86,27),]
#19 observations sont supprimés de la base car contenant des NA sur les variables sur lesquelles nous aimerons travailler
#Modification de la base sans les variables non utilisés
PaysEnDevBis<-PaysEnDev[,-c(4,18,20,23,22,32,33,34,35,36)]
summary(PaysEnDevBis)
PaysEnDevBis<-PaysEnDevBis[-c(18),]
#------ Test des Outliers et BoxPlot ------
par(mfrow=c(1,1))
boxplot(PaysEnDevBis$Taux_Chomage)
rosnerTest(PaysEnDevBis$Taux_Chomage, k = 8, alpha = 0.05)
grubbs.test(PaysEnDevBis$Taux_Chomage,type=10, two.sided = TRUE) #Pas d outlier
par(mfrow=c(3,3))
for(k in 3:18){
  boxplot(PaysEnDevBis[,k], main=names(PaysEnDevBis[k]))
}
#Possible outliers sur Masse_Monétaire, Emploi, Investissement education , Agriculture, Impot, Taux Rural, salarie 

rosnerTest(PaysEnDevBis$Masse_Monétaire, k = 8, alpha = 0.05) # 2 outliers : 21 94
grubbs.test(PaysEnDevBis$Masse_Monétaire,type=10, two.sided = TRUE)
rosnerTest(PaysEnDevBis$Emploi_Agriculture, k = 8, alpha = 0.05) # Pas doutlier
grubbs.test(PaysEnDevBis$Emploi_Agriculture,type=10, two.sided = TRUE)
rosnerTest(PaysEnDevBis$Emploi_Industrie, k = 8, alpha = 0.05) # Pas doutlier
grubbs.test(PaysEnDevBis$Emploi_Industrie,type=10, two.sided = TRUE)
rosnerTest(PaysEnDevBis$Emploi_Service, k = 8, alpha = 0.05) # Pas doutlier
grubbs.test(PaysEnDevBis$Emploi_Service,type=10, two.sided = TRUE)
rosnerTest(PaysEnDevBis$Investissement_E, k = 10, alpha = 0.05) # 7 outliers : 24,41,62,60,79,19,5
grubbs.test(PaysEnDevBis$Investissement_E,type=10, two.sided = TRUE)
rosnerTest(PaysEnDevBis$Agriculture, k = 8, alpha = 0.05) # 1 outlier : 83
grubbs.test(PaysEnDevBis$Agriculture,type=10, two.sided = TRUE)
rosnerTest(PaysEnDevBis$Impot_Benef, k = 8, alpha = 0.05) # 1 outlier : 39
grubbs.test(PaysEnDevBis$Impot_Benef,type=10, two.sided = TRUE)
rosnerTest(PaysEnDevBis$Impot_Travail, k = 8, alpha = 0.05) # Pas d outlier
grubbs.test(PaysEnDevBis$Impot_Travail,type=10, two.sided = TRUE)
rosnerTest(PaysEnDevBis$Impot_Paiement, k = 8, alpha = 0.05) # 2 outlier : 26 77
grubbs.test(PaysEnDevBis$Impot_Paiement,type=10, two.sided = TRUE) 
order(PaysEnDevBis$Impot_Paiement)
rosnerTest(PaysEnDevBis$Impot_Autres, k = 10, alpha = 0.05) # 10 outliers au mini
grubbs.test(PaysEnDevBis$Impot_Autres,type=10, two.sided = TRUE)
rosnerTest(PaysEnDevBis$Taux_Rural, k = 8, alpha = 0.05) # 48
grubbs.test(PaysEnDevBis$Taux_Rural,type=10, two.sided = TRUE)
rosnerTest(PaysEnDevBis$Salarie, k = 8, alpha = 0.05) # 1 outlier : 9 
grubbs.test(PaysEnDevBis$Salarie,type=10, two.sided = TRUE)
order(PaysEnDevBis$Salarie)
rosnerTest(PaysEnDevBis$Taux_Jeune_DescBIT, k = 8, alpha = 0.05) # Pas d outlier
grubbs.test(PaysEnDevBis$Taux_Jeune_DescBIT,type=10, two.sided = TRUE)
rosnerTest(PaysEnDevBis$PIB_Hab, k = 8, alpha = 0.05) # 1 outlier : 61 
grubbs.test(PaysEnDevBis$PIB_Hab,type=10, two.sided = TRUE)
order(PaysEnDevBis$PIB_Hab)
rosnerTest(PaysEnDevBis$Emploi_Vul, k = 8, alpha = 0.05) # 1 outlier : 9
grubbs.test(PaysEnDevBis$Emploi_Vul,type=10, two.sided = TRUE)
order(PaysEnDevBis$Emploi_Vul)
rosnerTest(PaysEnDevBis$Entreprise_Procedure, k = 8, alpha = 0.05) # Pas d outlier
grubbs.test(PaysEnDevBis$Entreprise_Procedure,type=10, two.sided = TRUE)
#On supprime les outliers
PaysEnDevBis <- PaysEnDevBis[-c(21,94,24,41,62,60,79,19,5,83,39,26,77,48,9,61),]


#------ Plot PaysEnDev ----
par(mfrow=c(2,2))
plot(PaysEnDevBis$Taux_Chomage ~ PaysEnDevBis$Agriculture)
abline(PaysEnDevBis$Taux_Chomage ~ PaysEnDevBis$Agriculture)

#------ ACP sur PaysEnDev -----
nb = estim_ncpPCA(PaysEnDev[,4:23],ncp.max=5)
res.comp = imputePCA(PaysEnDev[,4:23],ncp=2)
res.pca=PCA(res.comp)
fviz_pca_ind(res.pca)
fviz_pca_var(res.pca,axes=c(1,3))
#Valeurs propre et le nombre d'axes
res.pca$eig
round(res.pca$eig,2)
# on garde 3 axe car le cumul en % de la variance est de 75,26%
# Variables
res.pca
#Contribution
round(res.pca$var$contrib,2)
#Corrélations
dimdesc(res.pca)
#Cosinus
round(res.pca$var$cos2,2)
#  Cercle de corrélation
plot.PCA(res.pca,tilte="Graph des obs",xlim=c(-7,6),axes=c(1,2),choix="var",cex=.8)
fviz_pca_var(res.pca, col.var="contrib", c(1,2), title="Cercle de corrélation selon l'axe 1 & 2")+
  scale_color_gradient2(low="Grey", mid="blue",
                        high="Red", midpoint=6, space ="Lab") + theme_minimal()
plot.PCA(res.pca,tilte="Graph des obs",xlim=c(-7,6),axes=c(1,2),choix="var",cex=.8)
fviz_pca_var(res.pca, col.var="contrib", c(1,3), title="Cercle de corrélation selon l'axe 1 & 3")+
  scale_color_gradient2(low="Grey", mid="blue",
                        high="Red", midpoint=6, space ="Lab") + theme_minimal()

#------ Moindres Carrés -----
#---------- Correlation et Shapiro PaysenDev ----
cor(PaysEnDevBis[,-c(1,2,3,19,20,21,22,23,24,25,26)],use="complete.obs")
cor(PaysEnDevBis[,-c(1,2,3,19,20,21,22,23,24,25,26)],method=c("spearman"))
#Probleme de correlation entre ce groupe de variable : PIB_Hab ; Agriculture ; Emploi_Vul ; Salarie ; Emploi_ Service ; Emploi_Industrie ; Emploi_Agriculture
shapiro.test(PaysEnDevBis$PIB_Hab)#Rejet H0 , donc elle ne suit pas une loi normale
shapiro.test(PaysEnDevBis$Taux_Rural)# Accepte H0 : elle suit une loi normale
shapiro.test(PaysEnDevBis$Agriculture) #Rejet H0 : elle ne suit pas une loi normale
shapiro.test(PaysEnDevBis$Impot_Benef)# Accepte H0 : elle suit une loi normale
shapiro.test(PaysEnDevBis$Impot_Travail)# Accepte H0
shapiro.test(PaysEnDevBis$Impot_Paiement)#Rejet H0 :  elle ne suit pas une loi normale
shapiro.test(PaysEnDevBis$Impot_Autres)#Ne suit pas une loi normale
shapiro.test(PaysEnDevBis$Emploi_Vul)#Rejet H0 à 5% : elle ne suit pas une loi normale mais accepte a 5%
shapiro.test(PaysEnDevBis$Salarie)#Rejet H0 à 5% : elle ne suit pas une loi normale mais accepte a 5%
shapiro.test(PaysEnDevBis$Emploi_Industrie)#Accepte H0 : elle suit une loi normale
shapiro.test(PaysEnDevBis$Emploi_Service)#Accepte H0 : elle suit une loi normale
shapiro.test(PaysEnDevBis$Emploi_Agriculture)#Rejet H0 : elle ne suit pas une loi normale
shapiro.test(PaysEnDevBis$Masse_Monétaire)#Rejet H0 : elle ne suit pas une loi normale
shapiro.test(PaysEnDevBis$Entreprise_Procedure)#Accepte H0 : elle suit une loi normale
shapiro.test(PaysEnDevBis$Taux_Jeune_DescBIT)#Accepte H0 : elle suit une loi normale
shapiro.test(PaysEnDevBis$Investissement_E)#Rejet H0 : elle ne suit pas une loi normale

plot(PaysEnDevBis$PIB_Hab)
plot(PaysEnDevBis$Agriculture)
plot(PaysEnDevBis$Impot_Paiement)
plot(PaysEnDevBis$Emploi_Vul)
plot(PaysEnDevBis$Salarie)
plot(PaysEnDevBis$Masse_Monétaire)

PaysEnDevBis$Pib_Bin<-sapply(PaysEnDevBis$PIB_Hab, function(x){
  if(x>=5000 & x<10000){
    return(1)}
  else if(x>=10000 & x<15000){
    return(2)}
  else if(x>=15000){
    return(3)}
  else{
    return(0)}})
PaysEnDevBis$Agriculture_Bin<-sapply(PaysEnDevBis$Agriculture, function(x){
  if(x>=20){
    return(1)}
  else{
    return(0)}})
PaysEnDevBis$Impot_Paiement_Bin<-sapply(PaysEnDevBis$Impot_Paiement, function(x){
  if(x>25 & x<45){
    return(1)}
  else if(x>=45){
    return(2)}
  else{
    return(0)}})
PaysEnDevBis$Emploi_Vul_Bin<-sapply(PaysEnDevBis$Emploi_Vul, function(x){
  if(x>25 & x<60){
    return(1)}
  else if(x>=60){
    return(2)}
  else{
    return(0)}})
PaysEnDevBis$Salarie_Bin<-sapply(PaysEnDevBis$Salarie, function(x){
  if(x>25 & x<=60){
    return(1)}
  else if(x>60){
    return(2)}
  else{
    return(0)}})
PaysEnDevBis$Masse_Monétaire_Bin<-sapply(PaysEnDevBis$Masse_Monétaire, function(x){
  if(x<=60){
    return(0)}
  else{
    return(1)}})

PaysEnDevBis$Pib_Bin <- as.factor(PaysEnDevBis$Pib_Bin)
PaysEnDevBis$Agriculture_Bin <- as.factor(PaysEnDevBis$Agriculture_Bin)
PaysEnDevBis$Impot_Paiement_Bin <- as.factor(PaysEnDevBis$Impot_Paiement_Bin)
PaysEnDevBis$Emploi_Vul_Bin <- as.factor(PaysEnDevBis$Emploi_Vul_Bin)
PaysEnDevBis$Salarie_Bin <- as.factor(PaysEnDevBis$Salarie_Bin)
PaysEnDevBis$Masse_Monétaire_Bin <- as.factor(PaysEnDevBis$Masse_Monétaire_Bin)

#PaysEnDevBis$Pib_Bin +
#PaysEnDevBis$Agriculture_Bin +
#PaysEnDevBis$Impot_Paiement_Bin +
#PaysEnDevBis$Emploi_Vul_Bin +
#PaysEnDevBis$Salarie_Bin + 
#PaysEnDevBis$Masse_Monétaire_Bin +


#---------- Modèles PaysEnDev ---- 

regEnD1 <- lm(formula = PaysEnDevBis$Taux_Chomage ~
            PaysEnDevBis$Emploi_Industrie +  PaysEnDevBis$Emploi_Service +
            PaysEnDevBis$Taux_Jeune_DescBIT + 
            PaysEnDevBis$Conv_Syndic_098)
summary(regEnD1)

regEnD2 <- lm(formula = PaysEnDevBis$Taux_Chomage ~
                      PaysEnDevBis$Impot_Travail  + 
                      PaysEnDevBis$Emploi_Industrie + PaysEnDevBis$Emploi_Service +
                      PaysEnDevBis$Conv_TravForce_105)
summary(regEnD2)

regEnD4 <- lm(formula = PaysEnDevBis$Taux_Chomage ~
                PaysEnDevBis$Emploi_Vul_Bin + PaysEnDevBis$Emploi_Service + PaysEnDevBis$Taux_Rural + PaysEnDevBis$Impot_Travail)
summary(regEnD4)


regEnD3 <- lm(formula = PaysEnDevBis$Taux_Chomage ~
                      PaysEnDevBis$Impot_Travail  + 
                      PaysEnDevBis$Emploi_Industrie + PaysEnDevBis$Emploi_Service +
                      PaysEnDevBis$Taux_Jeune_DescBIT + 
                      PaysEnDevBis$Conv_TravEnfant_138)
summary(regEnD3)


#-------------------------- Modèle : RegEnD1, on peut supposer une endo sur taux chomage jeune ------------------
#Test RAMSEY
reset(regEnD1, data=PaysEnDevBis)#D'apres la p_value : 0.027, au seuil de risque 5% on refuse H0 donc la forme fonctionnelle linéaire pour ce modèle n'est pas le bonne
regEnD1 <- lm(formula = log(PaysEnDevBis$Taux_Chomage) ~
                PaysEnDevBis$Emploi_Industrie +  PaysEnDevBis$Emploi_Service +
                PaysEnDevBis$Taux_Jeune_DescBIT +
                PaysEnDevBis$Conv_Syndic_098)
reset(regEnD1, data=PaysEnDevBis) #p_value : 0.9642 on garde ce modele
#VIF
vif(regEnD1) #Pas de problkeme de multicolinéarité

#Test de Breush-Pagan
bptest(regEnD1)#P;Value = 0.2343, donc on accepte HHHH0 au seuil de risque 5% donc l'hypothsese d'homoscedasticité est vérifiée

# Detection de la source d hetero 
residualPlots(regEnD1)#On suppose que emploi industrie est la source 

residusEnD1=resid(regEnD1)
regLnresidEnD1<-lm(log(residusEnD1^2)~PaysEnDevBis$Emploi_Industrie)
expoEnD1=exp(fitted(regLnresidEnD1))
regFGLSEnD1 <- lm(log(PaysEnDevBis$Taux_Chomage) ~
                  PaysEnDevBis$Emploi_Industrie +  PaysEnDevBis$Emploi_Service +
                  PaysEnDevBis$Taux_Jeune_DescBIT +
                  PaysEnDevBis$Conv_Syndic_098,weights=1/expoEnD1 )
summary(regFGLSEnD1)#On garde ce modele pour traviller par la suite (ecart type plus petit et R² plus grand)
summary(regEnD1)

# Endogénéité : verification sur Taux_jeune  
regEnD1_DMC<-ivreg(formula = log(PaysEnDevBis$Taux_Chomage) ~
                 PaysEnDevBis$Emploi_Industrie +  PaysEnDevBis$Emploi_Service +
                 PaysEnDevBis$Taux_Jeune_DescBIT +
                 PaysEnDevBis$Conv_Syndic_098  | PaysEnDevBis$Emploi_Industrie +  PaysEnDevBis$Emploi_Service +
                 PaysEnDevBis$Conv_Syndic_098 + PaysEnDevBis$Salarie + PaysEnDevBis$Impot_Autres +
                 PaysEnDevBis$PIB_Hab + PaysEnDevBis$Impot_Benef,weights=1/expoEnD1)
summary(regEnD1_DMC,vcov=sandwich, diagnostics = TRUE)
# Les instruments sont pertinents et bon (weak et sargan) cependant on va preferer les mco (Haussman) 
summary(regFGLSEnD1)

#-------------------------- Modèle : RegEnD2 ------------------
#Test RAMSEY
reset(regEnD2, data=PaysEnDevBis)#D'apres la p_value : 0.0048, au seuil de risque 5% on refuse H0 donc la forme fonctionnelle linéaire pour ce modèle n'est pas le bonne
regEnD2 <- lm(formula = log(PaysEnDevBis$Taux_Chomage) ~
                PaysEnDevBis$Impot_Travail  +  
                PaysEnDevBis$Emploi_Industrie + PaysEnDevBis$Emploi_Service +
                PaysEnDevBis$Conv_TravForce_105)
reset(regEnD2, data=PaysEnDevBis) #p_value : 0.5728 on garde ce modele
#VIF
vif(regEnD2) #Pas de probleme de multicolinéarité

#Test de Breush-Pagan
bptest(regEnD2)#P;Value = 0.1469, donc on accepte HHHH0 au seuil de risque 5% donc l'hypothsese d'homoscedasticité est vérifiée

# Detection de la source d hetero 
residualPlots(regEnD2)#Impot travail est la source

residusEnD2=resid(regEnD2)
regLnresidEnD2<-lm(log(residusEnD2^2)~PaysEnDevBis$Impot_Travail)
expoEnD2=exp(fitted(regLnresidEnD2))
regFGLSEnD2 <- lm(log(PaysEnDevBis$Taux_Chomage) ~
                    PaysEnDevBis$Impot_Travail  +  
                    PaysEnDevBis$Emploi_Industrie + PaysEnDevBis$Emploi_Service +
                    PaysEnDevBis$Conv_TravForce_105,weights=1/expoEnD2 )
summary(regFGLSEnD2)#On garde ce modele pour travailler par la suite (ecart type plus petit)
summary(regEnD2)

# Endogénéité : verification sur Emploi Industrie et Service 
regEnD2_DMC<-ivreg(formula = log(PaysEnDevBis$Taux_Chomage) ~
                 PaysEnDevBis$Impot_Travail  +  
                 PaysEnDevBis$Emploi_Industrie + PaysEnDevBis$Emploi_Service +
                 PaysEnDevBis$Conv_TravForce_105  | 
                 PaysEnDevBis$Impot_Travail  +  
                 PaysEnDevBis$Conv_TravForce_105 + PaysEnDevBis$PIB_Hab + PaysEnDevBis$Salarie + PaysEnDevBis$Taux_Rural + PaysEnDevBis$Emploi_Agriculture,weights=1/expoEnD1)
summary(regEnD2_DMC,vcov=sandwich, diagnostics = TRUE)

regEnD2_DMC<-ivreg(formula = log(PaysEnDevBis$Taux_Chomage) ~
                 PaysEnDevBis$Impot_Travail  +  
                 PaysEnDevBis$Emploi_Industrie + PaysEnDevBis$Emploi_Service +
                 PaysEnDevBis$Conv_TravForce_105  | 
                 PaysEnDevBis$Impot_Travail  +  
                 PaysEnDevBis$Conv_TravForce_105 + PaysEnDevBis$PIB_Hab + PaysEnDevBis$Salarie + PaysEnDevBis$Taux_Rural,weights=1/expoEnD2)
summary(regEnD2_DMC,vcov=sandwich, diagnostics = TRUE)
# Les instruments sont pertinents et bon (weak et sargan) cependant on va preferer les mco (Haussman) 
summary(regFGLSEnD2)#On etudie a partir de ce modele
stargazer(regFGLSEnD2,)
#-------------------------- Modèle : RegEnD3, on peut supposer une endo sur taux chomage jeune ------------------
#Test RAMSEY
reset(regEnD3, data=PaysEnDevBis)#D'apres la p_value : 0.01229, au seuil de risque 5% on refuse H0 donc la forme fonctionnelle linéaire pour ce modèle n'est pas le bonne
regEnD3 <- lm(formula = log(PaysEnDevBis$Taux_Chomage) ~
                PaysEnDevBis$Impot_Travail  + 
                PaysEnDevBis$Emploi_Industrie + PaysEnDevBis$Emploi_Service +
                PaysEnDevBis$Taux_Jeune_DescBIT + 
                PaysEnDevBis$Conv_TravEnfant_138 
)
reset(regEnD3, data=PaysEnDevBis) #p_value : 0.9198 on garde ce modele
#VIF
vif(regEnD3) #Pas de probleme de multicolinéarité

#Test de Breush-Pagan
bptest(regEnD3)#P;Value = 0.3793, donc on accepte HHHH0 au seuil de risque 5% donc l'hypothsese d'homoscedasticité est vérifiée

# Detection de la source d hetero 
residualPlots(regEnD3)#Impot travail est la source

residusEnD3=resid(regEnD3)
regLnresidEnD3<-lm(log(residusEnD3^2)~PaysEnDevBis$Impot_Travail + PaysEnDevBis$Taux_Jeune_DescBIT + PaysEnDevBis$Emploi_Industrie)
regLnresidEnD3<-lm(log(residusEnD3^2)~PaysEnDevBis$Impot_Travail  PaysEnDevBis$Emploi_Industrie)
regLnresidEnD3<-lm(log(residusEnD3^2)~PaysEnDevBis$Impot_Travail + PaysEnDevBis$Taux_Jeune_DescBIT )
regLnresidEnD3<-lm(log(residusEnD3^2)~PaysEnDevBis$Impot_Travail)
expoEnD3=exp(fitted(regLnresidEnD3))
regFGLSEnD3 <- lm(log(PaysEnDevBis$Taux_Chomage) ~
                    PaysEnDevBis$Impot_Travail  + 
                    PaysEnDevBis$Emploi_Industrie + PaysEnDevBis$Emploi_Service +
                    PaysEnDevBis$Taux_Jeune_DescBIT + 
                    PaysEnDevBis$Conv_TravEnfant_138,weights=1/expoEnD3 )
summary(regFGLSEnD3)
summary(regEnD3) #Un modele permet de corriger le probleme :
regLnresidEnD3<-lm(log(residusEnD3^2)~PaysEnDevBis$Impot_Travail + PaysEnDevBis$Taux_Jeune_DescBIT + PaysEnDevBis$Emploi_Industrie)
expoEnD3=exp(fitted(regLnresidEnD3))
regFGLSEnD3 <- lm(log(PaysEnDevBis$Taux_Chomage) ~
                    PaysEnDevBis$Impot_Travail  + 
                    PaysEnDevBis$Emploi_Industrie + PaysEnDevBis$Emploi_Service +
                    PaysEnDevBis$Taux_Jeune_DescBIT + 
                    PaysEnDevBis$Conv_TravEnfant_138,weights=1/expoEnD3 )
summary(regFGLSEnD3) #On prefera ce modele



# Endogénéité : verification sur Taux Jeune
regEnD3_DMC<-ivreg(formula = log(PaysEnDevBis$Taux_Chomage) ~
                 PaysEnDevBis$Impot_Travail  + 
                 PaysEnDevBis$Emploi_Industrie + PaysEnDevBis$Emploi_Service +
                 PaysEnDevBis$Taux_Jeune_DescBIT + 
                 PaysEnDevBis$Conv_TravEnfant_138  | 
                 PaysEnDevBis$Impot_Travail  + 
                 PaysEnDevBis$Emploi_Industrie + PaysEnDevBis$Emploi_Service +
                 PaysEnDevBis$Conv_TravEnfant_138 + PaysEnDevBis$Salarie +  PaysEnDevBis$Impot_Autres,weights=1/expoEnD3)
summary(regEnD3_DMC,vcov=sandwich, diagnostics = TRUE) 



# Les instruments sont pertinents et bon (weak et sargan) cependant on va preferer les mco (Haussman) 
summary(regFGLSEnD3)#On etudie a partir de ce modele

#--- DATA Loi ----
summary(PaysLoi)#Taux epargne 7Na, agriculture : 5Na, Impot 1Na
#TechExportation : 20 Na , Taux interet : 41 Na , Entreprise Proc : 1Na , Taux_Syndic : 73 
#Taux couverture : 71 Na , Taux Jeune_DescBIT : 4 , Salaire min_men : 14 Na
#On ne va pas utiliser utiliser Masse monétaire Taux Interet, Tech Exportation, Taux syndic et ni Taux couverture
order(PaysLoi$Impot_Benef)#91
order(PaysLoi$Taux_Epargne)#91 88 87 80 41 33 3
order(PaysLoi$Agriculture)#91 88 67 36 21
order(PaysLoi$Salaire_Min_Mens)# 95 91 80 75 72 70 63 59 45 44 33 32 22 14
order(PaysLoi$Taux_Jeune_DescBIT)# 95 81 49 26
order(PaysLoi$Entreprise_Procedure)# 91
#Suppression des Na
PaysLoiBis <- PaysLoi[-c(91,88,87,80,41,33,3,67,36,21,95,75,72,70,63,59,45,44,32,22,14,81,49,26),]
#Supression des variables non utilisées
PaysLoiBis <- PaysLoiBis[,-c(18,19,20,22,35,36)]
summary(PaysLoiBis)
#------ Test des Outliers et BoxPlot -----

#Sur Tx de chomage
par(mfrow=c(1,1))
boxplot(PaysLoiBis$Taux_Chomage)
rosnerTest(PaysLoiBis$Taux_Chomage, k = 8, alpha = 0.05)
grubbs.test(PaysLoiBis$Taux_Chomage,type=10, two.sided = TRUE) #2 outlier : 2 41
PaysLoiBis <- PaysLoiBis [-c(2,41),]
summary(PaysLoiBis)
#BoxPlot toalité data
par(mfrow=c(3,3))
for(k in 4:20){
  boxplot(PaysLoiBis[,k], main=names(PaysLoiBis[k]))
}
for(k in 29:32){
  boxplot(PaysLoiBis[,k], main=names(PaysLoiBis[k]))
}
#Test Outlier
rosnerTest(PaysLoiBis$Taux_Epargne, k = 8, alpha = 0.05)
grubbs.test(PaysLoiBis$Taux_Epargne,type=10, two.sided = TRUE)# Pas doutlier
rosnerTest(PaysLoiBis$PIB_Hab, k = 8, alpha = 0.05)
grubbs.test(PaysLoiBis$PIB_Hab,type=10, two.sided = TRUE)#Pas d outlier
rosnerTest(PaysLoiBis$Taux_Rural, k = 8, alpha = 0.05) 
grubbs.test(PaysLoiBis$Taux_Rural,type=10, two.sided = TRUE)#1 outlier : 48
rosnerTest(PaysLoiBis$Agriculture, k = 8, alpha = 0.05)
grubbs.test(PaysLoiBis$Agriculture,type=10, two.sided = TRUE)#1 outlier : 48

rosnerTest(PaysLoiBis$Impot_Benef, k = 8, alpha = 0.05)
grubbs.test(PaysLoiBis$Impot_Benef,type=10, two.sided = TRUE)#pas d outlier
rosnerTest(PaysLoiBis$Impot_Autres, k = 10, alpha = 0.05) # trop d outlier (min 10)
rosnerTest(PaysLoiBis$Impot_Paiement, k = 8, alpha = 0.05)#1 outlier : 23
grubbs.test(PaysLoiBis$Impot_Paiement,type=10, two.sided = TRUE)
rosnerTest(PaysLoiBis$Impot_Travail, k = 8, alpha = 0.05)
grubbs.test(PaysLoiBis$Impot_Travail,type=10, two.sided = TRUE)#pas d outlier

rosnerTest(PaysLoiBis$Investissement_E, k = 10, alpha = 0.05)#trop d outlier (min 10)
grubbs.test(PaysLoiBis$Investissement_E,type=10, two.sided = TRUE)
rosnerTest(PaysLoiBis$Salarie, k = 8, alpha = 0.05)
grubbs.test(PaysLoiBis$Salarie,type=10, two.sided = TRUE)#Pas d outlier

rosnerTest(PaysLoiBis$Emploi_Vul, k = 8, alpha = 0.05)
grubbs.test(PaysLoiBis$Emploi_Vul,type=10, two.sided = TRUE)#Pas d outlier
rosnerTest(PaysLoiBis$Emploi_Industrie, k = 8, alpha = 0.05)
grubbs.test(PaysLoiBis$Emploi_Industrie,type=10, two.sided = TRUE)#Pas d outlier
rosnerTest(PaysLoiBis$Emploi_Agriculture, k = 8, alpha = 0.05)
grubbs.test(PaysLoiBis$Emploi_Agriculture,type=10, two.sided = TRUE)#Pas d outlier
rosnerTest(PaysLoiBis$Emploi_Service, k = 8, alpha = 0.05)
grubbs.test(PaysLoiBis$Emploi_Service,type=10, two.sided = TRUE)#Pas d outlier

rosnerTest(PaysLoiBis$Taux_Jeune_DescBIT, k = 8, alpha = 0.05)
grubbs.test(PaysLoiBis$Taux_Jeune_DescBIT,type=10, two.sided = TRUE)#Pas d outlier
rosnerTest(PaysLoiBis$Entreprise_Procedure, k = 8, alpha = 0.05)
grubbs.test(PaysLoiBis$Entreprise_Procedure,type=10, two.sided = TRUE)#Pas d outlier
rosnerTest(PaysLoiBis$Coeff_Salaire, k = 8, alpha = 0.05)#2 outliers : 1 64
grubbs.test(PaysLoiBis$Coeff_Salaire,type=10, two.sided = TRUE)

rosnerTest(PaysLoiBis$Loi_Indemnite, k = 8, alpha = 0.05)
grubbs.test(PaysLoiBis$Loi_Indemnite,type=10, two.sided = TRUE)#Pas d outlier
rosnerTest(PaysLoiBis$Loi_Periode_Essai, k = 6, alpha = 0.05)
grubbs.test(PaysLoiBis$Loi_Periode_Essai,type=10, two.sided = TRUE)#Pas d outlier
rosnerTest(PaysLoiBis$Loi_Proc_Demission, k = 8, alpha = 0.05)
grubbs.test(PaysLoiBis$Loi_Proc_Demission,type=10, two.sided = TRUE)#Pas d outlier

#On enleve investissement educ et impot autres car trop d outlier
PaysLoiBis <- PaysLoiBis[,-c(11,12)]
#et on enleve les outliers
PaysLoiBis <- PaysLoiBis[-c(48,23,1,64),]

#------ Correlation et plot sur PaysLoi-------

cor(PaysLoiBis[,-c(1,2,18,19,20,21,22,23,24,25)],use="complete.obs")
par(mfrow=c(2,2))
plot(PaysLoi$Taux_Chomage~PaysLoi$Taux_Epargne)
abline(lm(PaysLoi$Taux_Chomage~PaysLoi$Taux_Epargne))
plot(PaysLoi$Taux_Chomage~PaysLoi$Taux_Rural)
plot(PaysLoi$Taux_Chomage~PaysLoi$Impot_Benef)
plot(PaysLoi$Taux_Chomage~PaysLoi$Impot_Travail)

#------ ACP sur PaysLoi -----
nb = estim_ncpPCA(PaysLoi[,-c(1,2,24,25,26,27,28,29,30,31)],ncp.max=5)
res.comp = imputePCA(PaysLoi[,-c(1,2,24,25,26,27,28,29,30,31)],ncp=3)
res.pca=PCA(res.comp)
fviz_pca_ind(res.pca)
fviz_pca_var(res.pca,axes=c(1,3))
#Valeurs propre et le nombre d'axes
res.pca$eig
round(res.pca$eig,2)
# on garde 3 axe car le cumul en % de la variance est de 74.61%
# Variables
res.pca
#Contribution
round(res.pca$var$contrib,2)
#Corrélations
dimdesc(res.pca)
#Cosinus
round(res.pca$var$cos2,2)
#Cercle de corrélation
plot.PCA(res.pca,tilte="Graph des obs",xlim=c(-7,6),axes=c(1,2),choix="var",cex=.8)
fviz_pca_var(res.pca, col.var="contrib", c(1,2), title="Cercle de corrélation selon l'axe 1 & 2")+
  scale_color_gradient2(low="Grey", mid="blue",
                        high="Red", midpoint=6, space ="Lab") + theme_minimal()
plot.PCA(res.pca,tilte="Graph des obs",xlim=c(-7,6),axes=c(1,2),choix="var",cex=.8)
fviz_pca_var(res.pca, col.var="contrib", c(1,3), title="Cercle de corrélation selon l'axe 1 & 3")+
  scale_color_gradient2(low="Grey", mid="blue",
                        high="Red", midpoint=6, space ="Lab") + theme_minimal()
#------ Moindres Carrés -----
#---------- Correlation et Shapiro PaysLoi ----
cor(PaysLoiBis[,-c(1,2,3,18,19,20,21,22,23,24,25)],use="complete.obs")
cor(PaysLoiBis[,-c(1,2,3,18,19,20,21,22,23,24,25)],method=c("spearman"))
#Probleme de correlation entre ce groupe de variable : PIB_Hab ; Agriculture ; 
#Emploi_Vul ; Salarie ; Emploi_ Service  ; Emploi_Agriculture ; Taux rural
shapiro.test(PaysLoiBis$PIB_Hab)#rejet H0 : pas loi normale
shapiro.test(PaysLoiBis$Agriculture)#rejet H0
shapiro.test(PaysLoiBis$Emploi_Agriculture)#rejet H0
shapiro.test(PaysLoiBis$Emploi_Vul)#rejet H0
shapiro.test(PaysLoiBis$Salarie)#rejet H0
shapiro.test(PaysLoiBis$Taux_Rural)#rejet H0

shapiro.test(PaysLoiBis$Emploi_Service)#accepte H0 : suit une loi normale
shapiro.test(PaysLoiBis$Emploi_Industrie)#accepte H0
shapiro.test(PaysLoiBis$Taux_Epargne)#accepte H0
shapiro.test(PaysLoiBis$Impot_Benef)#accepte H0
shapiro.test(PaysLoiBis$Impot_Travail)#accepte H0
shapiro.test(PaysLoiBis$Coeff_Salaire)#accepte H0
shapiro.test(PaysLoiBis$Taux_Jeune_DescBIT)#accepte H0

shapiro.test(PaysLoiBis$Impot_Paiement)#rejet H0
shapiro.test(PaysLoiBis$Entreprise_Procedure)#rejet H0
shapiro.test(PaysLoiBis$Loi_Periode_Essai)#rejet H0
shapiro.test(PaysLoiBis$Loi_Proc_Demission)#rejet H0
shapiro.test(PaysLoiBis$Loi_Indemnite)#rejet H0

#---------- Modèles LoiBis ----
regLoi <- lm(PaysLoiBis$Taux_Chomage ~ PaysLoiBis$Taux_Epargne +
               PaysLoiBis$Emploi_Service + PaysLoiBis$Emploi_Industrie +
               PaysLoiBis$Impot_Benef + PaysLoiBis$Impot_Travail + 
               PaysLoiBis$Coeff_Salaire + PaysLoiBis$Taux_Jeune_DescBIT +
               PaysLoiBis$Impot_Paiement + PaysLoiBis$Entreprise_Procedure +
               PaysLoiBis$Loi_Indemnite + PaysLoiBis$Loi_Periode_Essai + PaysLoiBis$Loi_Proc_Demission +
               PaysLoiBis$Conv_Prio_122 + PaysLoiBis$Conv_Prio_129 +
               PaysLoiBis$Conv_Syndic_087 + PaysLoiBis$Conv_Syndic_098 +
               PaysLoiBis$Conv_TravEnfant182 + PaysLoiBis$Conv_TravEnfant_138 + 
               PaysLoiBis$Conv_TravForce_029 + PaysLoiBis$Conv_TravForce_105)
summary(regLoi)

regLoi <- lm(PaysLoiBis$Taux_Chomage ~ PaysLoiBis$Taux_Epargne +
               PaysLoiBis$Emploi_Service + PaysLoiBis$Emploi_Industrie +
               PaysLoiBis$Impot_Benef + PaysLoiBis$Impot_Travail + 
               PaysLoiBis$Coeff_Salaire + 
               PaysLoiBis$Impot_Paiement + PaysLoiBis$Entreprise_Procedure +
               PaysLoiBis$Loi_Indemnite + PaysLoiBis$Loi_Periode_Essai + PaysLoiBis$Loi_Proc_Demission +
               PaysLoiBis$Conv_Prio_122 + 
               PaysLoiBis$Conv_Syndic_098 +
               PaysLoiBis$Conv_TravEnfant182 + PaysLoiBis$Conv_TravEnfant_138 + 
               PaysLoiBis$Conv_TravForce_029 + PaysLoiBis$Conv_TravForce_105
               )
summary(regLoi)
#-------------- Creation de nouvelles varriables ------
#Afin d'ameliorer les modeles nous allons y integrer les variables 
#qui ne rentraient pas au depart dans le modele et qui paraissent interressante a etudier :
# ce sont celles qui presentent le plus de disparite qui vont nous interresse en l'occurence

#-------------------------- Plot ------
par(mfrow=c(2,3))
plot(PaysLoiBis$Emploi_Agriculture)
plot(PaysLoiBis$PIB_Hab)
plot(PaysLoiBis$Salarie)
plot(PaysLoiBis$Emploi_Service)
plot(PaysLoiBis$Emploi_Service)
plot(PaysLoiBis$Emploi_Vul)

#------------------------- Création et mise en place en facteur ------
PaysLoiBis$Pib_Bin<-sapply(PaysLoiBis$PIB_Hab, function(x){
  if(x>=mean(PaysLoiBis$PIB_Hab)){
    return(1)}
  else{
    return(0)}})
PaysLoiBis$Rural_Bin<-sapply(PaysLoiBis$Taux_Rural, function(x){
  if(x>=mean(PaysLoiBis$Taux_Rural)){
    return(1)}
  else{
    return(0)}})
PaysLoiBis$Vul_Bin<-sapply(PaysLoiBis$Taux_Rural, function(x){
  if(x>=mean(PaysLoiBis$Emploi_Vul)){
    return(1)}
  else{
    return(0)}})
PaysLoiBis$Sal_Bin<-sapply(PaysLoiBis$Salarie, function(x){
  if(x>=mean(PaysLoiBis$Salarie)){
    return(1)}
  else{
    return(0)}})
PaysLoiBis$Serv_Bin<-sapply(PaysLoiBis$Emploi_Service, function(x){
  if(x>=mean(PaysLoiBis$Emploi_Service)){
    return(1)}
  else{
    return(0)}})
PaysLoiBis$Pib_Bin <- as.factor(PaysLoiBis$Pib_Bin)
PaysLoiBis$Rural_Bin <- as.factor(PaysLoiBis$Rural_Bin)
PaysLoiBis$Vul_Bin <- as.factor(PaysLoiBis$Vul_Bin)
PaysLoiBis$Sal_Bin <- as.factor(PaysLoiBis$Sal_Bin)
PaysLoiBis$Serv_Bin <- as.factor(PaysLoiBis$Serv_Bin)
summary(PaysLoiBis)
#Expliquer pourquoi avoir rajouter ces variables pendant l'oral

#-------------- Nouveaux Modèles -----
#On recommence avec ces nouvelles variables créées 
regLoi <- lm(PaysLoiBis$Taux_Chomage ~ PaysLoiBis$Taux_Epargne +
               PaysLoiBis$Emploi_Service + PaysLoiBis$Emploi_Industrie +
               PaysLoiBis$Impot_Benef + PaysLoiBis$Impot_Travail + 
               PaysLoiBis$Coeff_Salaire + PaysLoiBis$Taux_Jeune_DescBIT +
               PaysLoiBis$Impot_Paiement + PaysLoiBis$Entreprise_Procedure +
               PaysLoiBis$Loi_Indemnite + PaysLoiBis$Loi_Periode_Essai + PaysLoiBis$Loi_Proc_Demission +
               PaysLoiBis$Conv_Prio_122 + PaysLoiBis$Conv_Prio_129 +
               PaysLoiBis$Conv_Syndic_087 + PaysLoiBis$Conv_Syndic_098 +
               PaysLoiBis$Conv_TravEnfant182 + PaysLoiBis$Conv_TravEnfant_138 + 
               PaysLoiBis$Conv_TravForce_029 + PaysLoiBis$Conv_TravForce_105 +
               PaysLoiBis$Agr_Bin + PaysLoiBis$Pib_Bin + PaysLoiBis$Sal_Bin +
               PaysLoiBis$Vul_Bin + PaysLoiBis$Serv_Bin + PaysLoiBis$Rural_Bin)
summary(regLoi)

regLoi <- lm(PaysLoiBis$Taux_Chomage ~ PaysLoiBis$Taux_Epargne +
               PaysLoiBis$Emploi_Service + PaysLoiBis$Emploi_Industrie +
               PaysLoiBis$Impot_Benef +  
               PaysLoiBis$Coeff_Salaire + 
               PaysLoiBis$Impot_Paiement + PaysLoiBis$Entreprise_Procedure +
               PaysLoiBis$Loi_Indemnite + PaysLoiBis$Loi_Periode_Essai + PaysLoiBis$Loi_Proc_Demission +
               PaysLoiBis$Conv_Prio_122 + PaysLoiBis$Conv_Prio_129 +
               PaysLoiBis$Conv_Syndic_087 + PaysLoiBis$Conv_Syndic_098 +
               PaysLoiBis$Conv_TravEnfant182 + PaysLoiBis$Conv_TravEnfant_138 + 
               PaysLoiBis$Conv_TravForce_029 + PaysLoiBis$Conv_TravForce_105 +
               PaysLoiBis$Pib_Bin + PaysLoiBis$Sal_Bin)
summary(regLoi)

regLoi <- lm(PaysLoiBis$Taux_Chomage ~ PaysLoiBis$Taux_Epargne +
               PaysLoiBis$Emploi_Service + 
               PaysLoiBis$Entreprise_Procedure +
               PaysLoiBis$Conv_TravForce_105 +
               PaysLoiBis$Pib_Bin + PaysLoiBis$Sal_Bin)
summary(regLoi)

regLoi <- lm(PaysLoiBis$Taux_Chomage ~ 
               PaysLoiBis$Pib_Bin + PaysLoiBis$Sal_Bin + 
               PaysLoiBis$Vul_Bin + PaysLoiBis$Serv_Bin + PaysLoiBis$Rural_Bin)
summary(regLoi)

regLoi <- lm(PaysLoiBis$Taux_Chomage ~ PaysLoiBis$PIB_Hab + PaysLoiBis$Emploi_Vul + PaysLoiBis$Taux_Rural + PaysLoiBis$Salarie + PaysLoiBis$Emploi_Agriculture + PaysLoiBis$Emploi_Service)
summary(regLoi)
