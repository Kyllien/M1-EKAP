library(corrplot)
library(fields)
library(FactoMineR)
library(caret)
library(pls)
library(chemometrics)

# session / set working directory
setwd("C:/AEV/STODOC/EKAP/M1 PLSR/WINE_GCMS_FTIR")
load("WINES_GCMS_FTIR.Rdata")

#------------------------------------------------------
# label des échantillons
Id
origin=factor(Id[,3],labels=c("ARG","AUS","CHI","SOU"))

#------------------------------------------------------
# aromatic compounds (COV)
# ACP (normée) des COV
# besoin du package "FactoMineR"
respcaCOV=PCA(data.frame(origin,aroma),quali.sup=1)
plot(respcaCOV,axes=c(1,2),choix="ind",habillage=1)
# matrice des corrélations
# besoin du package "corrplot"
corrplot::corrplot(cor(aroma),method="color",order="FPC",tl.pos="n")

#------------------------------------------------------
# quality parameters
# ACP (normée) des paramètres de qualité
respcaQP=PCA(data.frame(origin,qparam),quali.sup=1)
plot(respcaQP,axes=c(1,2),choix="ind",habillage=1)
# matrice des corrélations
corrplot::corrplot(cor(qparam),method="color",order="FPC")

#------------------------------------------------------
# spectres IR
# 2 plages ont été exclues : 
#       1545-1710 cm-1 (entre col. 159 et 160) 
#       2968-3620 cm-1 (entre col. 484 et 485) 
plot(as.numeric(colnames(IR)),IR[1,],type="l", xlab="wavelength (cm-1)",ylab="abundance")
for (i in 2:nrow(IR))lines(as.numeric(colnames(IR)),IR[i,],col=i)
abline(v=1545); abline(v=1710)
abline(v=2968); abline(v=3620)
# Multiplicative Scatter Correction for IR
# fonction msc du package "pls"
IRmsc=msc(IR)
plot(as.numeric(colnames(IRmsc)),IRmsc[1,],type="l", xlab="wavelength (cm-1)",ylab="abundance")
for (i in 2:nrow(IRmsc)) lines(as.numeric(colnames(IRmsc)),IRmsc[i,],col=i)
abline(v=1545); abline(v=1710)
abline(v=2968); abline(v=3620)
# matrice des corrélations
#corrplot::corrplot(cor(IRmsc))   # trop long
# besoin du package "fields"
image.plot(1:ncol(IRmsc),1:ncol(IRmsc),cor(IRmsc),breaks=seq(-1,1,0.1),nlevel = 20)
abline(h=160,v=160,lwd=5)
abline(h=485,v=485,lwd=5)



############################################################################
# modélisation du paramètre "Ethanol" en fonction de la composition en COV 

# --------------------------------------------------------------------------
# séparation des échantillons de vin en
# un jeu de calibration (train set) 
# et un jeu de validation (test set)
# besoin du package "caret"
testset=createDataPartition(origin,p=0.3,times=1)$Resample1
ntest=length(testset)
n=nrow(aroma)
trainset=setdiff(1:n,testset)
ntrain=length(trainset)

# --------------------------------------------------------------------------
# spécification des données pour le modéle considéré
y=qparam[,1]
aroma.Etha <- data.frame(X=aroma,y=qparam[,1])
n=nrow(aroma.Etha)
p=ncol(aroma.Etha)-1

# --------------------------------------------------------------------------
# modèle de régression sur comp. principales (PCR)
# utilisation des fonctions du package "pls"
help(pcr)
modelpcr<-pcr(y~.,data=aroma.Etha[trainset,],scale=TRUE,ncomp=min(ntrain-1,p))    
summary(modelpcr)
R2(modelpcr)
help(predict.mvr)
ypredpcr=predict(modelpcr,newdata=aroma.Etha[testset,], type="response")
dim(ypredpcr)

help(RMSEP)
rmseppcr=RMSEP(modelpcr, estimate="all",newdata=aroma.Etha[testset,])
rmseppcr
plot(rmseppcr, main="PCR model",ylab="RMSEP")
legend("right",legend=c("calib","valid"),col=1:2,lty=1:2)

help(coef.mvr)
betapcr=coef(modelpcr,ncomp=1:min(ntrain-1,p))
dim(betapcr)
barplot(apply(betapcr^2,3,sum),las=2, main= "PCR / squared norm of the estimated beta coeff vector")

#Calcul du R2 = 1 - train² ; train du rmsep = erreur
#test = pas a utilise car test
#train = a utiliser
#Voir cours


# --------------------------------------------------------------------------
# modèle de régression PLS
# utilisation des fonctions du package "pls"

#...... à compléter en vous inspirant des fonctions mises en oeuvre
#...... pour l'ajustement du modèle PCR

# ******************************************************************************
# Choisissez un nombre de composantes "optimales" pour chacun des deux modèles
# que vous avez ajusté sur votre jeu de calibration 
# et validé sur le jeu de validation
# Reportez sur le questionnaire disponible sur la plateforme pédagogique, 
# le nb de composantes, la valeur du RSMEP et du R2 de validation 
# pour chacun des deux modèles. 
# ******************************************************************************

