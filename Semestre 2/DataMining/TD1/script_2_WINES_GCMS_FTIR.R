library(pls)
library(chemometrics)
library(corrplot)

# session / set working directory
setwd("D:/M1 EKAP/Semestre 2/DataMining/TD1")
load("WINES_GCMS_FTIR.Rdata")

source("VIP.R")

#------------------------------------------------------
# label des échantillons
origin=factor(Id[,3],labels=c("ARG","AUS","CHI","SOU"))



############################################################################
# modélisation du paramètre "Ethanol" en fonction de la composition en COV 

# --------------------------------------------------------------------------
# spécification des données pour le modéle considéré
y=qparam[,1]
aroma.Etha <- data.frame(X=aroma,y=qparam[,1])
n=nrow(aroma.Etha)
p=ncol(aroma.Etha)-1


#--------------------------------------------------------------------------------------
#  PLS Regression & k-fold CV
help(plsr)
help(crossval)

plsLOO=plsr(y~.,data=aroma.Etha,scale=TRUE,ncomp=30,validation="LOO")
validationplot(plsLOO,estimate=c("train","CV"))
rmsepLOO=RMSEP(plsLOO,estimate=c("train","CV"))

TABRMSEP=NULL
for (essai in 1:100) {
  plskfcvr=plsr(y~.,data=aroma.Etha,scale=TRUE,ncomp=30,validation="CV",
                segments=5,segment.type='random')
  rmsepplsr=RMSEP(plskfcvr,estimate=c("train","CV"))
  TABRMSEP=rbind(TABRMSEP,rmsepplsr$val[2,1,])
}
boxplot(TABRMSEP,ylab="RMSEP",las=2)
lines(rmsepLOO$val[2,1,],col=1,lwd=3)
apply(TABRMSEP,2,mean)


#--------------------------------------------------------------------------------------
# PCR & k-fold CV

pcrLOO=pcr(y~.,data=aroma.Etha,scale=TRUE,ncomp=30,validation="LOO")
validationplot(pcrLOO,estimate=c("train","CV"))
rmsepLOOpcr=RMSEP(pcrLOO,estimate=c("train","CV"))

TABRMSEPpcr=NULL
for (essai in 1:100) {
  pcrkfcvr=pcr(y~.,data=aroma.Etha,scale=TRUE,ncomp=30,validation="CV",
               segments=5,segment.type='random')
  rmseppcrr=RMSEP(pcrkfcvr,estimate=c("train","CV"))
  TABRMSEPpcr=rbind(TABRMSEPpcr,rmseppcrr$val[2,1,])
}
boxplot(TABRMSEPpcr,ylab="RMSEP",las=2)
lines(rmsepLOOpcr$val[2,1,],col=1,lwd=3)
apply(TABRMSEPpcr,2,mean)


#--------------------------------------------------------------------------------------
# PLS regression & repeated double CV
help(mvr_dcv)

nrep=100
resplsdcv <- mvr_dcv(y~., data=aroma.Etha, ncomp=20, method="oscorespls", 
                         scale=TRUE,plot.opt=TRUE,
                         repl=nrep,segments=4, segments0=10, 
                         segment.type="random",segment0.type="random",
                         sdfact=1,selstrat="hastie",trace = FALSE)
# graphique de la fréquence du nb optimal de composantes sur l'ensemble des CV
plotcompmvr(resplsdcv)               # voir aussi le contenu de  resplsdcv$optcomp

# courbes des SEP (en fct du n de composantes)
# évalués dans la boucle externe, pour chacune des répétitions
SEPplsdcv=plotSEPmvr(resplsdcv,optcomp=1,y=y,X=aroma,complete = TRUE)
boxplot(SEPplsdcv$SEPdcv,ylab="SEP dcv",las=2)
# ATTENTION ici signifie "Standard Error of Prediction"
# équivalent à la notation RMSEP du cours
# au final pour la "repeated double CV"

# récupération des résultats pour choisir un autre nb de comp.
CUBplsdcv_pred=drop(resplsdcv$pred)   # size (n x Amax X nrep)
CUBplsdcv_resid=CUBplsdcv_pred-y
MATSEPdcv=sqrt(apply(CUBplsdcv_resid^2,c(2,3),mean)*n/(n-1))
matplot(MATSEPdcv,type="l")
boxplot(t(MATSEPdcv),ylab="SEP dcv",las=2)

A=3
# histogramme des résidus
hist(c(CUBplsdcv_resid[,A,]),main=paste("histogram of résiduals (CV), PLSR A=",A))
# predits vs obs
plot(y,apply(CUBplsdcv_pred[,A,],1,mean),pch="+",xlim=c(12.5,15.5),ylim=c(12.5,15.5))
for (r in 1:nrep) points(y,CUBplsdcv_pred[,A,r],pch='.',col="gray")
abline(a=0,b=1)
# SEPfinal calculé sur l'ensemble des résidus de CV
Afinal=A
SEPfinal=sqrt(mean(CUBplsdcv_resid[,A,]^2))
print(paste("retained solution, SEP=",round(SEPfinal,4),"for A",Afinal,"PLS components"))



# --------------------------------------------------------------------------
# Interprétation de modèle, variables "importantes"  
# PLS regression sur l'ensemble du jeu de données, 
# avec le choix d'un nb de composantes "optimal"
A=3
respls<-plsr(y~.,data=aroma.Etha,scale=TRUE,ncomp=A,method="oscorespls")
summary(respls)
# -------------------------------------------------------------------------
# graphiques
# "scores plot", composantes PLS 1 et 2
scoreplot(respls,comp=1:2,col=Id$class,pch=19,main="scores")
legend("bottomright",col=1:4, pch=19,c("ARG","AUS","CHI","SOU"))
abline(h=0,v=0)
# "loadings plot", composantes PLS 1 et 2
loadingplot(respls,comp=1:2, scatter = TRUE,pch="+",labels=1:p,main="loadings")
abline(h=0,v=0)
# loadingplot(respls,comp=1:2, scatter = TRUE,pch="+",identify=TRUE)
# abline(h=0,v=0)
pls::corrplot(respls,comp=1:2, plotx=TRUE, ploty=TRUE, labels=c(1:p,"Ethanol"),main="correlations")

# -------------------------------------------------------------------------
# VIP des COV (nb de composantes déterminé)
vip=VIP(respls)[A,]
par(mar=c(8,4,4,2)+0.1)
barplot(vip,las=2,ylab="VIP",cex.names=0.7)
abline(h=1,col=2)
# correlation entre les variables dont VIP>1.3
corrplot::corrplot(cor(aroma[,which(vip>1.3)]),method="color",order="FPC")

# -------------------------------------------------------------------------
# coefficients beta et jack-knife incertitude (nb de composantes déterminé)
betapls=coef(respls)[,1,1]
barplot(betapls,las=2,ylab="coeff. beta",cex.names=0.7)
betapls
#...... référez vous à la page 35 de poly de cours
#...... et mettez en oeuvre la procédure jack-knife
# ..... pour l'évaluation de l'incertitude associée aux coefficients
help(jack.test)
jack.test(aroma.Etha, ncomp=3)
respls
