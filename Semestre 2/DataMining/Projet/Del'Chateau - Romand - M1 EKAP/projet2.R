BiocManager::install('mixOmics')
library(readxl)
library(ggplot2)
library(FactoMineR)
library(DiscriMiner)
library(mixOmics)

base<-read_excel("D:/M1 EKAP/Semestre 2/DataMining/Projet/Base_pression_arterielle.xls")[,-1]
base<-as.data.frame(base)
summary(base)

View(base)
##Pour la varibale Pression -----------------------------------------------------------------

#Discrimination
res.desDA <- desDA(base[,-c(1,2)],as.integer(base[,2]), covar = "within")
summary(res.desDA)

round(res.desDA$power,3)
#Les variables les plus discriminates :
#IMC,Sport,Stress

round(res.desDA$discrivar,3)
#Coefficient de la function discriminante lin�aire


round(res.desDA$discor,3)
#une seule composante discrimantes pcq il y a deux groupes
#pas trop de corr�lation entre les variavbles et la composante iscrimante excpet�s IMC

round(res.desDA$values,3)

corRatio(res.desDA$scores[,1],base$Pression)
#La corr�lation est tr�s faible avec notre variable � expliquer

#validation crois�e
res.geoDA <- geoDA(base[,-c(1,2)],base$Pression,validation="crossval")

predict.total <- classify(res.geoDA,base[,-c(1,2)])

#Matrice de confusion
mat.confusion.app <- table(base$Pression,predict.total$pred_class)
mat.confusion.app
#Cette matrice nous dit que sur 500 individus 302 sont bine pr�dits
#pour ceux qui ont pas de pression, 102 sont mals pr�dits 
#pour ceux qui ont de la pression 100 sont mal pr�dits

#Taux d'erreur sur tt le jeu de donn�es
tx.err.app <- 1- sum(diag(mat.confusion.app)/sum(mat.confusion.app)) 
tx.err.app
#le taux d'erreur sur cet �chantillon est de 40.4%

#taux d'erreur en validation crois�
res.geoDA$error_rate
# en validation crois�, le taux d'erreur est de 44.6%

#courbe de roc optionnel
library(ROCR)
pred=prediction(res.desDA$scores ,base[,2])
perf=performance(pred,"tpr", "fpr")
plot(perf,colorize = TRUE)

#Mise en place de la matrice des variables explicatives et du vexteur de la matrice expliqu�e
X<-base[,-c(1,2)]
y<-base[,2]

#3 regles : centr�de, max-disc et mahalanobis (regle AFD)
#comment elle se comporte sur validation crois�



my_pls1<-plsDA(X, y, autosel=FALSE, cv="LKO",k=7)
dim(my_pls1$components)

my_pls1$confusion
#la matrice de confusion est diff�rente
my_pls1$error_rate
#le m�me taux d'erreur 
### partie pls
my_pls1$Q2
my_pls1$VIP
#>1 impact le plus important, IMC et Sport

err.rate <- vector("numeric",length=9)
for (h in 2:10) {
  my_pls1 = plsDA(X, y, autosel=FALSE, comps=h,cv="LKO",k=7)
  #my_pls1 = plsDA(X, y, autosel=FALSE, comps=k) 
  err.rate[h-1] <- my_pls1$error_rate
}
err.rate
#taux d'erreur par rapport aux diff�rent nombre de composante

#choix deux composantes
my_pls1 = plsDA(X, y, autosel=FALSE, comps=2,cv="LKO",k=7)
my_pls1$confusion
my_pls1$error_rate

set.seed(2543) # for reproducibility, only when the `cpus' argument is not used
perf.plsda_1 <- perf(my_pls1, validation = "Mfold", folds = 7, 
                     progressBar = FALSE, auc = TRUE, nrepeat = 5) 

#plot de la discrimination de la variable Pression
base$f1 = my_pls1$components[,1]
base$f2 = my_pls1$components[,2]
base$row = substr(rownames(base),1,2)
ggplot(data=base, aes(x=f1, y=f2,)) +
  geom_hline(yintercept=0, colour="gray70") +
  geom_vline(xintercept=0, colour="gray70") +
  geom_text(aes(label=row), size=4) +
  ggtitle("Discriminant Map - Pression")




#Pour pression arterielle --------------------------------------------------------

res.desDA <- desDA(base[,-c(1,2)],as.integer(base[,1]), covar = "within")
summary(res.desDA)
library(stargazer)
stargazer(corRatio(res.desDA$scores[,1],base$Press_arter),corRatio(res.desDA$scores[,2],base$Press_arter), corRatio(res.desDA$scores[,3],base$Press_arter), out="doc.txt",type="text")
round(res.desDA$power,3)
#Les variables les plus discriminates :
#Genre/FUmer/Sport

round(res.desDA$discrivar,3)


round(res.desDA$discor,3)
#trois composante discrimantes pcq il y a quatre groupes
#pas trop de corr�lation entre les variavbles et la composante iscrimante excpet�s IMC
#Age a une corr�lation avec la 3eme composante

round(res.desDA$values,3)

corRatio(res.desDA$scores[,1],base$Press_arter)
#La corr�lation est tr�s faible avec notre variable � expliquer

#validation crois�e
res.geoDA <- geoDA(base[,-c(1,2)],base$Press_arter,validation="crossval")
summary(res.geoDA)

predict.total <- classify(res.geoDA,base[,-c(1,2)])

#Matrice de confusion
mat.confusion.app <- table(base$Press_arter,predict.total$pred_class)
write.table(mat.confusion.app, file="doc.txt")
#187 individus sont bine pr�dits, le reste est donc mal pr�dit

#Taux d'erreur sur tt le jeu de donn�es
tx.err.app <- 1- sum(diag(mat.confusion.app)/sum(mat.confusion.app))
tx.err.app
#le taux d'erreur sur cet �chantillon est de 62.6%

#taux d'erreur en validation crois�
res.geoDA$error_rate
#tax d'erreur en validation crois� est de 71.4%


#Mise en place de la matrice des variables explicatives et du vexteur de la matrice expliqu�e
X<-base[,-c(1,2)]
Y<-base[,1]

my_pls1<-plsDA(X, Y, autosel=FALSE, cv="LKO",k=7)
dim(my_pls1$components)
write.table(my_pls1$confusion, file="doc.txt")
#la matrice de confusion est diff�rente
my_pls1$error_rate
#le m�me taux d'erreur 
### partie pls
my_pls1$Q2
stargazer(my_pls1$VIP, type="text", out="doc.txt")
#>1 impact le plus important, IMC et Sport

err.rate <- vector("numeric",length=9)
for (h in 2:10) {
  my_pls1 = plsDA(X, Y, autosel=FALSE, comps=h,cv="LKO",k=7)
  #my_pls1 = plsDA(X, y, autosel=FALSE, comps=k) 
  err.rate[h-1] <- my_pls1$error_rate
}
err.rate
#taux d'erreur par rapport aux diff�rent nombre de composante

#choix deux composantes
my_pls1 = plsDA(X, Y, autosel=FALSE, comps=2,cv="LKO",k=7)
my_pls1$confusion
my_pls1$error_rate

#plot de la discrimination de la variable Pression
base$f1 = my_pls1$components[,1]
base$f2 = my_pls1$components[,2]
base$row = substr(rownames(base),1,2)
ggplot(data=base, aes(x=f1, y=f2,)) +
  geom_hline(yintercept=0, colour="gray70") +
  geom_vline(xintercept=0, colour="gray70") +
  geom_text(aes(label=row), size=4) +
  ggtitle("Discriminant Map - Pression")


