# --- On charge toutes les library dont on va avoir besoin ----
library(readxl)
library(Hmisc)
library(ggplot2)
library(EnvStats)
library(PerformanceAnalytics)
library(factoextra)
library(FactoMineR)
library(lmtest)
library(car)
library(outliers)
library(normtest)


# --- On charge la base de donn�es ----

base <- read_excel("D:/M1 EKAP/Semestre 1/Intro R/Bdd_Projet_IntroR.xlsx")
View(base)

# --- Nous verifions s'il n'y a pas de NA dans notre base de donn�es ----

summary (base)

  #il n'y a donc aucun NA sur notre base, nous allons pouvoir travailler dessus

# --- Nous calculons moyennes, m�dianes, ecart-tytpes ... ----

describe(base)


# --- Pr�-D�t�ction des Outliers � l'aide de BoxPlots ----
par(mfrow=c(3,4))
for(k in 3: 30){
  boxplot(base[,k], main=names(base[k]))
}
  #Nous pouvons voir un apercu des valeurs atypiques de nos diff�rentes variables grace au boxplot que nous avons r�alis�

# --- D�tection des valeurs aberrantes ----

rosnerTest(base$TX_CH, k = 5, alpha = 0.05)#0
grubbs.test(base$TX_CH,type=10, two.sided = TRUE)
  #Pas d'outliers pour cette variable
rosnerTest(base$SAL_NET_MOY, k = 10, alpha = 0.05)
grubbs.test(base$SAL_NET_MOY,type=10, two.sided = TRUE)
  # 5 outliers : 76-93-79-95-92
rosnerTest(base$SAL_CADRES, k = 6, alpha = 0.05)
  # 3 outliers :76-79-93
rosnerTest(base$SAL_PROF_INTER, k = 6, alpha = 0.05)
  # 3 outliers :76-93-79
rosnerTest(base$SAL_EMP, k = 10, alpha = 0.05)
  # 7 outliers :93-76-79-92-78-95-96
rosnerTest(base$SAL_OUV, k = 3, alpha = 0.05)
grubbs.test(base$SAL_OUV,type=10, two.sided = TRUE)
  # pas d'outliers pour cette variable
rosnerTest(base$POP, k = 5, alpha = 0.05)
grubbs.test(base$POP,type=10, two.sided = TRUE)
  # 1 outliers :60
rosnerTest(base$NBRE_FISC, k = 5, alpha = 0.05)
  # 4 outliers :6-57-76-60
rosnerTest(base$REV_FIS_REF, k = 5, alpha = 0.05)
grubbs.test(base$REV_FIS_REF,type=10, two.sided = TRUE)  
  # 2 outliers :76-93
rosnerTest(base$IMPOTS_NET, k = 9, alpha = 0.05)
  # 7 outliers :76-93-79-70-13-95-60
rosnerTest(base$NBRE_FISC_IMP, k = 5, alpha = 0.05)
  # 3 outliers :6-57-76
rosnerTest(base$REV_FIS_REF_IMP, k = 8, alpha = 0.05)
  # 6 outliers :76-93-79-70-13-60
rosnerTest(base$NBRE_ETA_SEC, k = 4, alpha = 0.05)
  # 3 outliers :60-76-13
rosnerTest(base$NBRE_ETA_PRI, k = 3, alpha = 0.05)
grubbs.test(base$NBRE_ETA_PRI,type=10, two.sided = TRUE)
  # 1 outliers :60
rosnerTest(base$PART_2559, k = 4, alpha = 0.05)
grubbs.test(base$PART_2559,type=10, two.sided = TRUE)
  # 1 outliers :76
rosnerTest(base$TX_PRI, k = 4, alpha = 0.05)
grubbs.test(base$TX_PRI,type=10, two.sided = TRUE)
  #pas d'outliers pour cette variable au seuil de risque 5%
rosnerTest(base$MTANT_MOY, k = 7, alpha = 0.05)
  # 5 outliers :76-93-25-79-75
rosnerTest(base$ALLOC_FAMILLE, k = 1, alpha = 0.05)
grubbs.test(base$ALLOC_FAMILLE,type=10, two.sided = TRUE)
  # pas d'outliers pour cette variable
rosnerTest(base$ALLOC_LOG, k = 3, alpha = 0.05)
grubbs.test(base$ALLOC_LOG,type=10, two.sided = TRUE)
  # 1 outliers :94
rosnerTest(base$NBRE_LOG_SOC, k = 4, alpha = 0.05)
grubbs.test(base$NBRE_LOG_SOC,type=10, two.sided = TRUE)
  # pas d'outliers pour cette variable
rosnerTest(base$TX_BAC, k = 3, alpha = 0.05)#0
grubbs.test(base$TX_BAC,type=10, two.sided = TRUE)
  # pas d'outliers pour cette variable


# --- Suppression des valeurs aberrantes ----

base<-base[-c(76,93,79,95,92,78,96,60,6,57,70,13,25,75,94),]
summary(base)
View(base)

# --- R�partition et Distribution des Donn�es----
par(mfrow=c(3,3))
hist(base)

plot(base$TX_CH~base$TX_IMP)

# --- Nous allons cr�er un ACP pour avoir une id�e des corr�lation de nos variables en ne mettant pas notre variable � expliquer ----

res.pca=PCA(base[,4:30])
dim(base)
str(base)
fviz_pca_ind(res.pca)
fviz_pca_var(res.pca,axes=c(1,3))
res.pca$eig
round(res.pca$eig,2)
# On garde 3 axes car le cumul en % de la variance est de 78%
# Variables
res.pca
# Contribution
round(res.pca$var$contrib,2)
# Corr�lations
dimdesc(res.pca)
# Cosinus
round(res.pca$var$cos2,2)

# Cercle de corr�lation par rapport � l'axe 1 et 2
plot.PCA(res.pca,tilte="Graph des obs",xlim=c(-7,6),axes=c(1,2),choix="var",cex=.8)
fviz_pca_var(res.pca, col.var="contrib", c(1,2), title="Cercle de corr�lation selon l'axe 1 & 2")+
  scale_color_gradient2(low="Green", mid="blue",
                        high="Red", midpoint=6, space ="Lab") + theme_minimal()

# Cercle de corr�lation par rapport � l'axe 1 et 3
plot.PCA(res.pca,tilte="Graph des obs",xlim=c(-7,6),axes=c(1,3),choix="var",cex=.8)
fviz_pca_var(res.pca, col.var="contrib", c(1,3), title="Cercle de corr�lation selon l'axe 2 & 3")+
  scale_color_gradient2(low="Green", mid="blue",
                        high="red", midpoint=7.5, space ="Lab") + theme_minimal()

# --- Nous allons v�rifier la normalit� des variables ----

shapiro.test(base$SAL_NET_MOY)
#H0 est rejet�e, au seuil de risque 5%, avec une p-value de 0.007797 -> ne suit pas une loi normale

shapiro.test(base$SAL_CADRES)
#H0 est accept�e, au seuil de risque 5%, p_value : 0.6056 -> suit une loi normale

shapiro.test(base$SAL_PROF_INTER)
#H0 est accept�e avec une p-value de 0.1839 -> suit une loi normale

shapiro.test(base$SAL_EMP)
#H0 est rejet�e avec une p-value de 0.00227 -> ne suit pas une loi normale

shapiro.test(base$SAL_OUV)
#H0 est accept�e avec une p_value : 0.4187 -> suit une loi normales

shapiro.test(base$POP)
#H0 est rejet�e avec une p-value de 3.646e-06

shapiro.test(base$NBRE_FISC)
#H0 est rejet�e avec une p-value de 3.831e-6

shapiro.test(base$REV_FIS_REF)
#H0 est rejet�e avec une p-value de 1.518e-06

shapiro.test(base$IMPOTS_NET)
#H0 est rejet�e avec une p-value � 3.089e-08

shapiro.test(base$NBRE_FISC_IMP)
#H0 est rejet�e avec une p-value � 1.033e-06

shapiro.test(base$REV_FIS_REF_IMP)
#H0 est rejet�e avec une p-value de 3.545e-07

shapiro.test(base$PART_024)
#H0 est accept�e avec une p-value de 0.0277, au seuil de risque 1%

shapiro.test(base$PART_2559)
#H0 est accept�e avec une p-value de 0.1924

shapiro.test(base$PART_60)
#Ho est accept�e avec une p-value de 0.5293

shapiro.test(base$TX_SEC)
#Ho est accept� au seuil de 5% avec une p-value de 0.09408

shapiro.test(base$TX_PRI)
#H0 est accept� avec une p_value : 0.04617 au seuil de risque 1%

shapiro.test(base$MTANT_MOY)
#H0 est accept�e avec une p-value de 0.05368, au seuil de risque 5%

shapiro.test(base$ALLOC_FAMILLE)
#H0 est accept�e avec une p-value de 0.3063

shapiro.test(base$ALLOC_LOG)
#H0 est accept�e avec une p-value de 0.1122

shapiro.test(base$NBRE_LOG_SOC)
#H0 est accept� avec une p-value de 0.01749, au seuil de risque 1%

shapiro.test(base$TX_BAC)
#H0 est accept�e avbec une p_value : 0.2988

shapiro.test(base$TX_FISC)
#H0 refus�e, p_value inferieur � 2.2e-16

shapiro.test(base$TX_FISC_IMP)
#H0 refus�e, p_value inferieur � 2.2e-16

shapiro.test(base$DIFF_TX_FISC)
#On accepte H0 au seuil de risque 1%, p_value : 0.01522

shapiro.test(base$TX_IMP)
#On refuse H0, p_value : 4.392e-15

#11 variables ne suivent pas une loi normales : SAL_NET_MOY | SAL_PROF_INTER | SAL_EMP | POP | NBRE_FISC | REV_FIS_REF | NBRE_FISC_IMP | REV_FIS_REF_IMP | TX_FISC | TX_FISC_IMP | TX_IMP
#Nous allons effectuer un tableau de corr�lation afin de determiner quelle sont les variables corr�l�s entre elles, et donc permettre l'�laboration de diff�rents mod�les


# --- Nous allons effectuer les corr�lations entre nos variables explicatives ----

mybase <- base[,
                  c("SAL_NET_MOY",	"SAL_CADRES",	"SAL_PROF_INTER",	"SAL_EMP",
                    "SAL_OUV",	"POP",	"NBRE_FISC",	"REV_FIS_REF",	"IMPOTS_NET",	"NBRE_FISC_IMP",
                    "REV_FIS_REF_IMP",	"NBRE_ETA_SEC",	"NBRE_ETA_PRI",	"PART_024",	"PART_2559",
                    "PART_60",	"TX_SEC",	"TX_PRI",	"MTANT_MOY",	"ALLOC_FAMILLE",	"ALLOC_LOG",
                    "NBRE_LOG_SOC",	"TX_BAC", "TX_IMP", "TX_FISC_IMP", "TX_FISC", "DIFF_TX_FISC"
)]
chart.Correlation(mybase, histogram=TRUE,labels=TRUE, pch=19,method = c("spearman"))
cor(mybase,method="spearman")
cor(mybase,use="complete.obs") #Correlation de Pearson


  #Nous pouvons remarquer grace aux �toiles que R signal en fonction de la corr�lation que celle-ci est forte entre toutes nos variables


# --- Nous allons pouvoir passer sur des r�gressions ---

reg1 <- lm(base$TX_CH~
             base$SAL_NET_MOY +
             base$SAL_CADRES +
             base$SAL_PROF_INTER +
             base$SAL_EMP + 
             base$SAL_OUV +
             base$POP +
             base$NBRE_FISC +
             base$REV_FIS_REF +
             base$IMPOTS_NET +
             base$NBRE_FISC_IMP +
             base$REV_FIS_REF_IMP +
             base$NBRE_ETA_SEC +
             base$NBRE_ETA_PRI +
             base$PART_024 +
             base$PART_2559 +
             base$PART_60 +
             base$TX_SEC +
             base$TX_PRI +
             base$MTANT_MOY +
             base$ALLOC_FAMILLE +
             base$ALLOC_LOG +
             base$NBRE_LOG_SOC +
             base$TX_BAC)
summary(reg1)

reg2 <- lm(base$TX_CH ~ 
             base$SAL_CADRES +
             base$SAL_OUV +
             base$IMPOTS_NET +
             base$PART_024 +
             base$PART_2559 +
             base$TX_SEC +
             base$TX_PRI +
             base$MTANT_MOY +
             base$ALLOC_FAMILLE +
             base$ALLOC_LOG +
             base$NBRE_LOG_SOC +
             base$TX_BAC + 
             base$DIFF_TX_FISC) #Modele avec les variables explicatives qui suivent une lois normales
summary(reg2)
vif(reg2)

#Les deux modeles suivant vont etre etudie afin de determiner le meilleur � etudier

reg3 <- lm(base$TX_CH ~ 
             base$SAL_OUV +
             base$PART_024 +
             base$TX_SEC +
             base$TX_PRI +
             base$MTANT_MOY +
             base$ALLOC_LOG +
             base$NBRE_LOG_SOC +
             base$TX_BAC) #Modele avec les variables explicatives qui suivent une lois normales
summary(reg3)

reg4 <- lm(base$TX_CH ~ 
             base$SAL_OUV +
             base$PART_024 +
             base$TX_SEC +
             base$TX_PRI +
             base$TX_BAC + 
             base$DIFF_TX_FISC +
             base$SAL_NET_MOY) #Modele avec les variables explicatives qui suivent une lois normales
summary(reg4)

# Test de Ramsey 
reset(reg3) #Au seuil de risque 5%, on accepte H0 donc la forme utilis� est la bonne
reset(reg4) #Au seuil de risque 5%, on accepte H0 donc la forme utilis� est la bonne

#Multicolin�arit�
vif(reg4) #Au vu du VIF aucune variable ne depasse 5, de plus lors de l'�laboration du mod�le nous avons fait en sorte de regrouper les variables qui ont le moins de colin�arit� entre elles

vif(reg3) #Au vu du VIF aucune variable ne depasse 10 cependant il faudra comparer les deux modele a l aide de l AIC afin de determiner le meilleur modele a utiliser

# AIC

AIC(reg4)
 
AIC(reg3)

#BIC

BIC(reg3)
BIC(reg4)
#On preferera le modele avec l AIC et le BIC le plus faible et le R� le plus eleve, on utilisera le modele reg3
#On utilisera par la suite le modele reg3

#Normalite des residus

shapiro.test(residuals(reg3)) #Les residus suivent une loi normale car on accepte H0

jb.norm.test(residuals(reg3)) #On accepte H0, les residus sont normalement distribu� mais on aurait pu l'accepter directement avec le test de shapiro wilk car plus puissant comme test

#Homosc�dasticit� 

bptest(reg3) #On accepte H0 donc hypothese d homoscedasticit�

#test de white

library(skedastic)
white_lm(reg3, interactions=TRUE)
#rejet de HO, les r�sidus sont h�t�rosc�dastique au seuil de 5%

gqtest(reg3) #Meme conclusion

#Auto correlation des residus
dwtest(reg3) #On accepte H0 donc les residus ne sont pas auto correles
#les residus sont donc independant


