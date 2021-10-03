library(readxl)
library(stringr)
library(EnvStats)
library(ggplot2)

# Importation base ----
setwd("D:/M1 EKAP/Semestre 2/Econometrie Variable Quali/Projet")
base <- read_excel("base.xlsx")

#Split les reponses a choix multiples ----
summary(base)
#Split PP AXE
axe <- strsplit(base$PP_Axe, ", ")
axe <- lapply(axe, as.numeric)
axedata <- data.frame(t(sapply(axe, function(x) as.numeric(1:11 %in% x)))) 
colnames(axedata) <- c("Redressement.PP","Inegalite.PP","Ecologie.PP","Securite.PP","Immigration.PP","RevenuUniv.PP","NouvelleRep.PP","PouvoirAchat.PP","Souverainete.PP","Aucun.PP","Non.PP")

#Split Media Type
media <- strsplit(base$Media_Type, ", ")
media <- lapply(media, as.numeric)
mediadata <- data.frame(t(sapply(media, function(x) as.numeric(1:8 %in% x)))) 
colnames(mediadata) <- c("JournalTv","Réseaux","JournalPap","ChaineInfo","Appli","Radio","Internet","Aucun")


#Factor les variables dans une nouvelle data ----
base2 <- base[,-c(5,14,31)] #???On enleve media type et axe pp car seront rajoute avec les split pour pouvoir etre etudie
#On enleve aussi plus 18 ans car tous les sonde auront plus de 18 ans en 2022 (ininterressant à etudier)
for (i in 5:29){
  command <- str_c("base2$",names(base2[i])," <- as.factor(base2$",names(base2[i]),")")
  eval(parse(text = command))
}
summary(base2)

#Factor des split
for (i in 1:11){
  command <- str_c("axedata$",names(axedata[i])," <- as.factor(axedata$",names(axedata[i]),")")
  eval(parse(text = command))
}
summary(axedata)

for (i in 1:8){
  command <- str_c("mediadata$",names(mediadata[i])," <- as.factor(mediadata$",names(mediadata[i]),")")
  eval(parse(text = command))
}
summary(mediadata)


#Plot des split ----
par(mfrow=c(2,2))
#media principal
for (i in 1:8){
  command <- str_c("plot(mediadata$",names(mediadata[i]),",main = '",names(mediadata[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}

#Axe PP
for (i in 1:11){
  command <- str_c("plot(axedata$",names(axedata[i]),",main = '",names(axedata[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}

#Plot base normal sans split ----
par(mfrow=c(2,2))
for (i in 2:29){
  command <- str_c("plot(base2$",names(base2[i]),",main = '",names(base2[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}

#Concatenation de la base avec les split precedent ----
base3 <- cbind(base2,axedata,mediadata)

#plot par age ----

baseAge5 <- subset(base3, base3$Age == 5)
baseAge2 <- subset(base3, base3$Age == 2)
baseAge3 <- subset(base3, base3$Age == 3)
baseAge4 <- subset(base3, base3$Age == 4)
baseAge6 <- subset(base3, base3$Age == 6)

sessionInfo()

#Age 2
par(mfrow=c(2,2))
for (i in 2:48){
  command <- str_c("plot(baseAge2$",names(baseAge2[i]),",main = '",names(baseAge2[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}

#Age3
for (i in 2:48){
  command <- str_c("plot(baseAge3$",names(baseAge3[i]),",main = '",names(baseAge3[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}

#Age4
for (i in 2:48){
  command <- str_c("plot(baseAge4$",names(baseAge4[i]),",main = '",names(baseAge4[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}

#Age5
for (i in 2:48){
  command <- str_c("plot(baseAge5$",names(baseAge5[i]),",main = '",names(baseAge5[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}

#Age 6 
for (i in 2:48){
  command <- str_c("plot(baseAge6$",names(baseAge6[i]),",main = '",names(baseAge6[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}

#Plot en fonciton des tranches de revenu ----
summary(base3)
baseRevenu1 <- subset(base3, base3$Revenu == 1)
baseRevenu2 <- subset(base3, base3$Revenu == 2)
baseRevenu3 <- subset(base3, base3$Revenu == 3 | base3$Revenu == 4)
baseRevenu5 <- subset(base3, base3$Revenu == 5)
baseRevenu6 <- subset(base3, base3$Revenu == 6)
baseRevenu7 <- subset(base3, base3$Revenu == 7)
baseRevenu8 <- subset(base3, base3$Revenu == 8 | base3$Revenu == 9)


#Revenu 1
for (i in 2:48){
  command <- str_c("plot(baseRevenu1$",names(baseRevenu1[i]),",main = '",names(baseRevenu1[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}
#Revenu 2
for (i in 2:48){
  command <- str_c("plot(baseRevenu2$",names(baseRevenu2[i]),",main = '",names(baseRevenu2[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}
#Revenu 3 et 4
for (i in 2:48){
  command <- str_c("plot(baseRevenu3$",names(baseRevenu3[i]),",main = '",names(baseRevenu3[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}

#Revenu 5
for (i in 2:48){
  command <- str_c("plot(baseRevenu5$",names(baseRevenu5[i]),",main = '",names(baseRevenu5[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}
#Revenu 6
for (i in 2:48){
  command <- str_c("plot(baseRevenu6$",names(baseRevenu6[i]),",main = '",names(baseRevenu6[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}
#Revenu 7
for (i in 2:48){
  command <- str_c("plot(baseRevenu7$",names(baseRevenu7[i]),",main = '",names(baseRevenu7[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}
#Revenu 8 et 9
for (i in 2:48){
  command <- str_c("plot(baseRevenu8$",names(baseRevenu8[i]),",main = '",names(baseRevenu8[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}


#Plot en fonction des axes PP ----
baseRed <- subset(base3, base3$Redressement == 1)
baseIne <- subset(base3, base3$Inegalite == 1)
baseEco <- subset(base3, base3$Ecologie == 1)
baseSec <- subset(base3, base3$Securite ==1)
baseIm <- subset(base3, base3$Immigration == 1)
baseRev <- subset(base3, base3$RevenuUniv == 1)
baseNou <- subset(base3, base3$NouvelleRep == 1)
basePou <- subset(base3, base3$PouvoirAchat == 1)
baseSouv <- subset(base3, base3$Souverainete == 1)
#PLot PP, Age, Etude Nv, Revenu
for (i in c(2,22,25,28)){
  command <- str_c("plot(baseRed$",names(baseRed[i]),",main = '",names(baseRed[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}
for (i in c(2,22,25,28)){
  command <- str_c("plot(baseIne$",names(baseIne[i]),",main = '",names(baseIne[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}
for (i in c(2,22,25,28)){
  command <- str_c("plot(baseEco$",names(baseEco[i]),",main = '",names(baseEco[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}
for (i in c(2,22,25,28)){
  command <- str_c("plot(baseSec$",names(baseSec[i]),",main = '",names(baseSec[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}
for (i in c(2,22,25,28)){
  command <- str_c("plot(baseIm$",names(baseIm[i]),",main = '",names(baseIm[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}
for (i in c(2,22,25,28)){
  command <- str_c("plot(baseRev$",names(baseRev[i]),",main = '",names(baseRev[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}
for (i in c(2,22,25,28)){
  command <- str_c("plot(baseNou$",names(baseNou[i]),",main = '",names(baseNou[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}
for (i in c(2,22,25,28)){
  command <- str_c("plot(basePou$",names(basePou[i]),",main = '",names(basePou[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}
for (i in c(2,22,25,28)){
  command <- str_c("plot(baseSouv$",names(baseSou[i]),",main = '",names(baseSouv[i]),"',ylab='Nombre Individus')")
  eval(parse(text = command))
}

#Decoupage en fonction des votes ----
basePCF <- subset(base3, base3$PP==1)
baseLFI <- subset(base3, base3$PP==2)
basePS <- subset(base3, base3$PP==3)
baseEELV <- subset(base3, base3$PP==4)
baseDVG <- subset(base3, base3$PP==5)
baseLREM <- subset(base3, base3$PP==6)
baseMoDem <- subset(base3, base3$PP==7)
baseMr <- subset(base3, base3$PP==8)
baseLr <- subset(base3, base3$PP==9)
baseUdi <- subset(base3, base3$PP==10)
baseAgir <- subset(base3, base3$PP==11)
baseDVD <- subset(base3, base3$PP==12)
baseRn <- subset(base3, base3$PP==13)
baseLp <- subset(base3, base3$PP==14)
baseNoPol <- subset(base3, base3$PP==15)
baseNoRep <- subset(base3, base3$PP==16)
baseNoVot <- subset(base3, base3$PP==19)


summary(basePCF)
summary(baseLFI)
summary(basePS)
summary(baseDVG)
summary(baseEELV)
summary(baseLREM)
summary(baseMoDem)
summary(baseMr)
summary(baseLr)
summary(baseUdi)
summary(baseDVD)
summary(baseRn)
summary(baseLp)
summary(baseNoPol)
summary(baseNoRep)
summary(baseNoVot)


#Creation de la table AxePP ----

AxePP.1 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- ((as.numeric(axedata[i,2])-1)  + (as.numeric(axedata[i,6])-1) + (as.numeric(axedata[i,6])-1))
  AxePP.1 <- rbind(AxePP.1,vec)
}
names(AxePP.1) <- "AxePP.1"

AxePP.2 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- ((as.numeric(axedata[i,1])-1)*(1/3)  + (as.numeric(axedata[i,2])-1)*(2/3) + (as.numeric(axedata[i,3])-1)*(2/3) + (as.numeric(axedata[i,7])-1)*(2/3))
  AxePP.2 <- rbind(AxePP.2,vec)
}
names(AxePP.2) <- "AxePP.2"

AxePP.3 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- ((as.numeric(axedata[i,1])-1)*(1/10)  + (as.numeric(axedata[i,2])-1) + (as.numeric(axedata[i,3])-1)*(3/10) + (as.numeric(axedata[i,4])-1)*(1/5) + (as.numeric(axedata[i,5])-1)*(1/5) + (as.numeric(axedata[i,6])-1)*(1/10) + (as.numeric(axedata[i,8])-1)*(1/10))
  AxePP.3 <- rbind(AxePP.3,vec)
}
names(AxePP.3) <- "AxePP.3"

AxePP.4 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- ((as.numeric(axedata[i,1])-1)*(1/15)  + (as.numeric(axedata[i,2])-1)*(3/5) + (as.numeric(axedata[i,3])-1)*(4/5) + (as.numeric(axedata[i,4])-1)*(1/15) + (as.numeric(axedata[i,7])-1)*(1/15) + (as.numeric(axedata[i,6])-1)*(1/15) + (as.numeric(axedata[i,8])-1)*(1/15))
  AxePP.4 <- rbind(AxePP.4,vec)
}
names(AxePP.4) <- "AxePP.4"

AxePP.5 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- ((as.numeric(axedata[i,3])-1)  + (as.numeric(axedata[i,4])-1))
  AxePP.5 <- rbind(AxePP.5,vec)
}
names(AxePP.5) <- "AxePP.5"

AxePP.6 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- ((as.numeric(axedata[i,1])-1)*(9/19)  + (as.numeric(axedata[i,2])-1)*(5/19) + (as.numeric(axedata[i,3])-1)*(8/19) + (as.numeric(axedata[i,4])-1)*(6/19) + (as.numeric(axedata[i,5])-1)*(2/19) + (as.numeric(axedata[i,7])-1)*(1/19) + (as.numeric(axedata[i,8])-1)*(2/19) + (as.numeric(axedata[i,9])-1)*(1/19))
  AxePP.6 <- rbind(AxePP.6,vec)
}
names(AxePP.6) <- "AxePP.6"

AxePP.7 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- ((as.numeric(axedata[i,2])-1))
  AxePP.7 <- rbind(AxePP.7,vec)
}
names(AxePP.7) <- "AxePP.7"

AxePP.8 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- ((as.numeric(axedata[i,1])-1))
  AxePP.8 <- rbind(AxePP.8,vec)
}
names(AxePP.8) <- "AxePP.8"

AxePP.9 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- ((as.numeric(axedata[i,1])-1)*(10/19)  + (as.numeric(axedata[i,2])-1)*(3/19) + (as.numeric(axedata[i,4])-1)*(8/19) + (as.numeric(axedata[i,5])-1)*(4/19) + (as.numeric(axedata[i,7])-1)*(1/19) + (as.numeric(axedata[i,8])-1)*(3/19))
  AxePP.9 <- rbind(AxePP.9,vec)
}
names(AxePP.9) <- "AxePP.9"

AxePP.10 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- ((as.numeric(axedata[i,2])-1))
  AxePP.10 <- rbind(AxePP.10,vec)
}
names(AxePP.10) <- "AxePP.10"

AxePP.11 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- 0
  AxePP.11 <- rbind(AxePP.11,vec)
}
names(AxePP.11) <- "AxePP.11"

AxePP.12 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- ((as.numeric(axedata[i,1])-1)*(3/5)  + (as.numeric(axedata[i,2])-1)*(4/5) + (as.numeric(axedata[i,3])-1)*(2/5) + (as.numeric(axedata[i,4])-1)*(2/5))
  AxePP.12 <- rbind(AxePP.12,vec)
}
names(AxePP.12) <- "AxePP.12"

AxePP.13 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- ((as.numeric(axedata[i,1])-1)*(1/4)  + (as.numeric(axedata[i,2])-1)*(1/8) + (as.numeric(axedata[i,3])-1)*(1/8) + (as.numeric(axedata[i,4])-1)*(5/8) + (as.numeric(axedata[i,5])-1)*(7/8) + (as.numeric(axedata[i,7])-1)*(3/8))
  AxePP.13 <- rbind(AxePP.13,vec)
}
names(AxePP.13) <- "AxePP.13"

AxePP.14 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- ((as.numeric(axedata[i,9])-1))
  AxePP.14 <- rbind(AxePP.14,vec)
}
names(AxePP.14) <- "AxePP.14"

AxePP.15 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- ((as.numeric(axedata[i,1])-1)*(10/31)  + (as.numeric(axedata[i,2])-1)*(43/93) + (as.numeric(axedata[i,3])-1)*(43/93) + (as.numeric(axedata[i,4])-1)*(24/93) + (as.numeric(axedata[i,6])-1)*(1/31) + (as.numeric(axedata[i,5])-1)*(10/93) + (as.numeric(axedata[i,7])-1)*(1/31) + (as.numeric(axedata[i,8])-1)*(17/93) + (as.numeric(axedata[i,9])-1)*(2/93) + (as.numeric(axedata[i,10])-1)*(2/31) + (as.numeric(axedata[i,4])-1)*(1/93))
  AxePP.15 <- rbind(AxePP.15,vec)
}
names(AxePP.15) <- "AxePP.15"

AxePP.16 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- ((as.numeric(axedata[i,1])-1)*(1/3)  + (as.numeric(axedata[i,2])-1)*(2/9) + (as.numeric(axedata[i,3])-1)*(1/3) + (as.numeric(axedata[i,4])-1)*(2/9) + (as.numeric(axedata[i,5])-1)*(2/9) + (as.numeric(axedata[i,6])-1)*(1/9) + (as.numeric(axedata[i,8])-1)*(4/9))
  AxePP.16 <- rbind(AxePP.16,vec)
}
names(AxePP.16) <- "AxePP.16"

AxePP.17 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- 0
  AxePP.17 <- rbind(AxePP.17,vec)
}
names(AxePP.17) <- "AxePP.17"

AxePP.18 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- 0
  AxePP.18 <- rbind(AxePP.18,vec)
}
names(AxePP.18) <- "AxePP.18"

AxePP.19 <- data.frame()
for (i in 1:nrow(base3)){
  vec <- ((as.numeric(axedata[i,2])-1))
  AxePP.19 <- rbind(AxePP.19,vec)
}
names(AxePP.19) <- "AxePP.19"

#cbind AxePP

AxePP <- AxePP.1
for (i in 2:19){
  command <- str_c("AxePP <- cbind(AxePP,AxePP.",i,")")
  eval(parse(text = command))
}

#Codage Parent1PP ----
#Parent 1 PCF
Parent1PP.1 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==1){
    Parent1PP.1 <- rbind(Parent1PP.1,1)
  }
  else{
    Parent1PP.1 <- rbind(Parent1PP.1,0)
  }
}
names(Parent1PP.1) <- "Parent1PP.1"

#Parent 1 LFI
Parent1PP.2 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==2){
    Parent1PP.2 <- rbind(Parent1PP.2,1)
  }
  else{
    Parent1PP.2 <- rbind(Parent1PP.2,0)
  }
}
names(Parent1PP.2) <- "Parent1PP.2"

#parent1 PS
Parent1PP.3 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==3){
    Parent1PP.3 <- rbind(Parent1PP.3,1)
  }
  else{
    Parent1PP.3 <- rbind(Parent1PP.3,0)
  }
}
names(Parent1PP.3) <- "Parent1PP.3"

#parent1 EELV
Parent1PP.4 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==4){
    Parent1PP.4 <- rbind(Parent1PP.4,1)
  }
  else{
    Parent1PP.4 <- rbind(Parent1PP.4,0)
  }
}
names(Parent1PP.4) <- "Parent1PP.4"

#parent 1 DVG
Parent1PP.5 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==5){
    Parent1PP.5 <- rbind(Parent1PP.5,1)
  }
  else{
    Parent1PP.5 <- rbind(Parent1PP.5,0)
  }
}
names(Parent1PP.5) <- "Parent1PP.5"

#parent1 LREM
Parent1PP.6 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==6){
    Parent1PP.6 <- rbind(Parent1PP.6,1)
  }
  else{
    Parent1PP.6 <- rbind(Parent1PP.6,0)
  }
}
names(Parent1PP.6) <- "Parent1PP.6"

#parent 1 MoDem
Parent1PP.7 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==7){
    Parent1PP.7 <- rbind(Parent1PP.7,1)
  }
  else{
    Parent1PP.7 <- rbind(Parent1PP.7,0)
  }
}
names(Parent1PP.7) <- "Parent1PP.7"

#Parent 1 MR
Parent1PP.8 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==8){
    Parent1PP.8 <- rbind(Parent1PP.8,1)
  }
  else{
    Parent1PP.8 <- rbind(Parent1PP.8,0)
  }
}
names(Parent1PP.8) <- "Parent1PP.8"

#Parent 1 LR
Parent1PP.9 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==9){
    Parent1PP.9 <- rbind(Parent1PP.9,1)
  }
  else{
    Parent1PP.9 <- rbind(Parent1PP.9,0)
  }
}
names(Parent1PP.9) <- "Parent1PP.9"

#Parent 1 UDI
Parent1PP.10 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==10){
    Parent1PP.10 <- rbind(Parent1PP.10,1)
  }
  else{
    Parent1PP.10 <- rbind(Parent1PP.10,0)
  }
}
names(Parent1PP.10) <- "Parent1PP.10"

#Parent 1 Agir
Parent1PP.11 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==11){
    Parent1PP.11 <- rbind(Parent1PP.11,1)
  }
  else{
    Parent1PP.11 <- rbind(Parent1PP.11,0)
  }
}
names(Parent1PP.11) <- "Parent1PP.11"

#Parent 1 DVD
Parent1PP.12 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==12){
    Parent1PP.12 <- rbind(Parent1PP.12,1)
  }
  else{
    Parent1PP.12 <- rbind(Parent1PP.12,0)
  }
}
names(Parent1PP.12) <- "Parent1PP.12"

#Parent 1 RN
Parent1PP.13 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==13){
    Parent1PP.13 <- rbind(Parent1PP.13,1)
  }
  else{
    Parent1PP.13 <- rbind(Parent1PP.13,0)
  }
}
names(Parent1PP.13) <- "Parent1PP.13"

#Parent 1 LP
Parent1PP.14 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==14){
    Parent1PP.14 <- rbind(Parent1PP.14,1)
  }
  else{
    Parent1PP.14 <- rbind(Parent1PP.14,0)
  }
}
names(Parent1PP.14) <- "Parent1PP.14"

#Parent 1 aucun parti
Parent1PP.15 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==15){
    Parent1PP.15 <- rbind(Parent1PP.15,1)
  }
  else{
    Parent1PP.15 <- rbind(Parent1PP.15,0)
  }
}
names(Parent1PP.15) <- "Parent1PP.15"

#Parent 1 ne veux pas répondre 
Parent1PP.16 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==16){
    Parent1PP.16 <- rbind(Parent1PP.16,1)
  }
  else{
    Parent1PP.16 <- rbind(Parent1PP.16,0)
  }
}
names(Parent1PP.16) <- "Parent1PP.16"

#Parent 1 Ne sais pas
Parent1PP.17 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==17){
    Parent1PP.17 <- rbind(Parent1PP.17,1)
  }
  else{
    Parent1PP.17 <- rbind(Parent1PP.17,0)
  }
}
names(Parent1PP.17) <- "Parent1PP.17"

#Parent 1 vote dans un autre pays 
Parent1PP.18 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==18){
    Parent1PP.18 <- rbind(Parent1PP.18,1)
  }
  else{
    Parent1PP.18 <- rbind(Parent1PP.18,0)
  }
}
names(Parent1PP.18) <- "Parent1PP.18"

#Parent 1 ne vote pas
Parent1PP.19 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==19){
    Parent1PP.19 <- rbind(Parent1PP.19,1)
  }
  else{
    Parent1PP.19 <- rbind(Parent1PP.19,0)
  }
}
names(Parent1PP.19) <- "Parent1PP.19"

#cbindParent1PP

Parent1PP <- Parent1PP.1
for (i in 2:19){
  command <- str_c("Parent1PP <- cbind(Parent1PP,Parent1PP.",i,")")
  eval(parse(text = command))
}

#Codage Parent2PP ----

#Parent 2 PCF
Parent2PP.1 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==1){
    Parent2PP.1 <- rbind(Parent2PP.1,1)
  }
  else{
    Parent2PP.1 <- rbind(Parent2PP.1,0)
  }
}
names(Parent2PP.1) <- "Parent2PP.1"

#Parent 2 LFI
Parent2PP.2 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==2){
    Parent2PP.2 <- rbind(Parent2PP.2,1)
  }
  else{
    Parent2PP.2 <- rbind(Parent2PP.2,0)
  }
}
names(Parent2PP.2) <- "Parent2PP.2"

#parent2 PS
Parent2PP.3 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==3){
    Parent2PP.3 <- rbind(Parent2PP.3,1)
  }
  else{
    Parent2PP.3 <- rbind(Parent2PP.3,0)
  }
}
names(Parent2PP.3) <- "Parent2PP.3"

#parent2 EELV
Parent2PP.4 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==4){
    Parent2PP.4 <- rbind(Parent2PP.4,1)
  }
  else{
    Parent2PP.4 <- rbind(Parent2PP.4,0)
  }
}
names(Parent2PP.4) <- "Parent2PP.4"

#parent 2 DVG
Parent2PP.5 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==5){
    Parent2PP.5 <- rbind(Parent2PP.5,1)
  }
  else{
    Parent2PP.5 <- rbind(Parent2PP.5,0)
  }
}
names(Parent2PP.5) <- "Parent2PP.5"

#parent2 LREM
Parent2PP.6 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==6){
    Parent2PP.6 <- rbind(Parent2PP.6,1)
  }
  else{
    Parent2PP.6 <- rbind(Parent2PP.6,0)
  }
}
names(Parent2PP.6) <- "Parent2PP.6"

#parent 2 MoDem
Parent2PP.7 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==7){
    Parent2PP.7 <- rbind(Parent2PP.7,1)
  }
  else{
    Parent2PP.7 <- rbind(Parent2PP.7,0)
  }
}
names(Parent2PP.7) <- "Parent2PP.7"

#Parent 2 MR
Parent2PP.8 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==8){
    Parent2PP.8 <- rbind(Parent2PP.8,1)
  }
  else{
    Parent2PP.8 <- rbind(Parent2PP.8,0)
  }
}
names(Parent2PP.8) <- "Parent2PP.8"

#Parent 2 LR
Parent2PP.9 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==9){
    Parent2PP.9 <- rbind(Parent2PP.9,1)
  }
  else{
    Parent2PP.9 <- rbind(Parent2PP.9,0)
  }
}
names(Parent2PP.9) <- "Parent2PP.9"

#Parent 2 UDI
Parent2PP.10 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==10){
    Parent2PP.10 <- rbind(Parent2PP.10,1)
  }
  else{
    Parent2PP.10 <- rbind(Parent2PP.10,0)
  }
}
names(Parent2PP.10) <- "Parent2PP.10"

#Parent 2 Agir
Parent2PP.11 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==11){
    Parent2PP.11 <- rbind(Parent2PP.11,1)
  }
  else{
    Parent2PP.11 <- rbind(Parent2PP.11,0)
  }
}
names(Parent2PP.11) <- "Parent2PP.11"

#Parent 2 DVD
Parent2PP.12 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==12){
    Parent2PP.12 <- rbind(Parent2PP.12,1)
  }
  else{
    Parent2PP.12 <- rbind(Parent2PP.12,0)
  }
}
names(Parent2PP.12) <- "Parent2PP.12"

#Parent 2 RN
Parent2PP.13 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==13){
    Parent2PP.13 <- rbind(Parent2PP.13,1)
  }
  else{
    Parent2PP.13 <- rbind(Parent2PP.13,0)
  }
}
names(Parent2PP.13) <- "Parent2PP.13"

#Parent 2 LP
Parent2PP.14 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==14){
    Parent2PP.14 <- rbind(Parent2PP.14,1)
  }
  else{
    Parent2PP.14 <- rbind(Parent2PP.14,0)
  }
}
names(Parent2PP.14) <- "Parent2PP.14"

#Parent 2 aucun parti
Parent2PP.15 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==15){
    Parent2PP.15 <- rbind(Parent2PP.15,1)
  }
  else{
    Parent2PP.15 <- rbind(Parent2PP.15,0)
  }
}
names(Parent2PP.15) <- "Parent2PP.15"

#Parent 2 ne veux pas répondre 
Parent2PP.16 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==16){
    Parent2PP.16 <- rbind(Parent2PP.16,1)
  }
  else{
    Parent2PP.16 <- rbind(Parent2PP.16,0)
  }
}
names(Parent2PP.16) <- "Parent2PP.16"

#Parent 2 Ne sais pas
Parent2PP.17 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,3]==17){
    Parent2PP.17 <- rbind(Parent2PP.17,1)
  }
  else{
    Parent2PP.17 <- rbind(Parent2PP.17,0)
  }
}
names(Parent2PP.17) <- "Parent2PP.17"

#Parent 2 vote dans un autre pays 
Parent2PP.18 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==18){
    Parent2PP.18 <- rbind(Parent2PP.18,1)
  }
  else{
    Parent2PP.18 <- rbind(Parent2PP.18,0)
  }
}
names(Parent2PP.18) <- "Parent2PP.18"

#Parent 2 ne vote pas
Parent2PP.19 <- data.frame()
for (i in 1:nrow(base)){
  if(base[i,4]==19){
    Parent2PP.19 <- rbind(Parent2PP.19,1)
  }
  else{
    Parent2PP.19 <- rbind(Parent2PP.19,0)
  }
}
names(Parent2PP.19) <- "Parent2PP.19"

#cbind Parent2PP
Parent2PP <- Parent2PP.1
for (i in 2:19){
  command <- str_c("Parent2PP <- cbind(Parent2PP,Parent2PP.",i,")")
  eval(parse(text = command))
}

#Econometrie ----
#Creation de la base pour la partie econometrique ----

baseEco <- cbind(base[,-c(3,4,5,7,14,31,32)],mediadata,AxePP,Parent1PP,Parent2PP)



#Transformation de la base
library(mlogit)
BaseModele<-mlogit.data(baseEco,shape="wide",varying=c(34:51,53:70,72:89),choice="PP") 
summary(BaseModele)
View(baseEco)

#Pb apercu suite a la mise en place de la base car un des individus ne correspond pas a TRUE sur PP donc on va le chercher et le supprimer
dataCorrec <- subset(BaseModele, BaseModele$PP==TRUE) 
View(dataCorrec$Individus)#Individus 133 manquant donc il sera a supprimer

#Remise en place de la base
baseEco2 <- baseEco[-133,]
BaseModele<-mlogit.data(baseEco2,shape="wide",varying=c(34:51,53:70,72:89),choice="PP")
summary(baseEco2)
summary(BaseModele)
head(BaseModele)

#Modele multinomial general ----
summary(BaseModele)
ml1.PP <- mlogit(PP~0|Categorie_Parent1, data=BaseModele)
summary(ml1.PP)

ml1.PP2 <- mlogit(PP~0|Categorie_Parent1, heterosc = TRUE,data=BaseModele)
summary(ml1.PP2)#Ne fonctionne pas car matrice non inversible

ml2.PP <- mlogit(PP~AxePP| Categorie_Parent1 , data=BaseModele)
summary(ml2.PP)#Modele avec hypothese des erreurs homoscedastiques

ml2.PP2 <- mlogit(PP~AxePP| Categorie_Parent1 , heterosc = TRUE, data=BaseModele)
summary(ml2.PP2) #Modele ne marche pas

ml3.PP <- mlogit(PP~AxePP+Parent1PP| Categorie_Parent1 , data=BaseModele)
summary(ml3.PP)

ml4.PP <- mlogit(PP~AxePP+Parent1PP+Parent2PP| Categorie_Parent1, data=BaseModele)
summary(ml4.PP)

#VGLM
library(VGAM)
Fit <- vglm(PP~Categorie_Parent1,multinomial(refLevel=1),data=baseEco2)
summary(Fit)

#Test des LikeHood
library(lmtest)
library(stargazer)
lrtest(ml1.PP,ml2.PP,ml3.PP,ml4.PP)
lrtest(ml2.PP, ml1.PP)
lrtest(ml2.PP,ml4.PP)
lrtest(ml2.PP,ml3.PP)
#Rien ne ressort de ces tests

#Plot par rapport à la categorie des parents

ooo <- with(baseEco2, order(Categorie_Parent1))
with(baseEco2, matplot(Categorie_Parent1[ooo], fitted(Fit)[ooo,], ylim = c(0,1),
                     xlab = "Categorie Parent1", ylab = "Probabilité estimée", 
                     main = " Effet de la variable Categorie Parent1 ", type = "l", lwd = 2, col = PP))
with(baseEco2, rug(Categorie_Parent1))
legend("topright", col = baseEco2$PP, lty=1:3
       ,legend=colnames(Fit@y))


ml2.PP <- mlogit(PP~AxePP| Categorie_Parent1 , reflevel="14",data=BaseModele)
summary(ml2.PP)

#Revenu
Fit <- vglm(PP~Revenu,multinomial(refLevel=1),data=baseEco2)
summary(Fit)

#Mlogit avec en reference le parti politique 15 afin de verifier l'absence de corrélation
#changement de base en enlevant les aprtis n'ayant recu aucun vote ----
baseEco3 <- baseEco2[, -c(44,50,51,63,69,70,82,88,89)]
BaseModele<-mlogit.data(baseEco3,shape="wide",varying=c(34:48,50:64,66:80),choice="PP")

ml.PP21<-mlogit(PP~AxePP+Parent1PP+Parent2PP|Age + Type_Habitation,data=BaseModele)

stargazer(ml.PP2, type="text", out="ml_PP21.txt")

ml.PP2<-mlogit(PP~AxePP+Parent1PP+Parent2PP| Age + Type_Habitation,data=BaseModele,reflevel="15")
summary(ml.PP2)

lrtest(ml.PP21,ml.PP2)

#Hetero ne fonctionne toujours pas


#ScoreTest ne fonctionne pas pour age et type habitation
nl.PP2 <- mlogit(PP~AxePP+Parent1PP+Parent2PP|Age+Type_Habitation,data=BaseModele,reflevel="15",nests=list(Type1="15",Type2=c("1","2","3","4","5","6","7","8","9","10","12","13","14","16")),unscaled=TRUE)



nl.PP2 <- mlogit(PP~AxePP+Parent1PP+Parent2PP|Categorie_Parent1,data=BaseModele,reflevel="15",nests=list(Type1="15",Type2=c("1","2","3","4","5","6","7","8","9","10","12","13","14","16")),unscaled=TRUE)
lrtest(nl.PP2)
summary(nl.PP2)
stargazer(nl.PP2,type="text",out="scoretest.txt")

nl.PP2 <- mlogit(PP~AxePP+Parent1PP+Parent2PP|Categorie_Parent1,data=BaseModele,reflevel="15",nests=list(Type1="15",Type2=c("1","2","3","4","5","6","7","8","9","10","12","13","14","16")),unscaled=TRUE)
lrtest(nl.PP2)
summary(nl.PP2)


#Plot par rapport à la Type Habitation
Fit <- vglm(PP~Age + Type_Habitation,multinomial(refLevel=14),data=baseEco3)
summary(Fit)
ooo <- with(baseEco3, order(Type_Habitation))
with(baseEco3, matplot(Type_Habitation[ooo], fitted(Fit)[ooo,], ylim = c(0,1),
                       xlab = "Type Habitation", ylab = "Probabilité estimée", 
                       main = " Effet de la variable Type Habitation ", type = "l", lwd = 2, col = PP))
with(baseEco3, rug(Type_Habitation))
legend("topright", col = baseEco3$PP, lty=1:3
       ,legend=colnames(Fit@y))

#Plot par rapport à la Age
ooo <- with(baseEco3, order(Age))
with(baseEco3, matplot(Age[ooo], fitted(Fit)[ooo,], ylim = c(0,1),
                       xlab = "Age", ylab = "Probabilité estimée", 
                       main = " Effet de la variable Age ", type = "l", lwd = 2, col = PP))
with(baseEco3, rug(Age))
legend("topright", col = baseEco3$PP, lty=1:3
       ,legend=colnames(Fit@y))

#odds ratio et effet marginaux du modele ml.PP2

stargazer(exp(coef(ml.PP2)), type="text", out="odds_ratio.txt")

effects(ml.PP2, covariate="Age",data=BaseModele)#effet marginaux poour l'ensemble des individus

moy <- with(BaseModele, data.frame(AxePP = tapply(AxePP, index(BaseModele)$alt, mean),Parent1PP = tapply(Parent1PP, index(BaseModele)$alt, mean),Parent2PP = tapply(Parent2PP, index(BaseModele)$alt, mean), Age = mean(Age), Type_Habitation = mean(Type_Habitation)))
stargazer( effects(ml.PP2, covariate = "Age", data = moy), type="text", out="effet_marg.txt") #Effet Marginaux moyen pour l'age
effects(ml.PP2, covariate = "Type_Habitation", data = moy) #Effet marginaux moyen pour le type d'habitation

#Autre modele en supposant les erreurs homoscédastique car la fonciton hetero ne fonctionne toujours pas ----
#ce probleme est dû à un porbleme de matrice non inversible et donc aux variables utilisées
ml.PP4 <- mlogit(PP~AxePP+Parent1PP+Parent2PP| Age + Type_Habitation + Sexe ,data=BaseModele,reflevel="15")
summary(ml.PP4) #No Good

ml.PP5 <- mlogit(PP~AxePP+Parent1PP+Parent2PP| Age + Type_Habitation + Parent2_Categorie ,data=BaseModele,reflevel="15")
summary(ml.PP5) #Better

ml.PP6 <- mlogit(PP~AxePP+Parent1PP+Parent2PP| Age + Type_Habitation + Parent2_Categorie + JournalTv,data=BaseModele,reflevel="15")
summary(ml.PP6)
stargazer(ml.PP6, type="text",out="modeleannexe.txt")#More Better

ml.PP7 <- mlogit(PP~AxePP+Parent1PP+Parent2PP| Maire.PP + Type_Habitation + Parent2_Categorie + JournalTv,data=BaseModele,reflevel="15")
summary(ml.PP7) #More Better
#Maire.PP est considéré comme variable lié à l'individu et non au type de vote
stargazer(ml.PP7, type="text",out="modeleannexe2.txt")


#lrtest entre PP6 et PP7
lrtest(ml.PP6,ml.PP7)
lrtest(ml.PP7,ml.PP6) #en les comparant deux fois on remarque qu'ils sont tous les deux oconsideres comme meilleur
#pour expliquer PP, surement car il n'explique pas les memmes partis et avec des variables liés à l'individu différente

#Plot pour Maire.PP et Journal TV
Fit <- vglm(PP~Maire.PP + Type_Habitation + Parent2_Categorie + JournalTv,multinomial(refLevel=14),data=baseEco3)
summary(Fit)
ooo <- with(baseEco3, order(Maire.PP))
with(baseEco3, matplot(Maire.PP[ooo], fitted(Fit)[ooo,], ylim = c(0,1),
                       xlab = "Maire PP", ylab = "Probabilité estimée", 
                       main = " Effet de la variable Maire PP ", type = "l", lwd = 2, col = PP))
with(baseEco3, rug(Maire.PP))
legend("topright", col = baseEco3$PP, lty=1:8
       ,legend=colnames(Fit@y))
#plot 2
ooo <- with(baseEco3, order(JournalTv))
with(baseEco3, matplot(JournalTv[ooo], fitted(Fit)[ooo,], ylim = c(0,1),
                       xlab = "JournalTV", ylab = "Probabilité estimée", 
                       main = " Effet de la variable JournalTV ", type = "l", lwd = 2, col = PP))
with(baseEco3, rug(JournalTv))
legend("topright", col = baseEco3$PP, lty=1:3
       ,legend=colnames(Fit@y))
