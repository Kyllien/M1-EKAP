library(readxl)
library(tidyverse)

setwd("D:/M1 EKAP/Mémoire/Data/Variables Controles")

#Baltic Dry Exchange

BDI <- read.csv("Brut/BDI.csv")
BDI2 <- BDI[-c(1:1217),c(1,5)]

BDI2$time <- as.Date(BDI2$time)
BDI2$Jour <- format(BDI2$time,"%A")

BDI3 <- BDI2[1,-3]

for (i in (1:nrow(BDI2))){
  if(BDI2[i,3]=="mardi"){
    BDI3 <- rbind(BDI3,BDI2[i,c(1,2)])
  }
}

BDI3 <- BDI3[-1,]

BDI4 <- BDI3[-1,]
BDI4$Rd <- diff(log(BDI3$close))
BDI4$Rd <- BDI4$Rd*100

write.csv(BDI4,"Transfo/BDI.csv")

#MSCI

MSCI <- read.csv("Brut/MSCI.csv")
MSCI$time <- as.Date(MSCI$time)
MSCI$Jour <- format(MSCI$time,"%A")

MSCI2 <- MSCI[1,c(1,5)]

for (i in (1:nrow(MSCI))){
  if(MSCI[i,6]=="mardi"){
    MSCI2 <- rbind(MSCI2,MSCI[i,c(1,5)])
  }
}

MSCI2 <- MSCI2[-1,]

MSCI3 <- MSCI2[-1,]
MSCI3$Rd <- diff(log(MSCI2$close))
MSCI3$Rd <- MSCI3$Rd * 100

write.csv(MSCI3,"Transfo/MSCI.csv")
