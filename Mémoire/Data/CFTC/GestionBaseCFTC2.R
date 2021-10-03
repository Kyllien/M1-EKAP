library(readxl)
library(tidyverse)
library(tseries)

setwd("D:/M1 EKAP/Mémoire/Data/CFTC")

#-- SILVER ----
SILVER <- read.csv("Transfo1/SILVER.csv")

SILVER2 <- SILVER[,c(4,10:13,15,16,18,19)]

SILVER2$Tot_Long <- 0
SILVER2$Tot_Short <- 0
SILVER2$Tot_Long_NoProd <- 0
SILVER2$Tot_Short_NoProd <- 0

for (i in seq(2,8,by=2)){
  SILVER2$Tot_Long <- SILVER2$Tot_Long + SILVER2[,i]
}
for (i in seq(3,9,by=2)){
  SILVER2$Tot_Short <- SILVER2$Tot_Short + SILVER2[,i]
}
for (i in seq(4,8,by=2)){
  SILVER2$Tot_Long_NoProd <- SILVER2$Tot_Long_NoProd + SILVER2[,i]
}
for (i in seq(5,9,by=2)){
  SILVER2$Tot_Short_NoProd <- SILVER2$Tot_Short_NoProd + SILVER2[,i]
}

SILVER2$Tot_Net_Position <- SILVER2$Tot_Long - SILVER2$Tot_Short
SILVER2$Tot_Net_Position_NoProd <- SILVER2$Tot_Long_NoProd - SILVER2$Tot_Short_NoProd

for (i in (2:15)){
  for (j in (1:nrow(SILVER2))){
    if(SILVER2[j,i]==0){
      SILVER2[j,i] <- 1
    }
  }
}

SILVER3 <- as.data.frame(SILVER2[-1,1])
SILVER3$Rd_Long_Tot <- diff(log(SILVER2$Tot_Long)) * 100
SILVER3$Rd_Long_NoProd <- diff(log(SILVER2$Tot_Long_NoProd)) * 100
SILVER3$Rd_Short_Tot <- diff(log(SILVER2$Tot_Short)) * 100
SILVER3$Rd_Short_NoProd <- diff(log(SILVER2$Tot_Short_NoProd)) * 100

SILVER3$Rd_Net <- diff(SILVER2$Tot_Net_Position)
SILVER3$Rd_Net_NoProd <- diff(SILVER2$Tot_Net_Position_NoProd)

SILVER3$Rd_Merc_Long <- diff(log(SILVER2$Prod_Merc_Positions_Long_ALL)) * 100
SILVER3$Rd_Merc_Short <- diff(log(SILVER2$Prod_Merc_Positions_Short_ALL)) * 100
SILVER3$Rd_Swap_Long <- diff(log(SILVER2$Swap_Positions_Long_All)) * 100
SILVER3$Rd_Swap_Short <- diff(log(SILVER2$Swap__Positions_Short_All)) * 100
SILVER3$Rd_Money_Long <- diff(log(SILVER2$M_Money_Positions_Long_ALL)) * 100
SILVER3$Rd_Money_Short <- diff(log(SILVER2$M_Money_Positions_Short_ALL)) * 100
SILVER3$Rd_Other_Long <- diff(log(SILVER2$Other_Rept_Positions_Long_ALL)) * 100
SILVER3$Rd_Other_Short <- diff(log(SILVER2$Other_Rept_Positions_Short_ALL)) * 100

write.csv(SILVER3,"Transfo2/SILVER.csv")

#-- COPPER ----
COPPER <- read.csv("Transfo1/COPPER.csv")

COPPER2 <- COPPER[,c(4,10:13,15,16,18,19)]

COPPER2$Tot_Long <- 0
COPPER2$Tot_Short <- 0
COPPER2$Tot_Long_NoProd <- 0
COPPER2$Tot_Short_NoProd <- 0

for (i in seq(2,8,by=2)){
  COPPER2$Tot_Long <- COPPER2$Tot_Long + COPPER2[,i]
}
for (i in seq(3,9,by=2)){
  COPPER2$Tot_Short <- COPPER2$Tot_Short + COPPER2[,i]
}
for (i in seq(4,8,by=2)){
  COPPER2$Tot_Long_NoProd <- COPPER2$Tot_Long_NoProd + COPPER2[,i]
}
for (i in seq(5,9,by=2)){
  COPPER2$Tot_Short_NoProd <- COPPER2$Tot_Short_NoProd + COPPER2[,i]
}

COPPER2$Tot_Net_Position <- COPPER2$Tot_Long - COPPER2$Tot_Short
COPPER2$Tot_Net_Position_NoProd <- COPPER2$Tot_Long_NoProd - COPPER2$Tot_Short_NoProd

for (i in (2:15)){
  for (j in (1:nrow(COPPER2))){
    if(COPPER2[j,i]==0){
      COPPER2[j,i] <- 1
    }
  }
}

COPPER3 <- as.data.frame(COPPER2[-1,1])
COPPER3$Rd_Long_Tot <- diff(log(COPPER2$Tot_Long)) * 100
COPPER3$Rd_Long_NoProd <- diff(log(COPPER2$Tot_Long_NoProd)) * 100
COPPER3$Rd_Short_Tot <- diff(log(COPPER2$Tot_Short)) * 100
COPPER3$Rd_Short_NoProd <- diff(log(COPPER2$Tot_Short_NoProd)) * 100

COPPER3$Rd_Net <- diff(COPPER2$Tot_Net_Position)
COPPER3$Rd_Net_NoProd <- diff(COPPER2$Tot_Net_Position_NoProd)

COPPER3$Rd_Merc_Long <- diff(log(COPPER2$Prod_Merc_Positions_Long_ALL)) * 100
COPPER3$Rd_Merc_Short <- diff(log(COPPER2$Prod_Merc_Positions_Short_ALL)) * 100
COPPER3$Rd_Swap_Long <- diff(log(COPPER2$Swap_Positions_Long_All)) * 100
COPPER3$Rd_Swap_Short <- diff(log(COPPER2$Swap__Positions_Short_All)) * 100
COPPER3$Rd_Money_Long <- diff(log(COPPER2$M_Money_Positions_Long_ALL)) * 100
COPPER3$Rd_Money_Short <- diff(log(COPPER2$M_Money_Positions_Short_ALL)) * 100
COPPER3$Rd_Other_Long <- diff(log(COPPER2$Other_Rept_Positions_Long_ALL)) * 100
COPPER3$Rd_Other_Short <- diff(log(COPPER2$Other_Rept_Positions_Short_ALL)) * 100

write.csv(COPPER3,"Transfo2/COPPER.csv")


#-- GOLD ----
GOLD <- read.csv("Transfo1/GOLD.csv")

GOLD2 <- GOLD[,c(4,10:13,15,16,18,19)]

GOLD2$Tot_Long <- 0
GOLD2$Tot_Short <- 0
GOLD2$Tot_Long_NoProd <- 0
GOLD2$Tot_Short_NoProd <- 0

for (i in seq(2,8,by=2)){
  GOLD2$Tot_Long <- GOLD2$Tot_Long + GOLD2[,i]
}
for (i in seq(3,9,by=2)){
  GOLD2$Tot_Short <- GOLD2$Tot_Short + GOLD2[,i]
}
for (i in seq(4,8,by=2)){
  GOLD2$Tot_Long_NoProd <- GOLD2$Tot_Long_NoProd + GOLD2[,i]
}
for (i in seq(5,9,by=2)){
  GOLD2$Tot_Short_NoProd <- GOLD2$Tot_Short_NoProd + GOLD2[,i]
}

GOLD2$Tot_Net_Position <- GOLD2$Tot_Long - GOLD2$Tot_Short
GOLD2$Tot_Net_Position_NoProd <- GOLD2$Tot_Long_NoProd - GOLD2$Tot_Short_NoProd

for (i in (2:15)){
  for (j in (1:nrow(GOLD2))){
    if(GOLD2[j,i]==0){
      GOLD2[j,i] <- 1
    }
  }
}

GOLD3 <- as.data.frame(GOLD2[-1,1])
GOLD3$Rd_Long_Tot <- diff(log(GOLD2$Tot_Long)) * 100
GOLD3$Rd_Long_NoProd <- diff(log(GOLD2$Tot_Long_NoProd)) * 100
GOLD3$Rd_Short_Tot <- diff(log(GOLD2$Tot_Short)) * 100
GOLD3$Rd_Short_NoProd <- diff(log(GOLD2$Tot_Short_NoProd)) * 100

GOLD3$Rd_Net <- diff(GOLD2$Tot_Net_Position)
GOLD3$Rd_Net_NoProd <- diff(GOLD2$Tot_Net_Position_NoProd)

GOLD3$Rd_Merc_Long <- diff(log(GOLD2$Prod_Merc_Positions_Long_ALL)) * 100
GOLD3$Rd_Merc_Short <- diff(log(GOLD2$Prod_Merc_Positions_Short_ALL)) * 100
GOLD3$Rd_Swap_Long <- diff(log(GOLD2$Swap_Positions_Long_All)) * 100
GOLD3$Rd_Swap_Short <- diff(log(GOLD2$Swap__Positions_Short_All)) * 100
GOLD3$Rd_Money_Long <- diff(log(GOLD2$M_Money_Positions_Long_ALL)) * 100
GOLD3$Rd_Money_Short <- diff(log(GOLD2$M_Money_Positions_Short_ALL)) * 100
GOLD3$Rd_Other_Long <- diff(log(GOLD2$Other_Rept_Positions_Long_ALL)) * 100
GOLD3$Rd_Other_Short <- diff(log(GOLD2$Other_Rept_Positions_Short_ALL)) * 100

write.csv(GOLD3,"Transfo2/GOLD.csv")

#-- Brent ----
Brent <- read.csv("Transfo1/BRENT.csv")

Brent2 <- Brent[,c(4,10:13,15,16,18,19)]

Brent2$Tot_Long <- 0
Brent2$Tot_Short <- 0
Brent2$Tot_Long_NoProd <- 0
Brent2$Tot_Short_NoProd <- 0

for (i in seq(2,8,by=2)){
  Brent2$Tot_Long <- Brent2$Tot_Long + Brent2[,i]
}
for (i in seq(3,9,by=2)){
  Brent2$Tot_Short <- Brent2$Tot_Short + Brent2[,i]
}
for (i in seq(4,8,by=2)){
  Brent2$Tot_Long_NoProd <- Brent2$Tot_Long_NoProd + Brent2[,i]
}
for (i in seq(5,9,by=2)){
  Brent2$Tot_Short_NoProd <- Brent2$Tot_Short_NoProd + Brent2[,i]
}

Brent2$Tot_Net_Position <- Brent2$Tot_Long - Brent2$Tot_Short
Brent2$Tot_Net_Position_NoProd <- Brent2$Tot_Long_NoProd - Brent2$Tot_Short_NoProd

#Boucle pour passer les 0 positions en 1 afin de pouvoir calculer par la suite les pourcentages et eviter les infinies
#ce choix sera detailler dans le memoire
for (i in (2:15)){
  for (j in (1:nrow(Brent2))){
    if(Brent2[j,i]==0){
      Brent2[j,i] <- 1
    }
  }
}

Brent3 <- as.data.frame(Brent2[-1,1])
Brent3$Rd_Long_Tot <- diff(log(Brent2$Tot_Long)) * 100
Brent3$Rd_Long_NoProd <- diff(log(Brent2$Tot_Long_NoProd)) * 100
Brent3$Rd_Short_Tot <- diff(log(Brent2$Tot_Short)) * 100
Brent3$Rd_Short_NoProd <- diff(log(Brent2$Tot_Short_NoProd)) * 100

Brent3$Rd_Net <- diff(Brent2$Tot_Net_Position)
Brent3$Rd_Net_NoProd <- diff(Brent2$Tot_Net_Position_NoProd)

Brent3$Rd_Merc_Long <- diff(log(Brent2$Prod_Merc_Positions_Long_ALL)) * 100
Brent3$Rd_Merc_Short <- diff(log(Brent2$Prod_Merc_Positions_Short_ALL)) * 100
Brent3$Rd_Swap_Long <- diff(log(Brent2$Swap_Positions_Long_All)) * 100
Brent3$Rd_Swap_Short <- diff(log(Brent2$Swap__Positions_Short_All)) * 100
Brent3$Rd_Money_Long <- diff(log(Brent2$M_Money_Positions_Long_ALL)) * 100
Brent3$Rd_Money_Short <- diff(log(Brent2$M_Money_Positions_Short_ALL)) * 100
Brent3$Rd_Other_Long <- diff(log(Brent2$Other_Rept_Positions_Long_ALL)) * 100
Brent3$Rd_Other_Short <- diff(log(Brent2$Other_Rept_Positions_Short_ALL)) * 100


write.csv(Brent3,"Transfo2/Brent.csv")

#-- Cocoa ----
Cocoa <- read.csv("Transfo1/COCOA.csv")

Cocoa2 <- Cocoa[,c(4,10:13,15,16,18,19)]

Cocoa2$Tot_Long <- 0
Cocoa2$Tot_Short <- 0
Cocoa2$Tot_Long_NoProd <- 0
Cocoa2$Tot_Short_NoProd <- 0

for (i in seq(2,8,by=2)){
  Cocoa2$Tot_Long <- Cocoa2$Tot_Long + Cocoa2[,i]
}
for (i in seq(3,9,by=2)){
  Cocoa2$Tot_Short <- Cocoa2$Tot_Short + Cocoa2[,i]
}
for (i in seq(4,8,by=2)){
  Cocoa2$Tot_Long_NoProd <- Cocoa2$Tot_Long_NoProd + Cocoa2[,i]
}
for (i in seq(5,9,by=2)){
  Cocoa2$Tot_Short_NoProd <- Cocoa2$Tot_Short_NoProd + Cocoa2[,i]
}

Cocoa2$Tot_Net_Position <- Cocoa2$Tot_Long - Cocoa2$Tot_Short
Cocoa2$Tot_Net_Position_NoProd <- Cocoa2$Tot_Long_NoProd - Cocoa2$Tot_Short_NoProd

for (i in (2:15)){
  for (j in (1:nrow(Cocoa2))){
    if(Cocoa2[j,i]==0){
      Cocoa2[j,i] <- 1
    }
  }
}

Cocoa3 <- as.data.frame(Cocoa2[-1,1])
Cocoa3$Rd_Long_Tot <- diff(log(Cocoa2$Tot_Long)) * 100
Cocoa3$Rd_Long_NoProd <- diff(log(Cocoa2$Tot_Long_NoProd)) * 100
Cocoa3$Rd_Short_Tot <- diff(log(Cocoa2$Tot_Short)) * 100
Cocoa3$Rd_Short_NoProd <- diff(log(Cocoa2$Tot_Short_NoProd)) * 100

Cocoa3$Rd_Net <- diff(Cocoa2$Tot_Net_Position)
Cocoa3$Rd_Net_NoProd <- diff(Cocoa2$Tot_Net_Position_NoProd)

Cocoa3$Rd_Merc_Long <- diff(log(Cocoa2$Prod_Merc_Positions_Long_ALL)) * 100
Cocoa3$Rd_Merc_Short <- diff(log(Cocoa2$Prod_Merc_Positions_Short_ALL)) * 100
Cocoa3$Rd_Swap_Long <- diff(log(Cocoa2$Swap_Positions_Long_All)) * 100
Cocoa3$Rd_Swap_Short <- diff(log(Cocoa2$Swap__Positions_Short_All)) * 100
Cocoa3$Rd_Money_Long <- diff(log(Cocoa2$M_Money_Positions_Long_ALL)) * 100
Cocoa3$Rd_Money_Short <- diff(log(Cocoa2$M_Money_Positions_Short_ALL)) * 100
Cocoa3$Rd_Other_Long <- diff(log(Cocoa2$Other_Rept_Positions_Long_ALL)) * 100
Cocoa3$Rd_Other_Short <- diff(log(Cocoa2$Other_Rept_Positions_Short_ALL)) * 100

write.csv(Cocoa3,"Transfo2/Cocoa.csv")


#-- Coffee ----
Coffee <- read.csv("Transfo1/COFFEE.csv")

Coffee2 <- Coffee[,c(4,10:13,15,16,18,19)]

Coffee2$Tot_Long <- 0
Coffee2$Tot_Short <- 0
Coffee2$Tot_Long_NoProd <- 0
Coffee2$Tot_Short_NoProd <- 0

for (i in seq(2,8,by=2)){
  Coffee2$Tot_Long <- Coffee2$Tot_Long + Coffee2[,i]
}
for (i in seq(3,9,by=2)){
  Coffee2$Tot_Short <- Coffee2$Tot_Short + Coffee2[,i]
}
for (i in seq(4,8,by=2)){
  Coffee2$Tot_Long_NoProd <- Coffee2$Tot_Long_NoProd + Coffee2[,i]
}
for (i in seq(5,9,by=2)){
  Coffee2$Tot_Short_NoProd <- Coffee2$Tot_Short_NoProd + Coffee2[,i]
}

Coffee2$Tot_Net_Position <- Coffee2$Tot_Long - Coffee2$Tot_Short
Coffee2$Tot_Net_Position_NoProd <- Coffee2$Tot_Long_NoProd - Coffee2$Tot_Short_NoProd

for (i in (2:15)){
  for (j in (1:nrow(Coffee2))){
    if(Coffee2[j,i]==0){
      Coffee2[j,i] <- 1
    }
  }
}

Coffee3 <- as.data.frame(Coffee2[-1,1])
Coffee3$Rd_Long_Tot <- diff(log(Coffee2$Tot_Long)) * 100
Coffee3$Rd_Long_NoProd <- diff(log(Coffee2$Tot_Long_NoProd)) * 100
Coffee3$Rd_Short_Tot <- diff(log(Coffee2$Tot_Short)) * 100
Coffee3$Rd_Short_NoProd <- diff(log(Coffee2$Tot_Short_NoProd)) * 100

Coffee3$Rd_Net <- diff(Coffee2$Tot_Net_Position)
Coffee3$Rd_Net_NoProd <- diff(Coffee2$Tot_Net_Position_NoProd)

Coffee3$Rd_Merc_Long <- diff(log(Coffee2$Prod_Merc_Positions_Long_ALL)) * 100
Coffee3$Rd_Merc_Short <- diff(log(Coffee2$Prod_Merc_Positions_Short_ALL)) * 100
Coffee3$Rd_Swap_Long <- diff(log(Coffee2$Swap_Positions_Long_All)) * 100
Coffee3$Rd_Swap_Short <- diff(log(Coffee2$Swap__Positions_Short_All)) * 100
Coffee3$Rd_Money_Long <-  diff(log(Coffee2$M_Money_Positions_Long_ALL)) * 100
Coffee3$Rd_Money_Short <- diff(log(Coffee2$M_Money_Positions_Short_ALL)) * 100
Coffee3$Rd_Other_Long <- diff(log(Coffee2$Other_Rept_Positions_Long_ALL)) * 100
Coffee3$Rd_Other_Short <- diff(log(Coffee2$Other_Rept_Positions_Short_ALL)) * 100

write.csv(Coffee3,"Transfo2/Coffee.csv")


#-- Corn ----
Corn <- read.csv("Transfo1/CORN.csv")

Corn2 <- Corn[,c(4,10:13,15,16,18,19)]

Corn2$Tot_Long <- 0
Corn2$Tot_Short <- 0
Corn2$Tot_Long_NoProd <- 0
Corn2$Tot_Short_NoProd <- 0

for (i in seq(2,8,by=2)){
  Corn2$Tot_Long <- Corn2$Tot_Long + Corn2[,i]
}
for (i in seq(3,9,by=2)){
  Corn2$Tot_Short <- Corn2$Tot_Short + Corn2[,i]
}
for (i in seq(4,8,by=2)){
  Corn2$Tot_Long_NoProd <- Corn2$Tot_Long_NoProd + Corn2[,i]
}
for (i in seq(5,9,by=2)){
  Corn2$Tot_Short_NoProd <- Corn2$Tot_Short_NoProd + Corn2[,i]
}

Corn2$Tot_Net_Position <- Corn2$Tot_Long - Corn2$Tot_Short
Corn2$Tot_Net_Position_NoProd <- Corn2$Tot_Long_NoProd - Corn2$Tot_Short_NoProd

for (i in (2:15)){
  for (j in (1:nrow(Corn2))){
    if(Corn2[j,i]==0){
      Corn2[j,i] <- 1
    }
  }
}

Corn3 <- as.data.frame(Corn2[-1,1])
Corn3$Rd_Long_Tot <- diff(log(Corn2$Tot_Long)) * 100
Corn3$Rd_Long_NoProd <- diff(log(Corn2$Tot_Long_NoProd)) * 100
Corn3$Rd_Short_Tot <- diff(log(Corn2$Tot_Short)) * 100
Corn3$Rd_Short_NoProd <- diff(log(Corn2$Tot_Short_NoProd)) * 100

Corn3$Rd_Net <- diff(Corn2$Tot_Net_Position)
Corn3$Rd_Net_NoProd <- diff(Corn2$Tot_Net_Position_NoProd)

Corn3$Rd_Merc_Long <- diff(log(Corn2$Prod_Merc_Positions_Long_ALL)) * 100
Corn3$Rd_Merc_Short <- diff(log(Corn2$Prod_Merc_Positions_Short_ALL)) * 100
Corn3$Rd_Swap_Long <- diff(log(Corn2$Swap_Positions_Long_All)) * 100
Corn3$Rd_Swap_Short <- diff(log(Corn2$Swap__Positions_Short_All)) * 100
Corn3$Rd_Money_Long <-  diff(log(Corn2$M_Money_Positions_Long_ALL)) * 100
Corn3$Rd_Money_Short <- diff(log(Corn2$M_Money_Positions_Short_ALL)) * 100
Corn3$Rd_Other_Long <- diff(log(Corn2$Other_Rept_Positions_Long_ALL)) * 100
Corn3$Rd_Other_Short <- diff(log(Corn2$Other_Rept_Positions_Short_ALL)) * 100

write.csv(Corn3,"Transfo2/Corn.csv")

#-- Cotton ----
Cotton <- read.csv("Transfo1/COTTON.csv")

Cotton2 <- Cotton[,c(4,10:13,15,16,18,19)]

Cotton2$Tot_Long <- 0
Cotton2$Tot_Short <- 0
Cotton2$Tot_Long_NoProd <- 0
Cotton2$Tot_Short_NoProd <- 0

for (i in seq(2,8,by=2)){
  Cotton2$Tot_Long <- Cotton2$Tot_Long + Cotton2[,i]
}
for (i in seq(3,9,by=2)){
  Cotton2$Tot_Short <- Cotton2$Tot_Short + Cotton2[,i]
}
for (i in seq(4,8,by=2)){
  Cotton2$Tot_Long_NoProd <- Cotton2$Tot_Long_NoProd + Cotton2[,i]
}
for (i in seq(5,9,by=2)){
  Cotton2$Tot_Short_NoProd <- Cotton2$Tot_Short_NoProd + Cotton2[,i]
}

Cotton2$Tot_Net_Position <- Cotton2$Tot_Long - Cotton2$Tot_Short
Cotton2$Tot_Net_Position_NoProd <- Cotton2$Tot_Long_NoProd - Cotton2$Tot_Short_NoProd

for (i in (2:15)){
  for (j in (1:nrow(Cotton2))){
    if(Cotton2[j,i]==0){
      Cotton2[j,i] <- 1
    }
  }
}

Cotton3 <- as.data.frame(Cotton2[-1,1])
Cotton3$Rd_Long_Tot <- diff(log(Cotton2$Tot_Long)) * 100
Cotton3$Rd_Long_NoProd <- diff(log(Cotton2$Tot_Long_NoProd)) * 100
Cotton3$Rd_Short_Tot <- diff(log(Cotton2$Tot_Short)) * 100
Cotton3$Rd_Short_NoProd <- diff(log(Cotton2$Tot_Short_NoProd)) * 100

Cotton3$Rd_Net <- diff(Cotton2$Tot_Net_Position)
Cotton3$Rd_Net_NoProd <- diff(Cotton2$Tot_Net_Position_NoProd)

Cotton3$Rd_Merc_Long <- diff(log(Cotton2$Prod_Merc_Positions_Long_ALL)) * 100
Cotton3$Rd_Merc_Short <- diff(log(Cotton2$Prod_Merc_Positions_Short_ALL)) * 100
Cotton3$Rd_Swap_Long <- diff(log(Cotton2$Swap_Positions_Long_All)) * 100
Cotton3$Rd_Swap_Short <- diff(log(Cotton2$Swap__Positions_Short_All)) * 100
Cotton3$Rd_Money_Long <-  diff(log(Cotton2$M_Money_Positions_Long_ALL)) * 100
Cotton3$Rd_Money_Short <- diff(log(Cotton2$M_Money_Positions_Short_ALL)) * 100
Cotton3$Rd_Other_Long <- diff(log(Cotton2$Other_Rept_Positions_Long_ALL)) * 100
Cotton3$Rd_Other_Short <- diff(log(Cotton2$Other_Rept_Positions_Short_ALL)) * 100

write.csv(Cotton3,"Transfo2/Cotton.csv")

#-- WTI ----
WTI <- read.csv("Transfo1/CRUDE_OIL_NY.csv")

WTI2 <- WTI[,c(4,10:13,15,16,18,19)]

WTI2$Tot_Long <- 0
WTI2$Tot_Short <- 0
WTI2$Tot_Long_NoProd <- 0
WTI2$Tot_Short_NoProd <- 0

for (i in seq(2,8,by=2)){
  WTI2$Tot_Long <- WTI2$Tot_Long + WTI2[,i]
}
for (i in seq(3,9,by=2)){
  WTI2$Tot_Short <- WTI2$Tot_Short + WTI2[,i]
}
for (i in seq(4,8,by=2)){
  WTI2$Tot_Long_NoProd <- WTI2$Tot_Long_NoProd + WTI2[,i]
}
for (i in seq(5,9,by=2)){
  WTI2$Tot_Short_NoProd <- WTI2$Tot_Short_NoProd + WTI2[,i]
}

WTI2$Tot_Net_Position <- WTI2$Tot_Long - WTI2$Tot_Short
WTI2$Tot_Net_Position_NoProd <- WTI2$Tot_Long_NoProd - WTI2$Tot_Short_NoProd

for (i in (2:15)){
  for (j in (1:nrow(WTI2))){
    if(WTI2[j,i]==0){
      WTI2[j,i] <- 1
    }
  }
}

WTI3 <- as.data.frame(WTI2[-1,1])
WTI3$Rd_Long_Tot <- diff(log(WTI2$Tot_Long)) * 100
WTI3$Rd_Long_NoProd <- diff(log(WTI2$Tot_Long_NoProd)) * 100
WTI3$Rd_Short_Tot <- diff(log(WTI2$Tot_Short)) * 100
WTI3$Rd_Short_NoProd <- diff(log(WTI2$Tot_Short_NoProd)) * 100

WTI3$Rd_Net <- diff(WTI2$Tot_Net_Position)
WTI3$Rd_Net_NoProd <- diff(WTI2$Tot_Net_Position_NoProd)

WTI3$Rd_Merc_Long <- diff(log(WTI2$Prod_Merc_Positions_Long_ALL)) * 100
WTI3$Rd_Merc_Short <- diff(log(WTI2$Prod_Merc_Positions_Short_ALL)) * 100
WTI3$Rd_Swap_Long <- diff(log(WTI2$Swap_Positions_Long_All)) * 100
WTI3$Rd_Swap_Short <- diff(log(WTI2$Swap__Positions_Short_All)) * 100
WTI3$Rd_Money_Long <-  diff(log(WTI2$M_Money_Positions_Long_ALL)) * 100
WTI3$Rd_Money_Short <- diff(log(WTI2$M_Money_Positions_Short_ALL)) * 100
WTI3$Rd_Other_Long <- diff(log(WTI2$Other_Rept_Positions_Long_ALL)) * 100
WTI3$Rd_Other_Short <- diff(log(WTI2$Other_Rept_Positions_Short_ALL)) * 100

write.csv(WTI3,"Transfo2/WTI.csv")


#-- Natural_Gas ----
Natural_Gas <- read.csv("Transfo1/NATURAL_GAS.csv")

Natural_Gas2 <- Natural_Gas[,c(4,10:13,15,16,18,19)]

Natural_Gas2$Tot_Long <- 0
Natural_Gas2$Tot_Short <- 0
Natural_Gas2$Tot_Long_NoProd <- 0
Natural_Gas2$Tot_Short_NoProd <- 0

for (i in seq(2,8,by=2)){
  Natural_Gas2$Tot_Long <- Natural_Gas2$Tot_Long + Natural_Gas2[,i]
}
for (i in seq(3,9,by=2)){
  Natural_Gas2$Tot_Short <- Natural_Gas2$Tot_Short + Natural_Gas2[,i]
}
for (i in seq(4,8,by=2)){
  Natural_Gas2$Tot_Long_NoProd <- Natural_Gas2$Tot_Long_NoProd + Natural_Gas2[,i]
}
for (i in seq(5,9,by=2)){
  Natural_Gas2$Tot_Short_NoProd <- Natural_Gas2$Tot_Short_NoProd + Natural_Gas2[,i]
}

Natural_Gas2$Tot_Net_Position <- Natural_Gas2$Tot_Long - Natural_Gas2$Tot_Short
Natural_Gas2$Tot_Net_Position_NoProd <- Natural_Gas2$Tot_Long_NoProd - Natural_Gas2$Tot_Short_NoProd

for (i in (2:15)){
  for (j in (1:nrow(Natural_Gas2))){
    if(Natural_Gas2[j,i]==0){
      Natural_Gas2[j,i] <- 1
    }
  }
}

Natural_Gas3 <- as.data.frame(Natural_Gas2[-1,1])
Natural_Gas3$Rd_Long_Tot <- diff(log(Natural_Gas2$Tot_Long)) * 100
Natural_Gas3$Rd_Long_NoProd <- diff(log(Natural_Gas2$Tot_Long_NoProd)) * 100
Natural_Gas3$Rd_Short_Tot <- diff(log(Natural_Gas2$Tot_Short)) * 100
Natural_Gas3$Rd_Short_NoProd <- diff(log(Natural_Gas2$Tot_Short_NoProd)) * 100

Natural_Gas3$Rd_Net <- diff(Natural_Gas2$Tot_Net_Position)
Natural_Gas3$Rd_Net_NoProd <- diff(Natural_Gas2$Tot_Net_Position_NoProd)

Natural_Gas3$Rd_Merc_Long <- diff(log(Natural_Gas2$Prod_Merc_Positions_Long_ALL)) * 100
Natural_Gas3$Rd_Merc_Short <- diff(log(Natural_Gas2$Prod_Merc_Positions_Short_ALL)) * 100
Natural_Gas3$Rd_Swap_Long <- diff(log(Natural_Gas2$Swap_Positions_Long_All)) * 100
Natural_Gas3$Rd_Swap_Short <- diff(log(Natural_Gas2$Swap__Positions_Short_All)) * 100
Natural_Gas3$Rd_Money_Long <-  diff(log(Natural_Gas2$M_Money_Positions_Long_ALL)) * 100
Natural_Gas3$Rd_Money_Short <- diff(log(Natural_Gas2$M_Money_Positions_Short_ALL)) * 100
Natural_Gas3$Rd_Other_Long <- diff(log(Natural_Gas2$Other_Rept_Positions_Long_ALL)) * 100
Natural_Gas3$Rd_Other_Short <- diff(log(Natural_Gas2$Other_Rept_Positions_Short_ALL)) * 100

write.csv(Natural_Gas3,"Transfo2/Natural_Gas.csv")

#-- Rough_Rice ----
Rough_Rice <- read.csv("Transfo1/ROUGH_RICE.csv")

Rough_Rice2 <- Rough_Rice[,c(4,10:13,15,16,18,19)]

Rough_Rice2$Tot_Long <- 0
Rough_Rice2$Tot_Short <- 0
Rough_Rice2$Tot_Long_NoProd <- 0
Rough_Rice2$Tot_Short_NoProd <- 0

for (i in seq(2,8,by=2)){
  Rough_Rice2$Tot_Long <- Rough_Rice2$Tot_Long + Rough_Rice2[,i]
}
for (i in seq(3,9,by=2)){
  Rough_Rice2$Tot_Short <- Rough_Rice2$Tot_Short + Rough_Rice2[,i]
}
for (i in seq(4,8,by=2)){
  Rough_Rice2$Tot_Long_NoProd <- Rough_Rice2$Tot_Long_NoProd + Rough_Rice2[,i]
}
for (i in seq(5,9,by=2)){
  Rough_Rice2$Tot_Short_NoProd <- Rough_Rice2$Tot_Short_NoProd + Rough_Rice2[,i]
}

Rough_Rice2$Tot_Net_Position <- Rough_Rice2$Tot_Long - Rough_Rice2$Tot_Short
Rough_Rice2$Tot_Net_Position_NoProd <- Rough_Rice2$Tot_Long_NoProd - Rough_Rice2$Tot_Short_NoProd

for (i in (2:15)){
  for (j in (1:nrow(Rough_Rice2))){
    if(Rough_Rice2[j,i]==0){
      Rough_Rice2[j,i] <- 1
    }
  }
}

Rough_Rice3 <- as.data.frame(Rough_Rice2[-1,1])
Rough_Rice3$Rd_Long_Tot <- diff(log(Rough_Rice2$Tot_Long)) * 100
Rough_Rice3$Rd_Long_NoProd <- diff(log(Rough_Rice2$Tot_Long_NoProd)) * 100
Rough_Rice3$Rd_Short_Tot <- diff(log(Rough_Rice2$Tot_Short)) * 100
Rough_Rice3$Rd_Short_NoProd <- diff(log(Rough_Rice2$Tot_Short_NoProd)) * 100

Rough_Rice3$Rd_Net <- diff(Rough_Rice2$Tot_Net_Position)
Rough_Rice3$Rd_Net_NoProd <- diff(Rough_Rice2$Tot_Net_Position_NoProd)

Rough_Rice3$Rd_Merc_Long <- diff(log(Rough_Rice2$Prod_Merc_Positions_Long_ALL)) * 100
Rough_Rice3$Rd_Merc_Short <- diff(log(Rough_Rice2$Prod_Merc_Positions_Short_ALL)) * 100
Rough_Rice3$Rd_Swap_Long <- diff(log(Rough_Rice2$Swap_Positions_Long_All)) * 100
Rough_Rice3$Rd_Swap_Short <- diff(log(Rough_Rice2$Swap__Positions_Short_All)) * 100
Rough_Rice3$Rd_Money_Long <-  diff(log(Rough_Rice2$M_Money_Positions_Long_ALL)) * 100
Rough_Rice3$Rd_Money_Short <- diff(log(Rough_Rice2$M_Money_Positions_Short_ALL)) * 100
Rough_Rice3$Rd_Other_Long <- diff(log(Rough_Rice2$Other_Rept_Positions_Long_ALL)) * 100
Rough_Rice3$Rd_Other_Short <- diff(log(Rough_Rice2$Other_Rept_Positions_Short_ALL)) * 100

write.csv(Rough_Rice3,"Transfo2/Rough_Rice.csv")

#-- Soybeans ----
Soybeans <- read.csv("Transfo1/SOYBEANS.csv")

Soybeans2 <- Soybeans[,c(4,10:13,15,16,18,19)]

Soybeans2$Tot_Long <- 0
Soybeans2$Tot_Short <- 0
Soybeans2$Tot_Long_NoProd <- 0
Soybeans2$Tot_Short_NoProd <- 0

for (i in seq(2,8,by=2)){
  Soybeans2$Tot_Long <- Soybeans2$Tot_Long + Soybeans2[,i]
}
for (i in seq(3,9,by=2)){
  Soybeans2$Tot_Short <- Soybeans2$Tot_Short + Soybeans2[,i]
}
for (i in seq(4,8,by=2)){
  Soybeans2$Tot_Long_NoProd <- Soybeans2$Tot_Long_NoProd + Soybeans2[,i]
}
for (i in seq(5,9,by=2)){
  Soybeans2$Tot_Short_NoProd <- Soybeans2$Tot_Short_NoProd + Soybeans2[,i]
}

Soybeans2$Tot_Net_Position <- Soybeans2$Tot_Long - Soybeans2$Tot_Short
Soybeans2$Tot_Net_Position_NoProd <- Soybeans2$Tot_Long_NoProd - Soybeans2$Tot_Short_NoProd

for (i in (2:15)){
  for (j in (1:nrow(Soybeans2))){
    if(Soybeans2[j,i]==0){
      Soybeans2[j,i] <- 1
    }
  }
}

Soybeans3 <- as.data.frame(Soybeans2[-1,1])
Soybeans3$Rd_Long_Tot <- diff(log(Soybeans2$Tot_Long)) * 100
Soybeans3$Rd_Long_NoProd <- diff(log(Soybeans2$Tot_Long_NoProd)) * 100
Soybeans3$Rd_Short_Tot <- diff(log(Soybeans2$Tot_Short)) * 100
Soybeans3$Rd_Short_NoProd <- diff(log(Soybeans2$Tot_Short_NoProd)) * 100

Soybeans3$Rd_Net <- diff(Soybeans2$Tot_Net_Position)
Soybeans3$Rd_Net_NoProd <- diff(Soybeans2$Tot_Net_Position_NoProd)

Soybeans3$Rd_Merc_Long <- diff(log(Soybeans2$Prod_Merc_Positions_Long_ALL)) * 100
Soybeans3$Rd_Merc_Short <- diff(log(Soybeans2$Prod_Merc_Positions_Short_ALL)) * 100
Soybeans3$Rd_Swap_Long <- diff(log(Soybeans2$Swap_Positions_Long_All)) * 100
Soybeans3$Rd_Swap_Short <- diff(log(Soybeans2$Swap__Positions_Short_All)) * 100
Soybeans3$Rd_Money_Long <-  diff(log(Soybeans2$M_Money_Positions_Long_ALL)) * 100
Soybeans3$Rd_Money_Short <- diff(log(Soybeans2$M_Money_Positions_Short_ALL)) * 100
Soybeans3$Rd_Other_Long <- diff(log(Soybeans2$Other_Rept_Positions_Long_ALL)) * 100
Soybeans3$Rd_Other_Short <- diff(log(Soybeans2$Other_Rept_Positions_Short_ALL)) * 100

write.csv(Soybeans3,"Transfo2/Soybeans.csv")

#-- Sugar ----
Sugar <- read.csv("Transfo1/SUGAR.csv")

Sugar2 <- Sugar[,c(4,10:13,15,16,18,19)]

Sugar2$Tot_Long <- 0
Sugar2$Tot_Short <- 0
Sugar2$Tot_Long_NoProd <- 0
Sugar2$Tot_Short_NoProd <- 0

for (i in seq(2,8,by=2)){
  Sugar2$Tot_Long <- Sugar2$Tot_Long + Sugar2[,i]
}
for (i in seq(3,9,by=2)){
  Sugar2$Tot_Short <- Sugar2$Tot_Short + Sugar2[,i]
}
for (i in seq(4,8,by=2)){
  Sugar2$Tot_Long_NoProd <- Sugar2$Tot_Long_NoProd + Sugar2[,i]
}
for (i in seq(5,9,by=2)){
  Sugar2$Tot_Short_NoProd <- Sugar2$Tot_Short_NoProd + Sugar2[,i]
}

Sugar2$Tot_Net_Position <- Sugar2$Tot_Long - Sugar2$Tot_Short
Sugar2$Tot_Net_Position_NoProd <- Sugar2$Tot_Long_NoProd - Sugar2$Tot_Short_NoProd

for (i in (2:15)){
  for (j in (1:nrow(Sugar2))){
    if(Sugar2[j,i]==0){
      Sugar2[j,i] <- 1
    }
  }
}

Sugar3 <- as.data.frame(Sugar2[-1,1])
Sugar3$Rd_Long_Tot <- diff(log(Sugar2$Tot_Long)) * 100
Sugar3$Rd_Long_NoProd <- diff(log(Sugar2$Tot_Long_NoProd)) * 100
Sugar3$Rd_Short_Tot <- diff(log(Sugar2$Tot_Short)) * 100
Sugar3$Rd_Short_NoProd <- diff(log(Sugar2$Tot_Short_NoProd)) * 100

Sugar3$Rd_Net <- diff(Sugar2$Tot_Net_Position)
Sugar3$Rd_Net_NoProd <- diff(Sugar2$Tot_Net_Position_NoProd)

Sugar3$Rd_Merc_Long <- diff(log(Sugar2$Prod_Merc_Positions_Long_ALL)) * 100
Sugar3$Rd_Merc_Short <- diff(log(Sugar2$Prod_Merc_Positions_Short_ALL)) * 100
Sugar3$Rd_Swap_Long <- diff(log(Sugar2$Swap_Positions_Long_All)) * 100
Sugar3$Rd_Swap_Short <- diff(log(Sugar2$Swap__Positions_Short_All)) * 100
Sugar3$Rd_Money_Long <-  diff(log(Sugar2$M_Money_Positions_Long_ALL)) * 100
Sugar3$Rd_Money_Short <- diff(log(Sugar2$M_Money_Positions_Short_ALL)) * 100
Sugar3$Rd_Other_Long <- diff(log(Sugar2$Other_Rept_Positions_Long_ALL)) * 100
Sugar3$Rd_Other_Short <- diff(log(Sugar2$Other_Rept_Positions_Short_ALL)) * 100

write.csv(Sugar3,"Transfo2/Sugar.csv")

#-- Wheat_Hrw ----
Wheat_Hrw <- read.csv("Transfo1/WHEAT_HRW.csv")

Wheat_Hrw2 <- Wheat_Hrw[,c(4,10:13,15,16,18,19)]

Wheat_Hrw2$Tot_Long <- 0
Wheat_Hrw2$Tot_Short <- 0
Wheat_Hrw2$Tot_Long_NoProd <- 0
Wheat_Hrw2$Tot_Short_NoProd <- 0

for (i in seq(2,8,by=2)){
  Wheat_Hrw2$Tot_Long <- Wheat_Hrw2$Tot_Long + Wheat_Hrw2[,i]
}
for (i in seq(3,9,by=2)){
  Wheat_Hrw2$Tot_Short <- Wheat_Hrw2$Tot_Short + Wheat_Hrw2[,i]
}
for (i in seq(4,8,by=2)){
  Wheat_Hrw2$Tot_Long_NoProd <- Wheat_Hrw2$Tot_Long_NoProd + Wheat_Hrw2[,i]
}
for (i in seq(5,9,by=2)){
  Wheat_Hrw2$Tot_Short_NoProd <- Wheat_Hrw2$Tot_Short_NoProd + Wheat_Hrw2[,i]
}

Wheat_Hrw2$Tot_Net_Position <- Wheat_Hrw2$Tot_Long - Wheat_Hrw2$Tot_Short
Wheat_Hrw2$Tot_Net_Position_NoProd <- Wheat_Hrw2$Tot_Long_NoProd - Wheat_Hrw2$Tot_Short_NoProd

for (i in (2:15)){
  for (j in (1:nrow(Wheat_Hrw2))){
    if(Wheat_Hrw2[j,i]==0){
      Wheat_Hrw2[j,i] <- 1
    }
  }
}

Wheat_Hrw3 <- as.data.frame(Wheat_Hrw2[-1,1])
Wheat_Hrw3$Rd_Long_Tot <- diff(log(Wheat_Hrw2$Tot_Long)) * 100
Wheat_Hrw3$Rd_Long_NoProd <- diff(log(Wheat_Hrw2$Tot_Long_NoProd)) * 100
Wheat_Hrw3$Rd_Short_Tot <- diff(log(Wheat_Hrw2$Tot_Short)) * 100
Wheat_Hrw3$Rd_Short_NoProd <- diff(log(Wheat_Hrw2$Tot_Short_NoProd)) * 100

Wheat_Hrw3$Rd_Net <- diff(Wheat_Hrw2$Tot_Net_Position)
Wheat_Hrw3$Rd_Net_NoProd <- diff(Wheat_Hrw2$Tot_Net_Position_NoProd)

Wheat_Hrw3$Rd_Merc_Long <- diff(log(Wheat_Hrw2$Prod_Merc_Positions_Long_ALL)) * 100
Wheat_Hrw3$Rd_Merc_Short <- diff(log(Wheat_Hrw2$Prod_Merc_Positions_Short_ALL)) * 100
Wheat_Hrw3$Rd_Swap_Long <- diff(log(Wheat_Hrw2$Swap_Positions_Long_All)) * 100
Wheat_Hrw3$Rd_Swap_Short <- diff(log(Wheat_Hrw2$Swap__Positions_Short_All)) * 100
Wheat_Hrw3$Rd_Money_Long <-  diff(log(Wheat_Hrw2$M_Money_Positions_Long_ALL)) * 100
Wheat_Hrw3$Rd_Money_Short <- diff(log(Wheat_Hrw2$M_Money_Positions_Short_ALL)) * 100
Wheat_Hrw3$Rd_Other_Long <- diff(log(Wheat_Hrw2$Other_Rept_Positions_Long_ALL)) * 100
Wheat_Hrw3$Rd_Other_Short <- diff(log(Wheat_Hrw2$Other_Rept_Positions_Short_ALL)) * 100

write.csv(Wheat_Hrw3,"Transfo2/Wheat_Hrw.csv")

#-- Wheat_Srw ----
Wheat_Srw <- read.csv("Transfo1/WHEAT_SRW.csv")

Wheat_Srw2 <- Wheat_Srw[,c(4,10:13,15,16,18,19)]

Wheat_Srw2$Tot_Long <- 0
Wheat_Srw2$Tot_Short <- 0
Wheat_Srw2$Tot_Long_NoProd <- 0
Wheat_Srw2$Tot_Short_NoProd <- 0

for (i in seq(2,8,by=2)){
  Wheat_Srw2$Tot_Long <- Wheat_Srw2$Tot_Long + Wheat_Srw2[,i]
}
for (i in seq(3,9,by=2)){
  Wheat_Srw2$Tot_Short <- Wheat_Srw2$Tot_Short + Wheat_Srw2[,i]
}
for (i in seq(4,8,by=2)){
  Wheat_Srw2$Tot_Long_NoProd <- Wheat_Srw2$Tot_Long_NoProd + Wheat_Srw2[,i]
}
for (i in seq(5,9,by=2)){
  Wheat_Srw2$Tot_Short_NoProd <- Wheat_Srw2$Tot_Short_NoProd + Wheat_Srw2[,i]
}

Wheat_Srw2$Tot_Net_Position <- Wheat_Srw2$Tot_Long - Wheat_Srw2$Tot_Short
Wheat_Srw2$Tot_Net_Position_NoProd <- Wheat_Srw2$Tot_Long_NoProd - Wheat_Srw2$Tot_Short_NoProd

for (i in (2:15)){
  for (j in (1:nrow(Wheat_Srw2))){
    if(Wheat_Srw2[j,i]==0){
      Wheat_Srw2[j,i] <- 1
    }
  }
}

Wheat_Srw3 <- as.data.frame(Wheat_Srw2[-1,1])
Wheat_Srw3$Rd_Long_Tot <- diff(log(Wheat_Srw2$Tot_Long)) * 100
Wheat_Srw3$Rd_Long_NoProd <- diff(log(Wheat_Srw2$Tot_Long_NoProd)) * 100
Wheat_Srw3$Rd_Short_Tot <- diff(log(Wheat_Srw2$Tot_Short)) * 100
Wheat_Srw3$Rd_Short_NoProd <- diff(log(Wheat_Srw2$Tot_Short_NoProd)) * 100

Wheat_Srw3$Rd_Net <- diff(Wheat_Srw2$Tot_Net_Position)
Wheat_Srw3$Rd_Net_NoProd <- diff(Wheat_Srw2$Tot_Net_Position_NoProd)

Wheat_Srw3$Rd_Merc_Long <- diff(log(Wheat_Srw2$Prod_Merc_Positions_Long_ALL)) * 100
Wheat_Srw3$Rd_Merc_Short <- diff(log(Wheat_Srw2$Prod_Merc_Positions_Short_ALL)) * 100
Wheat_Srw3$Rd_Swap_Long <- diff(log(Wheat_Srw2$Swap_Positions_Long_All)) * 100
Wheat_Srw3$Rd_Swap_Short <- diff(log(Wheat_Srw2$Swap__Positions_Short_All)) * 100
Wheat_Srw3$Rd_Money_Long <-  diff(log(Wheat_Srw2$M_Money_Positions_Long_ALL)) * 100
Wheat_Srw3$Rd_Money_Short <- diff(log(Wheat_Srw2$M_Money_Positions_Short_ALL)) * 100
Wheat_Srw3$Rd_Other_Long <- diff(log(Wheat_Srw2$Other_Rept_Positions_Long_ALL)) * 100
Wheat_Srw3$Rd_Other_Short <- diff(log(Wheat_Srw2$Other_Rept_Positions_Short_ALL)) * 100

write.csv(Wheat_Srw3,"Transfo2/Wheat_Srw.csv")

#Plot 
Brent2$Report_Date_as_MM_DD_YYYY <- as.Date(Brent2$Report_Date_as_MM_DD_YYYY)
Corn2$Report_Date_as_MM_DD_YYYY <- as.Date(Corn2$Report_Date_as_MM_DD_YYYY)
Soybeans2$Report_Date_as_MM_DD_YYYY <- as.Date(Soybeans2$Report_Date_as_MM_DD_YYYY)
Sugar2$Report_Date_as_MM_DD_YYYY <- as.Date(Sugar2$Report_Date_as_MM_DD_YYYY)
WTI2$Report_Date_as_MM_DD_YYYY <- as.Date(WTI2$Report_Date_as_MM_DD_YYYY)
SILVER2$Report_Date_as_MM_DD_YYYY <- as.Date(SILVER2$Report_Date_as_MM_DD_YYYY)
GOLD2$Report_Date_as_MM_DD_YYYY <- as.Date(GOLD2$Report_Date_as_MM_DD_YYYY)
COPPER2$Report_Date_as_MM_DD_YYYY <- as.Date(COPPER2$Report_Date_as_MM_DD_YYYY)

str(Brent2)

(ggplot(WTI2) + geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Prod_Merc_Positions_Long_ALL,color="Long Prod Merc")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Prod_Merc_Positions_Short_ALL,color="Short Prod Merc")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Swap_Positions_Long_All,color="Long SWAP")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Swap__Positions_Short_All,color="Short SWAP")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=M_Money_Positions_Long_ALL,color="Long M Money")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=M_Money_Positions_Short_ALL,color="Short M Money")) +
    ylab("Nb Positions") + xlab("Date"))

(ggplot(WTI2) + geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Long,color="Total Long")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Long_NoProd,color="Total Long NoProd")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Short,color="Total Short")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Short_NoProd,color="Total Short NoProd")) +
    ylab("Nb Positions") + xlab("Date"))

(ggplot(COPPER2) + geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Prod_Merc_Positions_Long_ALL,color="Long Prod Merc")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Prod_Merc_Positions_Short_ALL,color="Short Prod Merc")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Swap_Positions_Long_All,color="Long SWAP")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Swap__Positions_Short_All,color="Short SWAP")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=M_Money_Positions_Long_ALL,color="Long M Money")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=M_Money_Positions_Short_ALL,color="Short M Money")) +
    ylab("Nb Positions") + xlab("Date"))

(ggplot(COPPER2) + geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Long,color="Total Long")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Long_NoProd,color="Total Long NoProd")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Short,color="Total Short")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Short_NoProd,color="Total Short NoProd")) +
    ylab("Nb Positions") + xlab("Date"))

(ggplot(GOLD2) + geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Prod_Merc_Positions_Long_ALL,color="Long Prod Merc")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Prod_Merc_Positions_Short_ALL,color="Short Prod Merc")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Swap_Positions_Long_All,color="Long SWAP")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Swap__Positions_Short_All,color="Short SWAP")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=M_Money_Positions_Long_ALL,color="Long M Money")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=M_Money_Positions_Short_ALL,color="Short M Money")) +
    ylab("Nb Positions") + xlab("Date"))

(ggplot(GOLD2) + geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Long,color="Total Long")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Long_NoProd,color="Total Long NoProd")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Short,color="Total Short")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Short_NoProd,color="Total Short NoProd")) +
    ylab("Nb Positions") + xlab("Date"))

(ggplot(SILVER2) + geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Prod_Merc_Positions_Long_ALL,color="Long Prod Merc")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Prod_Merc_Positions_Short_ALL,color="Short Prod Merc")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Swap_Positions_Long_All,color="Long SWAP")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Swap__Positions_Short_All,color="Short SWAP")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=M_Money_Positions_Long_ALL,color="Long M Money")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=M_Money_Positions_Short_ALL,color="Short M Money")) +
    ylab("Nb Positions") + xlab("Date"))

(ggplot(SILVER2) + geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Long,color="Total Long")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Long_NoProd,color="Total Long NoProd")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Short,color="Total Short")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Short_NoProd,color="Total Short NoProd")) +
    ylab("Nb Positions") + xlab("Date"))

(ggplot(Corn2) + geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Prod_Merc_Positions_Long_ALL,color="Long Prod Merc")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Prod_Merc_Positions_Short_ALL,color="Short Prod Merc")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Swap_Positions_Long_All,color="Long SWAP")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Swap__Positions_Short_All,color="Short SWAP")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=M_Money_Positions_Long_ALL,color="Long M Money")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=M_Money_Positions_Short_ALL,color="Short M Money")) +
    ylab("Nb Positions") + xlab("Date"))

(ggplot(Corn2) + geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Long,color="Total Long")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Long_NoProd,color="Total Long NoProd")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Short,color="Total Short")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Short_NoProd,color="Total Short NoProd")) +
    ylab("Nb Positions") + xlab("Date"))

(ggplot(Soybeans2) + geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Prod_Merc_Positions_Long_ALL,color="Long Prod Merc")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Prod_Merc_Positions_Short_ALL,color="Short Prod Merc")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Swap_Positions_Long_All,color="Long SWAP")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Swap__Positions_Short_All,color="Short SWAP")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=M_Money_Positions_Long_ALL,color="Long M Money")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=M_Money_Positions_Short_ALL,color="Short M Money")) +
    ylab("Nb Positions") + xlab("Date"))

(ggplot(Soybeans2) + geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Long,color="Total Long")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Long_NoProd,color="Total Long NoProd")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Short,color="Total Short")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Short_NoProd,color="Total Short NoProd")) +
    ylab("Nb Positions") + xlab("Date"))

(ggplot(Sugar2) + geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Prod_Merc_Positions_Long_ALL,color="Long Prod Merc")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Prod_Merc_Positions_Short_ALL,color="Short Prod Merc")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Swap_Positions_Long_All,color="Long SWAP")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Swap__Positions_Short_All,color="Short SWAP")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=M_Money_Positions_Long_ALL,color="Long M Money")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=M_Money_Positions_Short_ALL,color="Short M Money")) +
    ylab("Nb Positions") + xlab("Date"))

(ggplot(Sugar2) + geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Long,color="Total Long")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Long_NoProd,color="Total Long NoProd")) + 
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Short,color="Total Short")) +
    geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Tot_Short_NoProd,color="Total Short NoProd")) +
    ylab("Nb Positions") + xlab("Date"))

