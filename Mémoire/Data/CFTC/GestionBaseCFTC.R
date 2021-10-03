library(readxl)

setwd("D:/M1 EKAP/Mémoire/Data/CFTC/Brut")
data2006_2015 <- read_xls("c_2006_2015.xls")
data2016 <- read_xls("c_2016.xls")
data2017 <- read_xls("c_2017.xls")
data2018 <- read_xls("c_2018.xls")
data2019 <- read_xls("c_2019.xls")
data2020 <- read_xls("c_2020.xls")
data2021 <- read_xls("c_2021.xls")

#Creation base de donnee pour SILVER - COMMODITY EXCHANGE INC. et exportation ----
dataSILVER <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="SILVER - COMMODITY EXCHANGE INC."){
    dataSILVER <- rbind(dataSILVER,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="SILVER - COMMODITY EXCHANGE INC."){
    dataSILVER <- rbind(dataSILVER,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="SILVER - COMMODITY EXCHANGE INC."){
    dataSILVER <- rbind(dataSILVER,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="SILVER - COMMODITY EXCHANGE INC."){
    dataSILVER <- rbind(dataSILVER,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="SILVER - COMMODITY EXCHANGE INC."){
    dataSILVER <- rbind(dataSILVER,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="SILVER - COMMODITY EXCHANGE INC."){
    dataSILVER <- rbind(dataSILVER,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="SILVER - COMMODITY EXCHANGE INC."){
    dataSILVER <- rbind(dataSILVER,data2021[i,])
  }
}
write.csv(dataSILVER,"SILVER.csv")


#Creation base de donnee pour GOLD - COMMODITY EXCHANGE INC. et exportation ----
dataGOLD <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="GOLD - COMMODITY EXCHANGE INC."){
    dataGOLD <- rbind(dataGOLD,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="GOLD - COMMODITY EXCHANGE INC."){
    dataGOLD <- rbind(dataGOLD,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="GOLD - COMMODITY EXCHANGE INC."){
    dataGOLD <- rbind(dataGOLD,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="GOLD - COMMODITY EXCHANGE INC."){
    dataGOLD <- rbind(dataGOLD,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="GOLD - COMMODITY EXCHANGE INC."){
    dataGOLD <- rbind(dataGOLD,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="GOLD - COMMODITY EXCHANGE INC."){
    dataGOLD <- rbind(dataGOLD,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="GOLD - COMMODITY EXCHANGE INC."){
    dataGOLD <- rbind(dataGOLD,data2021[i,])
  }
}
write.csv(dataGOLD,"GOLD.csv")



#Creation base de donnee pour GOLD - COMMODITY EXCHANGE INC. et exportation ----
dataCOPPER <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="GOLD - COMMODITY EXCHANGE INC."){
    dataCOPPER <- rbind(dataCOPPER,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="GOLD - COMMODITY EXCHANGE INC."){
    dataCOPPER <- rbind(dataCOPPER,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="GOLD - COMMODITY EXCHANGE INC."){
    dataCOPPER <- rbind(dataCOPPER,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="GOLD - COMMODITY EXCHANGE INC."){
    dataCOPPER <- rbind(dataCOPPER,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="GOLD - COMMODITY EXCHANGE INC."){
    dataCOPPER <- rbind(dataCOPPER,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="GOLD - COMMODITY EXCHANGE INC."){
    dataCOPPER <- rbind(dataCOPPER,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="GOLD - COMMODITY EXCHANGE INC."){
    dataCOPPER <- rbind(dataCOPPER,data2021[i,])
  }
}
write.csv(dataCOPPER,"COPPER.csv")


#Creation base de donnee pour WHEAT-SRW - CHICAGO BOARD OF TRADE et exportation ----
dataWHEAT_SRW <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="WHEAT-SRW - CHICAGO BOARD OF TRADE"){
    dataWHEAT_SRW <- rbind(dataWHEAT_SRW,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="WHEAT-SRW - CHICAGO BOARD OF TRADE"){
    dataWHEAT_SRW <- rbind(dataWHEAT_SRW,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="WHEAT-SRW - CHICAGO BOARD OF TRADE"){
    dataWHEAT_SRW <- rbind(dataWHEAT_SRW,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="WHEAT-SRW - CHICAGO BOARD OF TRADE"){
    dataWHEAT_SRW <- rbind(dataWHEAT_SRW,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="WHEAT-SRW - CHICAGO BOARD OF TRADE"){
    dataWHEAT_SRW <- rbind(dataWHEAT_SRW,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="WHEAT-SRW - CHICAGO BOARD OF TRADE"){
    dataWHEAT_SRW <- rbind(dataWHEAT_SRW,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="WHEAT-SRW - CHICAGO BOARD OF TRADE"){
    dataWHEAT_SRW <- rbind(dataWHEAT_SRW,data2021[i,])
  }
}
write.csv(dataWHEAT_SRW,"WHEAT_SRW.csv")

#Creation base de donnee pour BLACK SEA WHEAT FINANCIAL - CHICAGO BOARD OF TRADE et exportation ----
dataBLACK_SEA_WHEAT <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="BLACK SEA WHEAT FINANCIAL - CHICAGO BOARD OF TRADE"){
    dataBLACK_SEA_WHEAT <- rbind(dataBLACK_SEA_WHEAT,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="BLACK SEA WHEAT FINANCIAL - CHICAGO BOARD OF TRADE"){
    dataBLACK_SEA_WHEAT <- rbind(dataBLACK_SEA_WHEAT,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="BLACK SEA WHEAT FINANCIAL - CHICAGO BOARD OF TRADE"){
    dataBLACK_SEA_WHEAT <- rbind(dataBLACK_SEA_WHEAT,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="BLACK SEA WHEAT FINANCIAL - CHICAGO BOARD OF TRADE"){
    dataBLACK_SEA_WHEAT <- rbind(dataBLACK_SEA_WHEAT,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="BLACK SEA WHEAT FINANCIAL - CHICAGO BOARD OF TRADE"){
    dataBLACK_SEA_WHEAT <- rbind(dataBLACK_SEA_WHEAT,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="BLACK SEA WHEAT FINANCIAL - CHICAGO BOARD OF TRADE"){
    dataBLACK_SEA_WHEAT <- rbind(dataBLACK_SEA_WHEAT,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="BLACK SEA WHEAT FINANCIAL - CHICAGO BOARD OF TRADE"){
    dataBLACK_SEA_WHEAT <- rbind(dataBLACK_SEA_WHEAT,data2021[i,])
  }
}
write.csv(dataBLACK_SEA_WHEAT,"BLACK_SEA_WHEAT.csv")

#Creation base de donnee pour WHEAT-HRW - CHICAGO BOARD OF TRADE et exportation ----
dataWHEAT_HRW <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="WHEAT-HRW - CHICAGO BOARD OF TRADE"){
    dataWHEAT_HRW <- rbind(dataWHEAT_HRW,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="WHEAT-HRW - CHICAGO BOARD OF TRADE"){
    dataWHEAT_HRW <- rbind(dataWHEAT_HRW,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="WHEAT-HRW - CHICAGO BOARD OF TRADE"){
    dataWHEAT_HRW <- rbind(dataWHEAT_HRW,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="WHEAT-HRW - CHICAGO BOARD OF TRADE"){
    dataWHEAT_HRW <- rbind(dataWHEAT_HRW,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="WHEAT-HRW - CHICAGO BOARD OF TRADE"){
    dataWHEAT_HRW <- rbind(dataWHEAT_HRW,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="WHEAT-HRW - CHICAGO BOARD OF TRADE"){
    dataWHEAT_HRW <- rbind(dataWHEAT_HRW,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="WHEAT-HRW - CHICAGO BOARD OF TRADE"){
    dataWHEAT_HRW <- rbind(dataWHEAT_HRW,data2021[i,])
  }
}
write.csv(dataWHEAT_HRW,"WHEAT_HRW.csv")

#Creation base de donnee pour CORN - CHICAGO BOARD OF TRADE et exportation ----
dataCORN <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="CORN - CHICAGO BOARD OF TRADE"){
    dataCORN <- rbind(dataCORN,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="CORN - CHICAGO BOARD OF TRADE"){
    dataCORN <- rbind(dataCORN,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="CORN - CHICAGO BOARD OF TRADE"){
    dataCORN <- rbind(dataCORN,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="CORN - CHICAGO BOARD OF TRADE"){
    dataCORN <- rbind(dataCORN,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="CORN - CHICAGO BOARD OF TRADE"){
    dataCORN <- rbind(dataCORN,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="CORN - CHICAGO BOARD OF TRADE"){
    dataCORN <- rbind(dataCORN,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="CORN - CHICAGO BOARD OF TRADE"){
    dataCORN <- rbind(dataCORN,data2021[i,])
  }
}
write.csv(dataCORN,"CORN.csv")





#Creation base de donnee pour OATS - CHICAGO BOARD OF TRADE et exportation ----
dataOATS <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="OATS - CHICAGO BOARD OF TRADE"){
    dataOATS <- rbind(dataOATS,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="OATS - CHICAGO BOARD OF TRADE"){
    dataOATS <- rbind(dataOATS,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="OATS - CHICAGO BOARD OF TRADE"){
    dataOATS <- rbind(dataOATS,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="OATS - CHICAGO BOARD OF TRADE"){
    dataOATS <- rbind(dataOATS,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="OATS - CHICAGO BOARD OF TRADE"){
    dataOATS <- rbind(dataOATS,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="OATS - CHICAGO BOARD OF TRADE"){
    dataOATS <- rbind(dataOATS,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="OATS - CHICAGO BOARD OF TRADE"){
    dataOATS <- rbind(dataOATS,data2021[i,])
  }
}
write.csv(dataOATS,"OATS.csv")

#Creation base de donnee pour SOYBEANS - CHICAGO BOARD OF TRADE et exportation ----
dataSOYBEANS <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="SOYBEANS - CHICAGO BOARD OF TRADE"){
    dataSOYBEANS <- rbind(dataSOYBEANS,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="SOYBEANS - CHICAGO BOARD OF TRADE"){
    dataSOYBEANS <- rbind(dataSOYBEANS,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="SOYBEANS - CHICAGO BOARD OF TRADE"){
    dataSOYBEANS <- rbind(dataSOYBEANS,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="SOYBEANS - CHICAGO BOARD OF TRADE"){
    dataSOYBEANS <- rbind(dataSOYBEANS,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="SOYBEANS - CHICAGO BOARD OF TRADE"){
    dataSOYBEANS <- rbind(dataSOYBEANS,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="SOYBEANS - CHICAGO BOARD OF TRADE"){
    dataSOYBEANS <- rbind(dataSOYBEANS,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="SOYBEANS - CHICAGO BOARD OF TRADE"){
    dataSOYBEANS <- rbind(dataSOYBEANS,data2021[i,])
  }
}
write.csv(dataSOYBEANS,"SOYBEANS.csv")


#Creation base de donnee pour MINI SOYBEANS - CHICAGO BOARD OF TRADE et exportation ----
dataMINI_SOYBEANS <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="MINI SOYBEANS - CHICAGO BOARD OF TRADE"){
    dataMINI_SOYBEANS <- rbind(dataMINI_SOYBEANS,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="MINI SOYBEANS - CHICAGO BOARD OF TRADE"){
    dataMINI_SOYBEANS <- rbind(dataMINI_SOYBEANS,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="MINI SOYBEANS - CHICAGO BOARD OF TRADE"){
    dataMINI_SOYBEANS <- rbind(dataMINI_SOYBEANS,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="MINI SOYBEANS - CHICAGO BOARD OF TRADE"){
    dataMINI_SOYBEANS <- rbind(dataMINI_SOYBEANS,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="MINI SOYBEANS - CHICAGO BOARD OF TRADE"){
    dataMINI_SOYBEANS <- rbind(dataMINI_SOYBEANS,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="MINI SOYBEANS - CHICAGO BOARD OF TRADE"){
    dataMINI_SOYBEANS <- rbind(dataMINI_SOYBEANS,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="MINI SOYBEANS - CHICAGO BOARD OF TRADE"){
    dataMINI_SOYBEANS <- rbind(dataMINI_SOYBEANS,data2021[i,])
  }
}
write.csv(dataMINI_SOYBEANS,"MINI_SOYBEANS.csv")

#Creation base de donnee pour SOYBEAN CSO - CHICAGO BOARD OF TRADE et exportation ----
dataSOYBEAN_CSO <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="SOYBEAN CSO - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_CSO <- rbind(dataSOYBEAN_CSO,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="SOYBEAN CSO - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_CSO <- rbind(dataSOYBEAN_CSO,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="SOYBEAN CSO - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_CSO <- rbind(dataSOYBEAN_CSO,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="SOYBEAN CSO - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_CSO <- rbind(dataSOYBEAN_CSO,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="SOYBEAN CSO - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_CSO <- rbind(dataSOYBEAN_CSO,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="SOYBEAN CSO - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_CSO <- rbind(dataSOYBEAN_CSO,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="SOYBEAN CSO - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_CSO <- rbind(dataSOYBEAN_CSO,data2021[i,])
  }
}
write.csv(dataSOYBEAN_CSO,"SOYBEAN_CSO.csv")



#Creation base de donnee pour SOYBEAN OIL - CHICAGO BOARD OF TRADE et exportation ----
dataSOYBEAN_OIL <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="SOYBEAN OIL - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_OIL <- rbind(dataSOYBEAN_OIL,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="SOYBEAN OIL - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_OIL <- rbind(dataSOYBEAN_OIL,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="SOYBEAN OIL - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_OIL <- rbind(dataSOYBEAN_OIL,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="SOYBEAN OIL - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_OIL <- rbind(dataSOYBEAN_OIL,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="SOYBEAN OIL - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_OIL <- rbind(dataSOYBEAN_OIL,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="SOYBEAN OIL - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_OIL <- rbind(dataSOYBEAN_OIL,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="SOYBEAN OIL - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_OIL <- rbind(dataSOYBEAN_OIL,data2021[i,])
  }
}
write.csv(dataSOYBEAN_OIL,"SOYBEAN_OIL.csv")


#Creation base de donnee pour CBT ETHANOL - CHICAGO BOARD OF TRADE et exportation ----
dataCBT_ETHANOL <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="CBT ETHANOL - CHICAGO BOARD OF TRADE"){
    dataCBT_ETHANOL <- rbind(dataCBT_ETHANOL,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="CBT ETHANOL - CHICAGO BOARD OF TRADE"){
    dataCBT_ETHANOL <- rbind(dataCBT_ETHANOL,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="CBT ETHANOL - CHICAGO BOARD OF TRADE"){
    dataCBT_ETHANOL <- rbind(dataCBT_ETHANOL,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="CBT ETHANOL - CHICAGO BOARD OF TRADE"){
    dataCBT_ETHANOL <- rbind(dataCBT_ETHANOL,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="CBT ETHANOL - CHICAGO BOARD OF TRADE"){
    dataCBT_ETHANOL <- rbind(dataCBT_ETHANOL,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="CBT ETHANOL - CHICAGO BOARD OF TRADE"){
    dataCBT_ETHANOL <- rbind(dataCBT_ETHANOL,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="CBT ETHANOL - CHICAGO BOARD OF TRADE"){
    dataCBT_ETHANOL <- rbind(dataCBT_ETHANOL,data2021[i,])
  }
}
write.csv(dataCBT_ETHANOL,"CBT_ETHANOL.csv")



#Creation base de donnee pour SOYBEAN MEAL - CHICAGO BOARD OF TRADE et exportation ----
dataSOYBEAN_MEAL <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="SOYBEAN MEAL - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_MEAL <- rbind(dataSOYBEAN_MEAL,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="SOYBEAN MEAL - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_MEAL <- rbind(dataSOYBEAN_MEAL,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="SOYBEAN MEAL - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_MEAL <- rbind(dataSOYBEAN_MEAL,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="SOYBEAN MEAL - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_MEAL <- rbind(dataSOYBEAN_MEAL,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="SOYBEAN MEAL - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_MEAL <- rbind(dataSOYBEAN_MEAL,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="SOYBEAN MEAL - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_MEAL <- rbind(dataSOYBEAN_MEAL,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="SOYBEAN MEAL - CHICAGO BOARD OF TRADE"){
    dataSOYBEAN_MEAL <- rbind(dataSOYBEAN_MEAL,data2021[i,])
  }
}
write.csv(dataSOYBEAN_MEAL,"SOYBEAN_MEAL.csv")


#Creation base de donnee pour ROUGH RICE - CHICAGO BOARD OF TRADE et exportation ----
dataROUGH_RICE <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="ROUGH RICE - CHICAGO BOARD OF TRADE"){
    dataROUGH_RICE <- rbind(dataROUGH_RICE,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="ROUGH RICE - CHICAGO BOARD OF TRADE"){
    dataROUGH_RICE <- rbind(dataROUGH_RICE,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="ROUGH RICE - CHICAGO BOARD OF TRADE"){
    dataROUGH_RICE <- rbind(dataROUGH_RICE,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="ROUGH RICE - CHICAGO BOARD OF TRADE"){
    dataROUGH_RICE <- rbind(dataROUGH_RICE,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="ROUGH RICE - CHICAGO BOARD OF TRADE"){
    dataROUGH_RICE <- rbind(dataROUGH_RICE,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="ROUGH RICE - CHICAGO BOARD OF TRADE"){
    dataROUGH_RICE <- rbind(dataROUGH_RICE,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="ROUGH RICE - CHICAGO BOARD OF TRADE"){
    dataROUGH_RICE <- rbind(dataROUGH_RICE,data2021[i,])
  }
}
write.csv(dataROUGH_RICE,"ROUGH_RICE.csv")


#Creation base de donnee pour MILK, Class III - CHICAGO MERCANTILE EXCHANGE et exportation ----
dataMILK <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="MILK, Class III - CHICAGO MERCANTILE EXCHANGE"){
    dataMILK <- rbind(dataMILK,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="MILK, Class III - CHICAGO MERCANTILE EXCHANGE"){
    dataMILK <- rbind(dataMILK,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="MILK, Class III - CHICAGO MERCANTILE EXCHANGE"){
    dataMILK <- rbind(dataMILK,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="MILK, Class III - CHICAGO MERCANTILE EXCHANGE"){
    dataMILK <- rbind(dataMILK,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="MILK, Class III - CHICAGO MERCANTILE EXCHANGE"){
    dataMILK <- rbind(dataMILK,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="MILK, Class III - CHICAGO MERCANTILE EXCHANGE"){
    dataMILK <- rbind(dataMILK,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="MILK, Class III - CHICAGO MERCANTILE EXCHANGE"){
    dataMILK <- rbind(dataMILK,data2021[i,])
  }
}
write.csv(dataMILK,"MILK.csv")





#Creation base de donnee pour LEAN HOGS - CHICAGO MERCANTILE EXCHANGE et exportation ----
dataLEAN_HOGS <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="LEAN HOGS - CHICAGO MERCANTILE EXCHANGE"){
    dataLEAN_HOGS <- rbind(dataLEAN_HOGS,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="LEAN HOGS - CHICAGO MERCANTILE EXCHANGE"){
    dataLEAN_HOGS <- rbind(dataLEAN_HOGS,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="LEAN HOGS - CHICAGO MERCANTILE EXCHANGE"){
    dataLEAN_HOGS <- rbind(dataLEAN_HOGS,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="LEAN HOGS - CHICAGO MERCANTILE EXCHANGE"){
    dataLEAN_HOGS <- rbind(dataLEAN_HOGS,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="LEAN HOGS - CHICAGO MERCANTILE EXCHANGE"){
    dataLEAN_HOGS <- rbind(dataLEAN_HOGS,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="LEAN HOGS - CHICAGO MERCANTILE EXCHANGE"){
    dataLEAN_HOGS <- rbind(dataLEAN_HOGS,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="LEAN HOGS - CHICAGO MERCANTILE EXCHANGE"){
    dataLEAN_HOGS <- rbind(dataLEAN_HOGS,data2021[i,])
  }
}
write.csv(dataLEAN_HOGS,"LEAN_HOGS.csv")



#Creation base de donnee pour LIVE CATTLE - CHICAGO MERCANTILE EXCHANGE et exportation ----
dataLIVE_CATTLE <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="LIVE CATTLE - CHICAGO MERCANTILE EXCHANGE"){
    dataLIVE_CATTLE <- rbind(dataLIVE_CATTLE,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="LIVE CATTLE - CHICAGO MERCANTILE EXCHANGE"){
    dataLIVE_CATTLE <- rbind(dataLIVE_CATTLE,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="LIVE CATTLE - CHICAGO MERCANTILE EXCHANGE"){
    dataLIVE_CATTLE <- rbind(dataLIVE_CATTLE,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="LIVE CATTLE - CHICAGO MERCANTILE EXCHANGE"){
    dataLIVE_CATTLE <- rbind(dataLIVE_CATTLE,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="LIVE CATTLE - CHICAGO MERCANTILE EXCHANGE"){
    dataLIVE_CATTLE <- rbind(dataLIVE_CATTLE,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="LIVE CATTLE - CHICAGO MERCANTILE EXCHANGE"){
    dataLIVE_CATTLE <- rbind(dataLIVE_CATTLE,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="LIVE CATTLE - CHICAGO MERCANTILE EXCHANGE"){
    dataLIVE_CATTLE <- rbind(dataLIVE_CATTLE,data2021[i,])
  }
}
write.csv(dataLIVE_CATTLE,"LIVE_CATTLE.csv")

#Creation base de donnee pour FEEDER CATTLE - CHICAGO MERCANTILE EXCHANGE et exportation ----
dataFEEDER_CATTLE <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="FEEDER CATTLE - CHICAGO MERCANTILE EXCHANGE"){
    dataFEEDER_CATTLE <- rbind(dataFEEDER_CATTLE,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="FEEDER CATTLE - CHICAGO MERCANTILE EXCHANGE"){
    dataFEEDER_CATTLE <- rbind(dataFEEDER_CATTLE,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="FEEDER CATTLE - CHICAGO MERCANTILE EXCHANGE"){
    dataFEEDER_CATTLE <- rbind(dataFEEDER_CATTLE,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="FEEDER CATTLE - CHICAGO MERCANTILE EXCHANGE"){
    dataFEEDER_CATTLE <- rbind(dataFEEDER_CATTLE,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="FEEDER CATTLE - CHICAGO MERCANTILE EXCHANGE"){
    dataFEEDER_CATTLE <- rbind(dataFEEDER_CATTLE,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="FEEDER CATTLE - CHICAGO MERCANTILE EXCHANGE"){
    dataFEEDER_CATTLE <- rbind(dataFEEDER_CATTLE,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="FEEDER CATTLE - CHICAGO MERCANTILE EXCHANGE"){
    dataFEEDER_CATTLE <- rbind(dataFEEDER_CATTLE,data2021[i,])
  }
}
write.csv(dataFEEDER_CATTLE,"FEEDER_CATTLE.csv")

#Creation base de donnee pour CHEESE (CASH-SETTLED) - CHICAGO MERCANTILE EXCHANGE et exportation ----
dataCHEESE <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="CHEESE (CASH-SETTLED) - CHICAGO MERCANTILE EXCHANGE"){
    dataCHEESE <- rbind(dataCHEESE,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="CHEESE (CASH-SETTLED) - CHICAGO MERCANTILE EXCHANGE"){
    dataCHEESE <- rbind(dataCHEESE,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="CHEESE (CASH-SETTLED) - CHICAGO MERCANTILE EXCHANGE"){
    dataCHEESE <- rbind(dataCHEESE,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="CHEESE (CASH-SETTLED) - CHICAGO MERCANTILE EXCHANGE"){
    dataCHEESE <- rbind(dataCHEESE,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="CHEESE (CASH-SETTLED) - CHICAGO MERCANTILE EXCHANGE"){
    dataCHEESE <- rbind(dataCHEESE,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="CHEESE (CASH-SETTLED) - CHICAGO MERCANTILE EXCHANGE"){
    dataCHEESE <- rbind(dataCHEESE,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="CHEESE (CASH-SETTLED) - CHICAGO MERCANTILE EXCHANGE"){
    dataCHEESE <- rbind(dataCHEESE,data2021[i,])
  }
}
write.csv(dataCHEESE,"CHEESE.csv")

#Creation base de donnee pour NATURAL GAS - NEW YORK MERCANTILE EXCHANGE et exportation ----
dataNATURAL_GAS <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="NATURAL GAS - NEW YORK MERCANTILE EXCHANGE"){
    dataNATURAL_GAS <- rbind(dataNATURAL_GAS,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="NATURAL GAS - NEW YORK MERCANTILE EXCHANGE"){
    dataNATURAL_GAS <- rbind(dataNATURAL_GAS,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="NATURAL GAS - NEW YORK MERCANTILE EXCHANGE"){
    dataNATURAL_GAS <- rbind(dataNATURAL_GAS,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="NATURAL GAS - NEW YORK MERCANTILE EXCHANGE"){
    dataNATURAL_GAS <- rbind(dataNATURAL_GAS,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="NATURAL GAS - NEW YORK MERCANTILE EXCHANGE"){
    dataNATURAL_GAS <- rbind(dataNATURAL_GAS,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="NATURAL GAS - NEW YORK MERCANTILE EXCHANGE"){
    dataNATURAL_GAS <- rbind(dataNATURAL_GAS,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="NATURAL GAS - NEW YORK MERCANTILE EXCHANGE"){
    dataNATURAL_GAS <- rbind(dataNATURAL_GAS,data2021[i,])
  }
}
write.csv(dataNATURAL_GAS,"NATURAL_GAS.csv")

#Creation base de donnee pour CRUDE OIL, LIGHT SWEET - NEW YORK MERCANTILE EXCHANGE et exportation ----
dataCRUDE_OIL_NY <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="CRUDE OIL, LIGHT SWEET - NEW YORK MERCANTILE EXCHANGE"){
    dataCRUDE_OIL_NY <- rbind(dataCRUDE_OIL_NY,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="CRUDE OIL, LIGHT SWEET - NEW YORK MERCANTILE EXCHANGE"){
    dataCRUDE_OIL_NY <- rbind(dataCRUDE_OIL_NY,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="CRUDE OIL, LIGHT SWEET - NEW YORK MERCANTILE EXCHANGE"){
    dataCRUDE_OIL_NY <- rbind(dataCRUDE_OIL_NY,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="CRUDE OIL, LIGHT SWEET - NEW YORK MERCANTILE EXCHANGE"){
    dataCRUDE_OIL_NY <- rbind(dataCRUDE_OIL_NY,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="CRUDE OIL, LIGHT SWEET - NEW YORK MERCANTILE EXCHANGE"){
    dataCRUDE_OIL_NY <- rbind(dataCRUDE_OIL_NY,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="CRUDE OIL, LIGHT SWEET - NEW YORK MERCANTILE EXCHANGE"){
    dataCRUDE_OIL_NY <- rbind(dataCRUDE_OIL_NY,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="CRUDE OIL, LIGHT SWEET - NEW YORK MERCANTILE EXCHANGE"){
    dataCRUDE_OIL_NY <- rbind(dataCRUDE_OIL_NY,data2021[i,])
  }
}
write.csv(dataCRUDE_OIL_NY,"CRUDE_OIL_NY.csv")


#Creation base de donnee pour BRENT CRUDE OIL LAST DAY - NEW YORK MERCANTILE EXCHANGE et exportation ----
dataBRENT <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="BRENT CRUDE OIL LAST DAY - NEW YORK MERCANTILE EXCHANGE"){
    dataBRENT <- rbind(dataBRENT,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="BRENT CRUDE OIL LAST DAY - NEW YORK MERCANTILE EXCHANGE"){
    dataBRENT <- rbind(dataBRENT,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="BRENT CRUDE OIL LAST DAY - NEW YORK MERCANTILE EXCHANGE"){
    dataBRENT <- rbind(dataBRENT,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="BRENT CRUDE OIL LAST DAY - NEW YORK MERCANTILE EXCHANGE"){
    dataBRENT <- rbind(dataBRENT,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="BRENT CRUDE OIL LAST DAY - NEW YORK MERCANTILE EXCHANGE"){
    dataBRENT <- rbind(dataBRENT,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="BRENT CRUDE OIL LAST DAY - NEW YORK MERCANTILE EXCHANGE"){
    dataBRENT <- rbind(dataBRENT,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="BRENT CRUDE OIL LAST DAY - NEW YORK MERCANTILE EXCHANGE"){
    dataBRENT <- rbind(dataBRENT,data2021[i,])
  }
}
write.csv(dataBRENT,"BRENT.csv")


#Creation base de donnee pour COTTON NO. 2 - ICE FUTURES U.S. et exportation ----
dataCOTTON <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="COTTON NO. 2 - ICE FUTURES U.S."){
    dataCOTTON <- rbind(dataCOTTON,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="COTTON NO. 2 - ICE FUTURES U.S."){
    dataCOTTON <- rbind(dataCOTTON,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="COTTON NO. 2 - ICE FUTURES U.S."){
    dataCOTTON <- rbind(dataCOTTON,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="COTTON NO. 2 - ICE FUTURES U.S."){
    dataCOTTON <- rbind(dataCOTTON,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="COTTON NO. 2 - ICE FUTURES U.S."){
    dataCOTTON <- rbind(dataCOTTON,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="COTTON NO. 2 - ICE FUTURES U.S."){
    dataCOTTON <- rbind(dataCOTTON,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="COTTON NO. 2 - ICE FUTURES U.S."){
    dataCOTTON <- rbind(dataCOTTON,data2021[i,])
  }
}
write.csv(dataCOTTON,"COTTON.csv")


#Creation base de donnee pour COCOA - ICE FUTURES U.S. et exportation ----
dataCOCOA <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="COCOA - ICE FUTURES U.S."){
    dataCOCOA <- rbind(dataCOCOA,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="COCOA - ICE FUTURES U.S."){
    dataCOCOA <- rbind(dataCOCOA,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="COCOA - ICE FUTURES U.S."){
    dataCOCOA <- rbind(dataCOCOA,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="COCOA - ICE FUTURES U.S."){
    dataCOCOA <- rbind(dataCOCOA,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="COCOA - ICE FUTURES U.S."){
    dataCOCOA <- rbind(dataCOCOA,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="COCOA - ICE FUTURES U.S."){
    dataCOCOA <- rbind(dataCOCOA,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="COCOA - ICE FUTURES U.S."){
    dataCOCOA <- rbind(dataCOCOA,data2021[i,])
  }
}
write.csv(dataCOCOA,"COCOA.csv")


#Creation base de donnee pour SUGAR NO. 11 - ICE FUTURES U.S. et exportation ----
dataSUGAR <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="SUGAR NO. 11 - ICE FUTURES U.S."){
    dataSUGAR <- rbind(dataSUGAR,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="SUGAR NO. 11 - ICE FUTURES U.S."){
    dataSUGAR <- rbind(dataSUGAR,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="SUGAR NO. 11 - ICE FUTURES U.S."){
    dataSUGAR <- rbind(dataSUGAR,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="SUGAR NO. 11 - ICE FUTURES U.S."){
    dataSUGAR <- rbind(dataSUGAR,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="SUGAR NO. 11 - ICE FUTURES U.S."){
    dataSUGAR <- rbind(dataSUGAR,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="SUGAR NO. 11 - ICE FUTURES U.S."){
    dataSUGAR <- rbind(dataSUGAR,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="SUGAR NO. 11 - ICE FUTURES U.S."){
    dataSUGAR <- rbind(dataSUGAR,data2021[i,])
  }
}
write.csv(dataSUGAR,"SUGAR.csv")



#Creation base de donnee pour COFFEE C - ICE FUTURES U.S. et exportation ----
dataCOFFEE <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="COFFEE C - ICE FUTURES U.S."){
    dataCOFFEE <- rbind(dataCOFFEE,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="COFFEE C - ICE FUTURES U.S."){
    dataCOFFEE <- rbind(dataCOFFEE,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="COFFEE C - ICE FUTURES U.S."){
    dataCOFFEE <- rbind(dataCOFFEE,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="COFFEE C - ICE FUTURES U.S."){
    dataCOFFEE <- rbind(dataCOFFEE,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="COFFEE C - ICE FUTURES U.S."){
    dataCOFFEE <- rbind(dataCOFFEE,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="COFFEE C - ICE FUTURES U.S."){
    dataCOFFEE <- rbind(dataCOFFEE,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="COFFEE C - ICE FUTURES U.S."){
    dataCOFFEE <- rbind(dataCOFFEE,data2021[i,])
  }
}
write.csv(dataCOFFEE,"COFFEE.csv")


#Creation base de donnee pour CRUDE OIL, LIGHT SWEET-WTI - ICE FUTURES EUROPE et exportation ----
dataWTI_EUR <- data2016[NULL,names(data2016)] #Mise en place de la data empty juste avec les noms des variables
for(i in nrow(data2006_2015):1){
  if(data2006_2015[i,1]=="CRUDE OIL, LIGHT SWEET-WTI - ICE FUTURES EUROPE"){
    dataWTI_EUR <- rbind(dataWTI_EUR,data2006_2015[i,])
  }
}
for(i in 1:nrow(data2016)){
  if(data2016[i,1]=="CRUDE OIL, LIGHT SWEET-WTI - ICE FUTURES EUROPE"){
    dataWTI_EUR <- rbind(dataWTI_EUR,data2016[i,])
  }
}
for(i in 1:nrow(data2017)){
  if(data2017[i,1]=="CRUDE OIL, LIGHT SWEET-WTI - ICE FUTURES EUROPE"){
    dataWTI_EUR <- rbind(dataWTI_EUR,data2017[i,])
  }
}
for(i in 1:nrow(data2018)){
  if(data2018[i,1]=="CRUDE OIL, LIGHT SWEET-WTI - ICE FUTURES EUROPE"){
    dataWTI_EUR <- rbind(dataWTI_EUR,data2018[i,])
  }
}
for(i in 1:nrow(data2019)){
  if(data2019[i,1]=="CRUDE OIL, LIGHT SWEET-WTI - ICE FUTURES EUROPE"){
    dataWTI_EUR <- rbind(dataWTI_EUR,data2019[i,])
  }
}
for(i in 1:nrow(data2020)){
  if(data2020[i,1]=="CRUDE OIL, LIGHT SWEET-WTI - ICE FUTURES EUROPE"){
    dataWTI_EUR <- rbind(dataWTI_EUR,data2020[i,])
  }
}
for(i in 1:nrow(data2021)){
  if(data2021[i,1]=="CRUDE OIL, LIGHT SWEET-WTI - ICE FUTURES EUROPE"){
    dataWTI_EUR <- rbind(dataWTI_EUR,data2021[i,])
  }
}
write.csv(dataWTI_EUR,"WTI_EUR.csv")

