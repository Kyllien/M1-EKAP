library(readxl)
library(dplyr)
library(tidyr)
library(car)
library(stringr)

setwd("D:/M1 EKAP/Semestre 2/Gretl")
WTI <- read.csv("WTI.csv")
SPGSCI <- read.csv("SPGSCI.csv")
SP500 <- read.csv("SP500.csv")

data <- as.data.frame(cbind(WTI$Dernier,SPGSCI$Dernier,SP500$Dernier)) #???A faire avec un merge by date
colnames(data)<-c("WTI","SPGSCI","SP500")
summary(data)

xs <- function(x){
  str_replace(x,"\\.","")
  
}
xz <- function(x){
  str_replace(x,",","\\.")
}

xd <- function(x){
  x <- sapply(x,xs)
  x <- sapply(x,xz)
}

data2 <- as.data.frame(sapply(data,xd))
data2 <- as.data.frame(sapply(data2[],as.numeric))

data2<-data2[dim(data2)[1]:1,]
write.csv(data2,"data.csv")


