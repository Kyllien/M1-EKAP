#ex5

info<-read.csv2("C:/Users/delch/Documents/M1/IntroR/base excel/base TD3A.csv")
str(info)
View(info)
info<-info[,-c(1)]
View(info)
summary(info)
str(info)

info2<-read.csv2("C:/Users/delch/Documents/M1/IntroR/base excel/base TD3B.csv")
str(info2)
View(info2)
info2<-info2[,-c(1)]
View(info2)
summary(info2)
str(info2)

#il y a un nombre d'obs différentes, mais les variables restent les même

library(prettyR)
describe(info)
describe(info2)

#question 10,11

ep<-rbind(data.frame(info),data.frame(info2))
View(ep)
attach(ep)
ep1<-ep[order(ep$ident),]
View(ep1)
str(ep1)

#question 12,13,14

infofin<-read.csv2("C:/Users/delch/Documents/M1/IntroR/base excel/base TD3C.csv")
View(infofin)
str(infofin)
infofin<-infofin[,-c(1)]
str(infofin)
View(infofin)
#Il y e 3 variables binaires et deux variables numérique

#question 15

completset<- merge(ep1,infofin, by="ident")
str(completset)
View(completset)

#question 16

write.csv2(completset,file="C:/Users/delch/Documents/M1/IntroR/compleset.csv",row.names=TRUE)

#question 17
library(dplyr)
femme<-case_when(
  completset$gender=="female"~1,  #== signifie équivaut à, ce qu'on l'on observe
  completset$gender=="male"~0)
homme<-case_when(
  completset$gender=="female"~0,
  completset$gender=="male"~1)

completset<-cbind(completset,homme)
completset<-cbind(completset,femme)
View(completset)

#question 18

write.csv2(completset,file="C:/Users/delch/Documents/M1/IntroR/compleset1.csv",row.names=TRUE)

#question 19

tapply(completset$age,completset$gender,"mean")
tapply(completset$inc,completset$gender,"mean")

#en moyenne dans l'échantillon le femmes ont 41 ans et les hommes 38 ans
#en moyenne les femmes gagnent 40 108 $ a l'année et les hommes gagnent en moyenne 35 934 $ à l'année



