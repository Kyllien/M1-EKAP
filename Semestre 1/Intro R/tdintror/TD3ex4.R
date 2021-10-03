#ex4

spp2<-read.csv2("C:/Users/delch/Documents/M1/IntroR/base excel/base TD2.csv")
str(spp2)

spp2<-spp2[,-c(15,25,26)]
View(spp2)


spp2$abus<-factor(spp2$abus, levels=c(0,1),labels=c("Non","Oui"))
table(spp2$abus,useNA="always")


spp2$discip<-factor(spp2$discip, levels=c(0,1),labels=c("Non","Oui"))
table(spp2$discip, useNA="always")

spp2$dep.cons<-factor(spp2$dep.cons, levels=c(0,1),labels=c("Non","Oui"))
table(spp2$dep.cons, useNA="always")

spp2$duree<-factor(spp2$duree, levels=c(1,2,3,4,5),labels=c("-1m","1-6m","6-12m","1-5an","+5an"))
table(spp2$duree, useNA="always")

View(spp2)

barplot(table(spp2$duree))
barplot(table(spp2$abus))
barplot(table(spp2$discip))
barplot(table(spp2$dep.cons))

boxplot(spp2$age, xlab="age")

#question 7
op <- par(mfrow = c(1, 2))
par(op)
peine = ifelse(spp2$duree == 5, 1, 0)
peine = data.frame(peine)
spp2<-cbind(spp2,peine)
View(spp2)

#Autre méthode
peine <- (duree)
levels(peine)
levels(peine)[1:4]<-"inf5"
levels[2] <- "sup5"
print(peine)
spp2<-cbind(spp2,peine)
View(spp2)

# Question 8

par(op)
barplot(prop.table(table(dep.cons, peine==0)))
barplot(prop.table(table(dep.cons, peine==1)))



