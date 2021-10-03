
#exercice 1
n<-10
x<-c(1,4,7,8,22,5,37)
varal<-rnorm(x)
y<-1-(0.05*x)+varal
plot(x,y)
reg<-lm(y~x)
summary(reg)


#exercice2
spp<-read.csv2("C:/Users/e164480H/Desktop/tdIntroR.csv")
str(spp)
table(spp$prof)
# autre facon pour la question 4
#attach(spp)
#summary(prof)
barplot(table(spp$prof))
pie(table(spp$prof))
hist(spp$age,xlab="age",main="historgramme age")
relation<-table(spp$prof, spp$n.enfant)
print(relation)


#Exercice 3
spp2<-read.csv2("C:/Users/delch/Documents/M1/IntroR/base excel/base TD2.csv")
str(spp2)
hist(spp2$age,xlab="age",main="historgramme des ages",col="green")
boxplot(spp2$duree) 
#il y a deux individus aberrant
plot(spp2$age,spp2$n.enfant,xlab="age",ylab="n.enfant")
#nous pouvons en déduire que bcp de détenu ont entre 0 et 4 enfants et très peu ont au dessus de 6 enfants

spp2$separation<-as.factor(spp2$separation)
spp2$juge.enfant<-as.factor(spp2$juge.enfant)
spp2$place<-as.factor(spp2$place)
spp2$abus<-as.factor(spp2$abus)
spp2$dep.cons<-as.factor(spp2$dep.cons)
spp2$scz.cons<-as.factor(spp2$scz.cons)
spp2$ago.cons<-as.factor(spp2$ago.cons)
spp2$alc.cons<-as.factor(spp2$alc.cons)
spp2$subst.cons<-as.factor(spp2$subst.cons)

summary(spp2)
mean(spp2$duree)
mean(spp2$duree)
median(spp2$duree)
mean(spp2$duree,na.rm=T)

boxplot(spp2$duree)
table(spp2$duree)


hist(spp2$duree,col="red")#ca ne représente pas une loi normale


attach(spp2)
borneinf<-4.3-1.96*0.96/sqrt(546)
bornesup<-4.3+1.96*0.96/sqrt(576)
borneinf
bornesup

plot(spp2$suicide.s,spp2$duree, xlab="suicide",ylab="duree") 


cor(duree,suicide.s,use="complete.obs") #il y a une corrélation négative netre les deux

plot(spp2$age,spp2$n.enfant,main="distrib jointe age-n.enfant", xlab="age",ylab="n.enfant")

cor(age,n.enfant,use="complete.obs")
