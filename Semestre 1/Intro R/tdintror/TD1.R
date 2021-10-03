n<-10
x<-c(1,4,7,8,22,5,37)
varal<-rnorm(x)
y<-1-(0.05*x)+varal
plot(x,y)
reg<-lm(y~x)
summary(reg)

#exercice2
spp<-read.csv2("C:/Users/delch/Documents/M1/IntroR/base excel/base TD1.csv")
str(spp)
attach(spp)
summary(prof)
pie(table(spp$prof))
hist(spp$age,xlab="age")
plot(prof,n.enfant)
