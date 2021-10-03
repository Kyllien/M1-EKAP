#uestion 1
cac40<- read.csv2("C:/Users/delch/Documents/M1/IntroR/base excel/cac40.csv", sep=",", header=TRUE, dec=".")
sp500<- read.csv2("C:/Users/delch/Documents/M1/IntroR/base excel/sp500.csv",sep=",", header=TRUE, dec=".")
dj<- read.csv2("C:/Users/delch/Documents/M1/IntroR/base excel/dj.csv",sep=",", header=TRUE, dec=".")
nsq<- read.csv2("C:/Users/delch/Documents/M1/IntroR/base excel/nsq.csv",sep=",", header=TRUE, dec=".")

#les extentions pour montrer que c'est un point qui est e décimal et une virgule en séparateur
View(cac40)
View(sp500)
View(dj)
View(nsq)

str(dj)

#question 2
#dj<-dj[order(dj[,1], decreasing=TRUE),]
#sp500<-sp500[order(dj[,1], decreasing=TRUE),]
#nsq<-nsq[order(dj[,1], decreasing=TRUE),]
#cac40<-cac40[order(dj[,1], decreasing=TRUE),]

#autre method, question 2/3/4/5
library(tidyverse)
library(dplyr)
cac40<-as_tibble(cac40)
dj<-as_tibble(dj)
nsq<-as_tibble(nsq)
sp500<-as_tibble(sp500)

dj1<-dj %>%
  arrange (desc(Date)) %>%
  slice (1:370) %>%
  rename(ind_dj=Close) %>%
  select(Date, ind_dj)

View(dj1)

sp<-sp500 %>%
  arrange (desc(Date)) %>%
  slice (1:370) %>%
  rename(ind_sp500=Close) %>%
  select(Date, ind_sp500)

View(sp)

CAC<-cac40 %>%
  arrange (desc(Date)) %>%
  slice (1:370) %>%
  rename(ind_CAC=Close) %>%
  select(Date, ind_CAC)

View(CAC)

nsq1<-nsq %>%
  arrange (desc(Date)) %>%
  slice (1:370) %>%
  rename(ind_nsq=Close) %>%
  select(Date, ind_nsq)

View(nsq1)

#question 6
indice<-merge(CAC,dj1, by="Date")
indice1<-merge(indice,sp, by="Date")
ind<-merge(indice1, nsq)

str(ind)
ind$Date<-as.Date(ind$Date)
ind1<-slice(ind,-370)
str(ind)
plot(ind$Date, ind$ind_dj, type="1")
plot(ind$ind_cac40, ind$ind_dj)
summary(ind1)


library(tseries)
ind_cac40<-ts(ind1$ind_CAC, frequency=12, start=c(1990,3), end=c(2020,11))
print(ind_cac40)
print(ind_cac40)
plot(ind_cac40)

ind_sp500<-ts(ind1$ind_sp500, frequency=12, start=c(1990,3), end=c(2020,11))
print(ind_sp500)
plot(ind_sp500)

ind_dj<-ts(ind1$ind_dj, frequency=12, start=c(1990,3), end=c(2020,11))
print(ind_dj)
plot(ind_dj)

ind_nsq<-ts(ind1$ind_nsq, frequency=12, start=c(1990,3), end=c(2020,11))

acf(ind_cac40)

plot(ind_cac40, ind_sp500)
cor(ind_cac40, ind_sp500)

rcac<-diff(log(ind_cac40))
rdj<-diff(log(ind_dj))
rnsq<-diff(log(ind_nsq))
rsp<-diff(log(ind_sp500))

plot(rcac, rsp)
plot(rcac)

acf(rdj)
acf(rcac)
acf(rsp)
acf(rnsq)


reg1=lm(rcac~rdj+rsp+rnsq)
summary(reg1)
#semblerai que la médianne des résidus soit proche de 0
#1Q et 3Q semblent symétrique
#on peut pense r que les résidus suivent une loi Normale

plot(reg1,2)
res<-residus(reg1)
plot(res, main="Résidus")
abline(h=0, col="red")
hist(res)

library(normtest)
jb.norm.test(residuals(reg1))
shapiro.test(res)

library(lmtest)
bptest(reg1)

library(skedastic)
white_lm(reg1, interactions=TRUE)

#p-value inf à 0,05, on rejette hyp nulle d'homo
#test Goldfield_Quants
#gqtest(reg1)

dwtest(reg1)
bgtest(reg1, order=2)

#nvlle régression
reg2=lm(rcac~redj+rsp)
summary(reg2)
plot(reg2,2)
res<-resi(reg2)
#la même chose que pour la premiere régression

library(sawdich)
rob<-NeweyWest(reg2, lag=4, preWhite=F)
print(rob)

reg2=lm(rcac~rdj+rsp)
coef(reg2)

reg3<-glm(rcac)