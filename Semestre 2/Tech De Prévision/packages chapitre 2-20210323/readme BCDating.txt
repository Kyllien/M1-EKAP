# Using R code for dating business cycles with BCDating package

# installer le package BCDating
# charger le package BCDating

# les données sont dans c:\R\data
# Monthly data
library(seasonal)
library(BCDating)
library(mFilter)

# Business cycle
y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)
seasX <- seas(yy)
trendcycle <- trend(seasX)

dat <- BBQ(trendcycle, name="Dating Business Cycles")
show(dat)
summary(dat)
plot(dat)

-------------------------------------------------------------
# Acceleration cycle
cvs <- final(seasX)
diffcvs <- diff(cvs, difference=1)
filter <- hpfilter(diffcvs, 13.9)
plot(filter)

trend <- filter$trend
dat <- BBQ(trend, name="Dating Acceleration Cycles")
show(dat)
summary(dat)
plot(dat)


***************************************************************
# Bry-Boschan algorithm
dat <- BBQ(yy, name="Dating Business Cycles")
show(dat)
summary(dat)
plot(dat)
***************************************************************



