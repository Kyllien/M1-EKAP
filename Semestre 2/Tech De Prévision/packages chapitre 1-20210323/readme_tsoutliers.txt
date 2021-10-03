# Using R code for outlier detection with tsoutliers package
# Detection of Outliers in Time Series
# January 2018

# installer le package stR
# charger le package stR

# les données sont dans c:\R\data
# Monthly data
library(tsoutliers)
y <- read.table("c:\\R\\data\\demandcvs.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)

# Automatic Procedure for Detection of Outliers
tso(yy)

fit <- tso(yy)
plot(fit)
show(fit)

# outlier-adjusted series
adj <- fit$yadj
plot(adj)
write(t(adj),file="ipi1984_TC.out",ncolumn=1,append=FALSE)
