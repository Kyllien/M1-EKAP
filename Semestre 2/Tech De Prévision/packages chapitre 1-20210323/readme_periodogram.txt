************************************************
Periodogram with TSA package
************************************************

library(TSA)
y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)
periodogram(yy)
