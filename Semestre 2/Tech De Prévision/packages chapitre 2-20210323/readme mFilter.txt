# Using R code for HP filter with mFilter package
# January 2020

# installer le package mFilter
# charger le package mFilter

# les données sont dans c:\R\data
# Monthly data

library(mFilter)

y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)


# HP filter with default lambda value
filter <- hpfilter(yy)
plot(filter)

# HP filter with lambda = 129 600
filter <- hpfilter(yy, 129600)
plot(filter)

# HP trend & cycle
trend <- hpfilter$trend
cycle <- hpfilter$cycle

