# Using R code for seasonal decomposition with stR package
# A Seasonal-Trend Decomposition Procedure Based on Regression
# January 2018

# installer le package stR
# charger le package stR

# les données sont dans c:\R\data
# Monthly data
y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)

# Automatically selects parameters for an STR decomposition of time series data. 
# The time series should be of class ts or msts.

AutoSTR(yy)

# Plots the results of decomposition
plot(STR)

# extracts components as time series from the result of an STR decomposition

decomp <- AutoSTR(yy)
plot(decomp)
show(decomp)
comp <- components(decomp)
plot(comp)
show(comp)

> library(xlsx)
> mydata <- comp
> write.xlsx(mydata, "c:/R/data/decomp-stR.xlsx")

# Seasonal adjustment based on STR seasadj.STR
seas <- seasadj(decomp)
plot(seas)

> write.xlsx(seas, "c:/R/data/cvs-stR.xlsx")




