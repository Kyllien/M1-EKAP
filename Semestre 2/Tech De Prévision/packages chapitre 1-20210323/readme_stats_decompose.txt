# Using R code for seasonal decomposition from stat package
# Decompose a time series into seasonal, trend and irregular components using moving averages. 
# Deals with additive or multiplicative seasonal component.

# les données sont dans c:\R\data
# Monthly data
y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)

fit <- decompose(yy)
plot(fit)
show(fit)

> library(xlsx)
> mydata <-fit$trend
> write.xlsx(mydata, "c:/R/data/mydata.xlsx")

> mydata <- fit$seasonal
> mydata <- fit$random



# package stats for STL decomposition
y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(y[1:264,1],frequency=12)
fit=stl(yy, s.window="per")
plot(fit)
show(fit)


# Utile quand on a le message d'erreur suivant avec ts()
# only univariate series are allowed
# seules les séries univariées sont autorisées

yy <- ts(y[1:264,1],frequency=12)


