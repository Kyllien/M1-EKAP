# Using R code for Structural time series models from stat package
# Decompose a time series into seasonal, trend and irregular components using moving averages. 

# les données sont dans c:\R\data
# Monthly data
y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)

fit <- StructTS(yy)
plot(cbind(fitted(fit), residuals(fit)))
show(fit)



