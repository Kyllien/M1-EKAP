# Using R code for seasonal decomposition from sltplus package

# les données sont dans c:\R\data
# Monthly data
library(stlplus)
y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(y[1:264,1],frequency=12)
fit=stlplus(yy, s.window="per")
plot(fit)
show(fit)
plot_trend(fit)

tendance = trend(fit)