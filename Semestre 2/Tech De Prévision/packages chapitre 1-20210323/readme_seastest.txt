# Using R code for seasonal tests from seastests package
library(readxl)
library(seastests)


# les données sont dans c:\R\data
# Monthly data
y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)

# Friedman test
ft <- fried(yy)
show(ft)

# Testing the seasonality of series
#  a boolean value is returned : TRUE or FALSE
is <- isSeasonal(yy, test="wo")
show(is)

# Kruskal-Wallis test
kwt <- kw(yy)
show(kwt)

# QS test
qst <- qs(yy)
show(qst)

# Seasonal dummies
sd <- seasdum(yy)
show(sd)

# Welch test
w <- welch(yy)
show(w)

# Webel-Ollech test
wot <- wo(yy)
show(wot)







