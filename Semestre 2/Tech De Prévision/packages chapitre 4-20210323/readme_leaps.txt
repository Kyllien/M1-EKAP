# Subsets regression using the leaps() function from the leaps package

library(readxl)
dlbase <- read_excel("c://R//data//database.xlsx", sheet = "dlbase")
dlbase <- data.frame(dlbase)
training_dlbase <- dlbase
data.frame(training_dlbase)

library(leaps)
leaps <- regsubsets(Ddemand ~Evoleff + Preveff + Dprod, data=training_dlbase, nbest=1, nvmax=3, method=c("exhaustive"))
summary(leaps)

# Choosing the optimal model
res.sum <- summary(leaps)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)


# plot a table of models showing variables in each model.
# models are ordered by the selection statistic
# Other options for plot( ) are bic, Cp, and adjr2
plot(leaps, scale="adjr2", main = "Adjusted R^2")
plot(leaps, scale="Cp", main = "Mallow's Cp")
plot(leaps, scale="bic", main = "BIC")

# plot statistic by subset size 
# Other options for plotting with subset( ) are bic, cp, adjr2, and rss
library(car)
subsets(leaps, statistic="rsq") 


# nbest: Number of subsets of each size to report
# nvmax: maximum size of subsets to examine
# method=c("exhaustive","backward", "forward", "seqrep"): exhaustive search, forward selection, 
# backward selection or sequential replacement to search

# Output
r-squared: leaps$rsq
Adjusted r-squared: leaps$adjr2
Mallows' Cp: leaps$cp 
BIC: leaps$bic
Residual sum of squares: leaps$rss

http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/155-best-subsets-regression-essentials-in-r/
