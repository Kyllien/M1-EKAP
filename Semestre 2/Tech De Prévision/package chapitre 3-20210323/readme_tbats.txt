# Using R code for seasonal decomposition with forecast package
# TBATS model: Exponential smoothing state space model with Box-Cox transformation, 
# ARMA errors, Trend and Seasonal components

library(forecast)
y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)

decomp <- tbats(yy)
show(decomp)
plot(decomp)

# Extract components of a TBATS model
comp <- tbats.components(decomp)
plot(comp)
show(comp)

> library(xlsx)
> write.xlsx(comp, "c:/R/data/decomp-tbats.xlsx")

# Returns seasonally adjusted data constructed by removing the seasonal component
seas <- seasadj(decomp)
plot(seas)

> write.xlsx(seas, "c:/R/data/cvs-tbats.xlsx")

*******************************************************
# Forecasting with h number of periods for forecasting
*******************************************************
prev <- forecast(decomp,12)
show(prev)


******************************************************
# Commandes
******************************************************
# Usage (options par defaut)
tbats(y, use.box.cox = NULL, use.trend = NULL, use.damped.trend = NULL,
seasonal.periods = NULL, use.arma.errors = TRUE,
use.parallel = length(y) > 1000, num.cores = 2, bc.lower = 0,
bc.upper = 1, biasadj = FALSE, model = NULL, ...)

use.box.cox TRUE/FALSE indicates whether to use the Box-Cox transformation or not. 
If NULL then both are tried and the best ?t is selected by AIC.

use.trend TRUE/FALSE indicates whether to include a trend or not. 
If NULL then both are tried and the best ?t is selected by AIC.

use.damped.trend TRUE/FALSE indicates whether to include a damping parameter in the trend or
not. 
If NULL then both are tried and the best ?t is selected by AIC.

seasonal.periods If y is numeric then seasonal periods can be speci?ed with this parameter.

use.arma.errors TRUE/FALSE indicates whether to include ARMA errors or not. If TRUE the best
?t is selected by AIC. 
If FALSE then the selection algorithm does not consider ARMA errors.
