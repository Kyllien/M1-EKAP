# Using R code for seasonal adjustment from seasonal package (X13-SEAT-ARIMA)
# A default call to seas also invokes the following automatic procedures of X -13ARIMA-SEATS:
# • Transformation selection (log / no log)
# • Detection of trading day and Easter eﬀects
# • Outlier detection
# • ARIMA model search


# The view function is a graphical tool for choosing a seasonal adjustment model, using the new

# les données sont dans c:\R\data
# Monthly data
library(seasonal)
y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)
plot(yy, main ="Time-Series demandeurs 1996 to 2017")

*************************************
# Seasonal adjustment
*************************************
# By default, seas calls the SEATS adjustment procedure
seasX <- seas(yy)
# Final CVS
final(seasX)
plot(seasX)
summary(seasX)

plot(irregular(seasX))
plot(trend(seasX))

# The X11 adjustment procedure
seasX11 <- seas(yy,  x11 = "")
# Final CVS
final(seasX11)
plot(seasX11)
summary(seasX11)

# Forecasting on 3 years
series(seasX, "forecast.forecasts")
or
series(seasX, "fct")

fore = series(seasX, "fct")
# one-ahead forecast
fore[1]


# The udg function provides access to a large number of diagnostical statistics
udg(seasX, "x13mdl")

# The out function shows the content of the main output using the HTML version of X-13
out(seasX)

# The view function is a graphical tool for choosing a seasonal adjustment model
install.packages("seasonalview")
view(seasX)

*********************************************
Outlier detection
*********************************************

# Linearized series ou outlier-adjusted series
# series series.outlieradjorig a19
adjseries = series(seasX, "a19")
write(t(adj),file="adj_seasonal.out",ncolumn=1,append=FALSE)


