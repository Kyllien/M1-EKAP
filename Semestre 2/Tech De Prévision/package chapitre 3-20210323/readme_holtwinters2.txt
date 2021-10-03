# Using R code holtwinters for exponential smoothing with timeSeries package
# June 2017

# installer le package timeSeries

# charger le package timeSeries

# les données sont dans c:\R\data
# Monthly data
library(seasonal)
library(timeSeries)

y <- read.table("c:\\R\\data\\demandbrut.txt")
yy <- ts(data = y, start=c(1996,01),frequency=12)
seasX <- seas(yy)
cvs <- final(seasX)

# lissage
m = HoltWinters(cvs)
show(m)
summary(m)
plot(m)
plot(m$fitted[,1])

**********************************************
Forecasting
**********************************************
# horizon h=50 - intervals 95%
p = predict(m, 50, prediction.interval = TRUE) 
plot(m, p)
show(p)
prevp = p[1]
show(prevp)

-----------------------------------------------
# with forecast package
-----------------------------------------------
# horizon h=50 - intervals 80% & 95%
library(forecast)
fit = forecast(m, h=50)
plot(fit)
show(fit)
# Point forecasts
prevf = fit$mean
show(prevf)
# A list containing information about the fitted model
mod = fit$model
show(mod)


*****************************************
No seasonal: LES LED HW
*****************************************
# lissage exponentiel simple :
m <- HoltWinters(cvs,beta=FALSE,gamma=FALSE)

# ou en fixant a = 0.5
m <- HoltWinters(cvs,alpha=0.5,beta=FALSE,gamma=FALSE)
-------------------------------------------------------

# un lissage exponentiel double (Holt-Winters sans composante saisonnière)
m <- HoltWinters(cvs,gamma=FALSE) 

# ou un lissage exponentiel double en fixant a et b (paramètre 1 - a') :
m <- HoltWinters(cvs,alpha=a, beta=b, gamma=FALSE) 
# avec a = 1 - (a')^2 , b = (1-a')/(1+a')


***************************************
Seasonal HW
***************************************
# un lissage Holt-Winters saisonnier
m <- HoltWinters(yy)

# un lissage Holt-Winters additif :
m <- HoltWinters(yy,seasonal="add")
m <- HoltWinters(yy,alpha=a,beta=b,gamma=c,seasonal="add")

# un lissage Holt-Winters multiplicatif :
m <- HoltWinters(yy,seasonal="mul")
m <- HoltWinters(yy,alpha=a,beta=b,gamma=c,seasonal="mul") .


**************************************
Remarque
**************************************

À noter que pour un lissage de Holt-Winters avec composante saisonnière la série temporelle x
doit être un objet de type série temporelle, défini avec la fonction ts en précisant la saisonnalité.

L’affichage et la visualisation des résultats peuvent être réalisés à l’aide des commandes :
– summary(xlisse) : description de l’objet xlisse obtenu précédemment par la procédure HoltWinters,
– plot(xlisse) : représentation des valeurs observées et des valeurs lissées,
– plot(xlisse$fitted[,1]) : représentation de l’ajustement de la série remis à jour à chaque observation.

Les prévisions à l’horizon h sont réalisées à l’aide de la fonction predict :
p<-predict(xlisse,n.ahead=h ) 
(attention le n fait partie de la commande et le h est une variable qui peut prendre les valeurs 1, 2, . . . ). 
Un intervalle de con?ance peut être obtenu en validant (à TRUE) l’option prediction.interval.

Tracer la prédiction et la série sur le même graphique : xlisse=HoltWinters(x,...);
p=predict(xlisse,n.ahead=50); plot(xlisse,p) .

Remarque : lorsqu’aucune valeur n’est précisée pour les constantes de lissage, un algorithme
interne à la procédure HoltWinters se charge d’estimer la meilleur constante possible à partir de
la série des observations.