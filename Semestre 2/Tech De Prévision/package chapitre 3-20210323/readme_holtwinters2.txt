# Using R code holtwinters for exponential smoothing with timeSeries package
# June 2017

# installer le package timeSeries

# charger le package timeSeries

# les donn�es sont dans c:\R\data
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

# un lissage exponentiel double (Holt-Winters sans composante saisonni�re)
m <- HoltWinters(cvs,gamma=FALSE) 

# ou un lissage exponentiel double en fixant a et b (param�tre 1 - a') :
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

� noter que pour un lissage de Holt-Winters avec composante saisonni�re la s�rie temporelle x
doit �tre un objet de type s�rie temporelle, d�fini avec la fonction ts en pr�cisant la saisonnalit�.

L�affichage et la visualisation des r�sultats peuvent �tre r�alis�s � l�aide des commandes :
� summary(xlisse) : description de l�objet xlisse obtenu pr�c�demment par la proc�dure HoltWinters,
� plot(xlisse) : repr�sentation des valeurs observ�es et des valeurs liss�es,
� plot(xlisse$fitted[,1]) : repr�sentation de l�ajustement de la s�rie remis � jour � chaque observation.

Les pr�visions � l�horizon h sont r�alis�es � l�aide de la fonction predict :
p<-predict(xlisse,n.ahead=h ) 
(attention le n fait partie de la commande et le h est une variable qui peut prendre les valeurs 1, 2, . . . ). 
Un intervalle de con?ance peut �tre obtenu en validant (� TRUE) l�option prediction.interval.

Tracer la pr�diction et la s�rie sur le m�me graphique : xlisse=HoltWinters(x,...);
p=predict(xlisse,n.ahead=50); plot(xlisse,p) .

Remarque : lorsqu�aucune valeur n�est pr�cis�e pour les constantes de lissage, un algorithme
interne � la proc�dure HoltWinters se charge d�estimer la meilleur constante possible � partir de
la s�rie des observations.