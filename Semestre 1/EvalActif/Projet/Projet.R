#---- Intro -----
install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
library(IntroCompFinR)
library(fPortfolio)
library(ggplot2) 
library(scales)
library(ggrepel)
library(extrafont);loadfonts() 
library(corrplot)
library(data.table)
library(scales)
library(ggplot2)
library(dplyr)
library(VIF)
library(pspearman)
library(PerformanceAnalytics)
library(car)
library(EnvStats)
library(factoextra)
library(ggplot2)
library(FactoMineR)
library(gridExtra)
library(lmtest)
library(AER)
library(tseries)

#Importation données
setwd("D:/M1 EKAP/Semestre 1/EvalActif/Projet")
VBISX <- data.table(read.csv("VBISX.csv"),ticker="VBISX")
VEIEX <- data.table(read.csv("VEIEX.csv"),ticker="VEIEX")
VBLLX <- data.table(read.csv("VBLLX.csv"),ticker="VBLLX")
VEURX <- data.table(read.csv("VEURX.csv"),ticker="VEURX")
VPACX <- data.table(read.csv("VPACX.csv"),ticker="VPACX")
VFINX <- data.table(read.csv("VFINX.csv"),ticker="VFINX")

#Changement format de la date
VBISX[, Date := as.Date(Date)]
VEIEX[, Date := as.Date(Date)]
VBLLX[, Date := as.Date(Date)]
VEURX[, Date := as.Date(Date)]
VPACX[, Date := as.Date(Date)]
VFINX[, Date := as.Date(Date)]

data<-rbind(VBISX,VEIEX,VBLLX,VEURX,VPACX,VFINX)

data2<-cbind(VBISX,VEIEX,VBLLX,VEURX,VPACX,VFINX)
data3 <- data2[c(1:231),]
data3 <- data2[,c(1,6,14,22,30,38,46)]
names(data3) <- c("date","VBISX", "VEIEX", "VBLLX","VEURX","VPACX","VFINX")
data3$VBISX<-as.numeric(data3$VBISX)
data3$VEIEX<-as.numeric(data3$VEIEX)
data3$VBLLX<-as.numeric(data3$VBLLX)
data3$VEURX<-as.numeric(data3$VEURX)
data3$VPACX<-as.numeric(data3$VPACX)
data3$VFINX<-as.numeric(data3$VFINX)
data3$date<-as.Date(data3$date)
summary(data3)



#---- Exercice 1 -----
#-------- Question 1 ----
dt<-data[,c("Date","Adj.Close","ticker")]
colnames(dt) <- c("date", "price","ticker")

# On index les prix par rapport au premier jour
dt[, idx_price := price/price[1] , by = ticker]

# Evolution des prix
ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Evolution des prix") +
  xlab("Date") + ylab("( 1/12/19 = 1)") +
  scale_color_discrete(name = "Indice")

# Les rendements arithmétique
dt[, ret := price / shift(price, 1) - 1, by = ticker]

# Creation nouvelle table qu'avec les rendements
dataR <- dt[!(ret), .(ticker, ret)]

# calcul du rendement attendu et de la volatilite
dataR <- dataR[, .(er = round(mean(ret), 4),
               sd = round(sd(ret), 4)),
           by = "ticker"]

# Affiche Le rendement par rapport à la volatilité

ggplot(dataR, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatilité") + ylab("Rendements Attendu") +
  scale_y_continuous(label = percent, limits = c(0, 0.001)) +
  scale_x_continuous(label = percent, limits = c(0, 0.03))

#-------- Question 2 ----

# Affiche evolution des rendements

ggplot(dt, aes(x = date, y = ret, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Evolution des prix") +
  xlab("Date") + ylab("( 1/12/19 = 1)") +
  scale_color_discrete(name = "Indice")
# Commenter les retours à la moyenne (apres d'avril) du à l'épidémie du covid et la mise en route des politiques monétaires des banques centrale (QE)

#-------- Question 3 ----
# Mise en place des rendements arthimetique dans les data
VBISX[, ret := Adj.Close / shift(Adj.Close, 1) - 1]
VEIEX[, ret := Adj.Close / shift(Adj.Close, 1) - 1]
VBLLX[, ret := Adj.Close / shift(Adj.Close, 1) - 1]
VEURX[, ret := Adj.Close / shift(Adj.Close, 1) - 1]
VPACX[, ret := Adj.Close / shift(Adj.Close, 1) - 1]
VFINX[, ret := Adj.Close / shift(Adj.Close, 1) - 1]

# Boite à Moustache 
par(mfrow = c(1,6))

boxplot(VBISX$ret,ylab="VBISX")
boxplot(VEIEX$ret,ylab="VEIEX")
boxplot(VBLLX$ret,ylab="VBLLX")
boxplot(VEURX$ret,ylab="VEURX")
boxplot(VPACX$ret,ylab="VPACX")
boxplot(VFINX$ret,ylab="VFINX")


#-------- Question 4 ----

dt <- na.omit(dt)
tapply(dt$ret,dt$ticker,mean) #Moyenne des rendements
tapply(dt$ret,dt$ticker,var) #Variance
tapply(dt$ret,dt$ticker,sd) #Ecart Type
tapply(dt$ret,dt$ticker,skewness) #Asymétrie
tapply(dt$ret,dt$ticker,kurtosis) #Kurtosis


#-------- Question 5 ----

Rp<-tapply(dt$ret,dt$ticker,mean)
O<-tapply(dt$ret,dt$ticker,sd)
Rp
Rf=0.0004167

# Ratio de Sharpe 
#Il permet  d'apprécier le rendement d'un investissement en comparaison de sa volatilité
#Ainsi, toute chose égale par ailleurs, plus le ratio de Sharpe est grand, plus l'actif considéré est intéressant.
S=(Rp-Rf)/O
S
# A commenter

#-------- Question 6 & 7 ----
# Calcul de covariance

VBISX<-mutate(VBISX, idx_price := Adj.Close/Adj.Close[1])
VEIEX<-mutate(VEIEX, idx_price := Adj.Close/Adj.Close[1])
VBLLX<-mutate(VBLLX, idx_price := Adj.Close/Adj.Close[1])
VEURX<-mutate(VEURX, idx_price := Adj.Close/Adj.Close[1])
VPACX<-mutate(VPACX, idx_price := Adj.Close/Adj.Close[1])
VFINX<-mutate(VFINX, idx_price := Adj.Close/Adj.Close[1])

VBISX<-mutate(VBISX, ret := Adj.Close / shift(Adj.Close, 1) - 1)
VEIEX<-mutate(VEIEX, ret := Adj.Close / shift(Adj.Close, 1) - 1)
VBLLX<-mutate(VBLLX, ret := Adj.Close / shift(Adj.Close, 1) - 1) 
VEURX<-mutate(VEURX, ret := Adj.Close / shift(Adj.Close, 1) - 1)
VPACX<-mutate(VPACX, ret := Adj.Close / shift(Adj.Close, 1) - 1)
VFINX<-mutate(VFINX, ret := Adj.Close / shift(Adj.Close, 1) - 1)


Table<-cbind(VBISX[c(1:231),],VEIEX[c(1:231),],VBLLX[c(1:231),],VEURX[c(1:231),],VPACX[c(1:231),],VFINX[c(1:231),])
Table$Date<-as.Date(Table$Date)
Table <- Table[-1,]

# Creation des matrice xts 
Matprix <- xts(Table[,c(6,16,26,36,46,56)], order.by=Table$Date)
Matindexprix <- xts(Table[,c(9,19,29,39,49,59)], order.by=Table$Date)
Matrendement <- xts(Table[,c(10,20,30,40,50,60)], order.by=Table$Date)

names(Matprix) <- c("VBISX", "VEIEX", "VBLLX","VEURX","VPACX","VFINX")
names(Matindexprix) <- c("VBISX", "VEIEX", "VBLLX","VEURX","VPACX","VFINX")
names(Matrendement) <- c("VBISX", "VEIEX", "VBLLX","VEURX","VPACX","VFINX")
Matrendement<-Matrendement[-1,]#On enleve la premiere ligne car inutile

# matrice variance covariance

cov(Matrendement)
#Plus les covariance sont eleves entre 2 actifs plus ils vont etre corrélés, elles sont dépendantes


# Matrice de corrélation
par(mfrow = c(1,1))

chart.Correlation(Matrendement, histogram = TRUE,pch=19,method = c("pearson"))
corrplot(cor(Matrendement), method="number",type="upper")

#-------- Question 9 ----


# Alpha de Jensen


J1<-CAPM.jensenAlpha(Matrendement[,1],Matrendement[,6],Rf)
J2<-CAPM.jensenAlpha(Matrendement[,2],Matrendement[,6],Rf)
J3<-CAPM.jensenAlpha(Matrendement[,3],Matrendement[,6],Rf)
J4<-CAPM.jensenAlpha(Matrendement[,4],Matrendement[,6],Rf)
J5<-CAPM.jensenAlpha(Matrendement[,5],Matrendement[,6],Rf)
J6<-CAPM.jensenAlpha(Matrendement[,6],Matrendement[,6],Rf)
Jensen<-c(J1,J2,J3,J4,J5,J6)
Names<-c("VBISX","VEIEX","VBLLX","VEURX","VPACX","VFINX")
library(stargazer)
Jensen<-rbind(Names,Jensen)
stargazer(Jensen, type="text", out="doc.txt")

# Le VBISX et VBLLX ont un rendement superieur au VFINX avec le VBLLX plus rentable

J1<-CAPM.jensenAlpha(Matrendement[,1],Matrendement[,3],Rf)
J2<-CAPM.jensenAlpha(Matrendement[,2],Matrendement[,3],Rf)
J3<-CAPM.jensenAlpha(Matrendement[,3],Matrendement[,1],Rf)
J4<-CAPM.jensenAlpha(Matrendement[,4],Matrendement[,3],Rf)
J5<-CAPM.jensenAlpha(Matrendement[,5],Matrendement[,3],Rf)
J6<-CAPM.jensenAlpha(Matrendement[,6],Matrendement[,3],Rf)
Jensen<-c(J1,J2,J3,J4,J5,J6)
Names<-c("VBISX","VEIEX","VBLLX","VEURX","VPACX","VFINX")
Jensen<-rbind(Names,Jensen)
Jensen
# Confirme que le plus rentable est le VBLLX

# Ratio de Treynor
# Le ratio de Treynor est un indicateur de risques permettant d'analyser la performance d'un portefeuille par rapport au risque pris
# Plus le ratio de Treynor est élevé, meilleure est la rentabilité du portefeuille vis-à-vis du risque encouru par l'investisseur
T1<-TreynorRatio(Matrendement[,1], Matrendement[,6],Rf)
T2<-TreynorRatio(Matrendement[,2], Matrendement[,6],Rf)
T3<-TreynorRatio(Matrendement[,3], Matrendement[,6],Rf)
T4<-TreynorRatio(Matrendement[,4], Matrendement[,6],Rf)
T5<-TreynorRatio(Matrendement[,5], Matrendement[,6],Rf)
T6<-TreynorRatio(Matrendement[,6], Matrendement[,6],Rf)
Treynor<-c(T1,T2,T3,T4,T5,T6)
Names<-c("VBISX","VEIEX","VBLLX","VEURX","VPACX","VFINX")
Treynor<-rbind(Names,Treynor)
stargazer(Treynor, type="text", out="doc.txt")
#VBLLX à toujours l'air d'etre le plus performant

T1<-TreynorRatio(Matrendement[,1], Matrendement[,3],Rf)
T2<-TreynorRatio(Matrendement[,2], Matrendement[,3],Rf)
T3<-TreynorRatio(Matrendement[,3], Matrendement[,3],Rf)
T4<-TreynorRatio(Matrendement[,4], Matrendement[,3],Rf)
T5<-TreynorRatio(Matrendement[,5], Matrendement[,3],Rf)
T6<-TreynorRatio(Matrendement[,6], Matrendement[,3],Rf)
Treynor<-c(T1,T2,T3,T4,T5,T6)
Names<-c("VBISX","VEIEX","VBLLX","VEURX","VPACX","VFINX")
Treynor<-rbind(Names,Treynor)
Treynor
#VBLLX plus performant

# Ratio de Sortino

# Le ratio de Sortino mesure l'excès de rentabilité d'un portefeuille par rapport 
# à un placement sans risque. Il évalue uniquement la volatilité à la baisse, 
# celle que redoute un investisseur, car elle est synonyme de pertes potentielles.
# Pour les investisseurs, un ratio de Sortino élevé est un indicateur à retenir 
# dans la mesure où il met en relief les fonds qui ont été performants tout en restant solides (résilients) durant les périodes de baisse du marché.


SortinoRatio(Matrendement,Rf)
stargazer(SortinoRatio(Matrendement,Matrendement$VFINX), type="text", out="doc.txt")
SortinoRatio(Matrendement,Matrendement$VBLLX)
#Les rendements sont supeiruer aux taux sans risque par rapport à la volatilité
# Le plus performant par rapport au risque pris est VBLLX


#Excess Returns from Bacon

print(MSquaredExcess(Matrendement['2020',1:6], Matrendement['2020',6:6], Rf,Method = "arithmetic"))
#Le plus performant est toujjours le VBLLX

# L'indice M²
# savoir si le rendement d'un portefeuille est suffisamment élevé compte tenu de son risque.


# Les mesures de Graham et Harvet
# cette mesure ramène la comparaison au niveau du risque du portefeuille activement géré



# L'indice d'Aftalion et Poncet
PR = Rp["VBLLX"]-Rf
AP = (Rp-Rp["VBLLX"])-PR*(O-O["VBLLX"])
View(AP)
# VFINX le plus performant

# Le Ratio d'Information (appraisal ratio)
AppraisalRatio(Matrendement,Matrendement$VFINX,Rf,method="appraisal")
#Seul VBLLX positif donc meilleur performance


# Bernard and Ledoit Ratio 
BernardoLedoitRatio(Matrendement)

#---- Exercice 2 ----
#-------- Question 1 ----
# Rendement du portefeuille equipondere 

RetPort<-Return.portfolio(Matrendement)
RetPort<-RetPort
Portefeuille <- data.table(RetPort,ticker="Portefeuille")
names(Portefeuille) <- c("ret","ticker")
mean(Portefeuille$ret)
sd(Portefeuille$ret)

liste<-rbind(dt,Portefeuille,fill=T)

tab <- liste[!is.na(ret), .(ticker, ret)]

# calculate the expected returns (historical mean of returns) and volatility (standard deviation of returns)
tab <- tab[, .(er = round(mean(ret), 4),
               sd = round(sd(ret), 4)),
           by = "ticker"]

ggplot(tab, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, 0.1)) +
  scale_x_continuous(label = percent, limits = c(0, 0.07))


# Tous les titres sont inefficients sauf VBISX


#-------- Question 2 ----
#Frontière d'efficience

#Calculating the Risk-Return Tradeoff of a Portfolio
#Efficience frontier

# load the data
df <- data.table(Matrendement$VFINX,Matrendement$VEURX, Matrendement$VEIEX)
names(df) <- c("x","y","z")

# calculate the necessary values:
# I) expected returns for the two assets
er_x <- mean(df$x)
er_y <- mean(df$y)
er_z <- mean(df$z)

# II) risk (standard deviation) as a risk measure
sd_x <- sd(df$x)
sd_y <- sd(df$y)
sd_z <- sd(df$z)

# III) covariance
cov_xy <- cov(df$x, df$y)
cov_xz <- cov(df$x, df$z)
cov_yz <- cov(df$y, df$z)

# create portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)

# create a data.table that contains the weights for the three assets
three_assets <- data.table(wx = rep(x_weights, each = length(x_weights)),
                           wy = rep(x_weights, length(x_weights)))

three_assets[, wz := 1 - wx - wy]


# calculate the expected returns and standard deviations for the 1000 possible portfolios
three_assets[, ':=' (er_p = wx * er_x + wy * er_y + wz * er_z,
                     sd_p = sqrt(wx^2 * sd_x^2 +
                                   wy^2 * sd_y^2 +
                                   wz^2 * sd_z^2 +
                                   2 * wx * wy * cov_xy +
                                   2 * wx * wz * cov_xz +
                                   2 * wy * wz * cov_yz))]

# take out cases where we have negative weights (shortselling)
three_assets <- three_assets[wx >= 0 & wy >= 0 & wz >= 0]
three_assets

# lastly plot the values
ggplot() +
  geom_point(data = three_assets, aes(x = sd_p, y = er_p, color = wx - wz)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y, sd_z), mean = c(er_x, er_y, er_z)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Possible Portfolios with Three Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(three_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(three_assets$sd_p) * 1.2)) +
  scale_color_gradientn(colors = c("red", "blue", "yellow"),
                        name = expression(omega[x] - omega[z]), labels = percent)



#Calculating the Efficient Frontier
#with Short selling
calcEFParams <- function(rets) {
  
  retbar <- colMeans(rets, na.rm = T)
  covs <- var(rets, na.rm = T) # calculates the covariance of the returns
  invS <- solve(covs)
  i <- matrix(1, nrow = length(retbar))
  
  alpha <- t(i) %*% invS %*% i
  beta <- t(i) %*% invS %*% retbar
  gamma <- t(retbar) %*% invS %*% retbar
  delta <- alpha * gamma - beta * beta
  
  retlist <- list(alpha = as.numeric(alpha),
                  beta = as.numeric(beta),
                  gamma = as.numeric(gamma),
                  delta = as.numeric(delta))
  
  return(retlist)
}


abcds <- calcEFParams(df)
abcds

#$alpha
#[1] 130.5582

#$beta
#[1] 120.8273

#$gamma
#[1] 117.8438

#$delta
#[1] 786.2557

calcEFValues <- function(x, abcd, upper = T) {
  alpha <- abcd$alpha
  beta <- abcd$beta
  gamma <- abcd$gamma
  delta <- abcd$delta
  
  if (upper) {
    retval <- beta / alpha + sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  } else {
    retval <- beta / alpha - sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  }
  
  return(retval)
}

# calculate the risk-return tradeoff the two assets (for plotting the points)
df_table <- melt(df)[, .(er = mean(value),
                         sd = sd(value)), by = variable]

# plot the values
ggplot(df_table, aes(x = sd, y = er)) +
  # add the stocks
  geom_point(size = 4, color = "red", shape = 18) +
  # add the upper efficient frontier
  stat_function(fun = calcEFValues, args = list(abcd = abcds, upper = T), n = 10000,
                color = "red", size = 1) +
  # add the lower "efficient" frontier
  stat_function(fun = calcEFValues, args = list(abcd = abcds, upper = F), n = 10000,
                color = "blue", size = 1) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Efficient Frontier with Short-Selling") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(df_table$er) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(df_table$sd) * 1.2))

# Portefeuille de variance minimum

assetSymbols <- c('VFINX','VEURX','VEIEX')
assetReturns <- df
assetReturns <- data.frame(assetReturns)
mu <- colMeans(assetReturns) # rendements espÃ©rÃ©s
cov.mat <- cov(assetReturns) # matrices de covariances

# Minimum Variance Portfolio function
getMinVariancePortfolio <- function(mu,covMat,assetSymbols) {
  U <- rep(1, length(mu)) # vecteur de 1
  O <- solve(covMat)     # inverse de la matrice de covariance
  w <- O%*%U /as.numeric(t(U)%*%O%*% U)
  Risk <- sqrt(t(w) %*% covMat %*% w)
  ExpReturn <- t(w) %*% mu
  Weights <- `names<-`(round(w, 5), assetSymbols)
  list(Weights = t(Weights),
       ExpReturn = round(as.numeric(ExpReturn), 5),
       Risk = round(as.numeric(Risk), 5))
}

MVP <- getMinVariancePortfolio(mu, cov.mat,assetSymbols)
stargazer(MVP,type="text",out="doc.txt")


# Portefeuille Tangencielle 
asset.names <- c('VFINX','VEURX','VEIEX')
assetReturns <- df
assetReturns <- data.frame(assetReturns)
names(assetReturns) = asset.names
r.free = 2/(100*360) #Calcul du taux jour

tangencyPortfolio(as.timeSeries(assetReturns))

# Portfolio Weights:
#   VFINX  VEURX  VEIEX 
# 0.6853 0.3147 0.0000 
# 
# Covariance Risk Budgets:
#   VFINX  VEURX  VEIEX 
# 0.7069 0.2931 0.0000 
# 
# Target Returns and Risks:
#   mean     Cov    CVaR     VaR 
# 0.9592  0.0908 -0.7311 -0.7673 

#Utilisation de la library fPortfolio pour tester les portefeuilles min variance et frontier efficience

efficientPortfolio(as.timeSeries(assetReturns))
minvariancePortfolio(as.timeSeries(assetReturns))

#Calcul du meilleur portefeuille en fonction des 6 actifs
efficientPortfolio(as.timeSeries(Matrendement))
tangencyPortfolio(as.timeSeries(Matrendement))
minvariancePortfolio(as.timeSeries(Matrendement))

# Title:
#   MV Tangency Portfolio 
# Estimator:         covEstimator 
# Solver:            solveRquadprog 
# Optimize:          minRisk 
# Constraints:       
#   
#   Portfolio Weights:
#   VBISX  VEIEX  VBLLX  VEURX  VPACX  VFINX 
# 0.9835 0.0000 0.0000 0.0165 0.0000 0.0000 
# 
# Covariance Risk Budgets:
#   VBISX  VEIEX  VBLLX  VEURX  VPACX  VFINX 
# 0.9857 0.0000 0.0000 0.0143 0.0000 0.0000 
# 
# Target Returns and Risks: 
#   mean     Cov    CVaR     VaR 
# 1.0285  0.0140 -1.0013 -1.0019  