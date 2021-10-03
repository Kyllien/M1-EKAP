#---- Intro -----

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
library(stringr)
library(tidyr)




#Importation données
setwd("D:/Recherche/Matiere Premiere/Cours des MP")
Avoine <- data.table(read.csv("Avoine.csv"),ticker="Avoine")
Ble <- data.table(read.csv("Ble.csv"),ticker="Ble")
Bovins <- data.table(read.csv("Bovins.csv"),ticker="Bovins")
Cacao <- data.table(read.csv("Cacao.csv"),ticker="Cacao")
Coffee <- data.table(read.csv("Coffee.csv"),ticker="Coffee")
Coton <- data.table(read.csv("Coton.csv"),ticker="Coton")


#Debut de boucle pour as date ou as numeric une data entiere
# for (i in (2:ncol(virtual))){
#   if(i%%3!=0){
#     print(i)
#     commands <- str_c("virtual[,",i,"] <- as.numeric(x$",names[,i],")")
#     eval(parse(text = commands))
#   }
# }
n=6 #Nb de titre
data<-rbind(Bovins,Avoine,Ble,Cacao,Coffee,Coton) #Mettre la serie avec le moins d observations en premier


RbindToCBind <- function (BDD,nb){
  virtual <- filter(BDD,ticker==ticker[1])
  compteur <- nrow(filter(BDD,ticker==ticker[1]))
  for (i in (2:nb)){
    virtual <- left_join(virtual,filter(BDD,ticker==ticker[compteur+1]),by=names(virtual[,1]))
    compteur <- nrow(filter(BDD,ticker==ticker[compteur+1])) + compteur
  }
  virtual <- drop_na(virtual)
  return(virtual)
}


EvalActif <- function (BDD,nb){
  BDD<-BDD[,c("Date","Adj.Close","ticker")]
  colnames(BDD) <- c("date", "price","ticker")
  BDD$date <- as.Date(BDD$date)
  BDD$price <- as.numeric(BDD$price)
  BDD <- BDD %>% filter(!is.na(price))
  # On index les prix par rapport au premier jour
  BDD <- BDD[, idx_price := price/price[1] , by = ticker]
  
  # Evolution des prix
  print(ggplot(BDD, aes(x = date, y = idx_price, color = ticker)) +
    geom_line() +
    # Miscellaneous Formatting
    theme_bw() + ggtitle("Evolution des prix") +
    xlab("Date") + ylab("( 1er jour de la data = 1)") +
    scale_color_discrete(name = "Contrat"))
  
  # Les rendements arithmétique
  BDD[, ret := price / shift(price, 1) - 1, by = ticker]
  
  # Creation nouvelle table qu'avec les rendements
  dataR <- BDD[!is.na(ret), .(ticker, ret)]
  # # calcul du rendement attendu et de la volatilite
  dataR <- dataR[, .(er = round(mean(ret), 4),
                     sd = round(sd(ret), 4)),
                by = "ticker"]
   
   # Affiche Le rendement par rapport à la volatilité

   print(ggplot(dataR, aes(x = sd, y = er, color = ticker)) +
     geom_point(size = 5) +
     # Miscellaneous Formatting
     theme_bw() + ggtitle("Risk-Return Tradeoff") +
     xlab("Volatilité") + ylab("Rendements Attendu") +
     scale_y_continuous(label = percent, limits = c(0, 0.001)) +
     scale_x_continuous(label = percent, limits = c(0, 0.03)))

  # #Affiche les rendements
   print(ggplot(BDD, aes(x = date, y = ret, color = ticker)) +
     geom_line() +
     # Miscellaneous Formatting
     theme_bw() + ggtitle("Evolution des prix") +
     xlab("Date") + ylab("( 1/12/19 = 1)") +
     scale_color_discrete(name = "Indice"))
   
  return(BDD)
}
data3 <- EvalActif(data,n)

data2 <- RbindToCBind(data3,n)
drop_na(data2)

Matrendement <- xts(select(data2,c(seq(5,(4*n)+1,by=4))), order.by=data2$date) #Creation de la mtrice de rendement
#A FAIRE#Mettre les noms sur les colonnes de la matrice

CovAndCor <- function(BDD,nb){
  print(cov(Matrendement))
  chart.Correlation(Matrendement, histogram = TRUE,pch=19,method = c("pearson"))
  corrplot(cor(Matrendement), method="number",type="upper")
} #Fonction qui calcul la covariance des titres a partir d'une matrice xts de rendement
#Et qui calcul aussi les correlations entre chacun d'entre eux

matr <- CovAndCor(v,n)

#Fonction qui calcul l'alpha de jensen pour une matrice de rendement, un benchmark et un taux sans risque
AlphaJensen <- function (Mat,benchmark,rf){
  alphaJ <- CAPM.jensenAlpha(Mat[,1],benchmark,rf)
  for (i in (2:ncol(Mat))){
    alphaJ <- cbind(alphaJ,CAPM.jensenAlpha(Mat[,i],benchmark,rf))
  }
  return(alphaJ)
}

#Fonction qui calcul le ratio de Treynor pour une matrice de rendement, un benchmark et un taux sans risque
Treynor <- function (Mat,benchmark,rf){
  Trey<- TreynorRatio(Mat[,1],benchmark,rf)
  for (i in (2:ncol(Mat))){
    Trey <- cbind(Trey,TreynorRatio(Mat[,i],benchmark,rf))
  }
  return(Trey)
}


#Fonction qui calcul les differents indice de performance des titres par rapport à un taux sans risque
IndicePerformance <- function(Mat,benchmark,rf){
  #Ratio de Sharpe
  Sharpe <- SharpeRatio(Mat,rf)
  
  # Alpha de Jensen
  AlphaJ <- AlphaJensen(Mat,benchmark,rf)
  
  #Ratio de Treynor
  TreynorR <- Treynor(Mat,benchmark,rf)
  
  #Ratio de Sortino
  Sortino <- SortinoRatio(Mat,rf)
  
  #Le Ratio d'Information (appraisal ratio)
  Info <-  AppraisalRatio(Mat,benchmark,rf,method="appraisal")
  
  #Ratio de Bernardo et Ledoit
  Bernard <- BernardoLedoitRatio(Mat)
  
  retour <- list(Sharpe,AlphaJ,TreynorR,Sortino,Bernard,Info)#ou avec rbind
  
  return(retour)
}

Rf <- 0.0004167 #Taux sans risque
indice <- IndicePerformance(Matrendement,Matrendement[,6],Rf)
indice

#Fonction qui calcul différents type de portefeuille
Portefeuille <- function (BDD,Mat,benchmark,rf){
  
  #Portefeuille Equiponderer
  PEquiP <- data.table(Return.portfolio(Mat),ticker="Portefeuille")
  PEquiPm <- str_c("rendement moyen du portefeuille equiponderer : ",mean(PEquiP$portfolio.returns))
  print(PEquiPm)
  PEquiPsd <- str_c("volatilité du portefeuille equiponderer :",sd(PEquiP$portfolio.returns))
  print(PEquiPsd)
  names(PEquiP) = c("ret","ticker")
  
  #Plot rendement et volatilité
  liste<-rbind(BDD[,c(3,5)],PEquiP,fill=T)
  liste<-drop_na(liste)
  tab <- liste[, .(er = round(mean(ret), 4),
                 sd = round(sd(ret), 4)),
             by = "ticker"]
  
  print(ggplot(tab, aes(x = sd, y = er, color = ticker)) +
    geom_point(size = 5) +
    # Miscellaneous Formatting
    theme_bw() + ggtitle("Risk-Return Tradeoff") +
    xlab("Volatility") + ylab("Expected Returns") +
    scale_y_continuous(label = percent, limits = c(0, 0.001)) +
    scale_x_continuous(label = percent, limits = c(0, 0.04)))
  
  
  
  
  
  
  return(tab)
}
P<-Portefeuille(data3,Matrendement,Matrendement[,6],Rf)
summary(P)

EfficientFrontier <- function(Mat){
  
  #Mise en place des moyennes et des ecart types et des covariances
  er <- mean(Matrendement[,1])
  for (i in (2:ncol(Mat))){
    er <- cbind(er,mean(Matrendement[,i]))
  }
  sd <- sd(Matrendement[,1])
  for (i in (2:ncol(Mat))){
    sd <- cbind(sd,sd(Matrendement[,i]))
  }
  x_weights <- seq(from = 0, to = 1, length.out = 1000)
  
  #Création des différents portefeuille possible
  
  #assets <- data.table(wx = rep(x_weights, each = length(x_weights)),
                             wy = rep(x_weights, length(x_weights)))
  #assets[, wz := 1 - wx - wy]
  
  
  
  return(sd)
}
v <- EfficientFrontier(Matrendement)
v


