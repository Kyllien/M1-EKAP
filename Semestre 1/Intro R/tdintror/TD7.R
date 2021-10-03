#exemple de cours

for (i in 1:99) 
  print (i)
for (i in seq (1,99, by=2)) 
  print (i)


i <- 1
while (i< 3) {
  print (i)
  i <- i + 1 }


i <- 1
repeat {
  print(i)
  i <- i+1
  if (i>3) break }


som(5)
som <- function(n) {
  resultat <- sum (1:n)
}
som(5)
g<-som(5)
g

#exercice 9

#question 1
j_semaine<-c("lundi","mardi","mercredi")
for (i in j_semaine) 
  print (i)


#question 2
i <- 0
while (i< 10) {
  print (i)
  i <- i + 1 }
#autre méthode
i<-0
repeat{
  print(i)
  i<-i+1
  if(i>9)
    break
}


#question 3

x <- sample(1:10,10, rep = T)
x
y <- numeric(length (x))
for (i in 1:length(x)) {
  if (x[i]==4)
    y[i]=0
  else y[i]=1}
y

#question 4

x<-1984
y<-1991

while (x<=y){
  print(x)
  x=x+1
}

#question 5
set.seed(545)
hasar<-matrix(sample(1:25,25), ncol=5)
x
apply(hasar, MARGIN=1, FUN=mean)
rowMeans(hasar)

#fonction ligne qui m'interese donc MARGIN=1 si colonne on aurait mis 2
#rowmeans la même que appplly mais en plus simple

#question 6
mafun<- function(hasar,y){
  z<-hasar+y^2
  return (1/z)
}
apply(hasar, MARGIN=c(1,2), FUN=mafun, y=2)

#question 7/8
data(iris)
str(iris)
iris
moyennesd=function(i){
  moy=mean(iris[,i])
  et=sd(iris[,i])
  return (c(moy=moy, et=et))
}
moyennesd(i=1)
moy=apply(iris[,c(1,2,3,3,4)],2,mean)
et=apply(iris[,c(1,2,3,4)],2,mean)
moy
et
#il y a deja une multitude de fonctions qui existent et donc pas besoin de créer des fonctions