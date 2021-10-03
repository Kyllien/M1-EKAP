#Exercice 9 
#Question 1
jours<-c("Lundi", "Mardi", "Mercredi")
for (i in jours){
  print(i)
}

#Question 2
i<-c(0)
while(i<10){
  i=i+1
  print(i)
}

#Questoin 3
x <- sample(1:10,10, rep = T)
y= numeric(length(x))
for (i in 1:length(x)){
  if (x[i]==4){
    y[i]=0
  }
  else{
    y[i]=1
  }
}
print(y)

#Question 4
X<-1984
y<-1991
while (X<=y){
  print(X)
  X=X+1
}

#Question 5
set.seed(545)
hasard <- sample(1:25,25, rep = T)
apply(hasard,margin=2,fun=mean)
rowMeans(hasard)
