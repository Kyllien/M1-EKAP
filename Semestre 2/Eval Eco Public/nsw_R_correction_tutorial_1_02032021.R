library(foreign)
mydata <-read.dta("D:/professionnel/PPE_M1_EKAP_TD/Tutorial_1/nswre74.dta")

lmodel<-lm(re78~treat,data=mydata)
lmodel$coefficients
summary(lmodel)

# The ATT of the program is +1794$ 
# significant at a 1% level if we assume homoskedasticity

# we construct confidence intervals at a 5% level
# with the commad confint

confint(lmodel,conf.level=0.95)

# Then we relax homoskedasticity
# We use the heteroskedasticity-robust
# White estimator of the variance-covariance matrix

# use the package "estimatr"

#install.packages("estimatr")
library("estimatr")

# use the command lm_robust

lmodel2<-lm_robust(re78~treat,data=mydata)
lmodel2$coefficients
summary(lmodel2)

# We add covariates in the regression

lmodel3<-lm_robust(re78~treat+married+ed+black+hisp,data=mydata)
lmodel3$coefficients
summary(lmodel3)

# the ATT decreases slightly when adjusting for more covariates
# According to the Frisch-Waugh-Lowell Theorem, the ATT
# should not vary when adding new covariates
# if treatment status is not correlated with any other variable

# Since we observe that ATT changes when adding covariates,
# This casts some doubts about the perfectly randomized treatment assignment





# Using non-experimental sample

mydata <-read.dta("D:/professionnel/PPE_M1_EKAP_TD/Tutorial_1/cps1re74.dta")

lmodel4<-lm_robust(re78~treat,data=mydata)
summary(lmodel4)

#Strong negative effect: expected negative selection bias
# Participants are likely to have poorer labour market prospects

# Adjusting for more covariates

# indicators for no earnings in 1974 or 1975

mydata$u74<-ifelse(test=(mydata$re74>0),1,0)
mydata$u75<-ifelse(test=(mydata$re75>0),1,0)

lmodel5<-lm_robust(re78~treat+age+age2+ed+nodeg+black+hisp+re74+re75+u74+u75,data=mydata)
summary(lmodel5)

# ATT starts being positive but strongly different
# from the unbiased ATT estimated in the experimental sample


# Looking at the balancing property
# using the package tableone


#install.packages("tableone")
library("tableone")

#covariates

variables<-c("age","ed","nodeg","black","hisp","re74","re75","u74","u75")

# With a standard t-test of difference in means

tab<-CreateTableOne(vars=variables,strata="treat",data=mydata,test=TRUE)
tab

# t-test of difference in means 
# show that we reject the null hypothesis of 
# non significance of the difference in means
# between the two groups (treated and control)
# for all the characteristics


# summarising this tab we also have the standardized mean diff

summary(tab)

# standardized difference in means exceed 0.2 for 
# all the variables
# There is limited overlap in distribution of covariates
# between the two groups





#Propensity score estimation


#install.packages(dplyr)
#install.packages(ggplot2)
#install.packages("MatchIt")

library(dplyr)
library(ggplot2)
library(MatchIt)

# First step Estimate the pscore with a logit model
# use the formula of Dehejia and Wahba (1999)

mydata$ed2<-mydata$ed^2
mydata$re74_2<-mydata$re74^2
mydata$re75_2<-mydata$re75^2

mydata$educ_re74<-mydata$re74*mydata$ed

mydata$age_3<-mydata$age^3

psModel<-glm(formula = treat~age+age2+ed+ed2+nodeg+married+black+hisp+re74+re75+u74+u75+educ_re74+age_3,
             family =binomial(link = "logit"),
             data=mydata) 
summary(psModel)


#predicted probability of being treated

mydata$pscore<-predict(psModel,type = "response")

# you could also use the command psModel$fitted
# to compute the fitted pscore

mydata$pscore<-psModel$fitted







#Examining the region of common support

#install.packages("Hmisc")

library("Hmisc")

histbackback(split(mydata$pscore,mydata$treat),main="Propensity score before matching",xlab=c("control","treatment"))

# Other package : cobalt (recommended)

#install.packages("cobalt")

library("cobalt")

# checking the balancing property graphically

bal.plot(treat~pscore,
         data=mydata,
         var.name="pscore",
         type="density")


# or using an histogram (as in Dehejia and Wahba, 1999)

bal.plot(treat~pscore,
         data=mydata,
         var.name="pscore",
         type="histogram")

# you can add a "mirror" effect

bal.plot(treat~pscore,
         data=mydata,
         var.name="pscore",
         type="histogram",
         mirror=TRUE)

# These graphs show that in the control group,
# the proportion of individuals with pscore close to 0
# is very high

# This is not the case in the treated group

# this shows that there is limited overlap in the distribution
# of the propensity score between the treated and control groups



# The comparison group contain a few units comparable to the treatment group

#ensuring overlap

# Min-Max method

# discarding control units whose pscore is lower than
# the min pscore among the treated (Dehejia and Wahba, 1999)

mydata_t<-subset(mydata,treat==1)

minp<-min(mydata_t$pscore)

# Another way to do the same thing with the command which
minp<-min(mydata_t[which(mydata_t$treat==1),]$pscore)

mydata_cs<-subset(mydata,mydata$pscore>minp)


tab<-CreateTableOne(vars=variables,strata="treat",data=mydata_cs,test=TRUE)
tab

# another way of ensuring overlap is to
# keep only individuals for which pscore ranges from 0.1 to 0.9
# (Crump et al., 2009)

mydata_crump<-subset(mydata,mydata$pscore>0.1&mydata$pscore<0.9)
  
bal.plot(treat~pscore,
           data=mydata_crump,
           var.name="pscore",
           type="histogram",
           mirror=TRUE)



# Using nearest-neighbor matching models

#install.packages("MatchIt")
library(MatchIt)

mod_match<-matchit(treat~age+age2+ed+ed2+nodeg+married+black+hisp+re74+re75+u74+u75+educ_re74+age_3,
                   method="nearest",
                   data=mydata_cs,
                   distance="glm", # matching based on pscore
                   replace=TRUE, # matching with replacement
                   caliper=0.25,
                   discard="both" # discards treated and control individuals that are out of the common support
)

#Look at discarded/unmatched observations

summary(mod_match)

# Looking at balancing property after matching
library("cobalt")

bal_nm<-bal.tab(mod_match,
                un=TRUE, # displays also diff in means before matching
                binary="std" # for binary variables the difference in means is standardized. For continuous variables, "std" is the default option
                )
bal_nm

# Graphical representation

love.plot(bal_nm,
          threshold=0.2)

# distribution of the pscore before and after matching

bal.plot(mod_match,
         var.name = "distance", # the name attributed to the pscore
         which = "both", # before and after matching
         type = "histogram"
         )

# create a data set from your matched sample

dta_m<-match.data(mod_match)

# Estimate the ATT after matching
# weighting control individuals
# the weight increases with the nb of times a control individual
# has been selected as a nearest neighbour

lmodel_nm<-lm(re78~treat,
              data=dta_m,
              weights=weights)

summary(lmodel_nm)


# ATT is closer to the one found in the experimental sample
# matching performs better than OLS regression 




# distance-based matching models
# using Mahalanobis distance as a metric

mod_match_dist<-matchit(treat~age+age2+ed+ed2+nodeg+married+black+hisp+re74+re75+u74+u75+educ_re74+age_3,
                   method="nearest",
                   data=mydata_cs,
                   distance="mahalanobis", # matching based on mahalanobis distance
                   replace=TRUE # matching with replacement
)

# create a data set from matched sample

dta_dist<-match.data(mod_match_dist)

# Estimate of the ATT

lmodel_dist<-lm(re78~treat,data=dta_dist,
                weights=weights)

summary(lmodel_dist)


# about matchit
# you can also indicate to find not only one nearest neighbour
# but k nearest neighbours
# you use the option ratio

mod_match_dist<-matchit(treat~age+age2+ed+ed2+nodeg+married+black+hisp+re74+re75+u74+u75+educ_re74+age_3,
                        method="nearest",
                        data=mydata_cs,
                        distance="mahalanobis", # matching based on mahalanobis distance
                        replace=TRUE,# matching with replacement
                        ratio=3 # for each treated individual, select the three nearest-neighbours in the control group
                        )

dta_dist_3<-match.data(mod_match_dist)

lmodel_dist3<-lm(re78~treat,data=dta_dist_3,weights=weights)

summary(lmodel_dist3)

# selecting k-nearest neighbours allow to reduce the
# variance
# BUT caution: because it also increases BIAS


# subclassification methods

mod_match_sub<-matchit(treat~age+age2+ed+ed2+nodeg+married+black+hisp+re74+re75+u74+u75+educ_re74+age_3,
                        method="subclass",
                        data=mydata_cs,
                        )

bal_sub<-bal.tab(mod_match_sub,
                 disp.subclass=TRUE)

bal_sub

# We can first estimate the effect of treatment on earnings
# in each subclass

dta_sub<-match.data(mod_match_sub)

lmodel_sub_1<-lm(re78~treat,data=dta_sub[dta_sub$subclass==1,])

# the coefficient associated to treat is obtained by

summary(lmodel_sub_1)$coefficients["treat",1]


# the weight associated to this effect
# depends on the share of treated individuals in this subclass

weight_1<-nrow(dta_sub[dta_sub$subclass==1&dta_sub$treat==1,])/nrow(dta_sub[dta_sub$treat==1,])

# to gain some time wd can do a loop
# to create lmodel_sub_i and weight_i for each subclass==i


# create a matrix with each line being a coefficient for the subclass i

# initialize the matrix coef

coef<-0

# do the same for the weight
# create a matrix w_sub with each line being a weight for each subclass

w_sub<-0


for (i in 1:6){
# in each subclass OLS regression of earnings on treatment
  lmodel_sub_i<-lm(re78~treat,data=dta_sub[dta_sub$subclass==i,])
 # store the coefficient associated to the treatment variable in each regression
  
  
  coef[i]<- summary(lmodel_sub_i)$coefficients["treat",1]
 # create the weight associated to each subclass 
   weight_i<-nrow(dta_sub[dta_sub$subclass==i&dta_sub$treat==1,])/nrow(dta_sub[dta_sub$treat==1,])
   
   w_sub[i]<-weight_i
}

cbind(coef,w_sub)

# to compute ATT_sub, we do a cross-product of the two matrices

ATT_sub<-t(w_sub) %*% coef



# Estimate the standard error for inference

std<-0

for(i in 1:6) {
  std[i]<-summary(lm(formula=re78~treat,
                     data=dta_sub[which(dta_sub$subclass==i),]))$coefficients["treat",c(2)]
}

std

overallATTse<-sqrt(t(w_sub^2) %*% std^2)

## To construct confidence intervals at a 5% level
# we compute first the corresponding quantile for a two-sided test
# of the Student distribution


qt<-qt(0.025,df=nrow(mydata_cs)-1,lower.tail = FALSE)

IClow <- ATT_sub -qt * overallATTse
ICup <- ATT_sub + qt * overallATTse

data.frame(ATT_sub,overallATTse,IClow,ICup)





# IPW method

mydata_cs$weight<-with(mydata_cs,
                       treat+(1-treat)*pscore/(1-pscore))

# estimating the ATT

glm.out=lm(re78~treat,
           weight=weight,data=mydata_cs)
summary(glm.out)




# Estimating variance with boostrap

#install.packages(boot)
library(boot)

# We define the estimator to compute

ipw_boot=function(dat,indices){
  dat=dat[indices,]
  ps.out<-glm(formula=treat~age+age2+ed+ed2+nodeg+married+black+hisp+re74+re75+u74+u75+educ_re74+age_3,
              family=binomial(link=logit),
              data=dat)
  dat$pscore<-ps.out$fitted
  dat$att.weights<-with(dat,treat+(1-treat)*pscore/(1-pscore))
  glm.out=lm(re78~treat,
              weight=att.weights,data=dat)
  summary(glm.out)$coefficients[2,1]
}

# initial estimate of the ATT

ipw_boot(mydata_cs,indices=1:nrow(mydata_cs))

# 5 replications by bootstrap

boot.out<-boot(data=mydata_cs,
               statistic=ipw_boot,
               R=5) # nb of replications (basically R should range from 500 to 1000)

boot.out$t

sd_boot<-sd(boot.out$t)








# standard errors of ATT in the case of 
# nearest neighbour matching
# with Abadie Imbens method

# # Using nearest-neighbor matching models

#install.packages("MatchIt")
library(MatchIt)

mod_match<-matchit(treat~age+age2+ed+ed2+nodeg+married+black+hisp+re74+re75+u74+u75+educ_re74+age_3,
                   method="nearest",
                   data=mydata_cs,
                   distance="glm", # matching based on pscore
                   replace=TRUE, # matching with replacement
                   caliper=0.25,
                   discard="both" # discards treated and control individuals that are out of the common support
)

# create a data set from your matched sample

dta_m<-match.data(mod_match)

# Estimate the ATT after matching
# weighting control individuals
# the weight increases with the nb of times a control individual
# has been selected as a nearest neighbour

lmodel_nm<-lm(re78~treat,
              data=dta_m,
              weights=weights)

summary(lmodel_nm)


# matchit creates several elements in mod_match

# match.matrix is a matrix that contains for each treated obs
# the corresponding nearest neighbour in the control group
# match.matrix has nt (nb of treated) lines
# and k (nb of nearest neighbours) columns

# remember that some treated individuals have not been matched
# and have been discarded. For them the weight=0

# you can look at these individuals (treated inviduals that have not been matched)

nb_unt<-as.matrix(mod_match$match.matrix[mod_match$weights[mod_match$treat == 1] == 0, ])

nrow(nb_unt)

# you can also look at the nearest neighbour in the control group for each treated individual
# that remains in the matched sample

mm <- as.matrix(mod_match$match.matrix[mod_match$weights[mod_match$treat == 1] > 0, ])

mm

# you create a matrix nm with the id of each individual
nm<-rownames(mod_match$X)

nm

# then you create yi the outcome for each treated individual
# that remains in the matched sample

yi <- mydata_cs$re78[match(rownames(mm), nm)] # the command match is used to select the lines of nm (id of individuals) that correspond to the row names of the lines of mm (id of treated individuals in the matched sample)
yi

# another way of doing this (more intuitive)

yibis<-dta_m$re78[dta_m$treat==1]
yibis

# create y1 the outcome for each control individual
# matched with each treated individual

y1 <- mydata_cs$re78[match(mm[,1], nm)] # here the command match is used to select the lines of nm (id of individuals) that correspond to the lines of mm (id of control individuals matched)
y1

# store the coefficient associated to the treatment variable
# tau.hat is the estimated ATT
tau.hat<-summary(lmodel_nm)$coefficients["treat",1]

# count the nb of treated individuals that
# remain in the matched sample
nt<-nrow(mm)

# compute the conditional variance of ATT
# assuming homoskedasticity and constant treatment effect

# to show Km(i) the nb of times one individual of the control group
# has been selected as nearest neighbour, we use
as.vector(table(mm[,1]))

# the conditional variance is:

sigma.t <- sum( ((yi - y1 - tau.hat)^2) / (2*nt) )  

# applying the formula of Abadie and Imbens 
c(rep(1,nt),as.vector(table(mm[,1])))

scale.sample.t <-sum(c(rep(1,nt),as.vector(table(mm[,1])))^2)/(nt^2)

# the standard error of ATT is

abadie_se<-sqrt(scale.sample.t * sigma.t)
















