#install.packages("readstata13")

library(readstata13)
mydata <-read.dta13("D:/professionnel/PPE_M1_EKAP_TD/Tutorial_2/angrist_krueger.dta")


mydata_sub<-subset(mydata,YOB>=30&YOB<=39)


# We construct birth-year dummies

#install.packages("fastDummies")
library("fastDummies")

mydata_sub<-dummy_cols(mydata_sub,
                       select_columns = c("QOB","YOB")
)

#install.packages("ivreg")
library("ivreg")

# 2SLS without adjusting for exogenous regressors

ols<-lm(LWKLYWGE~EDUC,data=mydata_sub)
summary(ols)

tsls<-ivreg(LWKLYWGE~EDUC|factor(QOB), # dependent variable ~ endogenous variable+exogenous variables | excluded instruments+exogenous covariates
            data=mydata_sub    )
summary(tsls)

# an additional year of schooling increases weekly earnings by around 10.2%

# tsls is obtained regressing first the endogenous variable
# on a set of instruments (first-stage) and then introducing the predicted endogenous variable
# in the structural (second-stage) equation

# first-stage ols model
fs<-lm(EDUC~factor(QOB),data=mydata_sub)
summary(fs)

# second-stage equation
tsls<-lm(LWKLYWGE~fs$fitted,data=mydata_sub)
summary(tsls)

# the coefficient is the same as the one obtained with ivreg command
# BUT standard errors are WRONG
# use the command ivreg directly



# adjusting for exogenous regressors age squared and year of birth dummies

mydata_sub$age_sq<-mydata_sub$AGE^2

tsls<-ivreg(LWKLYWGE~AGE+age_sq+factor(YOB)+EDUC|QOB_2+QOB_3+QOB_4+AGE+age_sq+factor(YOB),
        data=mydata_sub    )

summary(tsls)

confint(tsls,level=0.95)

# one additional year of education increases log weekly earnings by around 10.6%
# this effect ranges from 3.77% to 17.42%

# Is there an issue of weak instruments

# looking at the F-stat of the first-stage equation

fs_ur<-lm(EDUC~AGE+age_sq+factor(YOB)+factor(QOB),data=mydata_sub)
summary(fs_ur)

# F-stat=80.21

# But this F-stat only compares the unrestricted model
# with a restricted model in which we regress only educational attainment on a constant

# create a constant/intercept (a column of ones)

mydata_sub$constant<-ifelse(!is.na(mydata_sub$EDUC),1,0)

# the first-stage of the restricted model is

fs_r<-lm(EDUC~constant,data=mydata_sub)
summary(fs_r)

# we compute the F-stat using the R² of restricted and unrestricted models
# we test 13 restrictions (11 exogenous regressors and 2 excluded instruments (the 4th quarter of birth has been dropped from the regression due to multicolinearity))
# the nb of obs is 329509
# the total nb of regressors is 13+1 (don't forget the constant)
# so N-(13+1)=329495

F<-((summary(fs_ur)$r.squared-summary(fs_r)$r.squared)/13)/((1-summary(fs_ur)$r.squared)/(nrow(mydata_sub)-14))
F

# Not relevant to detect weak identification issues
# Indeed, it could be that high F stems only from strong exogenous predictors of the endogenous variable


# We have to compute the Cragg-Donald Stat
# we use the F-test to test the significance of excluded instruments

#the new restricted model is
fs_r<-lm(EDUC~AGE+age_sq+factor(YOB),data=mydata_sub)
summary(fs_r)

# The Cragg-Donald stat is the F comparing unrestricted model
# with this restricted model
# here we test 2 restrictions (2 excluded instruments)


Cragg<-((summary(fs_ur)$r.squared-summary(fs_r)$r.squared)/2)/((1-summary(fs_ur)$r.squared)/(nrow(mydata_sub)-14))
Cragg

# The Cragg-Donald Stat is around 16.12

# under H0: non significance of excluded instruments
# Cragg has a F(2,N-14) distribution
# to find the critical value (at a 5% level)

# we look at the corresponding quantile 
# for this F(2,N-14) distribution

qf(p=0.05,df1=2,df2=nrow(mydata_sub)-14,lower.tail=FALSE)

# the critical value is around 3
# The Cragg-Donald stat is strongly higher
# we reject H0 at a 5% level
# excluded instruments are jointly significant


# the corresponding p-value (the probability P(X>x), where X is the Cragg stat) is

pf(16.12,df1=2,df2=nrow(mydata_sub)-14,lower.tail=FALSE)
# p-value close to 0
# we strongly reject H0: very good first-stage




# overidentification case: more excluded instruments than endogenous regressors

# we can test the validity of instruments using a Sargan test

# what is Sargan test
# it is the value of N * R² of regressing u on all instruments

lm_r<-lm(tsls$residuals~QOB_2+QOB_3+QOB_4+AGE+age_sq+factor(YOB),data=mydata_sub)
summary(lm_r)
summary(lm_r)$r.squared*nrow(mydata_sub)

# the Sargan-stat is 3.23

# we want to test H0: the instruments are valid

# Under H0 the Sargan stat should have a chi-squared distribution of 
# 2 degrees of freedom (3 excluded instruments - 1 endogenous regressor)

pvalue=pchisq(3.23,df=2,lower.tail = FALSE)

# the p-value is around 0.199
# this means that the probability that the Sargan stat
# exceeds 3.23 is 0.199. 
# We can not reject H0 at a 5% and even at a 10% level
# instruments are valid


#Tutorial 2 : ex 2

jtpa<-read.csv("D:/professionnel/PPE_M1_EKAP_TD/Tutorial_2/jtpa.csv",
               header = TRUE,
               sep = ",",
               dec = "."
)

jtpa_men<-subset(jtpa,jtpa$male==1)

lmodel<-lm(income~treatment,data=jtpa_men)
summary(lmodel)

# Ignoring non-compliance issues we would conclude that
# the program increases income of participants by 3071.7$

library("tableone")

# First-stage (denominator of the Wald estimator)
tab<-CreateTableOne(vars="treatment",strata="instrument",data=jtpa_men,test=TRUE)
tab

0.64-0.01

# This shows that there are non-compliance issues
# among those assigned into the program only 64% do not participate in the program
# BUT there are a few always-takers (only 1%) : good news

# Reduced-form (numerator of the Wald estimator)

tab<-CreateTableOne(vars="income",strata="instrument",data=jtpa_men,test=TRUE)
tab

21753.78-20538.20

# The Wald estimator : 

(21753.78-20538.20)/(0.64-0.01)

library("ivreg")

lmodel_iv<-ivreg(income~treatment|instrument,data=jtpa_men)
summary(lmodel_iv)







