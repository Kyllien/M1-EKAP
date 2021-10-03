# Exercise 1

library(readstata13)
mydata <-read.dta13("D:/professionnel/PPE_M1_EKAP_TD/Tutorial_3/baseCDiD_H.dta")







library("tableone")

# looking at evolution of training rates among workers aged 45-49
# in firms employing 50 workers or more

stats_4549<-CreateTableOne(vars=c("formation98","formation99")
                      ,strata="large_firm98",data=mydata[mydata$age454998==1,])
stats_4549

stats_5054<-CreateTableOne(vars=c("formation98","formation99")
                           ,strata="large_firm98",data=mydata[mydata$age505498==1,])
stats_5054




# data are in "wide" format: repeated measurement in 1998 and 1999
# separate columns of the same record (ident)

# To estimate a DiD model, we need to reshape this data frame
# into a "long" format: repeated measurements in separate records

#install.packages("stats")
library("stats")

long_data<-reshape(mydata,
                   idvar="ident", # the name of the individual identifier
                   timevar="year", # the name of the time variable
                   direction="long", # we change a "wide" data set into a "long" one
                   varying=list(48:49), #it corresponds to formation98 formation99"
                   sep="", # because the year is directly after the name of the variable (no separator)
                   times=rep(c("98","99"),1), # the values for the years that correspond to the years 1998 and 1999
                   v.names = ("formation")) # the name of the new "long" variable created from formation98 and formation99

# descriptive statistics of the DiD

# Comparing evolution of training rates among workers aged
#45-49 years over the period 1998-1999 between large (treated by the change in the Delalande tax rate) 
# and small firms (not concerned by this change)

library("tableone")

# looking at evolution of training rates among workers aged 45-49
# in firms employing 50 workers or more

diff_treated<-CreateTableOne(vars="formation",strata="year",data=long_data[long_data$large_firm98==1&long_data$age454998==1,])
diff_treated

# In firms concerned by the increase in the tax rate, 
# training rates among workers aged 45-49 increased from 22% to 36%
# between 1998 (pre-reform period) and 1999 (post-reform period)



# looking at evolution of training rates among workers aged 45-49
# in firms employing less than 50 workers

diff_not_treated<-CreateTableOne(vars="formation",strata="year",data=long_data[long_data$large_firm98==0&long_data$age454998==1,])
diff_not_treated

# In firms NOT concerned by the increase in the tax rate, 
# training rates among workers aged 45-49 remains almost stable (from 12% to 14%)
# between 1998 (pre-reform period) and 1999 (post-reform period)

# The DiD estimator of the ATT is (36-22)-(14-12)=+12

# The increase in the Delalande tax in 1999 that concerned only firms
# with 50 employees or more has a positive causal effect on 
# training rates among workers just below the threshold age of the tax (50 years old)
# Indeed the ATT is around +12 percentage points.


# Simple DiD regression

# Constructing post-treatment dummy

long_data$post<-ifelse(test = (long_data$year==99),1,0)

# constructing an interaction term between the 
# treatment dummy and the post-reform dummy

long_data$interact<-long_data$post*long_data$large_firm98

did<-lm(formation~large_firm98+post+interact,
        data=long_data[long_data$age454998==1,])

summary(did)

# the DiD is the coefficient associated to the interaction term
# it is 0.108

# We do the same for workers aged 50-54

did_5054<-lm(formation~large_firm98+post+interact,
        data=long_data[long_data$age505498==1,])

summary(did_5054)

# the DiD is the coefficient associated to the interaction term
# it is only 0.056 not significant from 0


# This suggests that an increase in the Delalande tax rate
# has raised significantly the training rate among workers aged 45-49
# but not among those aged 50-54


# For this DiD estimator to be a valid estimator of the ATT
# we have made a key parallel trend assumption:
# training rates within large firms would have evolved in the same
# way as in the smaller ones if large firms had not been concerned by the reform


# To test this assumption, we do a placebo test, estimating the same DiD coefficients
# but among workers aged 25-29 years old.
# They should not been concerned by the change in the Delalande tax rate
# so DiD should not be significantly different from 0

# Placebo test

did_2529<-lm(formation~large_firm98+post+interact,
        data=long_data[long_data$age252998==1,])

summary(did_2529)


did_3034<-lm(formation~large_firm98+post+interact,
             data=long_data[long_data$age303498==1,])

summary(did_3034)


did_3539<-lm(formation~large_firm98+post+interact,
             data=long_data[long_data$age353998==1,])

summary(did_3539)


did_4044<-lm(formation~large_firm98+post+interact,
             data=long_data[long_data$age404498==1,])

summary(did_4044)

# We could ask whether training rates observed in small and large firms
# are really comparable. Indeed, it could be that workers
# employed in large firms have very different characteristics
# or face a very different work environment that non-concerned workers

# There is a way of combining propensity-score based matching methods with DiD

# First calculate the propensity score, that is the conditional probability
# of working in a firm employing 50 workers or more

# we condition on a set of pre-reform variables

# we restrict the sample to workers aged 45-49 years old
mydata_sub<-subset(mydata,mydata$age454998==1)

ps<-glm(formula = large_firm98~diplomeCAP98+diplomeBAC98+diplomeSUP98+anc_6_1098+anc_11_2098+anc_more2098+tps_partiel98+pcs_mid_management98+pcs_employees98+pcs_blue_collar98+plan_formation98,
             family =binomial(link = "logit"),
             data=mydata_sub) 
summary(ps)

mydata_sub$pscore<-ps$fitted

# looking at balancing property before matching

library("cobalt")

# checking the balancing property graphically

bal.plot(large_firm98~pscore,
         data=mydata_sub,
         var.name="pscore",
         type="histogram",
         mirror=TRUE)

# These graphs show that in the treated group
# (workers employed in large firms)
# the proportion of individuals with pscore close to 1
# is very high

# This is not the case in the control group

# this shows that there is limited overlap in the distribution
# of the propensity score between the treated and control groups

# nearest-neighbour matching method combined with DiD

library(MatchIt)

mod_match<-matchit(large_firm98~diplomeCAP98+diplomeBAC98+diplomeSUP98+anc_6_1098+anc_11_2098+anc_more2098+tps_partiel98+pcs_mid_management98+pcs_employees98+pcs_blue_collar98+plan_formation98,
                   method="nearest",
                   data=mydata_sub,
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

# estimating the DiD estimator combined with matching method

# First construct the difference in training incidence between 1998 and 1999

dta_m$diffform<-dta_m$formation99-dta_m$formation98

# Estimate the DiD after matching
# weighting control individuals
# the weight increases with the nb of times a control individual
# has been selected as a nearest neighbour

did_nm<-lm(diffform~large_firm98,
              data=dta_m,
              weights=weights)

summary(did_nm)




# IPW method

mydata_sub$weight<-with(mydata_sub,
                       large_firm98+(1-large_firm98)*pscore/(1-pscore))

# estimating the Combined DiD

# First construct the difference in training incidence between 1998 and 1999

mydata_sub$diffform<-mydata_sub$formation99-mydata_sub$formation98

# Then estimate the did estimator using these weights for
# non-treated individuals

ipw_did=lm(diffform~large_firm98,
           weight=weight,data=mydata_sub)
summary(ipw_did)







# Exercise 2
library(readstata13)

mlda <-read.dta13("D:/professionnel/PPE_M1_EKAP_TD/Tutorial_3/mlda.dta")

# subset for deaths in motor vehicle accidents, 1970-1983
mlda_sub <- subset(mlda, dtype=="MVA" & 
                      year <= 1983 & !is.na(beertaxa))

lm_did_1820 <- lm(mrate ~ legal + beertaxa + 
                      factor(state) + factor(year),
                      data = mlda_sub[mlda_sub$agegr=="18-20 yrs",])
summary(lm_did_1820)

death_MVA_1820<-mean(mlda_sub[mlda_sub$agegr=="18-20 yrs",]$mrate)

7.5877/60.22

#install.packages("clubSandwich")
library(clubSandwich)

coef_test(lm_did_1820, vcov = "CR1", 
          cluster = mlda_sub[mlda_sub$agegr=="18-20 yrs",]$state, test = "naive-t")[1:2,]

# inference is based at state-level 
# using BRL adjustment, implementing a correction in degrees of freedom
# we obtain

coef_test(lm_did_1820, vcov = "CR2", 
          cluster = mlda_sub[mlda_sub$agegr=="18-20 yrs",]$state, test = "Satterthwaite")[1:2,]











