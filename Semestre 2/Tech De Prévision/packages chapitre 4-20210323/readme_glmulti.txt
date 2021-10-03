# Variable selection with the package glmulti
# All-subset linear regression using lm() based on AIC

library(readxl)
dlbase <- read_excel("c://R//data//database.xlsx", sheet = "dlbase")
dlbase <- data.frame(dlbase)
training_dlbase <- dlbase
data.frame(training_dlbase)

# Warning: install Java-64bits if you use R-64bits: https://www.java.com/en/download/manual.jsp 
library(rJava)
library(glmulti)
multi <- glmulti(Ddemand ~Evoleff + Preveff + Dprod, data = training_dlbase,
level = 1, method = "h", crit = "aic", confsetsize = 2, plotty = F, report = F, fitfunction = "lm")      
summary(multi)
show(multi)


level = 1				# No interaction considered
method = "h"			# Exhaustive approach - "g": genetic algorithm
The genetic algo "g" is recommended when the number of candidate models make
the method "h" unapplicable.
crit = "aic"			# AIC as criteria - "aicc", "bic", "qaic" or "qaicc"
confsetsize = 2			# Keep 2 best models
plotty = F, report = F	# No plot or interim reports
fitfunction = "lm"		# lm function
