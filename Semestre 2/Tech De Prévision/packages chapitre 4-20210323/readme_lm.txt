# Estimation of multiple regression models with function lm()

library(readxl)
dlbase <- read_excel("c://R//data//database.xlsx", sheet = "dlbase")
dlbase <- data.frame(dlbase)
training_dlbase <- dlbase
data.frame(training_dlbase)

***********************************************************
# Model estimation
model1 <- lm(Ddemand ~Evoleff + Preveff + Dprod, data=training_dlbase)
summary(model1)
confint(model1)
anova(model1)
plot(model1)

model2 <- lm(Ddemand ~Evoleff + Preveff, data=training_dlbase)
summary(model2)
confint(model2)
anova(model2)
plot(model2)

**********************************************************
# Variable selection with backward and stepwize methods
library(olsrr)
forward <- ols_step_backward_aic(model, details=TRUE)
stps <- ols_step_both_p(model, details=TRUE)


**********************************************************
**********************************************************
## Forecasting
trainingbase <- dlbase[1:180,]
testingbase <- dlbase[181:202,]
observedbase <- dlbase[181:202,1]

----------------------------------------------------------------
# forecast horizon h=22
forecast <- NULL
modelf <- lm(Ddemand ~Evoleff + Preveff, data=trainingbase)
forecast <- predict(modelf, newdata = testingbase)	# 22-step ahead forecast
forecast
error <- observedbase - forecast
error

