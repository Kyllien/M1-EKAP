# Variable selection with stepwize selection with MASS package

library(readxl)
dlbase <- read_excel("c://R//data//database.xlsx", sheet = "dlbase")
dlbase <- data.frame(dlbase)
training_dlbase <- dlbase
data.frame(training_dlbase)

library(MASS)
fit <- lm(Ddemand ~Evoleff + Preveff + Dprod, data=training_dlbase)
step1 <- stepAIC(fit, direction="both")

step2 <- stepAIC(fit, direction="forward")

step3 <- stepAIC(fit, direction="backward")
