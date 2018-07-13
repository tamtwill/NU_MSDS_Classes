######## Predict 411, Logistic Metrics Extra Credit
######## Submitted by: Tamara Williams

# For the sake of good programming hygiene, start clean
#------------------------------------------------------
# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Set working directory
#------------------------------------------
setwd("~/NorthwesternU_MSPA/Classes/Generalized Linear Models - 411/Bonus Assignments/Logit Calculations")

# include required packages
#---------------------------
library(glmnet)

# Read in the data
#------------------------------------------
df <- read.csv("logit_sas.csv",header=F)
logitMod <- glm(formula = V1 ~ ., family = binomial(link = "logit"), data = df)
predicted <- predict(logitMod, df, type="response") 
P_1 <- round(predicted, 5)
P_0 <- 1 - P_1
Y <- df$V1
output <- cbind(Y, P_0)
output <- cbind(output, P_1)
write.csv(output, "logit_prep.csv", row.names = FALSE)
