######## Predict 411, HELOC Extra Credit
######## Submitted by: Tamara Williams

# For the sake of good programming hygiene, start clean
#------------------------------------------------------
# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")


# Set working directory
#------------------------------------------
setwd("~/NorthwesternU_MSPA/Classes/Generalized Linear Models - 411/Bonus Assignments/HELOC")


# include required packages
#---------------------------
library(glmnet)

# Read in the data
#------------------------------------------
df <- read.csv("heloc.csv",header=T)


############## Part 1: Data Exploration ##################
str(df)
summary(df)
sum(complete.cases(df))

# #convert the text cases to numerics
# df$REASON<- as.numeric(factor(df$REASON))
# df$JOB<- as.numeric(factor(df$JOB))

# convert the N/As to be the column means
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}

summary(df)
sum(complete.cases(df))
df_clean <- subset(df, select = -c(INDEX, REASON, JOB) )

my_model <- glm(TARGET_FLAG ~.,family=binomial(link='logit'),data=df_clean)
round(coefficients(my_model), 4)
