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
df <- read.csv("heloc_test.csv",header=T)


############## Part 1: Data Exploration ##################
str(df)
summary(df)
sum(complete.cases(df))

#convert the text cases to numerics
# df$REASON<- as.numeric(factor(df$REASON))
# df$JOB<- as.numeric(factor(df$JOB))

# Clean any NAs
df[is.na(df)] <- 0

# convert the N/As to be the column means
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}

df_clean <- subset(df, select = -c(INDEX, TARGET_FLAG) )

df$predict <- -2.2436 -0.0067 * df_clean$YOJ + 0.5607 * df_clean$DEROG +
   0.7564 * df_clean$DELINQ -0.0057 * df_clean$CLAGE + 0.1537 * df_clean$NINQ +
  -0.0125 * df_clean$CLNO + 0.0492 * df_clean$DEBTINC

df$tmp = exp(df$predict)
df$tmp = df$tmp / (1.0+df$tmp)
df$P_TARGET_FLAG = round(df$tmp, 2)


#subset of data set for the deliverable "Scored data file"
prediction <- df[c("INDEX","P_TARGET_FLAG")]

#####
#Note, this next function will output an Excel file in your work environment called write.xlsx.
#####

#Write prediction File 
write.csv(prediction, "tam_williams_heloc_scoredv3.csv", row.names = FALSE)

