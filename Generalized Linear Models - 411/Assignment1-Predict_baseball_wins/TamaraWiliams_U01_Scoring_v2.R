######## Predict 411, Unit 1, stand alone scoring
######## Submitted by: Tamara Williams

# start clean
#---------------------------
# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

#####
# Set working directory
#####
setwd("/Users/tamtwill/NorthwesternU_MSPA/Classes/Generalized Linear Models - 411/Unit 01/Assignment1")


# include required packages
#---------------------------
library(car)
library(leaps)
library(MASS)
library(glmnet)


#***************************************************************************************
#################### Test Data ##########################
#***************************************************************************************
moneyball_test=read.csv("moneyball_test.csv",header=T)
moneyball_train=read.csv("moneyball.csv",header=T)

# replicating the changes made to the training data on the test data
moneyball_test$TEAM_FIELDING_E[is.na(moneyball_test$TEAM_FIELDING_E)] = mean(moneyball_train$TEAM_FIELDING_E, na.rm = TRUE)
moneyball_test$TEAM_PITCHING_H[is.na(moneyball_test$TEAM_PITCHING_H)] = mean(moneyball_train$TEAM_PITCHING_H, na.rm = TRUE)
moneyball_test$TEAM_BATTING_BB[is.na(moneyball_test$TEAM_BATTING_BB)] = mean(moneyball_train$TEAM_BATTING_BB, na.rm = TRUE)
moneyball_test$TEAM_BATTING_H[is.na(moneyball_test$TEAM_BATTING_H)] = mean(moneyball_train$TEAM_BATTING_H, na.rm = TRUE)
moneyball_test$log_TEAM_BATTING_BB <- log(moneyball_test$TEAM_BATTING_BB)

# and clean up any artifacts of logs
#--------------------------------------------------------------------------
is.na(moneyball_test)<-sapply(moneyball_test, is.infinite)
moneyball_test[is.na(moneyball_test)]<-0


log_wins_predicted  <- 1.67153122 +
     -0.00049086 * moneyball_test$TEAM_BATTING_BB +
      0.00075988 * moneyball_test$TEAM_BATTING_H +
      0.30981277 * moneyball_test$log_TEAM_BATTING_BB +
     -0.00003232 * moneyball_test$TEAM_PITCHING_H +
     -0.00012413 * moneyball_test$TEAM_FIELDING_E 

#convert the Log(wins) back to wins, otherwise will be wildly off
moneyball_test$P_TARGET_WINS <- round(exp(log_wins_predicted),0)

#subset of data set for the deliverable "Scored data file"
tw_solution <- moneyball_test[c("INDEX","P_TARGET_WINS")]

#Write prediction File 
write.csv(tw_solution, "tam_williams_U01_scored_v2.csv", row.names = FALSE)

#for fun, look at the results
summary(tw_solution)
