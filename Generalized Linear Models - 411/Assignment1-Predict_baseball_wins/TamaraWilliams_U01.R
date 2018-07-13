######## Predict 411, Unit 1
######## Submitted by: Tamara Williams
######## Full code - all experimentation, and sample testing
######## Bingo-bonus code at the very end, look for the *****



# For the sake of good programming hygiene, start clean
#------------------------------------------------------
# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")


# Set working directory
#------------------------------------------
setwd("/Users/tamtwill/NorthwesternU_MSPA/Classes/Generalized Linear Models - 411/Unit 01/Assignment1")


# include required packages
#---------------------------
library(car)
library(leaps)
library(MASS)
library(glmnet)
library(rockchalk)

# Read in the data
#------------------------------------------
clean_df=read.csv("moneyball.csv",header=T)

moneyball = clean_df
					 


############## Part 1: Data Exploration ##################
str(moneyball)
summary(moneyball)
skew <- skewness(moneyball$TARGET_WINS)
kurt <- kurtosis(moneyball$TARGET_WINS)
complete_cases <- sum(complete.cases(moneyball))

# Wins - Use lower bound for lower outliers, upper bound for higher outliers.
par(mfrow=c(1,2))
hist(moneyball$TARGET_WINS, col = "#A71930", xlab = "TARGET_WINS", main = "Histogram of Wins")
boxplot(moneyball$TARGET_WINS, col = "#A71930", main = "Boxplot of Wins")
par(mfrow = c(1,1))

################# Batting ####################
# Hits and Doubles
par(mfrow=c(2,2))
hist(moneyball$TEAM_BATTING_H, col = "#A71930", xlab = "Team_Batting_H", main = "Histogram of Hits")
hist(moneyball$TEAM_BATTING_2B, col = "#09ADAD", xlab = "Doubles", main = "Histogram of Doubles")
boxplot(moneyball$TEAM_BATTING_H, col = "#A71930", main = "Boxplot of Hits")
boxplot(moneyball$TEAM_BATTING_2B, col = "#09ADAD", main = "Boxplot of Doubles")
par(mfrow=c(1,1))

# Triples and Home Runs
par(mfrow=c(2,2))
hist(moneyball$TEAM_BATTING_3B, col = "#A71930", xlab = "Triples", main = "Histogram of Triples")
hist(moneyball$TEAM_BATTING_HR, col = "#DBCEAC", xlab = "Home Runs", main = "Histogram of Home Runs")
boxplot(moneyball$TEAM_BATTING_3B, col = "#A71930", main = "Boxplot of Triples")
boxplot(moneyball$TEAM_BATTING_HR, col = "#DBCEAC", main = "Boxplot of Home Runs")
par(mfrow=c(1,1))

# Walks, Strikeouts, HBP
par(mfrow=c(2,3))
hist(moneyball$TEAM_BATTING_BB, col = "#A71930", xlab = "Walks", main = "Histogram of Walks")
hist(moneyball$TEAM_BATTING_SO, col = "#09ADAD", xlab = "Strikeouts", main = "Histogram of Strikeouts")
hist(moneyball$TEAM_BATTING_HBP, col = "#DBCEAC", xlab = "Hit By Pitches", main = "Histogram of HBP")
boxplot(moneyball$TEAM_BATTING_BB, col = "#A71930", main = "Boxplot of Walks")
boxplot(moneyball$TEAM_BATTING_SO, col = "#09ADAD", main = "Boxplot of Strikeouts")
boxplot(moneyball$TEAM_BATTING_HBP, col = "#DBCEAC", main = "Boxplot of HBP")
par(mfrow=c(1,1))

# Stolen Bases and Caught Stealing
par(mfrow=c(2,2))
hist(moneyball$TEAM_BASERUN_SB, col = "#A71930", xlab = "Stolen Bases", main = "Histogram of Steals")
hist(moneyball$TEAM_BASERUN_CS, col = "#DBCEAC", xlab = "Caught Stealing", main = "Histogram of CS")
boxplot(moneyball$TEAM_BASERUN_SB, col = "#A71930", main = "Boxplot of Steals")
boxplot(moneyball$TEAM_BASERUN_CS, col = "#DBCEAC", main = "Boxplot of CS")
par(mfrow=c(1,1))

################ Pitching ############
# Hits and Home Runs
par(mfrow=c(2,2))
hist(moneyball$TEAM_PITCHING_H, col = "#A71930", xlab = "Hits Against", main = "Histogram of Hits Against")
hist(moneyball$TEAM_PITCHING_HR, col = "#09ADAD", xlab = "Home Runs Against", main = "Histograms of HR Against")
boxplot(moneyball$TEAM_PITCHING_H, col = "#A71930", main = "Boxplot of Hits Against")
boxplot(moneyball$TEAM_PITCHING_HR, col = "#09ADAD", main = "Boxplot of HR Against")
par(mfrow=c(1,1))

# Walks and Strikeouts
par(mfrow=c(2,2))
hist(moneyball$TEAM_PITCHING_BB, col = "#A71930", xlab = "Walks Allowed", main = "Histogram of Walks Allowed")
hist(moneyball$TEAM_PITCHING_SO, col = "#DBCEAC", xlab = "Strikeouts", main = "Histograms of Strikeouts")
boxplot(moneyball$TEAM_PITCHING_BB, col = "#A71930", main = "Boxplot of Walks Allowed")
boxplot(moneyball$TEAM_PITCHING_SO, col = "#DBCEAC", main = "Boxplot of Strikeouts")
par(mfrow=c(1,1))

############## Fielding ###########
# Double Plays and Errors 
par(mfrow=c(2,2))
hist(moneyball$TEAM_FIELDING_DP, col = "#A71930", xlab = "Double Plays", main = "Histogram of Double Plays")
hist(moneyball$TEAM_FIELDING_E, col = "#09ADAD", xlab = "Errors Committed", main = "Histogram of Errors Committed")
boxplot(moneyball$TEAM_FIELDING_DP, col = "#A71930", main = "Boxplot of Double Plays")
boxplot(moneyball$TEAM_FIELDING_E, col = "#09ADAD", main = "Boxplot of Errors Committed")
par(mfrow=c(1,1))


######## Scatterplot Matrix ##########

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Batting Stats and Wins
pairs(moneyball[2:8], lower.panel=panel.smooth, upper.panel = panel.cor)

#Baserunning  Stats and Wins
pairs(~ moneyball$TARGET_WINS + moneyball$TEAM_BASERUN_CS + moneyball$TEAM_BASERUN_SB, lower.panel = panel.smooth, upper.panel = panel.cor)

#Pitcher Stats and Wins
pairs(~ moneyball$TARGET_WINS + moneyball$TEAM_PITCHING_BB + moneyball$TEAM_PITCHING_H + 
        moneyball$TEAM_PITCHING_HR + moneyball$TEAM_PITCHING_SO, lower.panel = panel.smooth, upper.panel = panel.cor)

pairs(moneyball[c(2,3,4,11,12)],lower.panel=panel.smooth, upper.panel = panel.cor)

######################### Part 2: Data Preparation - Instructor's Template #####################
#******************************************************

#Fix Missing Values Using Mean of All Seasons
moneyball$TEAM_BATTING_SO[is.na(moneyball$TEAM_BATTING_SO)] = mean(moneyball$TEAM_BATTING_SO, na.rm = TRUE)
moneyball$TEAM_BATTING_HBP[is.na(moneyball$TEAM_BATTING_HBP)] = mean(moneyball$TEAM_BATTING_HBP, na.rm = TRUE)
moneyball$TEAM_BASERUN_SB[is.na(moneyball$TEAM_BASERUN_SB)] = mean(moneyball$TEAM_BASERUN_SB, na.rm = TRUE)
moneyball$TEAM_BASERUN_CS[is.na(moneyball$TEAM_BASERUN_CS)] = mean(moneyball$TEAM_BASERUN_CS, na.rm = TRUE)
moneyball$TEAM_FIELDING_DP[is.na(moneyball$TEAM_FIELDING_DP)] = mean(moneyball$TEAM_FIELDING_DP, na.rm = TRUE)
moneyball$TEAM_PITCHING_SO[is.na(moneyball$TEAM_PITCHING_SO)] = mean(moneyball$TEAM_PITCHING_SO, na.rm = TRUE)

#Straighten Relationships
moneyball$TEAM_BATTING_1B <- moneyball$TEAM_BATTING_H - moneyball$TEAM_BATTING_HR - moneyball$TEAM_BATTING_3B -
                             moneyball$TEAM_BATTING_2B
moneyball$log_TEAM_BATTING_1B <- log(moneyball$TEAM_BATTING_1B)
moneyball$log_TEAM_BATTING_2B <- log(moneyball$TEAM_BATTING_2B)
moneyball$log_TEAM_BATTING_3B <- log(moneyball$TEAM_BATTING_3B)
moneyball$log_TEAM_BATTING_HR <- log(moneyball$TEAM_BATTING_HR)
moneyball$log_TEAM_BATTING_SO <- log(moneyball$TEAM_BATTING_SO)
moneyball$log_TEAM_BATTING_HBP <- log(moneyball$TEAM_BATTING_HBP)
moneyball$log_TEAM_BATTING_H <- log(moneyball$TEAM_BATTING_H)
moneyball$log_TEAM_BATTING_BB <- log(moneyball$TEAM_BATTING_BB)
moneyball$log_TEAM_BASERUN_SB <- log(moneyball$TEAM_BASERUN_SB)
moneyball$log_TEAM_BASERUN_CS <- log(moneyball$TEAM_BASERUN_CS)
moneyball$TEAM_BATTING_SO[is.na(moneyball$TEAM_BATTING_SO)] = mean(moneyball$TEAM_BATTING_SO, na.rm = TRUE)
moneyball$TEAM_FIELDING_E[(moneyball$TEAM_FIELDING_E > 500)] = 500
moneyball$sqrt_TEAM_PITCHING_HR <- sqrt(moneyball$TEAM_PITCHING_HR)
moneyball$SB_PCT <- moneyball$TEAM_BASERUN_SB/(1.0*moneyball$TEAM_BASERUN_SB+moneyball$TEAM_BASERUN_CS)

#Check that na's are gone. 
summary(moneyball)

#<<<<<<<<< WARNING: PER CLASS Q&A, THIS IS *NOT* ALLOWED.  DO NOT REMOVE RECORDS >>>>>>>>>>>>>
# #Remove bad data from data set
# #-----------------------------
# moneyball2 <- subset(moneyball, TARGET_WINS >= 21 & TARGET_WINS <= 120)
# moneyball2 <- subset(moneyball2, TEAM_PITCHING_H < 2000)

# there are some N/A forced by taking the log of 0, which the forbidden 
# code above would have handled, so let's make them 0 the hard way
is.na(moneyball)<-sapply(moneyball, is.infinite)
moneyball[is.na(moneyball)]<-0




#*************** Init summary data ***********************
# set up a dataframe to collect the comparision metrics
df_r2 <- data.frame(matrix(nrow = 0, ncol = 4))
x<-c("formula","adj_r2", "AIC","MSE")
colnames(df_r2) <- x
#*********************************************************


#################### Part 3: Model Creation ############################################
#---------------------------------------------------------------------------------------
#Function for Mean Square Error Calculation
mse <- function(sm) 
  mean(sm$residuals^2)

# Stepwise Approach
stepwisemodel <- lm(formula = TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
               TEAM_BATTING_H + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_HR + 
               TEAM_PITCHING_BB + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + log_TEAM_BATTING_1B + 
               log_TEAM_BATTING_3B + log_TEAM_BASERUN_SB + SB_PCT + log_TEAM_BASERUN_CS + 
               sqrt_TEAM_PITCHING_HR, data = moneyball)

stepwise <- stepAIC(stepwisemodel, direction = "both")
summary(stepwise)
vif(stepwise)
sqrt(vif(stepwise)) > 2
v1 <- AIC(stepwise)
v2 <- mse(stepwise)
adjr <- summary(stepwise)$adj.r.squared
tmp <- data.frame("stepwise", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)


# All subsets regression
subsets <- regsubsets(TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
                 TEAM_BATTING_H + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + 
                 TEAM_PITCHING_HR + TEAM_PITCHING_BB + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + 
                  SB_PCT + log_TEAM_BASERUN_CS + sqrt_TEAM_PITCHING_HR, data = moneyball, nbest = 2)

plot(subsets, scale="adjr2")
summary(subsets)
subsets$nbest
summary(subsets)
summary_lst <- summary(subsets)
i <- which.max(summary_lst$adjr2)
feature_lst <- summary_lst$which[i,]

# Model 3
model3 <- lm(TARGET_WINS ~ 
     TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
       TEAM_BASERUN_SB + TEAM_BASERUN_CS + 
       TEAM_FIELDING_E + TEAM_FIELDING_DP +
       TEAM_PITCHING_SO + TEAM_PITCHING_BB, data = moneyball)
summary(model3)
vif(model3)
v1 <- AIC(stepwise)
v2 <- mse(stepwise)
adjr <- summary(model3)$adj.r.squared
tmp <- data.frame("model3", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)

############################### Part 4: Student Exploration ############################################
#-------------------------------------------------------------------------------------------------------
#*******************************************************************************************************
#                                 Data Prep Explorations
#******************************************************************************************************* 
#************************  Change the way missing values are handled *********************

moneyball3 = clean_df

#Fix Missing Values Using median instead of mean, let's see what that does)
moneyball3$TEAM_BATTING_SO[is.na(moneyball3$TEAM_BATTING_SO)] = median(moneyball$TEAM_BATTING_SO, na.rm = TRUE)
moneyball3$TEAM_BATTING_HBP[is.na(moneyball3$TEAM_BATTING_HBP)] = median(moneyball$TEAM_BATTING_HBP, na.rm = TRUE)
moneyball3$TEAM_BASERUN_SB[is.na(moneyball3$TEAM_BASERUN_SB)] = median(moneyball$TEAM_BASERUN_SB, na.rm = TRUE)
moneyball3$TEAM_BASERUN_CS[is.na(moneyball3$TEAM_BASERUN_CS)] = median(moneyball$TEAM_BASERUN_CS, na.rm = TRUE)
moneyball3$TEAM_FIELDING_DP[is.na(moneyball3$TEAM_FIELDING_DP)] = median(moneyball$TEAM_FIELDING_DP, na.rm = TRUE)
moneyball3$TEAM_PITCHING_SO[is.na(moneyball3$TEAM_PITCHING_SO)] = median(moneyball$TEAM_PITCHING_SO, na.rm = TRUE)
moneyball3$TEAM_BATTING_2B[is.na(moneyball3$TEAM_BATTING_2B)] = median(moneyball$TEAM_BATTING_2B, na.rm = TRUE)
moneyball3$TEAM_BATTING_BB[is.na(moneyball3$TEAM_BATTING_BB)] = median(moneyball$TEAM_BATTING_BB, na.rm = TRUE)
moneyball3$TEAM_BATTING_H[is.na(moneyball3$TEAM_BATTING_H)] = median(moneyball$TEAM_BATTING_H, na.rm = TRUE)

#Straighten Relationships3
#----------------------------------------------------------------------------------------------------
moneyball3$TEAM_BATTING_1B <- moneyball$TEAM_BATTING_H - moneyball$TEAM_BATTING_HR - moneyball$TEAM_BATTING_3B -
  moneyball3$TEAM_BATTING_2B
moneyball3$log_TEAM_BATTING_1B <- log(moneyball$TEAM_BATTING_1B)
moneyball3$log_TEAM_BATTING_2B <- log(moneyball$TEAM_BATTING_2B)
moneyball3$log_TEAM_BATTING_H <- log(moneyball$TEAM_BATTING_H)
moneyball3$log_TEAM_BATTING_BB <- log(moneyball$TEAM_BATTING_BB)
moneyball3$log_TEAM_BATTING_HBP <- log(moneyball$TEAM_BATTING_HBP)
moneyball3$log_TEAM_BATTING_3B <- log(moneyball$TEAM_BATTING_3B)
moneyball3$log_TEAM_BASERUN_SB <- log(moneyball$TEAM_BASERUN_SB)
moneyball3$log_TEAM_BASERUN_CS <- log(moneyball$TEAM_BASERUN_CS)
moneyball3$TEAM_BATTING_SO[is.na(moneyball$TEAM_BATTING_SO)] = mean(moneyball$TEAM_BATTING_SO, na.rm = TRUE)
moneyball3$TEAM_FIELDING_E[(moneyball$TEAM_FIELDING_E > 500)] = 500
moneyball3$sqrt_TEAM_PITCHING_HR <- sqrt(moneyball$TEAM_PITCHING_HR)
moneyball3$SB_PCT <- moneyball$TEAM_BASERUN_SB/(1.0*moneyball$TEAM_BASERUN_SB+moneyball$TEAM_BASERUN_CS)

# and clean up any artifacts of log(target_wins)
#--------------------------------------------------------------------------
is.na(moneyball3)<-sapply(moneyball3, is.infinite)
moneyball3[is.na(moneyball3)]<-0

# compare the results for the 2 methods of filling in missing values
#--------------------------------------------------------------------------
tam_mean <- lm(TARGET_WINS ~TEAM_BATTING_H+TEAM_BATTING_2B+TEAM_BATTING_3B+
                 TEAM_BASERUN_SB+TEAM_BASERUN_CS+TEAM_BATTING_HBP+
                 TEAM_PITCHING_H+TEAM_PITCHING_SO+TEAM_FIELDING_E+
                 TEAM_FIELDING_DP, data = moneyball)

summary(tam_mean)
vif (tam_mean)
v1 <-AIC(tam_mean)
v2 <- mse(tam_mean)
adjr <- summary(tam_mean)$adj.r.squared
tmp <- data.frame( "tam_mean", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)

tam_med <- lm(TARGET_WINS ~TEAM_BATTING_H+TEAM_BATTING_2B+TEAM_BATTING_3B+
                TEAM_BASERUN_SB+TEAM_BASERUN_CS+TEAM_BATTING_HBP+
                TEAM_PITCHING_H+TEAM_PITCHING_SO+TEAM_FIELDING_E+
                TEAM_FIELDING_DP, data = moneyball3)

summary(tam_med)
vif (tam_med)
v1 <-AIC(tam_med)
v2 <-mse(tam_med)
adjr <- summary(tam_med)$adj.r.squared
tmp <- data.frame( "tam_med", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)


#one last data tweak, this time to use flags
#-----------------------------------------------------------
moneyball4 = clean_df

#create Flag columns for data with missing values
moneyball4[ , paste0( "Flag_",names(moneyball4)[-1])] <- lapply(moneyball4[-1], function(x) as.numeric(is.na(x)) )

#Fix Missing Values Using median instead of mean, let's see what that does)
moneyball4$TEAM_BATTING_SO[is.na(moneyball4$TEAM_BATTING_SO)] = median(moneyball$TEAM_BATTING_SO, na.rm = TRUE)
moneyball4$TEAM_BATTING_HBP[is.na(moneyball4$TEAM_BATTING_HBP)] = median(moneyball$TEAM_BATTING_HBP, na.rm = TRUE)
moneyball4$TEAM_BASERUN_SB[is.na(moneyball4$TEAM_BASERUN_SB)] = median(moneyball$TEAM_BASERUN_SB, na.rm = TRUE)
moneyball4$TEAM_BASERUN_CS[is.na(moneyball4$TEAM_BASERUN_CS)] = median(moneyball$TEAM_BASERUN_CS, na.rm = TRUE)
moneyball4$TEAM_FIELDING_DP[is.na(moneyball4$TEAM_FIELDING_DP)] = median(moneyball$TEAM_FIELDING_DP, na.rm = TRUE)
moneyball4$TEAM_PITCHING_SO[is.na(moneyball4$TEAM_PITCHING_SO)] = median(moneyball$TEAM_PITCHING_SO, na.rm = TRUE)
moneyball4$TEAM_BATTING_2B[is.na(moneyball4$TEAM_BATTING_2B)] = median(moneyball$TEAM_BATTING_2B, na.rm = TRUE)
moneyball4$TEAM_BATTING_BB[is.na(moneyball4$TEAM_BATTING_BB)] = median(moneyball$TEAM_BATTING_BB, na.rm = TRUE)
moneyball4$TEAM_BATTING_H[is.na(moneyball4$TEAM_BATTING_H)] = median(moneyball$TEAM_BATTING_H, na.rm = TRUE)

#Straighten Relationships
#----------------------------------------------------------------------------------------------------
moneyball4$TEAM_BATTING_1B <- moneyball$TEAM_BATTING_H - moneyball$TEAM_BATTING_HR - moneyball$TEAM_BATTING_3B -
  moneyball4$TEAM_BATTING_2B
moneyball4$log_TEAM_BATTING_1B <- log(moneyball4$TEAM_BATTING_1B)
moneyball4$log_TEAM_BATTING_2B <- log(moneyball4$TEAM_BATTING_2B)
moneyball4$log_TEAM_BATTING_H <- log(moneyball4$TEAM_BATTING_H)
moneyball4$log_TEAM_BATTING_BB <- log(moneyball4$TEAM_BATTING_BB)
moneyball4$log_TEAM_BATTING_HBP <- log(moneyball4$TEAM_BATTING_HBP)
moneyball4$log_TEAM_BATTING_3B <- log(moneyball4$TEAM_BATTING_3B)
moneyball4$log_TEAM_BASERUN_SB <- log(moneyball4$TEAM_BASERUN_SB)
moneyball4$log_TEAM_BASERUN_CS <- log(moneyball4$TEAM_BASERUN_CS)
moneyball4$TEAM_BATTING_SO[is.na(moneyball4$TEAM_BATTING_SO)] = mean(moneyball$TEAM_BATTING_SO, na.rm = TRUE)
moneyball4$TEAM_FIELDING_E[(moneyball4$TEAM_FIELDING_E > 500)] = 500
moneyball4$sqrt_TEAM_PITCHING_HR <- sqrt(moneyball4$TEAM_PITCHING_HR)
moneyball4$SB_PCT <- moneyball4$TEAM_BASERUN_SB/(1.0*moneyball4$TEAM_BASERUN_SB+moneyball4$TEAM_BASERUN_CS)

# and clean up any artifacts of log(target_wins)
#--------------------------------------------------------------------------
is.na(moneyball4)<-sapply(moneyball4, is.infinite)
moneyball4[is.na(moneyball4)]<-0


# save for later, in case it is useful
write.csv(moneyball4, "moneyball_flags.csv", row.names = FALSE)


# Compare to the plain stepwide model to see if adding flags has an effect
stepwisemodel2 <- lm(formula = TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
                TEAM_BATTING_H + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_HR +                      TEAM_PITCHING_BB + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + log_TEAM_BATTING_1B + 
                log_TEAM_BATTING_3B + log_TEAM_BASERUN_SB + SB_PCT + log_TEAM_BASERUN_CS + 
                sqrt_TEAM_PITCHING_HR + Flag_TEAM_BATTING_H + Flag_TEAM_BATTING_2B+ Flag_TEAM_BATTING_3B+
                Flag_TEAM_BATTING_HR + Flag_TEAM_BATTING_BB + Flag_TEAM_BATTING_SO + Flag_TEAM_BASERUN_SB + 
                Flag_TEAM_BASERUN_CS + Flag_TEAM_PITCHING_H + Flag_TEAM_PITCHING_HR + Flag_TEAM_PITCHING_BB + 
                Flag_TEAM_PITCHING_SO + Flag_TEAM_FIELDING_E + Flag_TEAM_FIELDING_DP + Flag_TEAM_BATTING_HBP, data = moneyball4)

stepwise_flag <- stepAIC(stepwisemodel2, direction = "both")
summary(stepwise_flag)
vif(stepwise_flag)
v1 <-AIC(stepwise_flag)
v2 <-mse(stepwise_flag)
adjr <- summary(stepwise_flag)$adj.r.squared
tmp <- data.frame("stepwise_flag", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)


#************************************************************
# Tam Model 1 - Remove High VIF variables
#************************************************************
# Tam Model1, let's see what happens if we remove the fields which had high
# VIF values from the stepwise model

tam_drop_VIF<- lm(TARGET_WINS ~TEAM_BATTING_H+TEAM_BATTING_2B+TEAM_BATTING_3B+
                   TEAM_BASERUN_SB+TEAM_BASERUN_CS+TEAM_BATTING_HBP+
                   TEAM_PITCHING_H+TEAM_PITCHING_SO+TEAM_FIELDING_E+
                   TEAM_FIELDING_DP, data = moneyball, scale = TRUE, validation = "CV")

summary(tam_drop_VIF)
vif (tam_drop_VIF)
v1 <-AIC(tam_drop_VIF)
v2 <-mse(tam_drop_VIF)
# no improvement
adjr <- summary(tam_drop_VIF)$adj.r.squared
tmp <- data.frame( "tam_drop_VIF", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)

#************************************************************
# Tam Model 2 - Add Interaction terms
#************************************************************
int_var1 <- moneyball$TEAM_BATTING_H*10+moneyball$TEAM_BATTING_2B*15+moneyball$TEAM_BATTING_3B*20
tam_term <- lm(TARGET_WINS ~ int_var1+
                   TEAM_BATTING_BB+TEAM_BATTING_SO+TEAM_BASERUN_SB+TEAM_BATTING_HBP+
                   TEAM_PITCHING_H+TEAM_PITCHING_HR+TEAM_PITCHING_BB+TEAM_PITCHING_SO+TEAM_FIELDING_E+
                   TEAM_FIELDING_DP, data = moneyball, scale = TRUE, validation = "CV")

summary(tam_term)
vif (tam_term)
v1 <- AIC(tam_term)
v2 <- mse(tam_term)
adjr <- summary(tam_term)$adj.r.squared
tmp <- data.frame( "tam_term", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)
# still not as good as other options

int_var2 <- moneyball$TEAM_BATTING_H^2+moneyball$TEAM_BATTING_2B*moneyball$TEAM_BATTING_3B+moneyball$TEAM_PITCHING_SO^2
tam_term2 <- lm(TARGET_WINS ~ int_var2+
                 TEAM_BATTING_BB+TEAM_BATTING_SO+TEAM_BASERUN_SB+TEAM_BATTING_HBP+
                 TEAM_PITCHING_H+TEAM_PITCHING_HR+TEAM_PITCHING_BB+TEAM_FIELDING_E+
                 TEAM_FIELDING_DP, data = moneyball, scale = TRUE, validation = "CV")

summary(tam_term2)
vif (tam_term2)
v1 <- AIC(tam_term2)
v2 <- mse(tam_term2)
adjr <- summary(tam_term2)$adj.r.squared
tmp <- data.frame( "tam_term2", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)
# still not as good as other options

#************************************************************
# Tam Model Step models - play around with stepwise, try a subset of 
# variables 
#************************************************************
#---------------------------------
tam_step1 <- lm(TARGET_WINS ~TEAM_BATTING_1B+TEAM_BATTING_2B+TEAM_BATTING_3B+
                      TEAM_BASERUN_SB+TEAM_BASERUN_CS+TEAM_BATTING_HBP+
                      TEAM_PITCHING_H+TEAM_PITCHING_SO+TEAM_FIELDING_E+
                      sqrt_TEAM_PITCHING_HR, data = moneyball)
tam_step_1 <- stepAIC(tam_step1, direction = "both")
summary(tam_step_1)
vif(tam_step_1)
v1 <- AIC(tam_step_1)
v2 <- mse(tam_step_1)
adjr <- summary(tam_step_1)$adj.r.squared
tmp <- data.frame( "tam_step_1", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)



# try a model with the data from the regsubsets
#------------------------------------------------------------------------
feature_lst
tam_sub1<- lm(TARGET_WINS ~TEAM_BATTING_1B + TEAM_BATTING_3B + TEAM_BATTING_HR + TEAM_BATTING_BB + 
                TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP, data=moneyball)

summary(tam_sub1)
vif(tam_sub1)
v1 <- AIC(tam_sub1)
v2 <- mse(tam_sub1)
adjr <- summary(tam_sub1)$adj.r.squared
tmp <- data.frame( "tam_sub1", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)


# try  looking for log of target values  
#--------------------------------------------------------------------------
moneyball$log_TARGET_WINS <- log(moneyball$TARGET_WINS)

# and clean up any artifacts of log(target_wins)
#--------------------------------------------------------------------------
is.na(moneyball)<-sapply(moneyball, is.infinite)
moneyball[is.na(moneyball)]<-0

tam_model_dp6 <- lm(formula = log_TARGET_WINS ~ TEAM_PITCHING_BB + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + 
                       log_TEAM_BATTING_1B + log_TEAM_BATTING_3B + log_TEAM_BASERUN_SB + SB_PCT + log_TEAM_BASERUN_CS + 
                       sqrt_TEAM_PITCHING_HR, data = moneyball)

tam_log_step <- stepAIC(tam_model_dp6, direction = "both")
summary(tam_log_step)
vif(tam_log_step)
v1 <- AIC(tam_log_step)
v2 <- mse(tam_log_step)
adjr <- summary(tam_log_step)$adj.r.squared
tmp <- data.frame( "tam_log_step", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)

# try a model with the data from the regsubsets looking for log(target_wins)
#------------------------------------------------------------------------
tam_sub_log <- lm(TARGET_WINS ~TEAM_BATTING_1B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
                    TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_PITCHING_SO +
                    TEAM_FIELDING_E + TEAM_FIELDING_DP, data=moneyball)

summary(tam_sub_log)
vif(tam_sub_log)
v1 <- AIC(tam_sub_log)
v2 <- mse(tam_sub_log)
adjr <- summary(tam_sub_log)$adj.r.squared
tmp <- data.frame( "tam_sub_log", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)


# try a step model from all the variables
#------------------------------------------------------------------------
tam_model_s5 <- lm(formula = log_TARGET_WINS ~ TEAM_BATTING_H+TEAM_BATTING_2B+TEAM_BATTING_3B+TEAM_BATTING_HR+     
                     TEAM_BATTING_BB+TEAM_BATTING_SO+TEAM_BASERUN_SB+TEAM_BASERUN_CS+
                     TEAM_BATTING_HBP+TEAM_PITCHING_H+TEAM_PITCHING_HR+TEAM_PITCHING_BB+
                     TEAM_PITCHING_SO+TEAM_FIELDING_E+TEAM_FIELDING_DP+TEAM_BATTING_1B+    
                     log_TEAM_BATTING_1B+log_TEAM_BATTING_2B+log_TEAM_BATTING_H+
                     log_TEAM_BATTING_BB+log_TEAM_BATTING_HBP+log_TEAM_BATTING_3B+
                     log_TEAM_BASERUN_SB+log_TEAM_BASERUN_CS+sqrt_TEAM_PITCHING_HR+SB_PCT, data = moneyball)

tam_all_var <- stepAIC(tam_model_s5, direction = "both")

summary(tam_all_var)
vif(tam_all_var)
v1 <- AIC(tam_all_var)
v2 <- mse(tam_all_var)
adjr <- summary(tam_all_var)$adj.r.squared
tmp <- data.frame( "tam_all_var", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)

# what happens if we use square root instead of log for the transformation?
moneyball$sqrt_TARGET_WINS <- sqrt(moneyball$TARGET_WINS)

tam_s6 <-lm(sqrt_TARGET_WINS ~TEAM_BATTING_H+TEAM_BATTING_2B+TEAM_BATTING_3B+
                       TEAM_BASERUN_SB+TEAM_BASERUN_CS+TEAM_BATTING_HBP+
                       TEAM_PITCHING_H+TEAM_PITCHING_SO+TEAM_FIELDING_E+
                       TEAM_FIELDING_DP, data = moneyball)

tam_sqrt_step <- stepAIC(tam_s6, direction = "both")
summary(tam_sqrt_step)
vif(tam_sqrt_step)
v1 <- AIC(tam_sqrt_step)
v2 <- mse(tam_sqrt_step)
adjr <- summary(tam_sqrt_step)$adj.r.squared
tmp <- data.frame( "tam_sqrt_step", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)


# After using Weka to select variables, for bonus bingo, use that set of variables for the formulas
# in model building
#----------------------------------------------------------------------------------------------------
weka_vars_log <- lm(formula = log_TARGET_WINS ~ TEAM_BATTING_2B + TEAM_BATTING_H + TEAM_BATTING_BB + TEAM_BATTING_HBP , data = moneyball)
summary(weka_vars_log)
vif(weka_vars_log)
v1 <- AIC(weka_vars_log)
v2 <- mse(weka_vars_log)
adjr <- summary(weka_vars_log)$adj.r.squared
tmp <- data.frame( "weka_vars_log", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)

weka_vars_sqrt <- lm(formula = sqrt_TARGET_WINS ~ TEAM_BATTING_2B + TEAM_BATTING_H + TEAM_BATTING_BB + 
                       TEAM_BATTING_HBP, data = moneyball)
summary(weka_vars_sqrt)
vif(weka_vars_sqrt)
v1 <- AIC(weka_vars_sqrt)
v2 <- mse(weka_vars_sqrt)
adjr <- summary(weka_vars_sqrt)$adj.r.squared
tmp <- data.frame( "weka_vars_sqrt", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)

weka_no_trans <- lm(formula =TARGET_WINS ~ TEAM_BATTING_2B + TEAM_BATTING_H + TEAM_BATTING_BB + 
                    TEAM_BATTING_HBP , data = moneyball)
summary(weka_no_trans)
vif(weka_no_trans)
v1 <- AIC(weka_no_trans)
v2 <- mse(weka_no_trans)
adjr <- summary(weka_no_trans)$adj.r.squared
tmp <- data.frame( "weka_no_trans", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)

# Models built from the Weka Tree routines for feature selection
# the Random Forest result
#----------------------------------------------
weka_RF <- lm(formula =TARGET_WINS ~ TEAM_BATTING_H+
               TEAM_BATTING_HR+
               TEAM_BATTING_BB+
               TEAM_BASERUN_SB+
               TEAM_BASERUN_CS+
               TEAM_BATTING_HBP+
               TEAM_PITCHING_H+
               TEAM_PITCHING_SO+
               TEAM_FIELDING_E+
               TEAM_FIELDING_DP, data = moneyball)
summary(weka_RF)
vif(weka_RF)
v1 <- AIC(weka_RF)
v2 <- mse(weka_RF)
adjr <- summary(weka_RF)$adj.r.squared
tmp <- data.frame( "weka_RF", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)


# the M5P tree result
#----------------------------------------------
weka_m5p <- lm(formula =TARGET_WINS ~ TEAM_BATTING_H +
               TEAM_BATTING_2B+
               TEAM_BATTING_3B+
               TEAM_BATTING_HR+
               TEAM_BATTING_BB+
               TEAM_BATTING_SO+
               TEAM_BASERUN_SB+
               TEAM_BASERUN_CS+
               TEAM_BATTING_HBP+
               TEAM_PITCHING_H+
               TEAM_FIELDING_E+
               TEAM_FIELDING_DP, data = moneyball)
summary(weka_m5p)
vif(weka_m5p)
v1 <- AIC(weka_m5p)
v2 <- mse(weka_m5p)
adjr <- summary(weka_m5p)$adj.r.squared
tmp <- data.frame( "weka_m5p", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)



##----------- THIS TAKES A LONG TIME TO RUN
## comment out since it is **really** SLOW to do RFE, only run when you have to
## try RFE for feature selection

# library(caret)
# # define the feature set using a random forest selection function
# rfe_select <- rfe(moneyball[,3:30], moneyball[,31],
#           sizes = c(3:8), metric = "RMSE", maximize = F, rfeControl = rfeControl(functions = treebagFuncs))
# # summarize the results
# print(rfe_select)
# 
# # list the chosen features
# predictors(rfe_select)

# put the selected features into a model
rfe_model <- lm(formula = log_TARGET_WINS~ TEAM_BATTING_BB + TEAM_BATTING_H + log_TEAM_BATTING_BB +
                  TEAM_PITCHING_H + TEAM_FIELDING_E, data = moneyball)

summary(rfe_model)
vif(rfe_model)
v1 <- AIC(rfe_model)
v2 <- mse(rfe_model)
adjr <- summary(rfe_model)$adj.r.squared
tmp <- data.frame( "rfe_model", adjr, v1, v2)
names(tmp)<-c("formula","adj_r2", "AIC","MSE")
df_r2 <- rbind(df_r2, tmp)


#************************************************
######## Performance #######
# from template
AIC(stepwise)
AIC(subset)
AIC(model3)
mse(stepwise)
mse(subset)
mse(model3)

#-- generated by exploration of data prep methods

AIC(tam_mean)
AIC(tam_med)
AIC(stepwise_flag)

mse(tam_mean)
mse(tam_med)
mse(stepwise_flag)

# --- model attempts
AIC(tam_drop_VIF)
AIC(tam_term)
AIC(tam_term2)
AIC(tam_step_1)
AIC(tam_sub1)
AIC(tam_log_step)
AIC(tam_sub_log)
AIC(tam_all_var)
AIC(tam_sqrt_step)
AIC(weka_vars_log)
AIC(weka_vars_sqrt)
AIC(weka_no_trans)
AIC(weka_RF)
AIC(weka_m5p)
AIC(rfe_model)

mse(tam_drop_VIF)
mse(tam_term)
mse(tam_term2)
mse(tam_step_1)
mse(tam_sub1)
mse(tam_log_step)
mse(tam_sub_log)
mse(tam_all_var)
mse(tam_sqrt_step)
mse(tam_log_step)
mse(weka_vars_log)
mse(weka_vars_sqrt)
mse(weka_no_trans)
mse(weka_RF)
mse(weka_m5p)
mse(rfe_model)

df_r2[,-1] <-round(df_r2[,-1],3)
df_r2
# write out results in case you want to format nicely in Excel
write.csv(df_r2, "Model_Metrics.csv", row.names = FALSE)

# coefficients of candidate models, rounded for readability
round(coef(tam_all_var),5)
round(coef(rfe_model),5)
round(coef(tam_log_step),5)




# after reviewing the outcome, the selected model is weka_vars_sqrt
bst_model <- rfe_model

#################### Visualize results for selected model as final check ##############
#***************************************************************************************
par(mfrow = c(1, 3))
hist(bst_model$residuals, main = 'Histogram of Residuals',  xlab = 'Residuals', ylab = ' ', 
     col = 'cyan2')
qqnorm(bst_model$residuals,main = 'QQ Plot of Residuals', xlab = 'Theoretical Quantiles', 
       ylab = 'Sample Quartiles', col = 'cyan2') 
qqline(bst_model$residuals, datax = FALSE, distribution = qnorm,  col = 'red')

plot(resid(bst_model), ylab="Residuals", xlab="Wins Predicted", main="Target Win Residual plot") 
abline(h=0, col='red')   
par(mfrow = c(1, 1))

#Output the coefficients for the standalone scoring file
coef(bst_model)

# 
# #***************************************************************************************
# #################### Test Data  - test the testing            ##########################
# #***************************************************************************************
# #################### See separate scoring file for real test  ##########################
# #***************************************************************************************
# setwd("/Users/tamtwill/NorthwesternU_MSPA/Classes/Generalized Linear Models - 411/Unit 01/Assignment1")
# moneyball_test=read.csv("moneyball_test.csv",header=T)
# moneyball_train=read.csv("moneyball.csv",header=T)
# 
# # replicating the changes made to the training data on my best model
# moneyball_test$TEAM_BATTING_SO[is.na(moneyball_test$TEAM_BATTING_SO)] = mean(moneyball_train$TEAM_BATTING_SO, na.rm = TRUE)
# moneyball_test$TEAM_BATTING_HBP[is.na(moneyball_test$TEAM_BATTING_HBP)] = mean(moneyball_train$TEAM_BATTING_HBP, na.rm = TRUE)
# moneyball_test$TEAM_BASERUN_SB[is.na(moneyball_test$TEAM_BASERUN_SB)] = mean(moneyball_train$TEAM_BASERUN_SB, na.rm = TRUE)
# moneyball_test$TEAM_BASERUN_CS[is.na(moneyball_test$TEAM_BASERUN_CS)] = mean(moneyball_train$TEAM_BASERUN_CS, na.rm = TRUE)
# moneyball_test$TEAM_FIELDING_DP[is.na(moneyball_test$TEAM_FIELDING_DP)] = mean(moneyball_train$TEAM_FIELDING_DP, na.rm = TRUE)
# moneyball_test$TEAM_PITCHING_SO[is.na(moneyball_test$TEAM_PITCHING_SO)] = mean(moneyball_train$TEAM_PITCHING_SO, na.rm = TRUE)
# moneyball_test$TEAM_BATTING_2BP[is.na(moneyball_test$TEAM_BATTING_2B)] = mean(moneyball_train$TEAM_BATTING_2B, na.rm = TRUE)
# moneyball_test$TEAM_BATTING_BB[is.na(moneyball_test$TEAM_BATTING_BB)] = mean(moneyball_train$TEAM_BATTING_BB, na.rm = TRUE)
# moneyball_test$TEAM_BATTING_H[is.na(moneyball_test$TEAM_BATTING_H)] = mean(moneyball_train$TEAM_BATTING_H, na.rm = TRUE)
# 
# 
# #Straighten Relationships
# moneyball_test$TEAM_BATTING_1B <- moneyball_test$TEAM_BATTING_H - moneyball_test$TEAM_BATTING_HR - moneyball_test$TEAM_BATTING_3B - moneyball_test$TEAM_BATTING_2B
# moneyball_test$log_TEAM_BATTING_1B <- log(moneyball_test$TEAM_BATTING_1B)
# moneyball_test$log_TEAM_BATTING_3B <- log(moneyball_test$TEAM_BATTING_3B)
# moneyball_test$log_TEAM_BASERUN_SB <- log(moneyball_test$TEAM_BASERUN_SB)
# moneyball_test$log_TEAM_BASERUN_CS <- log(moneyball_test$TEAM_BASERUN_CS)
# moneyball_test$TEAM_FIELDING_E[(moneyball_test$TEAM_FIELDING_E > 500)] = 500
# moneyball_test$sqrt_TEAM_PITCHING_HR <- sqrt(moneyball_test$TEAM_PITCHING_HR)
# moneyball_test$SB_PCT <- moneyball_test$TEAM_BASERUN_SB/(1.0*moneyball_test$TEAM_BASERUN_SB+moneyball_test$TEAM_BASERUN_CS)
# 
# # and clean up any artifacts of log(target_wins)
# #--------------------------------------------------------------------------
# is.na(moneyball_test)<-sapply(moneyball_test, is.infinite)
# moneyball_test[is.na(moneyball_test)]<-0
# 
# #### Selected Model
# bst_model <- lm(formula = log_TARGET_WINS~ TEAM_BATTING_BB + TEAM_BATTING_H + log_TEAM_BATTING_BB +
#                   TEAM_PITCHING_H + TEAM_FIELDING_E, data = moneyball)
# fitted(bst_model)
# 
# #subset of data set for the deliverable "Scored data file"
# # log_wins_predicted <- predict(bst_model, newdata = moneyball_test)
# # moneyball_test$P_TARGET_WINS <- exp(log_wins_predicted)
# wins_predicted <- predict(bst_model, newdata = moneyball_test)
# moneyball_test$P_TARGET_WINS <- exp(wins_predicted)
# 
# 
# #subset of data set for the deliverable "Scored data file"
# tw_solution <- moneyball_test[c("INDEX","P_TARGET_WINS")]
# 
# #Write prediction File 
# write.csv(tw_solution, "tamara_williams_U01_train.csv", row.names = FALSE)


#***************************************************************************************
#################### Bingo Bonus  ##########################
#***************************************************************************************
# best model
modelbb <- lm(formula = log_TARGET_WINS~ TEAM_BATTING_BB + TEAM_BATTING_H + log_TEAM_BATTING_BB +
                             TEAM_PITCHING_H + TEAM_FIELDING_E, data = moneyball)


# using GLM on the best model
glm_mod <-glm(formula = log_TARGET_WINS~ TEAM_BATTING_BB + TEAM_BATTING_H + log_TEAM_BATTING_BB +
                               TEAM_PITCHING_H + TEAM_FIELDING_E, data = moneyball)

summary(modelbb)
summary(glm_mod)

vif(modelbb)
AIC(modelbb)
vif(glm_mod)
AIC(glm_mod)

round(coef(modelbb),5)
round(coef(glm_mod),5)


# imputing with trees
library(randomForest)
# pattern is dataToImputed <- na.roughfix(dataWithMissingValues)
#Fix Missing Values Using median instead of mean, let's see what that does)
tree_df <- clean_df
tree_df$BB_TEAM_BATTING_SO = na.roughfix(tree_df$TEAM_BATTING_SO, na.rm = TRUE)
tree_df$BB_TEAM_BATTING_HBP = na.roughfix(tree_df$TEAM_BATTING_HBP, na.rm = TRUE)
tree_df$BB_TEAM_BASERUN_SB = na.roughfix(tree_df$TEAM_BASERUN_SB, na.rm = TRUE)
tree_df$BB_TEAM_BASERUN_CS = na.roughfix(tree_df$TEAM_BASERUN_CS, na.rm = TRUE)
tree_df$BB_TEAM_FIELDING_DP = na.roughfix(tree_df$TEAM_FIELDING_DP, na.rm = TRUE)
tree_df$BB_TEAM_PITCHING_SO = na.roughfix(tree_df$TEAM_PITCHING_SO, na.rm = TRUE)
tree_df$BB_TEAM_BATTING_2B = na.roughfix(tree_df$TEAM_BATTING_2B, na.rm = TRUE)
tree_df$BB_TEAM_BATTING_BB = na.roughfix(tree_df$TEAM_BATTING_BB, na.rm = TRUE)
tree_df$BB_TEAM_BATTING_H = na.roughfix(moneyball4$TEAM_BATTING_H, na.rm = TRUE)
tree_df<- subset(tree_df, TARGET_WINS >= 70 & TARGET_WINS <= 140)
tree_df<- subset(tree_df, TEAM_PITCHING_H < 1680)


library(compare)
res <- compareIdentical(moneyball_med,tree_df)
res   #the randomForest filled in the same way as 


