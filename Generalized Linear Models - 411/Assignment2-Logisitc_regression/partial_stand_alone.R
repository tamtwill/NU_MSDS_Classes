
#train_df=read.csv("logit_insurance.csv",header=T, stringsAsFactors = TRUE)
train_df <- working_df
clean_df<-read.csv("logit_insurance_test.csv",header=T, stringsAsFactors = TRUE)

test_df<-clean_df
test_df$TARGET_FLAG[is.na(test_df$TARGET_FLAG)] <- 0
test_df$TARGET_AMT[is.na(test_df$TARGET_AMT)] <- 0


# get the numeric data, proces those pesky factors, so we can take a look at correlations more easily
test_df$SEX <- as.factor(test_df$SEX)
test_df$EDUCATION <- as.factor(test_df$EDUCATION)
test_df$PARENT1 <- as.factor(test_df$PARENT1)
test_df$MSTATUS <- as.factor(test_df$MSTATUS)
test_df$REVOKED <- as.factor(test_df$REVOKED)
test_df$RED_CAR <- as.factor(ifelse(test_df$RED_CAR=="yes", 1, 0))
test_df$URBANICITY <- ifelse(test_df$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
test_df$URBANICITY <- as.factor(test_df$URBANICITY)
test_df$CAR_USE <- as.factor(test_df$CAR_USE)
test_df$CAR_TYPE <- as.factor(test_df$CAR_TYPE)

#deal with those dollar signs
test_df$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test_df$INCOME)))
test_df$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test_df$HOME_VAL)))
test_df$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test_df$OLDCLAIM)))
test_df$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test_df$BLUEBOOK)))

test_df$JOB <- sub("^$", "Other", test_df$JOB)
test_df$JOB <- as.factor(test_df$JOB)

summary(test_df)

# handling the missing data
#-------------------------------------------------------
# add impute flags to new dataframe 
#-------------------------------------------------------
tmp <- as.data.frame(lapply(test_df[-1], function(x) as.numeric(is.na(x))))
flagged <- (colSums(tmp, na.rm=T) != 0)
tmp <- tmp[, flagged]
colnames(tmp) <- paste("FLAG", colnames(tmp), sep = "_")

# Cbind the imputed columns to the original dataframe
# use mean to impute missing values
test_df <- cbind(test_df, tmp)

# Bin Income
test_df$INCOME_bin[is.na(test_df$INCOME)] <- "NA"
test_df$INCOME_bin[test_df$INCOME == 0] <- "Zero"
test_df$INCOME_bin[test_df$INCOME >= 1 & test_df$INCOME < 30000] <- "Low"
test_df$INCOME_bin[test_df$INCOME >= 30000 & test_df$INCOME < 80000] <- "Medium"
test_df$INCOME_bin[test_df$INCOME >= 80000] <- "High"
test_df$INCOME_bin <- factor(test_df$INCOME_bin)
test_df$INCOME_bin <- factor(test_df$INCOME_bin, levels=c("NA","Zero","Low","Medium","High"))

test_df$TRAVTIME[is.na(test_df$TRAVTIME)] <-mean(train_df$TRAVTIME, na.rm=TRUE)
test_df$OLDCLAIM[is.na(test_df$OLDCLAIM)] <-mean(train_df$OLDCLAIM, na.rm=TRUE)
test_df$BLUEBOOK [is.na(test_df$BLUEBOOK)] <-mean(train_df$BLUEBOOK, na.rm=TRUE)
test_df$HOME_VAL[is.na(test_df$HOME_VAL)] <-mean(train_df$HOME_VAL, na.rm=TRUE)
test_df$INCOME[is.na(test_df$INCOME)] <-mean(train_df$INCOME, na.rm=TRUE)
test_df$CAR_AGE[is.na(test_df$CAR_AGE)] <-mean(train_df$CAR_AGE, na.rm=TRUE)
test_df$AGE[is.na(test_df$AGE)] <-mean(train_df$AGE, na.rm=TRUE)
test_df$YOJ[is.na(test_df$YOJ)] <-mean(train_df$YOJ, na.rm=TRUE)
test_df$TIF[is.na(test_df$TIF)] <-mean(train_df$TIF, na.rm=TRUE)
test_df$CLM_FREQ[is.na(test_df$CLM_FREQ)] <-mean(train_df$CLM_FREQ, na.rm=TRUE)
test_df$MVR_PTS[is.na(test_df$MVR_PTS)] <-mean(train_df$MVR_PTS, na.rm=TRUE)
test_df$HOMEKIDS[is.na(test_df$HOMEKIDS)] <-mean(train_df$HOMEKIDS, na.rm=TRUE)

test_df$CAR_AGE[test_df$CAR_AGE < 0 ] <- 0 
test_df$HOME_OWNER <- ifelse(test_df$HOME_VAL == 0, 0, 1)
test_df$SQRT_TRAVTIME <- sqrt(test_df$TRAVTIME)
test_df$SQRT_BLUEBOOK <- sqrt(test_df$BLUEBOOK)
test_df$LOG_HOME_VAL <- log(test_df$HOME_VAL+1)
test_df$LOG_OLDCLAIM <- log(test_df$OLDCLAIM+1)

# Again, double-checking to make sure we don't have any NA's in our Test Data Set
summary(test_df)

########### STAND ALONE SCORING PROGRAM ###############
##### Model coefficients used to create P_TARGET_FLAG, Mean TARGET_AMT aggregated by CAR was used to predict
######## P-TARGET AMOUNT
######## Note that you may want to do a multiple linear regression for this model.
######## The model below is based on CAR multiplied by the amount and probability of a claim (27%)

test_df$P_TARGET_FLAG <- predict(Model6, newdata = test_df, type = "response")
# df_flag1 <- subset(imp_df, TARGET_FLAG == 1 )
# targetbycar <- aggregate(df_flag1$TARGET_AMT, list(df_flag1$CAR_TYPE), mean)
l_df <- dim(test_df)[1]
for (i in 1:l_df){
  if (test_df$P_TARGET_FLAG[i] > .50){
    test_df$P_TARGET_AMT[i] <- ifelse(test_df$CAR_TYPE[i]=="Minivan", 5601.67%*%test_df$P_TARGET_FLAG[i],
                                      ifelse(test_df$CAR_TYPE[i]=="Panel Truck",7464.70%*%test_df$P_TARGET_FLAG[i],
                                             ifelse(test_df$CAR_TYPE[i]=="Pickup", 5430.11%*%test_df$P_TARGET_FLAG[i],
                                                    ifelse(test_df$CAR_TYPE[i]=="Sports Car", 5412.73%*%test_df$P_TARGET_FLAG[i],
                                                           ifelse(test_df$CAR_TYPE[i]=="Van", 6908.553%*%test_df$P_TARGET_FLAG[i],
                                                                  ifelse(test_df$CAR_TYPE[i]=="z_SUV", 5241.104%*%test_df$P_TARGET_FLAG[i], 0.00))))))
  }else{test_df$P_TARGET_AMT[i] <- 0}
  i=i+1
}

summary(test_df)

# Scored Data File
scores <- test_df[c("INDEX","P_TARGET_FLAG", "P_TARGET_AMT")]
write.csv(scores, file = "Williams_U02a_Scored.csv", row.names = FALSE)
