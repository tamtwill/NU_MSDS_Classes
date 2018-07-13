######## Predict 411, Unit 2
######## Submitted by: Tamara Williams
######## Full code - all experimentation, and sample testing
######## The section for running test code is delimited by +++++++++++++++
######## Bingo-bonus logistic regression model code at the very end, look for the *****
######## Bingo-bonus decision tree used for varible selection in the main body of program.  
######## See lines 228 - 253 for code and output of feature importance


# For the sake of good programming hygiene, start clean
# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Set working directory
#------------------------------------------
setwd("/Users/tamtwill/NorthwesternU_MSPA/Classes/Generalized Linear Models - 411/Unit 02/Assignment2")


# include required packages
#---------------------------
library(car)
library(caret)
library(corrplot)
library(leaps)
library(MASS)
library(glmnet)
library(rockchalk)
library(ggplot2)
library(reshape2)
library(VIM)
library(mice)
library(randomForest)
library(InformationValue)
library(mctest)
library(ROCR)
library(pROC)

########## PART 1
########## DATA PREP
################################################################################

# Read in the data
#------------------------------------------
clean_df=read.csv("logit_insurance.csv",header=T, stringsAsFactors = T)
summary(clean_df)

working_df <- clean_df
# get the numeric data, proces those pesky factors, so we can take a look at correlations more easily
working_df$TARGET_FLAG <- as.factor(working_df$TARGET_FLAG)
working_df$SEX <- as.factor(working_df$SEX)
working_df$EDUCATION <- as.factor(working_df$EDUCATION)
working_df$PARENT1 <- as.factor(working_df$PARENT1)
working_df$MSTATUS <- as.factor(working_df$MSTATUS)
working_df$REVOKED <- as.factor(working_df$REVOKED)
working_df$RED_CAR <- as.factor(ifelse(working_df$RED_CAR=="yes", 1, 0))
working_df$URBANICITY <- ifelse(working_df$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
working_df$URBANICITY <- as.factor(working_df$URBANICITY)
working_df$JOB <- as.factor(working_df$JOB)
working_df$CAR_USE <- as.factor(working_df$CAR_USE)
working_df$CAR_TYPE <- as.factor(working_df$CAR_TYPE)
working_df$JOB <- sub("^$", "Other", working_df$JOB)
working_df$JOB <- as.factor(working_df$JOB)

#deal with those dollar signs
working_df$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", working_df$INCOME)))
working_df$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", working_df$HOME_VAL)))
working_df$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", working_df$OLDCLAIM)))
working_df$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", working_df$BLUEBOOK)))
summary(working_df)

# use VIM to look at the missing data
#-------------------------------------------------------
#matrixplot(working_df, interactive = F, sortby = "INDEX")
aggr_plot <- aggr(working_df, col=c('cadetblue3','brown1'), numbers=TRUE, sortVars=TRUE, labels=names(working_df), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


# add impute flags to new dataframe in case they are useful
#-------------------------------------------------------
imp_df <-working_df
tmp <- as.data.frame(lapply(imp_df[-1], function(x) as.numeric(is.na(x))))
flagged <- (colSums(tmp, na.rm=T) != 0)
tmp <- tmp[, flagged]
colnames(tmp) <- paste("FLAG", colnames(tmp), sep = "_")
  
# Cbind theimputed columns to the original dataframe
# use mean to impute missing values
imp_df <- cbind(imp_df, tmp)

# Histograms for Numeric Variables
#-------------------------------------------------------
plot(working_df$TARGET_FLAG, col = "lightblue1", xlab = "Accident", main = "Accident or not")

par(mfrow=c(2,2))
data0<- subset(imp_df, TARGET_FLAG == 1 )
hist(log(data0$TARGET_AMT), col = "cyan2", xlab = "Log TARGET_AMT", main = "Log TARGET_AMT Hist")
hist(log(imp_df$HOME_VAL), col = "lightblue1", xlab = "Log HOME_VAL", main = "Log HOME_VAL Hist")
hist(sqrt(imp_df$TRAVTIME), col = "lightseagreen", xlab = "SQRT TRAVTIME", main = "SQRT TRAVTIME Hist")
hist(sqrt(imp_df$BLUEBOOK), col = "deepskyblue4", xlab = "SQRT BLUEBOOK", main = "SQRT BLUEBOOK Hist")

par(mfrow=c(1,1))
rm(data0)
gc()

df_flag1<- subset(imp_df, TARGET_FLAG == 1 )
hist(log(df_flag1$OLDCLAIM), col = "deepskyblue4", xlab = "Log OLDCLAIM", main = "Log OLDCLAIM Hist")



# do some garbage collection for the sake of working memory
rm(df_flag1)
gc()

########## PART 2
########## DATA TRANSFORMS
################################################################################

# Bin Income
imp_df$INCOME_bin[is.na(imp_df$INCOME)] <- "NA"
imp_df$INCOME_bin[imp_df$INCOME == 0] <- "Zero"
imp_df$INCOME_bin[imp_df$INCOME >= 1 & imp_df$INCOME < 30000] <- "Low"
imp_df$INCOME_bin[imp_df$INCOME >= 30000 & imp_df$INCOME < 80000] <- "Medium"
imp_df$INCOME_bin[imp_df$INCOME >= 80000] <- "High"
imp_df$INCOME_bin <- factor(imp_df$INCOME_bin)
imp_df$INCOME_bin <- factor(imp_df$INCOME_bin, levels=c("NA","Zero","Low","Medium","High"))


imp_df$TRAVTIME[is.na(imp_df$TRAVTIME)] <-mean (imp_df$TRAVTIME, na.rm=TRUE)
imp_df$OLDCLAIM[is.na(imp_df$OLDCLAIM)] <-mean (imp_df$OLDCLAIM, na.rm=TRUE)
imp_df$BLUEBOOK [is.na(imp_df$BLUEBOOK)] <-mean (imp_df$BLUEBOOK, na.rm=TRUE)
imp_df$HOME_VAL[is.na(imp_df$HOME_VAL)] <-mean (imp_df$HOME_VAL, na.rm=TRUE)
imp_df$INCOME[is.na(imp_df$INCOME)] <-mean (imp_df$INCOME, na.rm=TRUE)
imp_df$CAR_AGE[is.na(imp_df$CAR_AGE)] <-mean(imp_df$CAR_AGE, na.rm=TRUE)
imp_df$AGE[is.na(imp_df$AGE)] <-mean (imp_df$AGE, na.rm=TRUE)
imp_df$YOJ[is.na(imp_df$YOJ)] <-mean (imp_df$YOJ, na.rm=TRUE)
imp_df$TIF[is.na(imp_df$TIF)] <-mean (imp_df$TIF, na.rm=TRUE)
imp_df$CLM_FREQ[is.na(imp_df$CLM_FREQ)] <-mean (imp_df$CLM_FREQ, na.rm=TRUE)
imp_df$MVR_PTS[is.na(imp_df$MVR_PTS)] <-mean (imp_df$MVR_PTS, na.rm=TRUE)
imp_df$HOMEKIDS[is.na(imp_df$HOMEKIDS)] <-mean (imp_df$HOMEKIDS, na.rm=TRUE)

summary(imp_df)

imp_df$CAR_AGE[imp_df$CAR_AGE < 0 ] <- 0 
imp_df$HOME_OWNER <- ifelse(imp_df$HOME_VAL == 0, 0, 1)
imp_df$SQRT_TRAVTIME <- sqrt(imp_df$TRAVTIME)
imp_df$SQRT_BLUEBOOK <- sqrt(imp_df$BLUEBOOK)
imp_df$LOG_HOME_VAL <- log(imp_df$HOME_VAL+1)
imp_df$LOG_OLDCLAIM <- log(imp_df$OLDCLAIM+1)


# correlation plot of the numeric variables
corr_df <- subset(imp_df, select = c(TARGET_AMT, AGE, HOMEKIDS, YOJ, INCOME, HOME_VAL, TRAVTIME, BLUEBOOK, TIF,
                                     CLM_FREQ, MVR_PTS, CAR_AGE), na.rm = TRUE)
c <- cor(corr_df)
corrplot(c, method = "square")


# do some garbage collection for the sake of working memory
rm(aggr_plot)
rm(tmp)
rm(corr_df)
rm(c)
gc()

sparse_df <- subset(imp_df, select = -c(SEX, EDUCATION, PARENT1,MSTATUS,REVOKED,URBANICITY,JOB,CAR_USE, CAR_TYPE, INCOME_bin))
tmp <- model.matrix( ~ SEX - 1, data=imp_df)
sparse_df <- cbind(sparse_df, tmp)
tmp <- model.matrix( ~ EDUCATION - 1, data=imp_df)
sparse_df <- cbind(sparse_df, tmp)
tmp <- model.matrix( ~ PARENT1 - 1, data=imp_df)
sparse_df <- cbind(sparse_df, tmp)
tmp <- model.matrix( ~ MSTATUS - 1, data=imp_df)
sparse_df <- cbind(sparse_df, tmp)
tmp <- model.matrix( ~ REVOKED - 1, data=imp_df)
sparse_df <- cbind(sparse_df, tmp)
tmp <- model.matrix( ~ RED_CAR - 1, data=imp_df)
sparse_df <- cbind(sparse_df, tmp)
tmp <- model.matrix( ~ URBANICITY - 1, data=imp_df)
sparse_df <- cbind(sparse_df, tmp)
tmp <- model.matrix( ~ JOB - 1, data=imp_df)
sparse_df <- cbind(sparse_df, tmp)
tmp <- model.matrix( ~ CAR_USE - 1, data=imp_df)
sparse_df <- cbind(sparse_df, tmp)
tmp <- model.matrix( ~ CAR_TYPE - 1, data=imp_df)
sparse_df <- cbind(sparse_df, tmp)
tmp <- model.matrix( ~ INCOME_bin - 1, data=imp_df)
sparse_df <- cbind(sparse_df, tmp)
colnames(sparse_df) <- gsub(" ","_",colnames(sparse_df))
colnames(sparse_df) <- gsub("<","_LT_",colnames(sparse_df))

tmp_df <- subset(sparse_df, select = -c(RED_CAR) )

findLinearCombos(tmp_df)

short_sparse <- subset(sparse_df, select = -c(33,35,37,39,41,43,52,54,60,61,65))

# get the accidents
df_flag1 <- subset(imp_df, TARGET_FLAG == 1 )
targetbycar <- aggregate(df_flag1$TARGET_AMT, list(df_flag1$CAR_TYPE), mean)

rm(tmp)
rm(tmp_df)
rm(df_flag1)
gc()

# look at feature importance
#---------------------------------------------
# look without INDEX which should not be predictive
# and without the secondary target of TARGET_AMT
tmp_df <- subset(imp_df, select = -c(TARGET_AMT, INDEX))
fit_rf2<-randomForest(factor(tmp_df$TARGET_FLAG)~., data=tmp_df)
varImpPlot(fit_rf2,type=2)
varImp(fit_rf2)
features2 <- varImp(fit_rf2,type=2)
rownames(features2)[order(features2$Overall, decreasing=TRUE)]

# and for the sparse version
tmp_df <- subset(sparse_df, select = -c(TARGET_AMT, INDEX))
fit_rf3<-randomForest(factor(tmp_df$TARGET_FLAG)~., data=tmp_df)
varImpPlot(fit_rf3,type=2)
varImp(fit_rf3)
features3 <- varImp(fit_rf3,type=2)
rownames(features3)[order(features3$Overall, decreasing=TRUE)]

# and for the sparse version after reducing based on linear combos
tmp_df <- subset(short_sparse, select = -c(TARGET_AMT, INDEX))
fit_rf4<-randomForest(factor(tmp_df$TARGET_FLAG)~., data=tmp_df)
varImpPlot(fit_rf4,type=2)
varImp(fit_rf4)
features4 <- varImp(fit_rf4,type=2)
rownames(features4)[order(features4$Overall, decreasing=TRUE)]


# more garbage collection
rm(tmp_df)
rm(fit_rf2)
rm(fit_rf3)
rm(features2)
rm(features3)
rm(features4)
gc()

########## PART 3
########## MODEL BUILDING
################################################################################
#Model Development for TARGET_FLAG using Unit 02 sample code as a jump start

# using the sparse matrix version of the data
Model1 <- glm(TARGET_FLAG ~ .,data = sparse_df, family = binomial(),control = list(maxit = 50))
summary(Model1)
sparse_df$Model1Prediction <- predict(Model1, type = "response")

Model7 <-glm(TARGET_FLAG ~ INCOME + SQRT_BLUEBOOK + BLUEBOOK + AGE + TRAVTIME +                
               SQRT_TRAVTIME + OLDCLAIM + HOME_VAL + LOG_HOME_VAL + LOG_OLDCLAIM +            
               MVR_PTS + CAR_AGE + YOJ + TIF + CLM_FREQ + URBANICITYRural + 
               HOMEKIDS + KIDSDRIV + CAR_TYPEMinivan + CAR_USECommercial + REVOKEDNo +              
               JOBManager + PARENT1No + MSTATUSYes + EDUCATIONBachelors + HOME_OWNER +                
               SEXM + CAR_TYPESports_Car + INCOME_binMedium + CAR_TYPEPickup +          
               JOBClerical + EDUCATION_LT_High_School + EDUCATIONMasters +        
               JOBProfessional + INCOME_binLow + CAR_TYPEVan + FLAG_CAR_AGE + FLAG_YOJ,                
               data = sparse_df, family = binomial())
summary(Model7)
sparse_df$Model7Prediction <- predict(Model7, type = "response")

# using the reduced sparse model
Model8 <-glm(TARGET_FLAG ~ ., data = short_sparse, family = binomial(),control = list(maxit = 50))
summary(Model8)
short_sparse$Model8Prediction <- predict(Model8, type = "response")

Model9 <-glm(TARGET_FLAG ~ SQRT_BLUEBOOK + BLUEBOOK + INCOME + AGE + TRAVTIME +                
               SQRT_TRAVTIME + OLDCLAIM + HOME_VAL + LOG_HOME_VAL + MVR_PTS +                 
               LOG_OLDCLAIM + CAR_AGE + URBANICITYUrban + YOJ + TIF +                     
               CLM_FREQ + HOMEKIDS + CAR_USEPrivate + REVOKEDYes + KIDSDRIV +                
               CAR_TYPEMinivan + PARENT1Yes + JOBz_Blue_Collar + MSTATUSz_No + EDUCATIONz_High_School, 
               data = short_sparse, family = binomial(), control = list(maxit=100))
summary(Model9)
short_sparse$Model9Prediction <- predict(Model9, type = "response")
vif(Model9)

Model10 <-glm(TARGET_FLAG ~ SQRT_BLUEBOOK + BLUEBOOK + INCOME + AGE + TRAVTIME +                
                SQRT_TRAVTIME + OLDCLAIM + HOME_VAL + LOG_HOME_VAL + MVR_PTS +                 
                LOG_OLDCLAIM + CAR_AGE + URBANICITYUrban + YOJ + TIF +                     
                CLM_FREQ + HOMEKIDS + CAR_USEPrivate + REVOKEDYes + KIDSDRIV +                
                CAR_TYPEMinivan + PARENT1Yes + JOBz_Blue_Collar + MSTATUSz_No + EDUCATIONz_High_School +  
                JOBManager + RED_CAR1 + SEXM + EDUCATIONBachelors + SEXz_F +                  
                CAR_TYPESports_Car + HOME_OWNER + CAR_TYPEPickup + JOBClerical + INCOME_binHigh +          
                EDUCATION_LT_High_School + EDUCATIONMasters + JOBProfessional + INCOME_binLow + RED_CAR +                 
              FLAG_CAR_AGE + FLAG_YOJ + FLAG_INCOME + JOBLawyer + INCOME_binZero +          
              JOBHome_Maker + CAR_TYPEPanel_Truck + JOBOther + FLAG_HOME_VAL + JOBDoctor +               
              FLAG_AGE + INCOME_binNA, data = short_sparse, family = binomial())
summary(Model10)
short_sparse$Model10Prediction <- predict(Model10, type = "response")
vif(Model10)



# using the imputed flagged data
Model2 <- glm(TARGET_FLAG ~ ., data = imp_df, family = binomial(), control = list(maxit = 50))
summary(Model2)
imp_df$Model2Prediction <- predict(Model2, type = "response")

Model3 <- glm(TARGET_FLAG ~ SQRT_BLUEBOOK + SQRT_TRAVTIME + JOB + OLDCLAIM + MVR_PTS + LOG_OLDCLAIM +
                TIF + CAR_TYPE + EDUCATION + URBANICITY + CLM_FREQ + HOMEKIDS + KIDSDRIV + REVOKED,
              data = imp_df, family = binomial())
summary(Model3)
imp_df$Model3Prediction <- predict(Model3, type = "response")


Model4 <-glm(TARGET_FLAG ~ BLUEBOOK + SQRT_BLUEBOOK + TRAVTIME + SQRT_TRAVTIME + JOB + OLDCLAIM + MVR_PTS + LOG_OLDCLAIM +
               TIF + CAR_TYPE + EDUCATION + URBANICITY + CLM_FREQ + HOMEKIDS + KIDSDRIV + REVOKED +
               CAR_USE + MSTATUS + PARENT1 + RED_CAR + SEX + FLAG_CAR_AGE + FLAG_YOJ + FLAG_INCOME +
               FLAG_HOME_VAL + FLAG_AGE, data = imp_df, family = binomial())
summary(Model4)
imp_df$Model4Prediction <- predict(Model4, type = "response")

Model5 <-glm(TARGET_FLAG ~ SQRT_BLUEBOOK + SQRT_TRAVTIME + JOB + OLDCLAIM +MVR_PTS + LOG_OLDCLAIM + TIF + 
               CAR_TYPE + EDUCATION + URBANICITY + CLM_FREQ + HOMEKIDS + KIDSDRIV + REVOKED + CAR_USE, 
               data = imp_df, family = binomial())
summary(Model5)
imp_df$Model5Prediction <- predict(Model5, type = "response")


Model6 <-glm(TARGET_FLAG ~ SQRT_BLUEBOOK + SQRT_TRAVTIME + AGE + LOG_HOME_VAL + LOG_OLDCLAIM + JOB + 
               MVR_PTS + CAR_AGE + YOJ + CAR_TYPE + TIF + URBANICITY + CLM_FREQ + EDUCATION + 
               HOMEKIDS + INCOME_bin + KIDSDRIV + CAR_USE + REVOKED + MSTATUS + PARENT1 + 
               HOME_OWNER + RED_CAR + SEX, data = imp_df, family = binomial())
summary(Model6)
imp_df$Model6Prediction <- predict(Model6, type = "response")
vif(Model6)




########## PART 4
########## MODEL SELECTION
################################################################################ 
print("Models 1, 2 and 8 do not converge")

AIC_list <- c("N/A", "N/A", AIC(Model3), AIC(Model4), AIC(Model5), AIC(Model6), AIC(Model7),
              "N/A", AIC(Model9), AIC(Model10))
BIC_list <- c("N/A", "N/A", BIC(Model3), BIC(Model4), BIC(Model5), BIC(Model6), BIC(Model7),
              "N/A", BIC(Model9), BIC(Model10))

AIC_list
which.min(AIC_list)

BIC_list
which.min(BIC_list)

ll_list <- c(9999,9999,-2*logLik(Model3), -2*logLik(Model4),-2*logLik(Model5),-2*logLik(Model6),
                  -2*logLik(Model7),9999, -2*logLik(Model9), -2*logLik(Model10))
ll_list
which.min(ll_list)

#ks_stat(actuals=sparse_df$TARGET_FLAG, predictedScores=sparse_df$Model1Prediction)
ks_res <- list(c(0))
#ks_stat(actuals=imp_df$TARGET_FLAG, predictedScores=imp_df$Model2Prediction)
ks_res[2] <- 0
ks_res[3] <-ks_stat(actuals=imp_df$TARGET_FLAG, predictedScores=imp_df$Model3Prediction)
ks_res[4] <-ks_stat(actuals=imp_df$TARGET_FLAG, predictedScores=imp_df$Model4Prediction)
ks_res[5] <-ks_stat(actuals=imp_df$TARGET_FLAG, predictedScores=imp_df$Model5Prediction)
ks_res[6] <-ks_stat(actuals=imp_df$TARGET_FLAG, predictedScores=imp_df$Model6Prediction)
ks_res[7] <-ks_stat(actuals=sparse_df$TARGET_FLAG, predictedScores=sparse_df$Model7Prediction)
#ks_stat(actuals=short_sparse$TARGET_FLAG, predictedScores=short_sparse$Model8Prediction)
ks_res[8] <- 0
ks_res[9] <-ks_stat(actuals=short_sparse$TARGET_FLAG, predictedScores=short_sparse$Model9Prediction)
ks_res[10] <-ks_stat(actuals=short_sparse$TARGET_FLAG, predictedScores=short_sparse$Model10Prediction)
v_ks_res <- unlist(ks_res)
v_ks_res
which.max(v_ks_res)


# get the ROC curves for the top 3 model contenders
make_roc <- function(model_to_use, chart_num){
pred <- prediction(model_to_use,imp_df$TARGET_FLAG)
perf <- performance(pred,'tpr','fpr')
model_auc <- auc(imp_df$TARGET_FLAG, model_to_use)
chart_title <- paste("ROC for Model ", chart_num,"AUC =", model_auc)
print (round(model_auc, digits = 4))
plot(perf, colorize=F, lwd=2,main=chart_title)
}

make_roc(imp_df$Model6Prediction, '6')
make_roc(sparse_df$Model7Prediction, '7')
make_roc(short_sparse$Model10Prediction, '10')

rm(sparse_df)
rm(clean_df)
gc()

# We'll choose Model 6, it's nearly as good as 10 and a lot less complex
# here are its coefficients
coef(Model6)
formula(Model6)


# Visualize results for selected model as final check 
#***************************************************************************************
hist(Model6$residuals, main = 'Histogram of Residuals',  xlab = 'Residuals', ylab = ' ', 
     col = 'cyan2')

qqnorm(Model6$residuals,main = 'QQ Plot of Residuals', xlab = 'Theoretical Quantiles', 
       ylab = 'Sample Quartiles', col = 'cyan2') 
qqline(Model6$residuals, datax = FALSE, distribution = qnorm,  col = 'red')

plot(resid(Model6), ylab="Residuals", xlab="Predicted", main="Target Residual plot") 
abline(h=0, col='red')   




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
########## PART 5
########## MATCH TEST TO TRAIN DATA CHANGES AND SCORE
################################################################################
#### Part 5:  Score Model on Test Data set and output csv file
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

#****************************************************************************************************************
#
#                  BONUS BINGO
#
#****************************************************************************************************************
# trying the champion model with Logistic regression
bonus1 <-glm(TARGET_FLAG ~ SQRT_BLUEBOOK + SQRT_TRAVTIME + AGE + LOG_HOME_VAL + LOG_OLDCLAIM + JOB + 
               MVR_PTS + CAR_AGE + YOJ + CAR_TYPE + TIF + URBANICITY + CLM_FREQ + EDUCATION + 
               HOMEKIDS + INCOME_bin + KIDSDRIV + CAR_USE + REVOKED + MSTATUS + PARENT1 + 
               HOME_OWNER + RED_CAR + SEX, family = binomial(link = "logit"), data = imp_df)
summary(bonus1)
imp_df$bonus1Prediction <- predict(bonus1, type = "response")
vif(bonus1)
AIC(bonus1)
BIC(bonus1)
print(-2*logLik(bonus1))
ks_stat(actuals=imp_df$TARGET_FLAG, predictedScores=imp_df$bonus1Prediction)
make_roc(imp_df$bonus1Prediction,"bonus1")
