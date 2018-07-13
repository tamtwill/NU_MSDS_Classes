######## Predict 411, Unit 3
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
setwd("/Users/tamtwill/NorthwesternU_MSPA/Classes/Generalized Linear Models - 411/Unit 03/Williams_Assignment3")

# include required packages
#---------------------------
library(caret)
library(glmnet)
library(MASS)
library(VIM)
library(corrplot)
library(randomForest)
library(InformationValue)
library(pscl)


# Read in the data
#------------------------------------------
train_df=read.csv("wine_training.csv",header=T)

# let's take a look at the data
#------------------------------------------
summary(train_df)
var(train_df$TARGET)
mean(train_df$TARGET)
# variance is bigger than mean, so situation may need negitive binomial solution

# use VIM to look at the missing data
#-------------------------------------------------------
#matrixplot(train_df, interactive = F, sortby = "INDEX")
aggr_plot <- aggr(train_df, col=c('cadetblue3','brown1'), numbers=TRUE, sortVars=TRUE, labels=names(train_df), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#create IMP versions of each independent variable
train_df$FixedAcidity_IMP <- train_df$FixedAcidity
train_df$VolatileAcidity_IMP <- train_df$VolatileAcidity
train_df$CitricAcid_IMP <- train_df$CitricAcid
train_df$ResidualSugar_IMP <- train_df$ResidualSugar
train_df$Chlorides_IMP <- train_df$Chlorides
train_df$FreeSulfurDioxide_IMP <- train_df$FreeSulfurDioxide
train_df$TotalSulfurDioxide_IMP <- train_df$TotalSulfurDioxide
train_df$Density_IMP <- train_df$Density
train_df$pH_IMP <- train_df$pH
train_df$Sulphates_IMP <- train_df$Sulphates
train_df$Alcohol_IMP <- train_df$Alcohol
train_df$LabelAppeal_IMP <- train_df$LabelAppeal
train_df$AcidIndex_IMP <- train_df$AcidIndex
train_df$STARS_IMP <- train_df$STARS

#replace NA's in each column with median
train_df$FixedAcidity_IMP[which(is.na(train_df$FixedAcidity_IMP))] <- mean(train_df$FixedAcidity_IMP,na.rm = TRUE)
train_df$VolatileAcidity_IMP[which(is.na(train_df$VolatileAcidity_IMP))] <- mean(train_df$VolatileAcidity_IMP,na.rm = TRUE)
train_df$CitricAcid_IMP[which(is.na(train_df$CitricAcid_IMP))] <- mean(train_df$CitricAcid_IMP,na.rm = TRUE)
train_df$ResidualSugar_IMP[which(is.na(train_df$ResidualSugar_IMP))] <- mean(train_df$ResidualSugar_IMP,na.rm = TRUE)
train_df$Chlorides_IMP[which(is.na(train_df$Chlorides_IMP))] <- mean(train_df$Chlorides_IMP,na.rm = TRUE)
train_df$FreeSulfurDioxide_IMP[which(is.na(train_df$FreeSulfurDioxide_IMP))] <- mean(train_df$FreeSulfurDioxide_IMP,na.rm = TRUE)
train_df$TotalSulfurDioxide_IMP[which(is.na(train_df$TotalSulfurDioxide_IMP))] <- mean(train_df$TotalSulfurDioxide_IMP,na.rm = TRUE)
train_df$Density_IMP[which(is.na(train_df$Density_IMP))] <- mean(train_df$Density_IMP,na.rm = TRUE)
train_df$pH_IMP[which(is.na(train_df$pH_IMP))] <- mean(train_df$pH_IMP,na.rm = TRUE)
train_df$Sulphates_IMP[which(is.na(train_df$Sulphates_IMP))] <- mean(train_df$Sulphates_IMP,na.rm = TRUE)
train_df$Alcohol_IMP[which(is.na(train_df$Alcohol_IMP))] <- mean(train_df$Alcohol_IMP,na.rm = TRUE)
train_df$LabelAppeal_IMP[which(is.na(train_df$LabelAppeal_IMP))] <- mean(train_df$LabelAppeal_IMP,na.rm = TRUE)
train_df$AcidIndex_IMP[which(is.na(train_df$AcidIndex_IMP))] <- mean(train_df$AcidIndex_IMP,na.rm = TRUE)
train_df$STARS_IMP[which(is.na(train_df$STARS_IMP))] <- mean(train_df$STARS_IMP,na.rm = TRUE)

#flag NA values with new field
#first, create new field
#second, replace NA's with 1 else 0

train_df$ResidualSugar_IMP_Flag <- train_df$ResidualSugar
train_df$Chlorides_IMP_Flag <- train_df$Chlorides
train_df$FreeSulfurDioxide_IMP_Flag <- train_df$FreeSulfurDioxide
train_df$TotalSulfurDioxide_IMP_Flag <- train_df$TotalSulfurDioxide
train_df$pH_IMP_Flag <- train_df$pH
train_df$Sulphates_IMP_Flag <- train_df$Sulphates
train_df$Alcohol_IMP_Flag <- train_df$Alcohol
train_df$STARS_IMP_Flag <- train_df$STARS


#NEW BINARY FIELDS TO SHOW na's
train_df$ResidualSugar_IMP_Flag <- ifelse(is.na(train_df$ResidualSugar_IMP_Flag)==TRUE, 1,0) 
train_df$Chlorides_IMP_Flag <- ifelse(is.na(train_df$Chlorides_IMP_Flag)==TRUE, 1,0)
train_df$FreeSulfurDioxide_IMP_Flag <- ifelse(is.na(train_df$FreeSulfurDioxide_IMP_Flag)==TRUE, 1,0)
train_df$TotalSulfurDioxide_IMP_Flag <- ifelse(is.na(train_df$TotalSulfurDioxide_IMP_Flag)==TRUE, 1,0)
train_df$pH_IMP_Flag <- ifelse(is.na(train_df$pH_IMP_Flag)==TRUE, 1,0)
train_df$Sulphates_IMP_Flag <- ifelse(is.na(train_df$Sulphates_IMP_Flag)==TRUE, 1,0)
train_df$Alcohol_IMP_Flag <- ifelse(is.na(train_df$Alcohol_IMP_Flag)==TRUE, 1,0)
train_df$STARS_IMP_Flag <- ifelse(is.na(train_df$STARS_IMP_Flag)==TRUE, 1,0) #LOOK FOR MISSING STAR OBSERVATIONS

#######################################################
# visualize the data
#-------------------------------------------------------
par(mfrow=c(1,2))
hist(train_df$TARGET, col = "#A71930", xlab = "TARGET", main = "Histogram of Cases Sold")
boxplot(train_df$TARGET, col = "#A71930", main = "Boxplot of Cases Sold")
par(mfrow = c(1,1))

par(mfrow=c(2,2))
hist(train_df$FixedAcidity, col = "cyan3", xlab = "FixedAcidity", main = "Histogram FixedAcidity")
boxplot(train_df$FixedAcidity, col = "cyan3", main = "Boxplot of FixedAcidity")
hist(train_df$VolatileAcidity, col = "chartreuse3", xlab = "VolatileAcidity", main = "Histogram FixedAcidity")
boxplot(train_df$VolatileAcidity, col = "chartreuse3", main = "Boxplot of VolatileAcidity")
par(mfrow = c(1,1))

par(mfrow=c(2,2))
hist(train_df$CitricAcid, col = "cornflowerblue", xlab = "CitricAcid", main = "Histogram CitricAcid")
boxplot(train_df$CitricAcid, col = "cornflowerblue", main = "Boxplot of CitricAcid")
hist(train_df$Density, col = "aquamarine4", xlab = "Density", main = "Histogram Density")
boxplot(train_df$Density, col = "aquamarine4", main = "Boxplot of Density")
par(mfrow = c(1,1))

par(mfrow=c(2,2))
hist(train_df$LabelAppeal, col = "cyan3", xlab = "LabelAppeal", main = "Histogram LabelAppeal")
boxplot(train_df$LabelAppeal, col = "cyan3", main = "Boxplot of LabelAppeal")
hist(train_df$AcidIndex, col = "chartreuse3", xlab = "AcidIndex", main = "Histogram AcidIndex")  
boxplot(train_df$AcidIndex, col = "chartreuse3", main = "Boxplot of AcidIndex")
par(mfrow = c(1,1))

par(mfrow=c(2,2))
hist(train_df$ResidualSugar, col = "cornflowerblue", xlab = "ResidualSugar", main = "Histogram ResidualSugar")
boxplot(train_df$ResidualSugar, col = "cornflowerblue", main = "Boxplot of ResidualSugar")
hist(train_df$Chlorides, col = "aquamarine4", xlab = "Chlorides", main = "Histogram Chlorides")
boxplot(train_df$Chlorides, col = "aquamarine4", main = "Boxplot of Chlorides")
par(mfrow = c(1,1))

par(mfrow=c(2,2))
hist(train_df$FreeSulfurDioxide, col = "cyan3", xlab = "FreeSulfurDioxide", main = "Histogram FreeSulfurDioxide")
boxplot(train_df$FreeSulfurDioxide, col = "cyan3", main = "Boxplot of FreeSulfurDioxide")
hist(train_df$TotalSulfurDioxide, col = "chartreuse3", xlab = "TotalSulfurDioxide", main = "Histogram TotalSulfurDioxide")
boxplot(train_df$TotalSulfurDioxide, col = "chartreuse3", main = "Boxplot of TotalSulfurDioxide")
par(mfrow = c(1,1))

par(mfrow=c(2,2))
hist(train_df$pH, col = "cornflowerblue", xlab = "pH", main = "Histogram pH")
boxplot(train_df$pH, col = "cornflowerblue", main = "Boxplot of pH")
hist(train_df$Sulphates, col = "aquamarine4", xlab = "Sulphates", main = "Histogram Sulphates")
boxplot(train_df$Sulphates, col = "aquamarine4", main = "Boxplot of Sulphates")
par(mfrow = c(1,1))

par(mfrow=c(2,2))
hist(train_df$Alcohol, col = "cyan3", xlab = "Alcohol", main = "Histogram Alcohol")
boxplot(train_df$Alcohol, col = "cyan3", main = "Boxplot of Alcohol")
hist(train_df$STARS, col = "chartreuse3", xlab = "STARS", main = "Histogram STARS")  # skewed a bit
boxplot(train_df$STARS, col = "chartreuse3", main = "Boxplot of STARS")
par(mfrow = c(1,1))

# correlation plot of the numeric variables
complete_col <- colnames(train_df)[complete.cases(t(train_df))]
complete_df <- subset(train_df, select = complete_col)
c <- cor(complete_df)
corrplot(c, method = "square")



# Transformations, do both train now and test later
#-----------------------------------------------------
# Acid Index is skewed, so use log to transform it
train_df$log_AcidIndex <- log(train_df$AcidIndex)

# Finally, convert remaining N/As to zeros
train_df[is.na(train_df)] <- 0

summary(train_df)

# look at feature importance
#---------------------------------------------
fit_rf1<-randomForest(factor(train_df$TARGET)~., data=complete_df)
varImpPlot(fit_rf1,type=2)
varImp(fit_rf1)
features <- varImp(fit_rf1,type=2)
rownames(features)[order(features$Overall, decreasing=TRUE)]

###############################################################################
###############################################################################
# try building some models
###############################################################################
###############################################################################

poisson3 <-glm(TARGET ~ .,data = complete_df, family = poisson)
summary(poisson3)
coefficients(poisson3)
complete_df$poisson3Prediction <- predict(poisson3, type = "response")

lm_model <- lm(TARGET ~ STARS_IMP_Flag + STARS_IMP + Alcohol_IMP + TotalSulfurDioxide_IMP + Chlorides_IMP +              
                 FreeSulfurDioxide_IMP + ResidualSugar_IMP + pH_IMP + INDEX + Sulphates_IMP +              
                 Density_IMP + LabelAppeal_IMP + VolatileAcidity_IMP, data = train_df)
summary(lm_model)
coefficients(lm_model)
train_df$lm_modelPrediction <- predict(lm_model, data=train_df)

lm_model2 <- lm(TARGET ~ STARS_IMP_Flag + STARS_IMP + Alcohol_IMP + TotalSulfurDioxide_IMP + Chlorides_IMP +              
                  FreeSulfurDioxide_IMP + ResidualSugar_IMP + pH_IMP + INDEX + Sulphates_IMP +           
                  LabelAppeal + Density_IMP + Density + LabelAppeal_IMP + VolatileAcidity_IMP +        
                  VolatileAcidity + CitricAcid_IMP + CitricAcid + FixedAcidity + FixedAcidity_IMP, data=train_df)
summary(lm_model2)
coefficients(lm_model2)
train_df$lm_model2Prediction <- predict(lm_model2, data=train_df)

poisson1 <-glm(TARGET ~ STARS_IMP_Flag + STARS_IMP + Alcohol_IMP + TotalSulfurDioxide_IMP + Chlorides_IMP +              
                 FreeSulfurDioxide_IMP + ResidualSugar_IMP + pH_IMP + INDEX + Sulphates_IMP +             
                 Density_IMP + LabelAppeal_IMP + VolatileAcidity_IMP + CitricAcid_IMP + FixedAcidity_IMP +           
                 AcidIndex_IMP + log_AcidIndex + Sulphates_IMP_Flag + FreeSulfurDioxide_IMP_Flag + 
                 ResidualSugar_IMP_Flag + TotalSulfurDioxide_IMP_Flag + Alcohol_IMP_Flag + Chlorides_IMP_Flag + pH_IMP_Flag, 
                 data = train_df, family = poisson)
summary(poisson1)
coefficients(poisson1)
train_df$poisson1Prediction <- predict(poisson1, type = "response")

neg_binom1 <-glm.nb(TARGET ~ STARS_IMP_Flag + STARS_IMP + Alcohol_IMP + TotalSulfurDioxide_IMP + Chlorides_IMP +              
                      FreeSulfurDioxide_IMP + ResidualSugar_IMP + pH_IMP + INDEX + Sulphates_IMP +             
                      Density_IMP + LabelAppeal_IMP + VolatileAcidity_IMP + CitricAcid_IMP + FixedAcidity_IMP +           
                      AcidIndex_IMP + log_AcidIndex + Sulphates_IMP_Flag + FreeSulfurDioxide_IMP_Flag + 
                      ResidualSugar_IMP_Flag + TotalSulfurDioxide_IMP_Flag + Alcohol_IMP_Flag + Chlorides_IMP_Flag + pH_IMP_Flag, data = train_df)
summary(neg_binom1)
train_df$neg_binom1Prediction <- predict(neg_binom1, type = "response")


poisson2 <-glm(TARGET ~ STARS_IMP_Flag + STARS_IMP + Alcohol_IMP + TotalSulfurDioxide_IMP + Chlorides_IMP +              
                 FreeSulfurDioxide_IMP + ResidualSugar_IMP + pH_IMP + INDEX + Sulphates_IMP +             
                 Density_IMP + LabelAppeal_IMP + VolatileAcidity_IMP , data = train_df, family = "poisson"(link="log"))
summary(poisson2)
coefficients(poisson2)
train_df$poisson2Prediction <- predict(poisson2, type = "response")

poisson2A <-glm(TARGET ~ LabelAppeal + TotalSulfurDioxide + Density + Alcohol + Chlorides + VolatileAcidity + 
                 FreeSulfurDioxide + ResidualSugar + pH + CitricAcid + Sulphates + FixedAcidity + 
                 STARS_IMP_Flag, data = train_df, family = "poisson"(link="log"))
summary(poisson2A)
coefficients(poisson2A)
train_df$poisson2APrediction <- predict(poisson2A, type = "response")

poisson2B <-glm(TARGET ~ STARS_IMP_Flag + STARS_IMP + Alcohol_IMP + TotalSulfurDioxide_IMP + Chlorides_IMP +              
                  FreeSulfurDioxide_IMP + ResidualSugar_IMP + pH_IMP + INDEX + Sulphates_IMP +              
                  Density_IMP + LabelAppeal_IMP + VolatileAcidity_IMP + CitricAcid_IMP + FixedAcidity_IMP +           
                  AcidIndex_IMP + log_AcidIndex + Sulphates_IMP_Flag + FreeSulfurDioxide_IMP_Flag + 
                  ResidualSugar_IMP_Flag + TotalSulfurDioxide_IMP_Flag + Alcohol_IMP_Flag + Chlorides_IMP_Flag + pH_IMP_Flag, data = train_df, family = poisson)
summary(poisson2B)
coefficients(poisson2B)
train_df$poisson2BPrediction <- predict(poisson2B, type = "response")

neg_binom2 <-glm.nb(TARGET ~ STARS_IMP_Flag + STARS_IMP + Alcohol_IMP + TotalSulfurDioxide_IMP + Chlorides_IMP +              
                      FreeSulfurDioxide_IMP + ResidualSugar_IMP + pH_IMP + INDEX + Sulphates_IMP +              
                      Density_IMP + LabelAppeal_IMP + VolatileAcidity_IMP, data = train_df)
summary(neg_binom2)
coefficients(neg_binom2)
train_df$neg_binom2Prediction <- predict(neg_binom2, type = "response")

neg_binom2A <-glm.nb(TARGET ~ STARS_IMP_Flag + STARS_IMP + Alcohol_IMP + TotalSulfurDioxide_IMP + Chlorides_IMP +              
                       FreeSulfurDioxide_IMP + ResidualSugar_IMP + pH_IMP + INDEX + Sulphates_IMP +              
                       Density_IMP + LabelAppeal_IMP + VolatileAcidity_IMP + CitricAcid_IMP + FixedAcidity_IMP +           
                       AcidIndex_IMP + log_AcidIndex + Sulphates_IMP_Flag + FreeSulfurDioxide_IMP_Flag + 
                       ResidualSugar_IMP_Flag + TotalSulfurDioxide_IMP_Flag + Alcohol_IMP_Flag + Chlorides_IMP_Flag + 
                       pH_IMP_Flag, data = train_df)
summary(neg_binom2A)
coefficients(neg_binom2A)
train_df$neg_binom2APrediction <- predict(neg_binom2A, type = "response")


zero_inf1 <- zeroinfl(TARGET ~ STARS_IMP_Flag + STARS_IMP + Alcohol_IMP + TotalSulfurDioxide_IMP + Chlorides_IMP + 
                        FreeSulfurDioxide_IMP + pH_IMP + ResidualSugar_IMP + Sulphates_IMP + LabelAppeal_IMP + 
                        Density_IMP + VolatileAcidity_IMP + CitricAcid_IMP + FixedAcidity_IMP, 
                        data=train_df)
summary(zero_inf1)
coefficients(zero_inf1)
train_df$zero_inf1Prediction <- predict(zero_inf1, type = "response")


zero_inf_nb1<-zeroinfl(TARGET ~  LabelAppeal + TotalSulfurDioxide + Density + Alcohol + Chlorides + VolatileAcidity + 
                       FreeSulfurDioxide + ResidualSugar + pH + CitricAcid + Sulphates + FixedAcidity + 
                       STARS_IMP_Flag, data=train_df, dist = "negbin", EM=TRUE)
summary(zero_inf_nb1)
coefficients(zero_inf_nb1)
train_df$zero_inf_nb1Prediction <- predict(zero_inf_nb1, newdata = train_df, type = "response")

zero_inf_nb2<-zeroinfl(TARGET ~  LabelAppeal_IMP + TotalSulfurDioxide_IMP + Density_IMP + Alcohol_IMP + Chlorides_IMP + 
                         VolatileAcidity_IMP + FreeSulfurDioxide_IMP + ResidualSugar_IMP + pH_IMP + CitricAcid_IMP + 
                         Sulphates_IMP + FixedAcidity_IMP + STARS_IMP_Flag, data=train_df, dist = "negbin", EM=TRUE)
summary(zero_inf_nb2)
coefficients(zero_inf_nb2)
train_df$zero_inf_nb2Prediction <- predict(zero_inf_nb2, type = "response")

zero_inf_nb3<-zeroinfl(TARGET ~ STARS_IMP_Flag + STARS_IMP + Alcohol_IMP + TotalSulfurDioxide_IMP + Chlorides_IMP + 
                         FreeSulfurDioxide_IMP + pH_IMP + ResidualSugar_IMP + Sulphates_IMP + LabelAppeal_IMP + 
                         Density_IMP + VolatileAcidity_IMP + CitricAcid_IMP + FixedAcidity_IMP, 
                         data=train_df, dist = "negbin", EM=TRUE)
summary(zero_inf_nb3)
coefficients(zero_inf_nb3)
train_df$zero_inf_nb3Prediction <- predict(zero_inf_nb3, type = "response")

AIC(lm_model)
AIC(lm_model2)
AIC(poisson1)
AIC(neg_binom1)
AIC(poisson2)
AIC(poisson2A)
AIC(poisson3)
AIC(neg_binom2)
AIC(neg_binom2A)
AIC(zero_inf_nb1)
AIC(zero_inf_nb2)
AIC(zero_inf_nb3)

print(-2*logLik(lm_model))
print(-2*logLik(lm_model2))
print(-2*logLik(poisson1))
print(-2*logLik(neg_binom1))
print(-2*logLik(poisson2))
print(-2*logLik(poisson2A))
print(-2*logLik(neg_binom2))
print(-2*logLik(neg_binom2A))
print(-2*logLik(zero_inf_nb1))
print(-2*logLik(zero_inf_nb2))
print(-2*logLik(zero_inf_nb3))


#what type of dispersion does sample have?
mean(train_df$TARGET)
var(train_df$TARGET)


###################################################################################################################
###################################################################################################################
##########                              TEST PORTION OF THE PROGRAM                                      ########## 
###################################################################################################################
###################################################################################################################

#Read File in from your working directory
test_df = read.csv("WINE_TEST.csv")  # read csv file

#call libraries
library(ggplot2)
library(MASS)
library(pscl)
library(dplyr)

test_df$INDEX <- as.factor(test_df$INDEX)
test_dft$TARGET <- as.factor(test_df$TARGET)

#create IMP versions of each independent variable (wine)
test_df$FixedAcidity_IMP <- test_df$FixedAcidity
test_df$VolatileAcidity_IMP <- test_df$VolatileAcidity
test_df$CitricAcid_IMP <- test_df$CitricAcid
test_df$ResidualSugar_IMP <- test_df$ResidualSugar
test_df$Chlorides_IMP <- test_df$Chlorides
test_df$FreeSulfurDioxide_IMP <- test_df$FreeSulfurDioxide
test_df$TotalSulfurDioxide_IMP <- test_df$TotalSulfurDioxide
test_df$Density_IMP <- test_df$Density
test_df$pH_IMP <- test_df$pH
test_df$Sulphates_IMP <- test_df$Sulphates
test_df$Alcohol_IMP <- test_df$Alcohol
test_df$LabelAppeal_IMP <- test_df$LabelAppeal
test_df$AcidIndex_IMP <- test_df$AcidIndex
test_df$STARS_IMP <- test_df$STARS

#replace NA's in each column with mean from training set
test_df$FixedAcidity_IMP[which(is.na(test_df$FixedAcidity_IMP))] <- mean(train_df$FixedAcidity_IMP,na.rm = TRUE)
test_df$VolatileAcidity_IMP[which(is.na(test_df$VolatileAcidity_IMP))] <- mean(train_df$VolatileAcidity_IMP,na.rm = TRUE)
test_df$CitricAcid_IMP[which(is.na(test_df$CitricAcid_IMP))] <- mean(train_df$CitricAcid_IMP,na.rm = TRUE)
test_df$ResidualSugar_IMP[which(is.na(test_df$ResidualSugar_IMP))] <- mean(train_df$ResidualSugar_IMP,na.rm = TRUE)
test_df$Chlorides_IMP[which(is.na(test_df$Chlorides_IMP))] <- mean(train_df$Chlorides_IMP,na.rm = TRUE)
test_df$FreeSulfurDioxide_IMP[which(is.na(test_df$FreeSulfurDioxide_IMP))] <- mean(train_df$FreeSulfurDioxide_IMP,na.rm = TRUE)
test_df$TotalSulfurDioxide_IMP[which(is.na(test_df$TotalSulfurDioxide_IMP))] <- mean(train_df$TotalSulfurDioxide_IMP,na.rm = TRUE)
test_df$Density_IMP[which(is.na(test_df$Density_IMP))] <- mean(train_df$Density_IMP,na.rm = TRUE)
test_df$pH_IMP[which(is.na(test_df$pH_IMP))] <- mean(train_df$pH_IMP,na.rm = TRUE)
test_df$Sulphates_IMP[which(is.na(test_df$Sulphates_IMP))] <- mean(train_df$Sulphates_IMP,na.rm = TRUE)
test_df$Alcohol_IMP[which(is.na(test_df$Alcohol_IMP))] <- mean(train_df$Alcohol_IMP,na.rm = TRUE)
test_df$LabelAppeal_IMP[which(is.na(test_df$LabelAppeal_IMP))] <- mean(train_df$LabelAppeal_IMP,na.rm = TRUE)
test_df$AcidIndex_IMP[which(is.na(test_df$AcidIndex_IMP))] <- mean(train_df$AcidIndex_IMP,na.rm = TRUE)
test_df$STARS_IMP[which(is.na(test_df$STARS_IMP))] <- mean(train_df$STARS_IMP,na.rm = TRUE)

#flag NA values with new field...first, create new field...second, replace NA's with 1 else 0 (test_df)
test_df$ResidualSugar_IMP_Flag <- test_df$ResidualSugar
test_df$Chlorides_IMP_Flag <- test_df$Chlorides
test_df$FreeSulfurDioxide_IMP_Flag <- test_df$FreeSulfurDioxide
test_df$TotalSulfurDioxide_IMP_Flag <- test_df$TotalSulfurDioxide
test_df$pH_IMP_Flag <- test_df$pH
test_df$Sulphates_IMP_Flag <- test_df$Sulphates
test_df$Alcohol_IMP_Flag <- test_df$Alcohol
test_df$STARS_IMP_Flag <- test_df$STARS

#NEW BINARY FIELDS TO SHOW na's (test_df)
test_df$ResidualSugar_IMP_Flag <- ifelse(is.na(test_df$ResidualSugar_IMP_Flag)==TRUE, 1,0) 
test_df$Chlorides_IMP_Flag <- ifelse(is.na(test_df$Chlorides_IMP_Flag)==TRUE, 1,0)
test_df$FreeSulfurDioxide_IMP_Flag <- ifelse(is.na(test_df$FreeSulfurDioxide_IMP_Flag)==TRUE, 1,0)
test_df$TotalSulfurDioxide_IMP_Flag <- ifelse(is.na(test_df$TotalSulfurDioxide_IMP_Flag)==TRUE, 1,0)
test_df$pH_IMP_Flag <- ifelse(is.na(test_df$pH_IMP_Flag)==TRUE, 1,0)
test_df$Sulphates_IMP_Flag <- ifelse(is.na(test_df$Sulphates_IMP_Flag)==TRUE, 1,0)
test_df$Alcohol_IMP_Flag <- ifelse(is.na(test_df$Alcohol_IMP_Flag)==TRUE, 1,0)
test_df$STARS_IMP_Flag <- ifelse(is.na(test_df$STARS_IMP_Flag)==TRUE, 1,0) #LOOK FOR MISSING STAR OBSERVATIONS


# Transformations, do both train now and test later
#-----------------------------------------------------
# Acid Index is skewed, so use log to transform it
test_df$log_AcidIndex <- log(test_df$AcidIndex)

##########################################################################################
##########################################################################################
## SCORE THE TEST DATA WITH THE CHAMPION MODEL ...  zero_inf_nb3               ###########
##########################################################################################
test_df$P_TARGET <- predict(zero_inf_nb3, newdata = test_df)

par(mfrow=c(1,2))
hist(test_df$P_TARGET, col = "#A71930", xlab = "TARGET", main = "Histogram of Cases Sold")
boxplot(test_df$P_TARGET, col = "#A71930", main = "Boxplot of Cases Sold")
par(mfrow = c(1,1))


summary(test_df)

select <- dplyr::select

# Scored Data File
scores <- test_df[c("INDEX","P_TARGET")]
write.csv(scores, file = "Williams_U03_Scored.csv", row.names = FALSE)

##########################################################################################
##########################################################################################
##                              Bonus Bingo                                    ###########
##########################################################################################

## trying a logistic + poisson
train_df$FLAG_TARGET <- ifelse(train_df$TARGET == 0,0,1)
logist <-  glm(FLAG_TARGET ~  STARS_IMP_Flag + STARS_IMP + Alcohol_IMP + TotalSulfurDioxide_IMP + Chlorides_IMP + 
                 FreeSulfurDioxide_IMP + pH_IMP + ResidualSugar_IMP + Sulphates_IMP + LabelAppeal_IMP + 
                 Density_IMP + VolatileAcidity_IMP + CitricAcid_IMP + FixedAcidity_IMP, data=train_df, family = binomial())

logist_pois<-  glm(TARGET ~  STARS_IMP_Flag + STARS_IMP + Alcohol_IMP + TotalSulfurDioxide_IMP + Chlorides_IMP + 
                     FreeSulfurDioxide_IMP + pH_IMP + ResidualSugar_IMP + Sulphates_IMP + LabelAppeal_IMP + 
                     Density_IMP + VolatileAcidity_IMP + CitricAcid_IMP + FixedAcidity_IMP, data=train_df, family = "poisson"(link="log"))

summary(logist_pois)
coefficients(logist_pois)
AIC(logist_pois)

test_df$P_YES <- predict(logist, newdata = test_df, type = "response")
test_df$P_AMT <- predict(logist_pois, newdata=test_df, type='response')
l_df <- dim(test_df)[1]
for (i in 1:l_df){
  if (test_df$P_YES[i] >= .5){test_df$P_TARGET_BONUS[i] <- test_df$P_AMT[i]}else{test_df$P_TARGET_BONUS[i] <- 0}
  i=i+1
}

par(mfrow=c(1,2))
hist(test_df$P_TARGET_BONUS, col = "#A71930", xlab = "TARGET", main = "Histogram of Cases Sold")
boxplot(test_df$P_TARGET_BONUS, col = "#A71930", main = "Boxplot of Cases Sold")
par(mfrow = c(1,1))
summary(test_df)

bonus_scores <- test_df[c("INDEX","P_TARGET_BONUS")]
write.csv(bonus_scores, file = "Williams_U03_bonus.csv", row.names = FALSE)

############################################################################################
## Fitting a decision tree
# clean out the previous predictions
library(rpart)
tmp_df = subset(train_df, select = -c(lm_model2Prediction, poisson1Prediction, neg_binom1Prediction, poisson2Prediction,
          poisson2APrediction, poisson2BPrediction, neg_binom2Prediction, neg_binom2APrediction, zero_inf_nb1Prediction, 
          zero_inf_nb2Prediction,FLAG_TARGET))
fit_bonusRF<-rpart(tmp_df$TARGET~., method = "anova",  data=tmp_df)
summary(fit_bonusRF)
plot(fit_bonusRF)
text(fit_bonusRF)
p <- predict(fit_bonusRF, tmp_df, type = 'matrix')
