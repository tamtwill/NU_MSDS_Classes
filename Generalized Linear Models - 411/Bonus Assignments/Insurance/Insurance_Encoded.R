# Tamara Williams extra credit, Insurance
# include required packages
#---------------------------
library(leaps)
library(MASS)
library(ggplot2)
library(reshape2)
library(car)


#####
# Set working directory
#####

setwd("~/NorthwesternU_MSPA/Classes/Generalized Linear Models - 411/Bonus Assignments/Insurance")
df=read.csv("insurance.csv",header=T, stringsAsFactors = FALSE)

summary(df)


df$SEX<- as.numeric(factor(df$SEX))
df$REVOKED <- as.numeric(factor(df$REVOKED))
df$RED_CAR <- as.numeric(factor(df$RED_CAR))
df$PARENT1 <- as.numeric(factor(df$PARENT1))
df$EDUCATION <- as.numeric(factor(df$EDUCATION))
df$JOB <- as.numeric(factor(df$JOB))
df$CAR_USE <- as.numeric(factor(df$CAR_USE))
df$CAR_TYPE <- as.numeric(factor(df$CAR_TYPE))
df$MSTATUS <- as.numeric(factor(df$MSTATUS))
df$URBANICITY <- as.numeric(factor(df$URBANICITY))

df$HOME_VAL <- as.numeric(gsub('[$,]', '', df$HOME_VAL))
df$BLUEBOOK <- as.numeric(gsub('[$,]', '', df$BLUEBOOK))
df$OLDCLAIM <- as.numeric(gsub('[$,]', '', df$OLDCLAIM))
df$INCOME <- as.numeric(gsub('[$,]', '', df$INCOME))

#fix missing values
df$AGE[is.na(df$AGE)] = mean(df$AGE, na.rm = TRUE)
df$YOJ[is.na(df$YOJ)] = mean(df$YOJ, na.rm = TRUE)
df$CAR_AGE[is.na(df$CAR_AGE)] = mean(df$CAR_AGE, na.rm = TRUE)
df$INCOME[is.na(df$INCOME)] = mean(df$INCOME, na.rm = TRUE)
df$HOME_VAL[is.na(df$HOME_VAL)] = mean(df$HOME_VAL, na.rm = TRUE)
df$SEX[is.na(df$SEX)] = median(df$SEX, na.rm = TRUE)



target <- df$TARGET                          
train <- as.data.frame(df[-2])

# look at correlations to see if the rotten score can improve, code from R-
cormat <- round(cor(df),2)
melted_cormat <- melt(cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()



model1 <- lm(target~ KIDSDRIV+AGE+HOMEKIDS+YOJ+INCOME+PARENT1+HOME_VAL+MSTATUS+SEX+ 
               EDUCATION+JOB+TRAVTIME+CAR_USE+BLUEBOOK+TIF+CAR_TYPE+RED_CAR+OLDCLAIM+CLM_FREQ+REVOKED+
               MVR_PTS+CAR_AGE+URBANICITY, data = train)
summary(model1)
AIC(model1)
vif(model1)

model2 <- lm(target~BLUEBOOK+MSTATUS+SEX, data = train)
summary(model2)
AIC(model2)
vif(model2)

model3 <- lm(target~ BLUEBOOK+REVOKED+CAR_AGE+URBANICITY, data = train)
summary(model3)
AIC(model3)
vif(model3)


# using the test data
df_tst=read.csv("insurance_test.csv",header=T, stringsAsFactors = FALSE)

df_tst$SEX<- as.numeric(factor(df_tst$SEX))
df_tst$REVOKED <- as.numeric(factor(df_tst$REVOKED))
df_tst$RED_CAR <- as.numeric(factor(df_tst$RED_CAR))
df_tst$PARENT1 <- as.numeric(factor(df_tst$PARENT1))
df_tst$EDUCATION <- as.numeric(factor(df_tst$EDUCATION))
df_tst$JOB <- as.numeric(factor(df_tst$JOB))
df_tst$CAR_USE <- as.numeric(factor(df_tst$CAR_USE))
df_tst$CAR_TYPE <- as.numeric(factor(df_tst$CAR_TYPE))
df_tst$MSTATUS <- as.numeric(factor(df_tst$MSTATUS))
df_tst$URBANICITY <- as.numeric(factor(df_tst$URBANICITY))

df_tst$HOME_VAL <- as.numeric(gsub('[$,]', '', df_tst$HOME_VAL))
df_tst$BLUEBOOK <- as.numeric(gsub('[$,]', '', df_tst$BLUEBOOK))
df_tst$OLDCLAIM <- as.numeric(gsub('[$,]', '', df_tst$OLDCLAIM))
df_tst$INCOME <- as.numeric(gsub('[$,]', '', df_tst$INCOME))

#fix missing values
df_tst$AGE[is.na(df_tst$AGE)] = mean(df$AGE, na.rm = TRUE)
df_tst$YOJ[is.na(df_tst$YOJ)] = mean(df$YOJ, na.rm = TRUE)
df_tst$CAR_AGE[is.na(df_tst$CAR_AGE)] = mean(df$CAR_AGE, na.rm = TRUE)
df_tst$BLUEBOOK[is.na(df_tst$CAR_AGE)] = mean(df$BLUEBOOK, na.rm = TRUE)
df_tst$SEX[is.na(df_tst$CAR_AGE)] = median(df$SEX, na.rm = TRUE)
df_tst$INCOME[is.na(df_tst$INCOME)] = mean(df_tst$INCOME, na.rm = TRUE)
df_tst$HOME_VAL[is.na(df_tst$HOME_VAL)] = mean(df$HOME_VAL, na.rm = TRUE)
df_tst$SEX[is.na(df_tst$SEX)] = median(df$SEX, na.rm = TRUE)

p_target <- predict(model2, newdata = df_tst)
INDEX <- df_tst$INDEX
output <- cbind(df_tst$INDEX, p_target)
colnames(output)<-c("INDEX", "P_TARGET")
write.csv(output, "insurance_out.csv", row.names = FALSE)
