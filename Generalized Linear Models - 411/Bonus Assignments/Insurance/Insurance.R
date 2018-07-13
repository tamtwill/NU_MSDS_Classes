# Tamara Williams extra credit, Insurance
# include required packages
#---------------------------
library(readr)
library(pbkrtest)
library(car)
library(leaps)
library(MASS)
library(data.table)
library(ggplot2)
library(reshape2)


#####
# Set working directory
#####

setwd("~/NorthwesternU_MSPA/Classes/Generalized Linear Models - 411/Bonus Assignments/Insurance")
df=read.csv("insurance.csv",header=T, stringsAsFactors = FALSE)

summary(df)

#fix missing values
df$AGE[is.na(df$AGE)] = mean(df$AGE, na.rm = TRUE)
df$YOJ[is.na(df$YOJ)] = mean(df$YOJ, na.rm = TRUE)
df$CAR_AGE[is.na(df$CAR_AGE)] = mean(df$CAR_AGE, na.rm = TRUE)


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
                          
train <- as.data.frame(df)
target <- train$TARGET
train <- train[-2]

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

model2 <- lm(target~BLUEBOOK+MSTATUS+SEX, data = train)
summary(model2)
