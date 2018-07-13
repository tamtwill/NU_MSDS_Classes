# Tamara Williams extra credit, Abalone
# include required packages

library(ModelMetrics)


#####
# Set working directory
#####

setwd("~/NorthwesternU_MSPA/Classes/Generalized Linear Models - 411/Bonus Assignments/Abalone/")
df=read.csv("zip_abalone-1.csv",header=T, stringsAsFactors = TRUE)

summary(df)
# no missing data, so we are OK there

# handle the factor
df$Sex <- as.factor(df$Sex)

boxplot(df$TARGET_RINGS)
hist(df$TARGET_RINGS)
plot(df$Sex)
plot(df$Sex, df$TARGET_RINGS)
df_infant <- subset(df, df$Sex == "I")
df_adult <- subset(df, df$Sex != "I")

hist(df_infant$TARGET_RINGS)
hist(df_adult$TARGET_RINGS)

# # Logistic regression, is infant or not, drop target rings so can use with test data
df_tmp <- subset(df, select= -c(TARGET_RINGS))
lr_model <- glm(Sex ~ ., data = df_tmp, family = binomial())
summary(lr_model)
df$IsAdult <- predict(lr_model, type = "response")

# Linear regression model for Rings on Adults, since they
# don't look like a Possion distribution
lr_rings_A <- glm(TARGET_RINGS ~ ., data = df, family = gaussian())
summary(lr_rings_A)
df$AdultPrediction <- predict(lr_rings_A, type = "response")

# Possion regression for the infants
lr_rings_I <- glm(TARGET_RINGS ~ ., data = df, family = poisson())
summary(lr_rings_I)
df$InfantPrediction <- predict(lr_rings_I, type = "response")

 l_df <- dim(df)[1]
 for (i in 1:l_df){
   if (df$IsAdult[i] > .50){
     df$P_TARGET_RINGS[i] <- df$AdultPrediction[i]
   }else{ df$P_TARGET_RINGS[i]<- df$InfantPrediction[i]}
   i=i+1
 }


mse(df$TARGET_RINGS, df$P_TARGET_RINGS)

###################################################################
# Scoring 
###################################################################

df_test=read.csv("zip_abalone_test-1.csv",header=T, stringsAsFactors = TRUE)
df_test$Sex <- as.factor(df_test$Sex)

df_test$IsAdult <- predict(lr_model, newdata=df_test, type = "response")
df_test$AdultPrediction <- predict(lr_rings_A, newdata = df_test, type = "response")
df_test$InfantPrediction <- predict(lr_rings_I, newdata = df_test, type = "response")

l_df_test <- dim(df_test)[1]

for (i in 1:l_df_test){
  if (df_test$IsAdult[i] > .75){
    df_test$P_TARGET_RINGS[i] <- df_test$AdultPrediction[i]
  }else{ df_test$P_TARGET_RINGS[i]<- df_test$InfantPrediction[i]}
  i=i+1
}

#subset of data set for the deliverable "Scored data file"
tw_solution <- df_test[c("INDEX","P_TARGET_RINGS")]

#Write prediction File 
write.csv(tw_solution, "tamara_williams_abalone_scored.csv", row.names = FALSE)
