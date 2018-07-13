# Code for Data Analysis Project
# NWU Predict 401, Sp2017
# Written by - Tamara Williams

#Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

library(rtf)
library(reshape)
library(ggplot2)
library(gridExtra)
library(e1071)

setwd('~/NorthwesternU_MSPA/Statistics_Predict_401/Data Analysis Assignments/Assignment1')
dfAbalone <- read.csv("abalones.csv", header = T, sep = " ", stringsAsFactors = T)
str(dfAbalone)   ## per assignment spec s/b = 1036 obs of 8 vars

## check against sample in self-check doc
head(dfAbalone)
tail(dfAbalone)

## define the volume and ratio as computed columns within the dataframe
dfAbalone$VOLUME <- dfAbalone$LENGTH * dfAbalone$HEIGHT * dfAbalone$DIAM
dfAbalone$RATIO <- dfAbalone$SHUCK/dfAbalone$VOLUME
write.csv(dfAbalone, file = "myabalone.csv", quote = TRUE, row.names = FALSE)

## check against sample in self-check doc
head(dfAbalone$VOLUME, 10)
head(dfAbalone$RATIO, 10)

summary(dfAbalone)
## makes a file on disk you can open with a tesxt editor and screenshot for semi-nice looking
## without the pain for trying to get the summary into a form Excel/Word like
capture.output(print(summary(dfAbalone), prmsd=TRUE, digits=1), file="out1.txt")

## create table with just sex and age, then add the marginal sums
tabAbalone <- addmargins(table(dfAbalone$SEX, dfAbalone$CLASS))
#tabAbalone <-addmargins(tabAbalone, FUN = sum)
tabAbalone

## generate a bar chart of the tabAbalone table
barplot(table(dfAbalone$SEX, dfAbalone$CLASS)[c(2,1,3), ],
legend.text = c("Infant", "Female", "Male"),
main = "Comparison of Abalone Sex Frequency",
xlab = "Age Class of specimen", ylab = 'Frequency', beside = TRUE,
col = c("orange", "chartreuse3", "cadetblue3"), 
names.arg = c('A1','A2','A3','A4','A5','A6'))

## select 200 random samples from the data, then do the matrix of plots excluding Volume and Ratio
set.seed(123)
work <- dfAbalone[sample(nrow(dfAbalone), 200), ]
plot(work[, 2:6], col='sienna')

## Plot Whole versus Volume
plot(dfAbalone$VOLUME, dfAbalone$WHOLE, main = "Whole weight as a function of Volume",
     xlab = "Volume - in cubic centimeters",ylab = "Whole - weight in grams",
     pch=18, col = "sienna")

## plot Shuck versus Whole
plot(dfAbalone$WHOLE,dfAbalone$SHUCK, main = "Shucked versus Whole",
     xlab = "Whole weight in grams", ylab = "Shucked meat in grams",
     pch=18, col = "tomato")
## find slope for abline
intercept = max(dfAbalone$SHUCK/dfAbalone$WHOLE)
abline(a=0, b=intercept, col="blue", lwd = 2)

## Create the subset dataframes by sex
dfMale = subset(dfAbalone,dfAbalone$SEX =='M')
dfFemale = subset(dfAbalone,dfAbalone$SEX =='F')
dfInfant = subset(dfAbalone,dfAbalone$SEX =='I')
par(mfrow = c(3, 3))
hist(dfFemale$RATIO, main = 'Female Ratio', xlab = '', ylab = 'Frequency', xlim = c(0.0, .30),  col = 'chartreuse3')
hist(dfInfant$RATIO, main = 'Infant Ratio',  xlab = '', ylab = 'Frequency', xlim = c(0.0, .30),col = 'orange')
hist(dfMale$RATIO, main = 'Male Ratio', xlab = '', ylab = 'Frequency',xlim = c(0.0, .30), col = 'cadetblue3')
boxplot(dfFemale$RATIO, main = 'Female Ratio', xlab = '', col = 'chartreuse3', ylim = c(0.0, .30) )
boxplot(dfInfant$RATIO ,main = 'Infant Ratio', xlab = '', col = 'orange', ylim = c(0.0, .30) )
boxplot(dfMale$RATIO, main = 'Male Ratio', xlab = '', col = 'cadetblue3', ylim = c(0.0, .30) )
qqnorm(dfFemale$RATIO, main = 'Female Ratio', xlab = 'Theoretical Quantiles', ylab = 'Sample Quartiles', col = 'chartreuse3', ylim = c(0.0, .30))
qqline(dfFemale$RATIO,datax = FALSE, distribution = qnorm)
qqnorm(dfInfant$RATIO, main = 'Infant Ratio', xlab = 'Theoretical Quantiles', ylab = 'Sample Quartiles', col = 'orange', ylim = c(0.0, .30))
qqline(dfInfant$RATIO,datax = FALSE, distribution = qnorm)
qqnorm(dfMale$RATIO, main = 'Male Ratio', xlab = 'Theoretical Quantiles', ylab = 'Sample Quartiles', col = 'cadetblue3', ylim = c(0.0, .30))
qqline(dfMale$RATIO,datax = FALSE, distribution = qnorm)
par(mfrow = c(1, 1)) ## reset 'mfrow' to default value

par(mfrow = c(2,2))
boxplot(dfAbalone$VOLUME~dfAbalone$CLASS, xlab = 'Class', ylab = 'Volume', col = "tomato", main = 'Volume vs Class')
boxplot(dfAbalone$WHOLE~dfAbalone$CLASS, xlab = 'Class', ylab = 'Whole', col = "sienna", main = 'Whole vs Class')

plot(dfAbalone$RINGS, dfAbalone$VOLUME, xlab = 'Rings', ylab = "Volume", col = "tomato", main = 'Volume vs Rings')
plot(dfAbalone$RINGS, dfAbalone$WHOLE, xlab = 'Rings', ylab = "Whole", col = "sienna", main = 'Whole vs Rings')
par(mfrow = c(1, 1)) ## reset 'mfrow' to default value

## for fun, let's experiment with ggplot for a change of pace
aggAbaloneVol <- aggregate(VOLUME~SEX + CLASS, data=dfAbalone, FUN = mean)
mVol <- cast(aggAbaloneVol, SEX~CLASS, mean)
rownames(mVol)[1] <- 'Female'
rownames(mVol)[2] <-'Infant'
rownames(mVol)[3] <- 'Male'
## drop the SEX col now that rownames are in place 
mVol <- mVol[,-1]
capture.output(round(mVol, 2),file="out2.txt")
volPlot <- ggplot(data = aggAbaloneVol, aes(x=CLASS, y=VOLUME, group=SEX, color=SEX))+
  geom_line() + geom_point(size=4)+ggtitle('Plot of Mean Volume vs Class for the Three Sexes')+
  scale_colour_manual(values = c('chartreuse3','orange','cadetblue3')) + theme_bw()

aggAbaloneRatio <- aggregate(RATIO~SEX + CLASS, data=dfAbalone, FUN = mean)
mRatio <- cast(aggAbaloneRatio, SEX~CLASS, mean)
rownames(mRatio)[1] <- 'Female'
rownames(mRatio)[2] <-'Infant'
rownames(mRatio)[3] <- 'Male'
# drop the SEX col now that rownames are in place 
mRatio <- mRatio[,-1]
capture.output(round(mRatio,4), file="out3.txt")

ratioPlot<-ggplot(data = aggAbaloneRatio, aes(x=CLASS, y=RATIO, group=SEX, color=SEX))+
  geom_line() + geom_point(size=4)+ggtitle('Plot of Mean Ratio vs Class for the Three Sexes')+
  scale_colour_manual(values = c('chartreuse3','orange','cadetblue3')) + theme_bw()

## had to use the save to Var, and grid.arrange(var) to get around an error
## grid throws.  I realize doing it all together is more efficient
# grid.arrange(volPlot, ratioPlot, nrow = 1)

volPlot
ratioPlot

## ----- Extra coding done to support statements in my document ---------##
## ----- Code appendix ------- ##

## ----- additional chart - Figure 1, plot the distributions of the measured data
par(mfrow = c(2,3))
hist(dfAbalone$LENGTH, main = 'Distribution of Lengths', 
     xlab = 'Centimeters', ylab = 'Frequency', col = 'ivory3')
hist(dfAbalone$HEIGHT, main = 'Distribution of Heights', 
     xlab = 'Centimeters', ylab = 'Frequency', col = 'ivory3')
hist(dfAbalone$DIAM, main = 'Distribution of Diamters', 
     xlab = 'Centimeters', ylab = 'Frequency', col = 'ivory3')
hist(dfAbalone$SHUCK, main = 'Distribution of Shuck weights', 
     xlab = 'Weight in grams', ylab = 'Frequency', col = 'ivory3')
hist(dfAbalone$WHOLE, main = 'Distribution of Whole weights', 
     xlab = 'Weight in grams', ylab = 'Frequency', col = 'ivory3')
hist(dfAbalone$RINGS, main = 'Distribution of Ring Counts', 
     xlab = 'Number of Rings Counted', ylab = 'Frequency', col = 'ivory3')
par(mfrow = c(1,1))

## ----- extra calculations for outliers section - section 1 of assignment
quantile(dfAbalone$SHUCK, .75) + 3.0*IQR(dfAbalone$SHUCK)  ## checking for extreme outlier
quantile(dfAbalone$SHUCK, .25) - 1.5*IQR(dfAbalone$SHUCK)  ## checking for outlier on the other side
quantile(dfAbalone$SHUCK, .75) + 1.5*IQR(dfAbalone$SHUCK)  ## outlier checks
quantile(dfAbalone$WHOLE, .75) + 1.5*IQR(dfAbalone$WHOLE)
quantile(dfAbalone$LENGTH, .75) + 1.5*IQR(dfAbalone$LENGTH)
quantile(dfAbalone$DIAM, .75) + 1.5*IQR(dfAbalone$DIAM)
quantile(dfAbalone$HEIGHT, .75) + 1.5*IQR(dfAbalone$HEIGHT)
quantile(dfAbalone$RINGS, .75) + 1.5*IQR(dfAbalone$RINGS)
quantile(dfAbalone$VOLUME, .75) + 1.5*IQR(dfAbalone$VOLUME)
quantile(dfAbalone$RATIO, .75) + 1.5*IQR(dfAbalone$RATIO)
## ---- just make a boxplot to show outliers, color the outliers for visibility
par(mfrow = c(2,3))
boxplot(dfAbalone$LENGTH, main = 'Length', xlab = '', col = 'ivory3',outcol='red')
boxplot(dfAbalone$HEIGHT, main = 'Height', xlab = '', col = 'ivory3',outcol='red')
boxplot(dfAbalone$DIAM, main = 'Diameter', xlab = '', col = 'ivory3',outcol='red')
boxplot(dfAbalone$WHOLE, main = 'Whole', xlab = '', col = 'ivory3',outcol='red')
boxplot(dfAbalone$SHUCK, main = 'Shuck', xlab = '', col = 'ivory3',outcol='red')
boxplot(dfAbalone$RINGS, main = 'Rings', xlab = '', col = 'ivory3',outcol='red')
par(mfrow = c(1,1))

## bar chart of the tabAbalone table with number showing the counts of each sex by class - section 1b
sPlot <-barplot(table(dfAbalone$SEX, dfAbalone$CLASS)[c(2,1,3), ],
        legend.text = c("Infant", "Female", "Male"),
        main = "Comparison of Abalone Sex Frequency",
        xlab = "Age Class of specimen", ylab = 'Frequency', beside = TRUE,
        col = c("orange", "chartreuse3", "cadetblue3"), 
        names.arg = c('A1','A2','A3','A4','A5','A6'))
text(sPlot, 0, (table(dfAbalone$SEX, dfAbalone$CLASS)[c(2,1,3), ]), cex = 1, pos=3, offset = .25)

## Plot Whole versus Volume AND Shuck versus Whole on one chart - section 2b, Figure 8
plot(dfAbalone$VOLUME, dfAbalone$WHOLE, main = "Overlay of Whole vs Volume AND Shucked vs Whole", 
     axes = FALSE, pch=18, col = "sienna", xlab = '', ylab = '')
par(new=T)
plot(dfAbalone$WHOLE,dfAbalone$SHUCK, axes = FALSE, pch=18, col = "tomato", xlab = '', ylab = '')
intercept = max(dfAbalone$SHUCK/dfAbalone$WHOLE)
abline(a=0, b=intercept, col="blue", lwd = 2)
legend('bottomright', legend=c('Whole vs Volume', 'Shuck vs Whole'), cex = 1, bg = "transparent", bty = 'n', text.col = c("sienna",'tomato'))

## Skewness and kurtosis of Ratio calculations - section 3a
skewness(dfFemale$RATIO)
skewness(dfInfant$RATIO)
skewness(dfMale$RATIO)
kurtosis(dfFemale$RATIO)
kurtosis(dfInfant$RATIO)
kurtosis(dfMale$RATIO)

## find the individual outliers, start by finding the values for Q3+1.5*IQR, and Q1-1.5*IQR if needed
## resulting table, after some formatting, is in the Appendix as Table 5
oFH<-quantile(dfFemale$RATIO, .75) + 1.5*IQR(dfFemale$RATIO)
oFE<-quantile(dfFemale$RATIO, .75) + 3.0*IQR(dfFemale$RATIO)
oFL<-quantile(dfFemale$RATIO, .25) - 1.5*IQR(dfFemale$RATIO)
oFLE<-quantile(dfFemale$RATIO, .25) - 3.0*IQR(dfFemale$RATIO)
oI <- quantile(dfInfant$RATIO, .75) + 1.5*IQR(dfInfant$RATIO)
oM <- quantile(dfMale$RATIO, .75) + 1.5*IQR(dfMale$RATIO)
## get the individuals
df_oFH <- dfFemale[dfFemale$RATIO >= oFH,]
df_oFE <- dfFemale[dfFemale$RATIO >= oFE,]
df_oFL <- dfFemale[dfFemale$RATIO <= oFL,]
df_oFLE <- dfFemale[dfFemale$RATIO <= oFLE,]
df_oIH <- dfInfant[dfInfant$RATIO >= oI,]
df_oMH <- dfMale[dfMale$RATIO >= oM,]

## ----- additional chart - Figure 13
temp <- subset(work, select = c(RINGS, LENGTH, DIAM, HEIGHT))
plot(temp, col = 'sienna3')
