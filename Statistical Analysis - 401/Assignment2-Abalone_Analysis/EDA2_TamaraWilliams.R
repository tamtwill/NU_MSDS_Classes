# Code for Data Analysis Project 2
# NWU Predict 401, Sec 55, Sp2017
# Written by - Tamara Williams

#Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

setwd('~/NorthwesternU_MSPA/Classes/Statistical Analysis - 401/Data Analysis Assignments/Assignment2/')
library(stats)

dfAbalone <- read.csv("myAbaloneData.csv", header = T, sep = " ", stringsAsFactors = T)


# (1)(a) Form a histogram and QQ plots using RATIO. Calculate the skewness and kurtosis 
# (be aware with rockchalk the kurtosis value has 3.0 subtracted from it which differs from the
# moments package.). 
library(rockchalk)
library(moment)

par(mfrow = c(1,2))
hist(dfAbalone$RATIO, main = 'Abalone Ratios', xlab = 'RATIO', ylab = 'Frequency', 
     xlim = c(0.0, .30),  col = 'cyan3')
qqnorm(dfAbalone$RATIO, main = 'QQ Plot of Abalone Ratios', xlab = 'Theoretical Quantiles', 
       ylab = 'Sample Quartiles', col = 'cyan3', ylim = c(0.0, .30))
qqline(dfAbalone$RATIO,datax = FALSE, distribution = qnorm,  col = 'red')
par(mfrow = c(1, 1)) ## reset 'mfrow' to default value

rc_skew <- rockchalk::skewness(dfAbalone$RATIO, na.rm = TRUE, unbiased = TRUE)
m_skew <- moments::skewness(dfAbalone$RATIO)
rc_skew 
m_skew
rc_kurtosis<-rockchalk::kurtosis(dfAbalone$RATIO, na.rm = TRUE, unbiased = TRUE)
m_kurtosis <- moments::kurtosis(dfAbalone$RATIO)
rc_kurtosis
m_kurtosis

detach('package:moment', unload = TRUE)


# (1)(b) Transform RATIO using log10() to create L_RATIO  
dfAbalone$L_RATIO <- log10(dfAbalone$RATIO)

# Form a histogram and QQ plots using L_RATIO. 
par(mfrow = c(1, 2))
hist(dfAbalone$L_RATIO, main = 'Abalone Transformed Ratios', xlab = 'L_RATIO', 
     ylab = 'Frequency', col = 'cyan3')
qqnorm(dfAbalone$L_RATIO, main = 'QQ Plot of Abalone Transformed Ratios', 
       xlab = 'Theoretical Quantiles', ylab = 'Sample Quartiles', col = 'cyan3')
qqline(dfAbalone$L_RATIO,datax = FALSE, distribution = qnorm,  col = 'red')
par(mfrow = c(1, 1)) ## reset 'mfrow' to default value

# checking skew and kurtosis.  Skew = 0 for Normal dist. and kurtosis = 0 (in Rockchalk) 
# for a Normal dist.
rcl_skew <- rockchalk::skewness(dfAbalone$L_RATIO, na.rm = TRUE, unbiased = TRUE)
rcl_skew 
rcl_kurtois<-rockchalk::kurtosis(dfAbalone$L_RATIO, na.rm = TRUE, unbiased = TRUE)
rcl_kurtois

# Display boxplots of L_RATIO differentiated by CLASS.
boxplot(dfAbalone$L_RATIO~dfAbalone$CLASS, xlab = 'Class', ylab = 'log10(Ratio)', 
        col = "cyan4", main = 'Transformed Ratio vs Class')

# (1)(c) Test the homogeneity of variance across classes using the bartlett.test() 

# bartlett.test() tests null hypothesis of homogeneity of variance of a numeric 
#variable across two (2) or more groups or levels of a factor. 
# bartlett.test(numeric ~ factor, data = ...) 
# OR, bartlett.test(x = numeric, g = factor, data = ...)

bartlett.test(L_RATIO~CLASS, data=dfAbalone)

# (2)(a) Perform an analysis of variance with aov() on L_RATIO using CLASS and SEX as 
# the independent variables 
# Assume equal variances. Perform two analyses. First, use the model *with* an interaction 
# term CLASS:SEX, and then a model *without* the interaction term CLASS:SEX. 
# Use summary() to obtain the analysis of variance table. 

lr_anova_terms <- aov(L_RATIO ~CLASS*SEX, data = dfAbalone)
summary(lr_anova_terms)
lr_anova_noterms <- aov(L_RATIO ~CLASS+SEX, data = dfAbalone)
summary(lr_anova_noterms)
#export it to wordfile, to assisting in report writing
capture.output(summary(lr_anova_terms),file="2a_lrAnovaTerms.doc")
capture.output(summary(lr_anova_noterms),file="2a_lrAnovaNoTerms.doc")

#2)(b) For the model without CLASS:SEX, obtain multiple comparisons with the TukeyHSD() function. 

TukeyHSD(lr_anova_noterms)
capture.output(TukeyHSD(lr_anova_noterms),file="Tukey_lrAnovaNoTerms.doc")

# (3)(a) Use combineLevels() from the rockchalk package to combine "M" and "F" into a 
# new level "ADULT". 
# Use par() to form two histograms using VOLUME. 
# One would display infant volumes, and the other ADULT volumes. 

dfAbalone$TYPE <- combineLevels(dfAbalone$SEX, levs = c("M","F"), "ADULT")

par(mfrow = c(1, 2))
dfAdult <- subset(dfAbalone,dfAbalone$TYPE =='ADULT')
dfInfant <- subset(dfAbalone,dfAbalone$TYPE =='I')
hist(dfInfant$VOLUME, main = 'Infant Volume',  xlab = 'Volume in cm^3', ylab = 'Frequency', 
     col = 'cyan2')
hist(dfAdult$VOLUME, main = 'Adult Volume', xlab = 'Volume in cm^3', ylab = 'Frequency',  
     col = 'cyan4')
par(mfrow = c(1, 1))

# (3)(b) Form a scatterplot of SHUCK versus VOLUME and a scatterplot of their base ten logarithms,
# labeling the variables as L_SHUCK and the latter as L_VOLUME. 
# The variables L_SHUCK and L_VOLUME present the data as orders of magnitude 
# (i.e. VOLUME = 100 = 10^2 becomes L_VOLUME = 2). 
# Use color to differentiate CLASS in the plots. 
# Repeat using color to differentiate only by TYPE. 

# define the base ten logarithm vectors
dfAbalone$L_SHUCK <- log10(dfAbalone$SHUCK) 
dfAbalone$L_VOLUME <- log10(dfAbalone$VOLUME)
require(ggplot2)
require(gridExtra)

plot1 <- ggplot(dfAbalone, aes(x=dfAbalone$VOLUME, y=dfAbalone$SHUCK, color=CLASS)) +
  geom_point()+geom_point(size=2)+ggtitle('Shuck weight vs Volume values')+
  labs(x= "Volume", y="Shucked Weight (gm)")+
  theme(plot.title = element_text(size = 10, face = "bold"))

plot2 <- ggplot(dfAbalone, aes(x=dfAbalone$L_VOLUME, y=dfAbalone$L_SHUCK, color=CLASS)) + 
  geom_point()+geom_point(size=2)+ggtitle('Transformed Shuck wt. vs Transformed Volume')+
  labs(x= "Log10(Volume)", y="log10(Shuck)")+
  theme(plot.title = element_text(size = 10, face = "bold"))
grid.arrange(plot1, plot2, nrow=1, ncol=2)

plot3 <- ggplot(dfAbalone, aes(x=dfAbalone$VOLUME, y=dfAbalone$SHUCK, color=TYPE)) + 
  geom_point()+geom_point(size=2)+ggtitle('Shuck weight vs Volume values')+
  labs(x= "Volume", y="Shucked Weight (gm)")+
  theme(plot.title = element_text(size = 10, face = "bold"))

plot4 <- ggplot(dfAbalone, aes(x=dfAbalone$L_VOLUME, y=dfAbalone$L_SHUCK, color=TYPE)) +
  geom_point()+geom_point(size=2)+ggtitle('Transformed Shuck wt. vs Transformed Volume')+
  labs(x= "Log10(Volume)", y="log10(Shuck)")+
  theme(plot.title = element_text(size = 10, face = "bold"))
grid.arrange(plot3, plot4, nrow=1, ncol=2)

# (4)(a) Regress L_SHUCK as the dependent variable on L_VOLUME, CLASS and TYPE 
# Use the multiple regression model: L_SHUCK~L_VOLUME+CLASS+TYPE. 
# Apply summary() to the model object to produce results.

linear_model <- lm(L_SHUCK ~ L_VOLUME + CLASS + TYPE, data = dfAbalone)
summary(linear_model)
capture.output(summary(linear_model),file="4a_linearModel.doc")

# (5)(a) Perform an analysis of the residuals resulting from the regression model in (3) 
# If "linear_model" is the regression object, use linear_model$residuals and construct a 
# histogram and QQ plot. Compute the skewness and kurtosis.

par(mfrow = c(1, 2))
hist(linear_model$residuals, main = 'Histogram of Residuals',  xlab = 'Residuals', ylab = ' ', 
     col = 'cyan2')
qqnorm(linear_model$residuals,main = 'QQ Plot of Residuals', xlab = 'Theoretical Quantiles', 
       ylab = 'Sample Quartiles', col = 'cyan2') 
qqline(linear_model$residuals, datax = FALSE, distribution = qnorm,  col = 'red')
par(mfrow = c(1, 1))

rockchalk::skewness(linear_model$residuals) 
rockchalk::kurtosis(linear_model$residuals) 

# (5)(b) Plot the residuals versus L_VOLUME coloring the data points by CLASS, 
# and a second time coloring the data points by TYPE 
# Present boxplots of the residuals differentiated by CLASS and TYPE 
# Test the homogeneity of variance of the residuals across classes using the bartlett.test()

#Scatterplot of model residuals as a function of L_VOLUME, CLASS, ggplot2
plot5 <- ggplot(linear_model, aes(x = L_VOLUME,y = linear_model$residuals)) + 
    ggtitle('Chart A') +
    geom_point(aes(color = CLASS)) + labs(x = "L_VOLUME", y = "Residual")
plot6 <- ggplot(linear_model, aes(x = L_VOLUME,y = linear_model$residuals)) + 
    ggtitle('Chart B') +
    geom_point(aes(color = TYPE)) + labs(x = "L_VOLUME", y = "Residual")
plot7 <- ggplot(linear_model, aes(x = CLASS,y = linear_model$residuals)) + 
    ggtitle('Chart C') +
    geom_boxplot(aes(color = CLASS)) + labs(x = "CLASS", y = "Residual")
plot8 <- ggplot(linear_model, aes(x = TYPE,y = linear_model$residuals)) + 
    ggtitle('Chart D') +
    geom_boxplot(aes(color = TYPE))+ labs(x = "TYPE", y = "Residual")
grid.arrange(plot5, plot6,plot7, plot8, nrow=2, ncol=2)

# Barlett test of homogeneity of variances
bartlett.test(linear_model$residuals ~ CLASS, data = dfAbalone)

# (6)(a) Calculate the proportion of infant abalones and adult abalones which fall beneath a 
# specified volume or "cutoff". A series of volumes covering the range from minimum to maximum 
# abalone volume will be used in a "for loop" to determine how the harvest proportions change
# as the "cutoff" changes. 
# -- Code from instructor's RMD file --
idxi <- dfAbalone$TYPE=="I"
idxa <- dfAbalone$TYPE=="ADULT"
max.v <- max(dfAbalone$VOLUME)
min.v <- min(dfAbalone$VOLUME)
delta <- (max.v - min.v)/1000
prop.infants <- numeric(0)
prop.adults <- numeric(0)
volume.value <- numeric(0)
total.infants <- length(dfAbalone$TYPE[idxi]) 
total.adults <- length(dfAbalone$TYPE[idxa])
for (k in 1:1000) {
  value <- min.v + k*delta
  volume.value[k] <- value
  prop.infants[k] <- sum(dfAbalone$VOLUME[idxi] <= value)/total.infants
  prop.adults[k] <- sum(dfAbalone$VOLUME[idxa] <= value)/total.adults
}

n.infants <- sum(prop.infants <= 0.5)
split.infants <- min.v + (n.infants + 0.5)*delta # This estimates the desired volume. 
n.adults <- sum(prop.adults <= 0.5)
split.adults <- min.v + (n.adults + 0.5)*delta
# -- end instructor's code --

# check the outcome of the split
head (prop.adults, 20)
head (prop.infants,20)
head (volume.value, 20)

# (6)(b) Present a plot showing the infant proportions and the adult proportions versus volume. 
# Compute the 50% "split" volume.value for each and show on the plot. 
# The two split points suggest an interval within which potential cutpoints may be located.
par(mfrow = c(1, 1))
plot(prop.adults, main = 'Proportion of Adults and Infants Protected', xlab = 'Volume', 
     ylab = 'Proportion', col = 'cyan4', type = 'l', lwd = 2)
lines(prop.infants, col = 'cyan3', lwd = 2)
legend('bottomright',legend = c('Adult', 'Infant'), cex = 1, bg = "transparent", 
       col = c('cyan4', 'cyan2'), lty = 1, lwd = 2)
abline(v= split.adults, col='orange')
abline(v= split.infants, col='orange')
abline(h= .50, col='orange')
text(split.adults, .46,  round(split.adults, 2),  pos = 4, offset = 1, col = "black")
text(split.infants, .46,  round(split.infants, 2),  pos = 4, offset = 1, col = "black")

# (7)(a) Evaluate a plot of the difference ((1-prop.adults)-(1-prop.infants)) versus volume.value. 
# Compare to the 50% split points determined in (6)(b). There is considerable variability present
# in the peak area of this plot. The observed "peak" difference may not be the best 
# representation of the data. One solution is to smooth the data to determine a more 
# representative estimate of the maximum difference.

difference <- (1-prop.adults) - (1-prop.infants)
plot(volume.value, difference, main = "Difference in Harvested Proportions", col = 'cyan4', type = "l", lwd = 2, ylab = 'Difference in Proportions Harvested', 
     xlab = 'VOLUME')

peak_x <- which.max(difference)
peak_x
abline(v=peak_x, lty = 2)
text(305, .3, paste("Volume =",peak_x), pos = 4, srt = 90)


# (7)(b) Since curve smoothing is not studied in this course, code is supplied below. 
# Execute the following code to determine a smoothed version of the plot in (a). 

#loess, local polynomial regression fitting
y.loess.a <- loess(1-prop.adults ~ volume.value, span = 0.25, family = c("symmetric")) 
y.loess.i <- loess(1-prop.infants ~ volume.value, span = 0.25, family = c("symmetric")) 
smooth.difference <- predict(y.loess.a) - predict(y.loess.i)


# (7)(c) Present a plot of the difference ((1-prop.adults)-(1-prop.infants)) versus 
# volume.value with the variable smooth.difference superimposed.  Show the estimated peak 
# location corresponding to the cutoff determined.
plot(volume.value, difference, main = "Difference in Harvested Proportions", col = 'cyan4', type = "l", lwd = 2, 
     ylab = 'Difference in Proportions Harvested', xlab = 'VOLUME')
lines(smooth.difference, col = 'orange', lwd = 2.5, lty=1)
max_val <- which.max(smooth.difference)
max_smooth <- volume.value[max_val]
abline(v=max_smooth, lty = 2, col = 'black')
note <- paste('Volume = ', round(max_smooth, 4)) 
text(max_smooth+20, .4, note, col = 'black', srt = 90)

# (7)(d) What separate harvest proportions for infants and adults would result if this cutoff 
# is used? (NOTE: the adult harvest proportion is the "true positive rate" and the 
# infant harvest proportion is the "false positive rate.")
TP_maxdiff <- (1-prop.adults)[which.max(smooth.difference)]
FP_maxdiff <- (1-prop.infants)[which.max(smooth.difference)]
TP_maxdiff
FP_maxdiff


# (8)(a) Harvesting of infants in CLASS "A1" must be minimized. The volume.value cutoff that 
# produces a zero harvest of infants from CLASS "A1" is 207. Any smaller cutoff would result in 
# harvesting infants from CLASS "A1." Calculate the separate harvest proportions for infants 
# and adults if this cutoff is used. Report your results.

TP_0A1 <- (1-prop.adults)[207]
FP_0A1 <- (1-prop.infants)[207]
TP_0A1
FP_0A1


# Although the relevant volume.value - 207 - is given to you, we can demonstrate
# how it was arrived at. Specifically, we want to return the volume.value corresponding, 
# element-wise, to the smallest volume.value greater than the largest VOLUME among 
# CLASS "A1" infants.
v1 <- volume.value[volume.value > 
      max(dfAbalone[dfAbalone$CLASS == "A1" & dfAbalone$TYPE == "I", "VOLUME"])][1] # [1] 206.9844
v1


# Now, to determine the proportions harvested, we can look to the proportions # of infants
# and adults with VOLUMEs greater than this threshold.

tot_p_1<-sum(dfAbalone["VOLUME"]>v1)/(sum(dfAbalone$TYPE=="ADULT")+sum(dfAbalone$TYPE=="I"))


# (8)(b) Another cutoff can be determined for which the proportion of adults not harvested equals
# the proportion of infants harvested. This cutoff would equate these rates; 
# effectively, our two errors: 'missed' adults and wrongly-harvested infants. 

TP_hc <- (1-prop.adults[253.6])
FP_hc <- (1-prop.infants)[253.6]
TP_hc
FP_hc


v2 <- volume.value[which.min(abs(prop.adults - (1-prop.infants)))]     # [1] 253.6113
v2
tot_p_2<-sum(dfAbalone["VOLUME"]>v2)/(sum(dfAbalone$TYPE=="ADULT")+sum(dfAbalone$TYPE=="I"))


# (9) Construct an ROC curve by plotting (1-prop.adults) versus (1-prop.infants). 
# Each point which appears corresponds to a particular volume.value. 
# Show the locations of the cutoffs determined in (7) and (8) on this plot. 
# Numerically integrate the area under the ROC curve and report your result. 

plot(1-prop.infants, 1-prop.adults, type = 'l', lwd = 2, col='cyan4', 
     main = "ROC of adult and infant havest proportions", 
     xlab = 'Infant Harvest Prop', ylab='Adult Harvest Proportion')
abline(0, 1, col = 'orange', lty = 3, lwd = 2)

points((1-prop.infants)[207],(1-prop.adults)[207], col = 'red', pch=15)
text((1-prop.infants)[207],(1-prop.adults)[207], "harvesting 0 A1, vol = 207",
     col = 'black', pos = 4, offset = 1)

points((1-prop.infants)[253.6],(1-prop.adults)[253.6], col = 'red', pch=15)
text((1-prop.infants)[253.6],(1-prop.adults)[253.6], "equal harvest/conserve\n vol = 253.6", 
     col = 'black', pos = 2, offset = 1)

points((1-prop.infants)[281.4],(1-prop.adults)[281.4], col = 'red', pch=15)
text((1-prop.infants)[281.4],(1-prop.adults)[281.4], "max. diff, vol = 281.4", 
     col = 'black', pos = 4, offset = 1)
require(flux)
area <- round(flux::auc((1-prop.infants), (1-prop.adults)), 4)
note <- paste('AUC = ', area)
text (.8, .2, note)

#(10) Prepare a table showing each cutoff along with the following: 
# 1) true positive rate (1-prop.adults), 
# 2) false positive rate (1-prop.infants), and 
# 3) harvest proportion of the total population (all adults and infants considered).

# To calculate the total harvest proportions, we need to consider all individuals,
# regardless of SEX or TYPE, and what proportion are greater|less than a given
# cutoff. An example calculation, for the "maximum difference" approach is given here:
v3 <- volume.value[which.max(smooth.difference)]
tot_p_3<-sum(dfAbalone$VOLUME>=volume.value[which.max(smooth.difference)])/(total.adults+total.infants) # [1] 0.5501931
v3
tot_p_3 

harvest<- c("zero harvest", "equal harvest", "max difference")
volume <- round(c(v1,v2,v3), 3)
FPR <- round(c(FP_0A1,FP_hc,FP_maxdiff), 3)
TPR <-round(c(TP_0A1,TP_hc,TP_maxdiff), 3)
PropYield <- round(c(tot_p_1, tot_p_2, tot_p_3),3)
prop_table <- cbind (harvest, volume, TPR, FPR, PropYield)
prop_table
capture.output(prop_table,file="10_Table.doc")

## ***** Auxilary Material, not part of assignment PDF
# Figure 5
par(mfrow = c(1, 2))
dfAdult$L_VOL <- log10(dfAdult$VOLUME)
dfInfant$L_VOL <- log10(dfInfant$VOLUME)
hist(dfInfant$L_VOL, main = 'Infant log10(Volume)',  xlab = 'Volume', ylab = 'Frequency', col = 'dodgerblue')
hist(dfAdult$L_VOL, main = 'Adult log10(Volume)', xlab = 'Volume', ylab = 'Frequency',  col = 'dodgerblue4')
par(mfrow = c(1, 1))

# Data for Table 5-A
rockchalk::skewness(dfInfant$L_VOL)
rockchalk::kurtosis(dfInfant$L_VOL)
rockchalk::skewness(dfAdult$L_VOL)
rockchalk::kurtosis(dfAdult$L_VOL)


## Skew and kurtosis for Volume
rockchalk::skewness(dfAdult$VOLUME)
rockchalk::kurtosis(dfAdult$VOLUME)
rockchalk::skewness(dfInfant$VOLUME)
rockchalk::kurtosis(dfInfant$VOLUME)

## Figure 9, Chart A with 2sd lines
r_sd <- sd(linear_model$residuals)
ggplot(linear_model, aes(x = L_VOLUME,y = linear_model$residuals)) + 
   geom_hline(yintercept = 2*r_sd, lty = 3) + geom_hline(yintercept = -2*r_sd, lty = 3)+
   geom_hline(yintercept = 3*r_sd, lty = 3) + geom_hline(yintercept = -3*r_sd, lty = 3, col='blue')+
   geom_point(aes(color = CLASS)) + labs(x = "L_VOLUME", y = "Residual")

# getting the class means of L_RATIO for Tukey discussion
C1 <-dfAbalone[dfAbalone$CLASS == 'A1',]
summary(C1)
C2 <-dfAbalone[dfAbalone$CLASS == 'A2',]
summary(C2)

