######## Predict 413, midterm
######## Submitted by: Tamara Williams
########
######## Apologies for the cut-and-paste nature
######## of the code, for production I'd have
######## made it into functions


# For the sake of good programming hygiene, start clean
#-------------------------------------------------------------------
# clear Workspace, then clear console
rm(list=ls())
cat("\014")

# Set working directory
#-------------------------------------------------------------------
setwd("/Users/tamtwill/NorthwesternU_MSPA/Classes/Time Series - 413/midterm_Kaggle")

# include required packages
#-------------------------------------------------------------------
library(caret)
library(fpp)
library(lubridate)
library(reshape2)
library(dplyr)
library(psych)
library(VIM)
library(mice)
library(tidyr)
library(forecast)
library(stl)
library(ggplot2)
library(robustHD)

#**********************************************
# Function to output the forecast in the right format 
# for submitting to Kaggle.
#**********************************************
make_submission <- function (my_forecast,m_name){
print("entered function")

# get the date range for the submission
last_train <- as.Date(max(train_ar$visit_date)) + days(1) 
day_tmp <- as.character.Date(seq(last_train, as.Date("2017-05-31"), by = "day"))
id_tmp <- colnames(ts_df)

k=1
make_it=melt(my_forecast)
len_id <- dim(make_it)[1]
len_day<- length(day_tmp)
for(i in seq(1,len_id, by=39)){
  for(j in 1:39){
      store=make_it$Var2[i]
      make_it$Var1[k]<-as.character(paste0(store,'_',day_tmp[j]))
      print(paste0("Current k: ", k))
      k=k+1
      next #j
  }
  next  #i
}


submission <- make_it[-c(2)]
names(submission)<-c('id', 'visitors')
submission$visitors <- pmax(submission$visitors,0)

# get rid of the id's NOT in the submission file
print("starting the extraction of good ids")
good_id <- submit_only_id$id
all_id <- submission$id
submission <- submission[submission$id %in% good_id, ]

#getNow <- as.integer(now())
submissionFile <-paste("TW_", m_name, ".csv", sep="")
write.csv(submission, submissionFile, quote=FALSE, row.names = FALSE)

}
#**********************************************




###############################################################################
######                        do data prep - run all                 ##########
###############################################################################

# get the data
# join_info=read.csv("store_id_relation.csv", header = T, stringsAsFactors = FALSE)
clean_df=read.csv("air_visit_data.csv",header=T, stringsAsFactors = FALSE)
submit_only_id=read.csv("sample_submission-2.csv",header=T, stringsAsFactors = FALSE)
d_of_w <- read.csv("date_info.csv",header=T, stringsAsFactors = FALSE)

# check for incomplete rows of data
missing <-length(clean_df[!complete.cases(clean_df)])
missing

# make a clean copy with date formating
df_format_clean <- clean_df
df_format_clean$visit_date <- as.Date(df_format_clean$visit_date)
# cast the data to a pivot-table-like format
train_ar <- dcast(clean_df, visit_date~air_store_id, value.var="visitors", fun.aggregate=sum)

# Ok, nothing to impute on input, but check all the dates are there
all_dates = seq(as.Date('2016-1-1'), as.Date('2017-4-22'), by=1)
df_dates= as.Date(unique(clean_df$visit_date))
setequal(all_dates, df_dates)
df_all_dates=as.data.frame(all_dates)
colnames(df_all_dates)<-('visit_date')

# store id list
stores=unique(clean_df$air_store_id)
first_date=min(clean_df$visit_date)

# cast to time series
#-------------------------------------------------------------------
ts_df = ts(train_ar[, -c(1)], start=c(2016,1,1), frequency=365)
ts_df2 = ts(train_ar[, -c(1)], start=c(2016,1,1), frequency=7)
#-------------------------------------------------------------------

# Fix up the column types, starting with visit date
#-------------------------------------------------------------------
ar_split =t(as.data.frame(strsplit(train_ar$visit_date,'-')))
train_ar$Year=ar_split[,1]
train_ar$Month=ar_split[,2]
train_ar$Day=ar_split[,3]
rm(ar_split)

# let's reorder the columns to make it easier to see things
clist <- colnames(train_ar)
clist<- clist[c(831:833, 1:830)]
train_ar<- train_ar[, clist]

# look at the daily aggregates
#weekday <-  merge(clean_df, d_of_w, x.by ="visit_date", y.by='calendar_date' )
weekday <- left_join(clean_df, d_of_w, by = c("visit_date" = "calendar_date"))

wins_df$visitors=winsor(clean_df$visitors, na.rm = FALSE)
wins_df$visit_date=as.Date(wins_df$visit_date)

###############################################################################


#******************************************************************************
######                    Visualize the data graphically             ##########
#******************************************************************************

# Since time series forecasts are basically linear models, let's graph the data to see if there are any
# linear model concerns
# visits by date, which should be the same as the normal time series plot
tmp <- clean_df
tmp$date <- as.Date(tmp$visit_date)
gf <- aggregate(x= tmp[c("visitors")],FUN = sum, by = list(tmp$date))

qplot(Group.1, visitors, data=gf, geom="line") +
  geom_line(col='cyan4') +
  labs(x="Date", y="Total Visitors") +
  ggtitle("Visitor Counts by Month") +
  theme(aspect.ratio = 1/4)

# Look at the monthly aggregates
tmp2 <- clean_df
t_split =t(as.data.frame(strsplit(tmp2$visit_date,'-')))
tmp2$Year=t_split[,1]
tmp2$Month=t_split[,2]
tmp2$Day=t_split[,3]
gf2 <- aggregate(x= tmp2[c("visitors")],FUN = sum, by = list(tmp2$Month))

ggplot(gf2, aes(x = Group.1 ,y = visitors)) + 
  ggtitle('Total Visitors per Month') +
  geom_col(aes(fill = Group.1)) + labs(x = "Month", y = "Visitors")

ggplot(tmp2, aes(x = Month ,y = log(visitors))) + 
  ggtitle('Boxplot of log(Visitors) per Month') +
  geom_boxplot(aes(color = Month)) + labs(x = "Month", y = "log(Visitors)")

ggplot(tmp2, aes(x = Month ,y = visitors)) + 
  ggtitle('Boxplot of Visitors per Month') +
  geom_boxplot(aes(color = Month)) + labs(x = "Month", y = "Visitors")

# look at the aggregates by day
gf3 <- aggregate(x= weekday[c("visitors")],FUN = sum, by = list(weekday$day_of_week))
ggplot(gf3, aes(x = Group.1 ,y = visitors)) + 
  ggtitle('Plot of Visitors by day of the week') +
  geom_col(aes(fill = Group.1)) + labs(x = "Day", y = "Visitors")

# look at the aggregates by holiday
gf4 <- aggregate(x= weekday[c("visitors")],FUN = sum, by = list(weekday$day_of_week, weekday$holiday_flg))
colnames(gf4)<-c("Day", "Is_Holiday", "Visitors")
ggplot(gf4, aes(x = Day ,y = Visitors)) + 
  ggtitle('Plot of Visitors by day of the week, showing Holidays') +
  geom_col(aes(fill = Is_Holiday)) + labs(x = "Day", y = "Visitors") 

# get rid of the month, day, year columns, they aren't helping
train_ar=train_ar[,-c(1,2,3)]
write.csv(describe(train_ar), "describe_info.csv")

# looking for missing patterns with VIM
# get the first instance
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_collate<-df_visits
# now get the rest
for(i in 1:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_collate <- rbind(df_collate, df_visits)
}

aggr_plot <- aggr(df_collate, col=c('cadetblue3','brown1'), numbers=TRUE, sortVars=TRUE, labels=names(df_collate), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


##############################################################################
# look at the time series data with ts tools
# tools only work with univariate series, so just look at one store
# at a time, re-running as needed to get a sense of the data
#
# using period 365 usually just gets 'non-seasonal or less than 2 period'
# messages, so try frequency=7 also
#-------------------------------------------------------------------
#-------------------------------------------------------------------

# look at a random restaurant to see how patterns compare
rand_col=sample(stores, 1, replace=TRUE)
small_ts_rand=ts(train_ar[,rand_col], start=c(2016,1,1), end=c(2017,4,22),frequency=365)
c_title=paste0("Decomposition of additive time series, column ",rand_col)
plot(small_ts_rand, main=c_title)
c_title=paste0("Time series frequency = 365, column ",rand_col)
acf(small_ts_rand, main=c_title)
pacf(small_ts_rand)
Acf(small_ts_rand, main=c_title)

# look another random restaurant, this time with frequency = 7
rand_col=sample(stores, 1, replace=TRUE)
small_tsw_rand=ts(train_ar[,rand_col], start=c(2016,1,1), frequency=7)
c_title=paste0("Decomposition of time series, frequency=7, column = ",rand_col)
plot(stl(small_tsw_rand, s.window = 'periodic'), main = c_title)
# look at correlation by lags = 7 (days in a week)
c_title=paste0("Time series frequency = 7, column ",rand_col)
acf(small_tsw_rand,main=c_title)
Acf(small_tsw_rand,main=c_title)


#**********************************************
# Get first submission working 
#**********************************************
# let's use Doc Larry's starter 
# code to work thru submission issues
#_______________________________
fcast1=forecast(ets(ts_df[,1]),39)$mean
#simple ETS
for (i in 2:829) {
  tmp_fc=forecast(ets(ts_df[,i]),39)$mean
  fcastdl=cbind(tmp_fc,fcast1)
}
colnames(fcastdl)=colnames(ts_df)

make_submission(fcastdl, "baseline")
#________________________________

#---------------------------------------------------------------------------
#-------------------------------------------------------------------
# we can see from the earlier random STL graphs, the series may
# not be stationary, so first we have to check for that before
# we can use ARIMA
#-------------------------------------------------------------------
#---------------------------------------------------------------------------
x=ts(train_ar[,2],start=c(2016,1,1), frequency=365)
adt<- adf.test(x, alternative = 'stationary')
kpt <-kpss.test(x)
nst<- nsdiffs(x)

# ok, so the first restaurant is OK, let's check the rest
for(i in 3:830){
  x=ts(train_ar[,i],start=c(2016,1,1), frequency=365)
  nst<- nsdiffs(x)
  if (nst > 0) {
    print(paste0("Non-stationary ", i))
  }
}
print(paste0("nsdiff(x) = ", nst))
# cool the series is determined to be stationary


#***************************************************************************
#--------------------------------- BEST MODEL   ----------------------------
# Ultimately, this was my best model.  
# NOTE: it takes a long time to run
# yes, this is built using the training data not the full data set, I talk
# about it in the paper.
#---------------------------------------------------------------------------
##***************************************************************************


#-------------------------------------------------------------------
# Arima (1,1,7)(1,2,7)
#-------------------------------------------------------------------
#look the train/test result for 39 days for store1
# %>% fliter construct from Stackoverflow
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
ggtsdisplay(ts(df_visits$visitors), plot.type = c('partial'))
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts=tsclean(ts(df_train$visitors))
fit117_127 <- Arima(tmp_ts, order=c(1,1,7), seasonal = list(order=c(1,2,7)))
fcast117_127 <- forecast(fit117_127, h=39)$mean
accuracy(fcast117_127, df_test$visitors)
acf(fcast117_127)
pacf(fcast117_127)
plot(fit117_127$residuals, main="Residuals, Store1, Arima (1,1,7)(1,2,7)")
summary(fit117_127)
Box.test(fcast117_127)
Box.test(fcast117_127, type = 'Lj')
ggtsdisplay(tmp_ts, plot.type = c('partial'))


# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
fit117_127 <- Arima(df_train$visitors, order=c(1,1,7), seasonal = list(order=c(1,2,7)))
fcast117_127 <- forecast(fit117_127, h=39)$mean
summary(fcast117_127)

# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
  tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
  fit117_127 <- Arima(df_train$visitors, order=c(1,1,7), seasonal = list(order=c(1,2,7)))
  fcast_i <- forecast(fit117_127, h=39)$mean
  fcast117_127<-cbind.data.frame(fcast_i, fcast117_127)
}

colnames(fcast117_127)=colnames(ts_df)
fcast117_127<-as.matrix(fcast117_127)
make_submission(fcast117_127, "Arima_117_127")
plot(fit117_127$residuals, main="Arima (1,1,7)(1,2,7) Residuals")
hist(fit117_127$residuals, main="Histogram Arima (1,1,7)(1,2,7) Residuals")

#**************************** End of best model **************************************


#=========================================================================================
#=========================================================================================
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#                     THE OTHER EXPERIMENTS
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# all of the other things I tried, without any great success.
# The first section is using the data I created as a dcast of the 
# raw data. 
#
# The second section uses slightly more nuanced data prep
# Code is repeated with only the model beings changed,
# should have made a function, but didn't think of it until it
# would have taken mode time than I felt it was worth to "retro-fit"
#
#**********************************************
# try other Time Series approaches
#**********************************************

# Basic ETS
#-------------------------------------------------------------------
store1_all=ts(train_ar[,2],start=c(2016,1,1), frequency=365)
store1_train=ts(train_ar[1:439,2],start=c(2016,1,1), frequency=365)
store1_tst=ts(train_ar[440:478,2],start=c(2016,1,1), frequency=365)
f1=ets(store1_train,h=39) #get first forecast
fcast1=forecast(f1, h=39)$mean
accuracy(fcast1, store1_all[440:478])

f1=ets(ts_df[,1])
fcast1=forecast(f1, h=39)$mean
#simple ETS
for (i in 2:829) {
  f1=ets(ts_df[,i])
  tmp_fc=forecast(f1, h=39)$mean 
  fcast1=cbind(tmp_fc, fcast1)
} 

colnames(fcast1)=colnames(ts_df)
make_submission(fcast1, "fcast1")


# Random walk with drift
#-------------------------------------------------------------------
# Start with the first restaurant in the list
# make a sample forecast and get accuracy info
store1_all=ts(train_ar[,2],start=c(2016,1,1), frequency=365/7)
store1_train=ts(train_ar[1:439,2],start=c(2016,1,1), frequency=365/7)
store1_tst=ts(train_ar[440:478,2],start=c(2016,1,1), frequency=365/7)
f2b=rwf(store1_train, h=39,drift=TRUE)  #get first forecast
fcast2b=f2b$mean
accuracy(f2b, store1_all)

store1=ts(train_ar[,2],start=c(2016,1,1), frequency=365/7)
f2b=rwf(store1, h=39,drift=TRUE)  #get first forecast
fcast2b=f2b$mean[1:39]
for(i in 3:830){
  small_df=train_ar[,i]
  small_ts=ts(small_df, start=c(2016,1,1), frequency=365/7)
  fcast_i <- rwf(store1, h=39,drift=TRUE) 
  fcast2b=cbind(fcast_i$mean[1:39],fcast2b)
}

fcast2b[is.na(fcast2b)] <-0
colnames(fcast2b)=colnames(ts_df)

make_submission(fcast2b, 'rwf_2_drift')


#-------------------------------------------------------------------
#       naive
#-------------------------------------------------------------------
# Start with the first restaurant in the list
# make a sample forecast and get accuracy info
store1_all=ts(train_ar[,2],start=c(2016,1,1), frequency=365/7)
store1_train=ts(train_ar[1:439,2],start=c(2016,1,1), frequency=365/7)
store1_tst=ts(train_ar[440:478,2],start=c(2016,1,1), frequency=365/7)
stl_small = stlm(store1_train, s.window = 'periodic')
f3=forecast(stlm(store1_train, s.window = 'periodic'), h=39/7, method = 'naive')  #get first forecast
#f3=forecast(stl_small, h=39, method = 'naive')  #get first forecast
accuracy(f3, store1_all)


store1=ts(train_ar[,2],start=c(2016,1,1), frequency=365/7)
stl_small = stl(store1, s.window = 'periodic')
f3=forecast(stl_small, h=6, method = 'naive')  #get first forecast
fcast3=f3$mean[1:39]
for(i in 3:830){
  small_df=train_ar[,i]
  small_ts=ts(small_df, start=c(2016,1,1), frequency=365/7)
  stl_small = stl(small_ts, s.window = 'periodic')
  fcast_i <- forecast(stl_small, h=6, method = 'naive')
  fcast3=cbind(fcast_i$mean[1:39],fcast3)
}

fcast3[is.na(fcast3)] <-0
colnames(fcast3)=colnames(ts_df)

make_submission(fcast3, "naive")

#-------------------------------------------------------------------
#   ETS - period = 7 days
#-------------------------------------------------------------------
# Start with the first restaurant in the list
# make a sample forecast and get accuracy info
store1_all=ts(train_ar[,2],start=c(2016,1,1), frequency=7)
store1_train=ts(train_ar[1:439,2],start=c(2016,1,1), frequency=7)
store1_tst=ts(train_ar[440:478,2],start=c(2016,1,1), frequency=7)
mod_small = ets(store1_train)
f4=forecast(mod_small, h=39)  #get first forecast
accuracy(f4, store1_all)

# get the predictions
store1=ts(train_ar[,2],start=c(2016,1,1), frequency=7)
mod_small = ets(store1)
f4=forecast(mod_small, h=39)  #get first forecast
fcast4=f4$mean[1:39]
for(i in 3:830){
  print(i)
  small_df=train_ar[,i]
  small_ts=ts(small_df, start=c(2016,1,1), frequency=7)
  mod_small = ets(small_ts)
  fcast_i=forecast(mod_small, h=39)  #get first forecast
  fcast4=cbind(fcast_i$mean[1:39],fcast4)
}

fcast4[is.na(fcast4)] <-0
colnames(fcast4)=colnames(ts_df)
make_submission(fcast4, 'ets_7_days')

#-------------------------------------------------------------------
# trying auto.Arima
#-------------------------------------------------------------------
store1_all=ts(train_ar[,2],start=c(2016,1,1), frequency=365)
store1_train=ts(train_ar[1:439,2],start=c(2016,1,1), frequency=365)
store1_tst=ts(train_ar[440:478,2],start=c(2016,1,1), frequency=365)
mod_small = auto.arima(store1_train)
f6=forecast(mod_small, h=39)  #get first forecast
accuracy(f6, store1_tst[1:39])
acf(f6)
pacf(f6)
plot(f6)

store1=ts(train_ar[,2],start=c(2016,1,1), frequency=365)
mod_small = auto.arima(store1)
f6=forecast(mod_small, h=39)  #get first forecast
fcast6=f6$mean[1:39]
for(i in 3:830){
  small_df=train_ar[,i]
  small_ts=ts(small_df, start=c(2016,1,1), frequency=365)
  mod_small = auto.arima(small_ts)
  fcast_i=forecast(mod_small, h=39)  #get first forecast
  fcast6=cbind(fcast_i$mean[1:39],fcast6)
}

fcast6[is.na(fcast6)] <-0
colnames(fcast6)=colnames(ts_df)
make_submission(fcast6, "auto_Arima")


#-------------------------------------------------------------------
# trying Arima with no seasonality
#-------------------------------------------------------------------
#look at Store1
store1_all=ts(train_ar[,2],start=c(2016,1,1), frequency=365)
store1_train=ts(train_ar[1:439,2],start=c(2016,1,1), frequency=365)
store1_tst=ts(train_ar[440:478,2],start=c(2016,1,1), frequency=365)
mod_small = auto.arima(store1_train)
f7=forecast(mod_small, h=39)  #get first forecast
accuracy(f7, store1_all[440:478])
summary(f7)
plot(f7)

store1=ts(train_ar[,2],start=c(2016,1,1), frequency=365)
mod_small = auto.arima(store1, seasonal = FALSE)
f7=forecast(mod_small, h=39)  #get first forecast
fcast7=f7$mean[1:39]
for(i in 3:830){
  print(i)
  small_df=train_ar[,i]
  small_ts=ts(small_df, start=c(2016,1,1), frequency=365)
  mod_small = auto.arima(small_ts, seasonal = FALSE)
  fcast_i=forecast(mod_small, h=39)  #get first forecast
  fcast7=cbind(fcast_i$mean[1:39],fcast7)
}

fcast7[is.na(fcast7)] <-0
colnames(fcast7)=colnames(ts_df)
make_submission(fcast7, "auto_arima_seas_no")

#-------------------------------------------------------------------
# trying Holt-Winters, additive on the dcast data
#-------------------------------------------------------------------

# get test/train split
store1_all=ts(train_ar[,2],start=c(2016,1,1), frequency=7)
store1_train=ts(train_ar[1:439,2],start=c(2016,1,1), frequency=7)
store1_tst=ts(train_ar[440:478,2],start=c(2016,1,1), frequency=7)
fit17 <- hw(store1_train, seasonal = "additive", h=39)
fcast17 <- forecast(fit17, h=39)$mean
accuracy(fit17, store1_all)
acf(fcast17)
plot(fit17$model$residuals, main="Residuals, Store1, Holt_Winters_add with dcast data")

# get the first prediction
store1_all=ts(train_ar[,2],start=c(2016,1,1), frequency=7)
fit17 <- hw(store1_all, seasonal = "additive", h=39)
fcast17 <- forecast(fit17, h=39)$mean

# now get the rest
for(i in 3:830){
  print(i)
  small_df=train_ar[,i]
  small_ts=ts(small_df, start=c(2016,1,1), frequency=7)
  fit17 <- hw(store1_all, seasonal = "additive", h=39)
  fcast_i <- forecast(fit17, h=39)$mean
  fcast17=cbind(fcast_i,fcast17)
}

colnames(fcast17)=colnames(ts_df)
#fcast16<-as.matrix(fcast16)
make_submission(fcast17, "HW_add_dcast_data")

#---------------------------------------------------------------------------
# trying tbats which I stumbled across in my research
# https://robjhyndman.com/hyndsight/dailydata/
#---------------------------------------------------------------------------

store1 <- msts(train_ar[,2], seasonal.periods=c(7,365.25))
f8p<- tbats(store1)
fcast8 <- forecast(f8p, h=39)
plot(fcast8)


store1_all=msts(train_ar[,2], seasonal.periods=c(7,365.25))
store1_train=msts(train_ar[1:439,2],seasonal.periods=c(7,365.25))
store1_tst=msts(train_ar[440:478,2],seasonal.periods=c(7,365.25))
mod_small =  tbats(store1_train)
f8p=forecast(mod_small, h=39)  #get first forecast
plot(f8p)
accuracy(f8p, store1_all)


store1=msts(train_ar[,2], seasonal.periods=c(7,365.25))
mod_small = tbats(store1_train)
f8p=forecast(mod_small, h=39)  #get first forecast
fcast8=f8p$mean[1:39]
for(i in 3:830){
  print(i)
  small_df=train_ar[,i]
  small_ts=msts(small_df,seasonal.periods=c(7,365.25))
  mod_small = tbats(store1_train)
  fcast_i=forecast(mod_small, h=39)  #get first forecast
  fcast8=cbind(fcast_i$mean[1:39],fcast8)
}

fcast8[is.na(fcast8)] <-0
colnames(fcast8)=colnames(ts_df)
make_submission(fcast8, "Tbats1_A")

#---------------------------------------------------------------------------
# tbats with (hopefullly) better parameter choices
#---------------------------------------------------------------------------
# get test/train split
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
split_point=(478-39)
df_train <- df_visits[start_index:split_point,]
df_test <- df_visits[(split_point+1):478,]
tmp_msts <-  msts(df_train$visitors, seasonal.periods=c(7,365.25))
fit_8p <- tbats(tmp_msts, use.box.cox = TRUE, use.trend = TRUE, seasonal.periods = 365.25/7, 
                use.arma.errors = TRUE, use.parallel=TRUE, num.cores=2)
fcast_8p <- forecast(fit_8p, h=39)$mean
tmp_chk=msts(df_test$visitors, seasonal.periods=c(7,365.25))
accuracy(fcast_8p, tmp_chk[1:39])
acf(fcast_8p)
#plot(fit_8p$model$residuals, main="Residuals, Store1, Holt_Winters_multi")

# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
max_date=as.Date(max(this_store$visit_date))
num_obs=(dim(this_store))[1]
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
df_visits <- df_visits[start_index:478,]
tmp_msts <-  msts(df_visits$visitors, seasonal.periods=c(7,365.25))
fit_8p <-tbats(tmp_msts, use.box.cox = TRUE, use.trend = TRUE, seasonal.periods = 365.25/7, 
               use.arma.errors = TRUE, use.parallel=TRUE, num.cores=2)
fcast_8p <- forecast(fit_8p, h=39)$mean

# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
  df_visits <- df_visits[start_index:478,]
  tmp_msts <-  msts(df_visits$visitors, seasonal.periods=c(7,365.25))
  fit_8p <- tbats(tmp_msts, use.box.cox = TRUE, use.trend = TRUE, seasonal.periods = 365.25/7, use.arma.errors = TRUE, 
                  use.parallel=TRUE, num.cores=2)
  fcast_i <- forecast(fit_8p, h=39)$mean
  fcast_8p<-cbind.data.frame(fcast_i, fcast_8p)
}

colnames(fcast_8p)=colnames(ts_df)
make_submission(fcast_8p, "Tbats8p")

#*****************************************************************************************
#
#=========================================================================================
# Let's try handling the data differently, dcast is masking missing data when id does
# the summation of visitors, so let's try something else.  Falling back to Doc Larry's
# data, until I can figure out how to do the aggregation I want
#=========================================================================================


# ***** NOTE: the imputation via the "mice" package is bloody slow, 
# ***** run at your own risk
#---------------------------------------------------------------------------
#look the train/test result for 39 days for store1
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
tmp_df <- mice(df_visits,m=5,maxit=50,meth='pmm',seed=13)
df_visits<-mice::complete(tmp_df,1)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
fit1 <- auto.arima(tsclean(ts(df_train$visitors, frequency = 7)))
fcast9 <- forecast(fit1, h=39)$mean
accuracy(fcast9, df_test$visitors)
acf(fcast9)

# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
#df_visits$visitors<- with(df_visits, impute(visitors, 'random'))
df_visits$visit_date <- as.character(df_visits$visit_date)
tmp_df <- mice::complete(mice(df_visits,m=5,maxit=5,meth='pmm',seed=13))
df_visits<-mice::complete(tmp_df)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
fit9 <- auto.arima(tsclean(ts(df_train$visitors, frequency = 7)))
fcast9 <- forecast(fit9, h=39)$mean
summary(fcast9)

# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  tmp_df <- mice(df_visits,m=5,maxit=5,meth='pmm',seed=13)
  df_visits<-complete(tmp_df)
  fit9 <- auto.arima(tsclean(ts(df_visits$visitors, frequency = 7)))
  fcast_i <- forecast(fit9, h=39)$mean
  fcast9<-cbind.data.frame(fcast_i, fcast9)
     }
   
colnames(fcast9)=colnames(ts_df)
fcast9<-as.matrix(fcast9)
make_submission(fcast9, "Arima_mice")
     
#-------------------------------------------------------------------
# trying ETS with imputation
#-------------------------------------------------------------------
stores=unique(clean_df$air_store_id)
min_date = as.Date(min(this_store$visit_date))

#look the train/test result for 39 days for store1
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
tmp_df <- mice(df_visits,m=2,maxit=2,meth='pmm',seed=13)
df_visits<-mice::complete(tmp_df,1)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
fit1 <- ets(tsclean(ts(df_train$visitors, frequency = 7)))
fcast10 <- forecast(fit1, h=39)$mean
accuracy(fcast10, df_test$visitors)

# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
#df_visits$visitors<- with(df_visits, impute(visitors, 'random'))
df_visits$visit_date <- as.character(df_visits$visit_date)
tmp_df <- mice(df_visits,m=2,maxit=2,meth='pmm',seed=13)
df_visits<-mice::complete(tmp_df,1)
df_impute<-df_visits
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
fit1 <- ets(tsclean(ts(df_train$visitors, frequency = 7)))
fcast10 <- forecast(fit1, h=39)$mean

# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  tmp_df <- mice(df_visits,m=2,maxit=2,meth='pmm',seed=13)
  df_visits<-mice::complete(tmp_df,1)
  df_impute <- rbind(df_impute, df_visits)
  fit <- ets(tsclean(ts(df_visits$visitors, frequency = 7)))
  fcast_i <- forecast(fit, h=39)$mean
  fcast10<-cbind.data.frame(fcast_i, fcast10)
}

colnames(fcast10)=colnames(ts_df)
fcast10<-as.matrix(fcast10)
make_submission(fcast10, "ets_mice_tsclean_2")

imputed <- dcast(df_impute, visit_date~air_store_id)


#-------------------------------------------------------------------
# trying ETS with imputation=mean, range based on actual start
#-------------------------------------------------------------------

# get test/train split
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
split_point=(478-39)
df_train <- df_visits[start_index:split_point,]
df_test <- df_visits[(split_point+1):478,]
fit11 <- ets(tsclean(ts(df_train$visitors, frequency = 7, start = min_date)))
fcast11 <- forecast(fit11, h=39)$mean
accuracy(fcast11, df_test$visitors)
acf(fcast11)

# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
max_date=as.Date(max(this_store$visit_date))
num_obs=(dim(this_store))[1]
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
df_impute<-df_visits
fit11 <- ets(tsclean(ts(df_train$visitors, frequency = 7, start=min_date)))
fcast11 <- forecast(fit11, h=39)$mean


# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
  df_impute <- rbind(df_impute, df_visits)
  fit11 <- ets(tsclean(ts(df_visits$visitors, frequency = 7, start = min_date)))
  fcast_i <- forecast(fit11, h=39)$mean
  fcast11<-cbind.data.frame(fcast_i, fcast11)
}

colnames(fcast11)=colnames(ts_df)
fcast11<-as.matrix(fcast11)
make_submission(fcast11, "ets_by_start_date_tsclean_v3")
# imputed <- dcast(df_impute, visit_date~air_store_id)
# write.csv(imputed, "ets_by_start_date_tsclean_dat.csv")


#-------------------------------------------------------------------
# trying Holt-Winters
#-------------------------------------------------------------------

# get test/train split
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
split_point=(478-39)
df_train <- df_visits[start_index:split_point,]
df_test <- df_visits[(split_point+1):478,]
fit12 <- hw(ts(df_train$visitors, frequency = 7, start = min_date), seasonal = "additive")
fcast12 <- forecast(fit11, h=39)$mean
accuracy(fcast12, df_test$visitors)
acf(fcast12)
plot(fit12$model$residuals, main="Residuals, Store1, Holt_Winters_add")

# get the first prediction
# also, saving the imputed data set because I want to look at it
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
max_date=as.Date(max(this_store$visit_date))
num_obs=(dim(this_store))[1]
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
df_impute<-df_visits
tmp_ts <- ts(df_train$visitors, frequency = 7, start = min_date)
fit12 <- hw(tmp_ts, seasonal = "additive", h=39)
fcast12 <- forecast(fit12, h=39)$mean


# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
  df_impute <- rbind(df_impute, df_visits)
  tmp_ts <- ts(df_train$visitors, frequency = 7, start = min_date)
  fit12 <- hw(tmp_ts, seasonal = "additive", h=39)
  fcast_i <- forecast(fit12, h=39)$mean
  fcast12<-cbind.data.frame(fcast_i, fcast12)
}

colnames(fcast12)=colnames(ts_df)
fcast12<-as.matrix(fcast12)
make_submission(fcast12, "Holt_Winters_add")


#-------------------------------------------------------------------
# trying ETS with imputation=mean, range based on actual start
# trying transformation of visitors as log1p
#-------------------------------------------------------------------

# get test/train split
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
df_visits$visitors <- log1p(df_visits$visitors)
split_point=(478-39)
df_train <- df_visits[start_index:split_point,]
df_test <- df_visits[(split_point+1):478,]
fit15 <- ets(tsclean(ts(df_train$visitors, frequency = 7, start = min_date)))
fcast15_l <- forecast(fit15, h=39)$mean
fcast15 <- expm1(fcast15_l)
accuracy(fcast15, df_test$visitors)
acf(fcast15)

# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
max_date=as.Date(max(this_store$visit_date))
num_obs=(dim(this_store))[1]
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
df_visits$visitors <- log1p(df_visits$visitors)
fit15 <- ets(tsclean(ts(df_visits$visitors, frequency = 7, start=min_date)))
fcast15_l <- forecast(fit15, h=39)$mean
fcast15 <- expm1(fcast15_l)


# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
  df_visits$visitors <- log1p(df_visits$visitors)
  fit15 <- ets(tsclean(ts(df_visits$visitors, frequency = 7, start = min_date)))
  fcast_l <- forecast(fit15, h=39)$mean
  fcast_i <- expm1(fcast_l)
  fcast15<-cbind.data.frame(fcast_i, fcast15)
}

colnames(fcast15)=colnames(ts_df)
fcast15<-as.matrix(fcast15)
make_submission(fcast15, "ets_log1pVisitors")

#-------------------------------------------------------------------
# trying Holt-Winters, multiplicative
#-------------------------------------------------------------------
# get test/train split
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
split_point=(478-39)
df_train <- df_visits[start_index:split_point,]
df_test <- df_visits[(split_point+1):478,]
tmp_ts <- ts(df_train$visitors, frequency = 7, start = min_date)
fit_16org <- hw(tmp_ts, seasonal = "multiplicative", h=39)
fcast_16org <- forecast(fit_16org, h=39)$mean
accuracy(fit_16org, df_test$visitors)
acf(fcast_16org)
#plot(fit_16org$model$residuals, main="Residuals, Store1, Holt_Winters_multi")

# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
max_date=as.Date(max(this_store$visit_date))
num_obs=(dim(this_store))[1]
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
df_visits <- df_visits[start_index:478,]
tmp_ts <- ts(df_visits$visitors, frequency = 7, start = min_date)
fit_16org <- hw(tmp_ts, seasonal = "multiplicative", h=39)
fcast_16org <- forecast(fit_16org, h=39)$mean

# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
  df_visits <- df_visits[start_index:478,]
  tmp_ts <- ts(df_visits$visitors, frequency = 7, start = min_date)
  fit_16org <- hw(tmp_ts, seasonal = "multiplicative", h=39)
  fcast_i <- forecast(fit_16org, h=39)$mean
  fcast_16org<-cbind.data.frame(fcast_i, fcast_16org)
}

colnames(fcast_16org)=colnames(ts_df)
fcast_16org<-as.matrix(fcast_16org)
make_submission(fcast_16org, "Holt_Winters_multi_16org")

#-------------------------------------------------------------------
# trying Holt-Winters, multiplicative - note the mistaken use
# of the training set in fitting the full model, gave better 
# results than the full set
#-------------------------------------------------------------------

# get test/train split
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
split_point=(478-39)
df_train <- df_visits[start_index:split_point,]
df_test <- df_visits[(split_point+1):478,]
tmp_ts <- ts(df_train$visitors, frequency = 7, start = min_date)
fit16 <- hw(tmp_ts, seasonal = "multiplicative", h=39)
fcast16 <- forecast(fit16, h=39)$mean
accuracy(fit16, df_test$visitors)
acf(fcast16)
pacf(fcast16)
plot(fit16$model$residuals, main="Residuals, Store1, Holt_Winters_multi")
plot(fit16, main='Forecast forStore1, Holt_Winters_multi')
plot(stl(fcast16))

# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
max_date=as.Date(max(this_store$visit_date))
num_obs=(dim(this_store))[1]
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
df_impute<-df_visits
tmp_ts <- ts(df_train$visitors, frequency = 7, start = min_date)
fit16 <- hw(tmp_ts, seasonal = "multiplicative", h=39)
fcast16 <- forecast(fit16, h=39)$mean

# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
  df_impute <- rbind(df_impute, df_visits)
  tmp_ts <- ts(df_train$visitors, frequency = 7, start = min_date)
  fit16 <- hw(tmp_ts, seasonal = "multiplicative", h=39)
  fcast_i <- forecast(fit16, h=39)$mean
  fcast16<-cbind.data.frame(fcast_i, fcast16)
}

colnames(fcast16)=colnames(ts_df)
fcast16<-as.matrix(fcast16)
make_submission(fcast16, "Holt_Winters_multi")

#-------------------------------------------------------------------
# trying Holt-Winters, now with damping
#-------------------------------------------------------------------
# get test/train split
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
split_point=(478-39)
df_train <- df_visits[start_index:split_point,]
df_test <- df_visits[(split_point+1):478,]
fit18 <- hw(ts(df_train$visitors, frequency = 7, start = min_date), seasonal = "multiplicative", damped = TRUE, h=39)
fcast18 <- forecast(fit18, h=39)$mean
accuracy(fit18, df_test$visitors)
acf(fcast18)
plot(fit18$model$residuals, main="Residuals, Store1, Holt_Winters_multi_damped")

# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
max_date=as.Date(max(this_store$visit_date))
num_obs=(dim(this_store))[1]
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
tmp_ts <- ts(df_visits$visitors, frequency = 7, start = min_date)
fit18 <- hw(tmp_ts, seasonal = "multiplicative", damped = TRUE,h=39)
fcast18 <- forecast(fit18, h=39)$mean


# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
  df_impute <- rbind(df_impute, df_visits)
  tmp_ts <- ts(df_visits$visitors, frequency = 7, start = min_date)
  fit18 <- hw(tmp_ts, seasonal = "multiplicative", damped = TRUE, h=39)
  fcast_i <- forecast(fit18, h=39)$mean
  fcast18<-cbind.data.frame(fcast_i, fcast18)
}

colnames(fcast18)=colnames(ts_df)
fcast18<-as.matrix(fcast18)
make_submission(fcast18, "Holt_Winters_multi_damped")


#-------------------------------------------------------------------
# repeat Holt-Winters, with damping, adding tsclean to remove outliers
#-------------------------------------------------------------------

# get test/train split
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
split_point=(478-39)
df_train <- df_visits[start_index:split_point,]
df_test <- df_visits[(split_point+1):478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
fit18_A <- hw(tmp_ts, seasonal = "multiplicative", h=39)
fcast18_A <- forecast(fit18_A, h=39)$mean
accuracy(fit18_A, df_test$visitors)
acf(fcast18_A)
plot(fit18_A$model$residuals, main="Residuals, Store1, Holt_Winters_multi_tsclean")

# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
max_date=as.Date(max(this_store$visit_date))
num_obs=(dim(this_store))[1]
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
fit18_A <- hw(tmp_ts, seasonal = "multiplicative", h=39)
fcast18_A <- forecast(fit18_A, h=39)$mean


# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
  df_impute <- rbind(df_impute, df_visits)
  tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
  fit18_A <- hw(tmp_ts, seasonal = "multiplicative", h=39)
  fcast_i <- forecast(fit18_A, h=39)$mean
  fcast18_A<-cbind.data.frame(fcast_i, fcast18_A)
}

colnames(fcast18_A)=colnames(ts_df)
fcast18_A<-as.matrix(fcast18_A)
make_submission(fcast18_A, "Holt_Winters_multi_tsclean")

#-------------------------------------------------------------------
# trying Holt-Winters, multiplicative, exponential
#-------------------------------------------------------------------
# get test/train split
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
split_point=(478-39)
df_train <- df_visits[start_index:split_point,]
df_test <- df_visits[(split_point+1):478,]
tmp_ts <- ts(df_train$visitors, frequency = 7, start = min_date)
fit16_EXP <- hw(tmp_ts, seasonal = "multiplicative", exponential = TRUE, h=39)
fcast16_EXP <- forecast(fit16_EXP, h=39)$mean
accuracy(fit16_EXP, df_test$visitors)
acf(fcast16_EXP)
plot(fit16_EXP$model$residuals, main="Residuals, Store1, Holt_Winters_multi_exp")

# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
max_date=as.Date(max(this_store$visit_date))
num_obs=(dim(this_store))[1]
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
start_index=(as.duration (first_date %--% min_date) / ddays(1))+1
for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
tmp_ts <- ts(df_visits$visitors, frequency = 7, start = min_date)
fit16_EXP <- hw(tmp_ts, seasonal = "multiplicative",exponential = TRUE,  h=39)
fcast16_EXP <- forecast(fit16_EXP, h=39)$mean


# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  for(j in start_index:478){df_visits$visitors[j][which(is.na(df_visits$visitors[j]))] <- mean(df_visits$visitors,na.rm = TRUE)}
  df_impute <- rbind(df_impute, df_visits)
  tmp_ts <- ts(df_visits$visitors, frequency = 7, start = min_date)
  fit16_EXP <- hw(tmp_ts, seasonal = "multiplicative", exponential = TRUE, h=39)
  fcast_i <- forecast(fit16_EXP, h=39)$mean
  fcast16_EXP<-cbind.data.frame(fcast_i, fcast16_EXP)
}

colnames(fcast16_EXP)=colnames(ts_df)
fcast16_EXP<-as.matrix(fcast16_EXP)
make_submission(fcast16_EXP, "Holt_Winters_multi_exp")


#-------------------------------------------------------------------
# Arima (1,1,7)
#-------------------------------------------------------------------
#look the train/test result for 39 days for store1
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts=tsclean(ts(df_train$visitors))
fit717 <- Arima(tmp_ts, order=c(1,1,7), seasonal = list(order=c(1,2,7)))
fcast717 <- forecast(fit717, h=39)$mean
accuracy(fcast717, df_test$visitors)
acf(fcast717)
pacf(fcast717)
plot(fit717$residuals, main="Residuals, Store1, Arima (1,1,7)")
summary(fit717)
ggtsdisplay(fcast717, plot.type = c('partial'))


# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
fit717 <- Arima(df_train$visitors, order=c(1,1,7), seasonal = list(order=c(1,2,7)))
fcast717 <- forecast(fit717, h=39)$mean
summary(fcast717)

# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
  tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
  fit717 <- Arima(df_train$visitors, order=c(1,1,7), seasonal = list(order=c(1,2,7)))
  fcast_i <- forecast(fit717, h=39)$mean
  fcast717<-cbind.data.frame(fcast_i, fcast717)
}

colnames(fcast717)=colnames(ts_df)
fcast717<-as.matrix(fcast717)
make_submission(fcast717, "Arima_717_v2")

#-------------------------------------------------------------------
# Arima (1,1,7)(1,2,7)  - using winsorized data
#-------------------------------------------------------------------
#look the train/test result for 39 days for store1
this_store=wins_df %>% filter(wins_df$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
tmp_all <- tsclean(ts(df_visits$visitors, frequency = 7, start = min_date), replace.missing = TRUE)
fit117v2 <- Arima(tmp_ts, order=c(1,1,7), seasonal = list(order=c(1,2,7)))
fcast117v2 <- forecast(fit117v2, h=39)$mean
accuracy(fit117v2$fitted, df_test$visitors)
acf(fit117v2$residuals)
pacf(fit117v2$residuals)
plot(fit117v2$residuals, main="Residuals, Store1, Arima 1,1,7")
Box.test(fit117v2$residuals)
plot(forecast(fit117v2))
summary(fit117v2)
Box.test(fit117v2$residuals, type="Lj")


# get the first prediction
this_store=wins_df %>% filter(wins_df$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date), replace.missing = TRUE)
fit117v2 <- Arima(tmp_ts, order=c(1,1,7), seasonal = list(order=c(1,2,7)))
fcast117v2 <- forecast(fit117v2, h=39)$mean

# now get the rest
for(i in 2:829){
  print(i)
  this_store=wins_df %>% filter(wins_df$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
  tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date), replace.missing = TRUE)
  fit117v2 <- Arima(df_train$visitors, order=c(1,1,7), seasonal = list(order=c(1,2,7)))
  fcast_i <- forecast(fit117v2, h=39)$mean
  fcast117v2<-cbind.data.frame(fcast_i, fcast117v2)
}

colnames(fcast117v2)=colnames(ts_df)
fcast117v2<-as.matrix(fcast117v2)
make_submission(fcast117v2, "Arima_wins_df")


#-------------------------------------------------------------------
# Arima (0,1,1) 
#-------------------------------------------------------------------
#look the train/test result for 39 days for store1
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
tmp_all <- tsclean(ts(df_visits$visitors, frequency = 7, start = min_date))
fit011 <- Arima(tmp_ts, order=c(0,1,1))
fcast011 <- forecast(fit011, h=39)$mean
accuracy(fcast011, tmp_all[440:478])
acf(fit011$residuals)
pacf(fit011$residuals)
plot(fit011$residuals, main="Residuals, Store1, Arima 0,1,1")
summary(fit011)
Box.test(fit011$residuals)
Box.test(fit011$residuals, type="Lj")
plot(forecast(fit011))
ggtsdisplay(fcast011, plot.type = c('partial'))

# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
fit011 <- Arima(tmp_ts, order=c(0,1,1))
fcast011 <- forecast(fit011, h=39)$mean

# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
  tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
  fit011 <- Arima(tmp_ts, order=c(0,1,1))
  fcast_i <- forecast(fit011, h=39)$mean
  fcast011<-cbind.data.frame(fcast_i, fcast011)
}
acf(fit011$residuals)
pacf(fit011$residuals)
plot(fit011$residuals, main="Residuals, Store1, Arima 0,1,1")
Box.test(fit011$residuals)
plot(forecast(fit011))
summary(fit011)
Box.test(fit011$residuals, type="Lj")


colnames(fcast011)=colnames(ts_df)
fcast011<-as.matrix(fcast011)
make_submission(fcast011, "Arima_101")

tsdisplay(fcast011[,829], main = 'Arima (0,1,1) store 829')

#-------------------------------------------------------------------
# Arima (1,1,1) 
#-------------------------------------------------------------------
#look the train/test result for 39 days for store1
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
tmp_all <- tsclean(ts(df_visits$visitors, frequency = 7, start = min_date))
fit111 <- Arima(tmp_ts, order=c(1,1,1))
fcast111 <- forecast(fit111, h=39)$mean
accuracy(fcast111, tmp_all[440:478])
acf(fit111$residuals)
pacf(fit111$residuals)
plot(fit111$residuals, main="Residuals, Store1, Arima order=c(1,1,1))")
Box.test(fit111$residuals)
plot(forecast(fit111), main=' Forecast Arima order=c(1,1,1)) first store')
summary(fit111)
Box.test(fit111$residuals, type="Lj")
ggtsdisplay(fcast111, main = 'Tsdiplay for Arima order=c(1,1,1)) first store')



# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
fit111 <- Arima(tmp_ts, order=c(1,1,1))
fcast111 <- forecast(fit111, h=39)$mean

# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
  tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
  fit111 <- Arima(tmp_ts, order=c(1,1,1))
  fcast_i <- forecast(fit111, h=39)$mean
  fcast111<-cbind.data.frame(fcast_i, fcast111)
}
plot(fit111$residuals, main="Residuals, Store1, Arima order=c(1,1,1))")
Box.test(fit111$residuals)
plot(forecast(fit111), main=' Forecast Arima order=c(1,1,1))last store')
summary(fit111)
Box.test(fit111$residuals, type="Lj")
tsdisplay(fcast111[,829], main = 'Tsdiplay for Arima order=c(1,1,1)) last store')

colnames(fcast111)=colnames(ts_df)
fcast111<-as.matrix(fcast111)
make_submission(fcast117v2, "Arima_111")


#-------------------------------------------------------------------
# Arima (0,1,7) 
#-------------------------------------------------------------------
#look the train/test result for 39 days for store1
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
tmp_all <- tsclean(ts(df_visits$visitors, frequency = 7, start = min_date))
fit017 <- Arima(tmp_ts, order=c(0,1,7))
fcast017 <- forecast(fit017, h=39)$mean
accuracy(fcast017, tmp_all[440:478])
acf(fit017$residuals)
pacf(fit017$residuals)
plot(fit017$residuals, main="Residuals, Store1, Arima 0,1,7")
Box.test(fit017$residuals)
plot(forecast(fit017), main=' Forecast Arima(0,1,7) first store')
summary(fit017)
Box.test(fit017$residuals, type="Lj")
ggtsdisplay(fcast017, main = 'Tsdiplay for Arima(0,1,7) first store')



# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
fit017 <- Arima(tmp_ts, order=c(0,1,7))
fcast017 <- forecast(fit017, h=39)$mean

# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
  tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
  fit017 <- Arima(tmp_ts, order=c(0,1,7))
  fcast_i <- forecast(fit017, h=39)$mean
  fcast017<-cbind.data.frame(fcast_i, fcast017)
}
plot(fit017$residuals, main="Residuals, Store1, Arima 0,1,7")
Box.test(fit017$residuals)
plot(forecast(fit017), main=' Forecast Arima(0,1,7) last store')
summary(fit017)
Box.test(fit017$residuals, type="Lj")
tsdisplay(fcast017, main = 'Tsdiplay for Arima(0,1,7) last store')

colnames(fcast017)=colnames(ts_df)
fcast017<-as.matrix(fcast017)
make_submission(fcast117v2, "Arima_017")


#-------------------------------------------------------------------
# Arima (0,1,1)(7,0,1)  
#-------------------------------------------------------------------
#look the train/test result for 39 days for store1
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
tmp_all <- tsclean(ts(df_visits$visitors, frequency = 7, start = min_date))
fit117v2 <- Arima(tmp_ts, order=c(0,1,1), seasonal = list(order=c(7,0,1)))
fcast117v2 <- forecast(fit117v2, h=39)$mean
accuracy(fcast117v2, tmp_all[440:478])
acf(fit117v2$residuals)
pacf(fit117v2$residuals)
plot(fit117v2$residuals, main="Residuals, Store1, Arima 0,1,1 - 7,0,1)")
Box.test(fit117v2$residuals)
plot(forecast(fit117v2))
summary(fit117v2)
Box.test(fit117v2$residuals, type="Lj")
tsdisplay(diff(fcast117v2, 7))


# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
fit117v2 <- Arima(tmp_ts, order=c(0,1,1), seasonal = list(order=c(7,0,1)))
fcast117v2 <- forecast(fit117v2, h=39)$mean

# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
  tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
  fit117v2 <- Arima(tmp_ts, order=c(0,1,1), seasonal = list(order=c(7,0,1)))
  fcast_i <- forecast(fit117v2, h=39)$mean
  fcast117v2<-cbind.data.frame(fcast_i, fcast117v2)
}

colnames(fcast117v2)=colnames(ts_df)
fcast117v2<-as.matrix(fcast117v2)
make_submission(fcast117v2, "Arima_011_701")

#-------------------------------------------------------------------
# Arima (1,1,7)(1,2,7)  - Runs very, very slowly
#-------------------------------------------------------------------
#look the train/test result for 39 days for store1
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
fit117v2 <- Arima(tmp_ts, order=c(1,1,7), seasonal = list(order=c(1,2,7)))
fcast117v2 <- forecast(fit117v2, h=39)$mean
accuracy(fcast117v2, df_test$visitors)
acf(fcast117v2)
pacf(fcast117v2)
plot(fit117v2$residuals, main="Residuals, Store1, Arima (1,1,7)(1,2,7)")
summary(fit117v2)
Box.test(fit117v2$residuals, type="Lj")


# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
fit117v2 <- arima(df_train$visitors, order=c(1,1,7), seasonal = list(order=c(1,2,7)))
fcast117v2 <- forecast(fit117v2, h=39)$mean
summary(fcast117v2)

# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
  tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
  fit117v2 <- arima(df_train$visitors, order=c(1,1,7), seasonal = list(order=c(1,2,7)))
  fcast_i <- forecast(fit117v2, h=39)$mean
  fcast117v2<-cbind.data.frame(fcast_i, fcast117v2)
}

colnames(fcast117v2)=colnames(ts_df)
fcast117v2<-as.matrix(fcast117v2)
make_submission(fcast117v2, "Arima_117_127")



#************************* Explore why train best full data set  *********************
# what is max value of visitors?
clean_df$visitors[(which.max(clean_df$visitors))]
which(clean_df$visitors == max(clean_df$visitors), arr.ind = TRUE)

# OK, I have an outlier problem, let's try again with outlier cleaning
# this is primative, but I am running out of time
wins_df=clean_df
wins_df$visitors=winsor(clean_df$visitors)
wins_df$visit_date=as.Date(wins_df$visit_date)

# re-run the auto.arima model
#---------------------------------------------------------------------------
#look the train/test result for 39 days for store1

this_store=wins_df %>% filter(wins_df$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
tmp_all <- tsclean(ts(df_visits$visitors, frequency = 7, start = min_date))
fit117ol <- auto.arima(df_train[,3])
fcast117ol <- forecast(fit117ol, h=39)$mean
accuracy(fcast117ol, df_test$visitors)
acf(fit117ol$residuals)
pacf(fit117ol$residuals)
plot(fit117ol$residuals, main="Residuals, Store1, Arima 1,1,7")
plot(forecast(fit117ol))
summary(fit117ol)
Box.test(fit117ol$residuals, type="Lj")


# get the first prediction
this_store=wins_df %>% filter(wins_df$air_store_id == stores[1])
min_date = as.Date(min(this_store$visit_date))
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
tmp_ts <- tsclean(ts(df_visits$visitors, frequency = 7, start = min_date))
fit117ol <- auto.arima(tmp_ts)
fcast117ol <- forecast(fit117ol, h=39)$mean

# now get the rest
for(i in 2:829){
  print(i)
  this_store=wins_df %>% filter(wins_df$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
  tmp_ts <- tsclean(ts(df_visits$visitors, frequency = 7, start = min_date))
  fit117ol <- auto.arima(tmp_ts)
  fcast_i <- forecast(fit117ol, h=39)$mean
  fcast117ol<-cbind.data.frame(fcast_i, fcast117ol)
}

colnames(fcast117ol)=colnames(ts_df)
fcast117ol<-as.matrix(fcast117ol)
make_submission(fcast117ol, "Arima_auto_outl")



#-------------------------------------------------------------------
# Arima (1,1,7)(1,2,7)
#-------------------------------------------------------------------
#look the train/test result for 39 days for store1
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts=tsclean(ts(df_train$visitors))
fit117_127 <- Arima(tmp_ts, order=c(1,1,7), seasonal = list(order=c(1,2,7)))
fcast117_127 <- forecast(fit117_127, h=39)$mean
accuracy(fcast117_127, df_test$visitors)
acf(fcast117_127)
pacf(fcast117_127)
plot(fit117_127$residuals, main="Residuals, Store1, Arima (1,1,7)(1,2,7)")
summary(fit117_127)
Box.test(fcast117_127)
Box.test(fcast117_127, type = 'Lj')
ggtsdisplay(fcast117_127, plot.type = c('partial'))


# get the first prediction
this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[1])
df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[1]
df_visits$visit_date <- as.character(df_visits$visit_date)
df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
df_train <- df_visits[1:439,]
df_test <- df_visits[440:478,]
tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
fit117_127 <- Arima(df_train$visitors, order=c(1,1,7), seasonal = list(order=c(1,2,7)))
fcast117_127 <- forecast(fit117_127, h=39)$mean
summary(fcast117_127)

# now get the rest
for(i in 2:829){
  print(i)
  this_store=df_format_clean %>% filter(df_format_clean$air_store_id == stores[i])
  df_visits <- right_join(this_store, df_all_dates, by ="visit_date" )
  df_visits$air_store_id[is.na(df_visits$air_store_id)] <-stores[i]
  df_visits$visit_date <- as.character(df_visits$visit_date)
  df_visits$visitors[which(is.na(df_visits$visitors))] <- mean(df_visits$visitors,na.rm = TRUE)
  tmp_ts <- tsclean(ts(df_train$visitors, frequency = 7, start = min_date))
  fit117_127 <- Arima(df_train$visitors, order=c(1,1,7), seasonal = list(order=c(1,2,7)))
  fcast_i <- forecast(fit117_127, h=39)$mean
  fcast117_127<-cbind.data.frame(fcast_i, fcast117_127)
}

colnames(fcast117_127)=colnames(ts_df)
fcast117_127<-as.matrix(fcast117_127)
make_submission(fcast117_127, "Arima_117_127")

#=========================================================================================
#=========================================================================================
#                 The End
#=========================================================================================
#=========================================================================================
