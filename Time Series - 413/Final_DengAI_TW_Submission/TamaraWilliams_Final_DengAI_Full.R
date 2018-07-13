######## Predict 413, Final
######## Submitted by: Tamara Williams
########
######## DengAI competition

# For the sake of good programming hygiene, start clean
# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")


# Set working directory
#-------------------------------------------------------------------
setwd("/Users/tamtwill/NorthwesternU_MSPA/Classes/Time Series - 413/Final_DengAI")


# Load Libraries
#-------------------------------------------------------------------
library(corrplot)
library(dplyr)
library(FactoMineR)
library(fGarch)
library(forecast)
library(gridExtra)
library(psych)
library(RColorBrewer)
library(stats)
library(timeSeries)
library(vars)
library(VIM)
library(zoo)

require(caret)
require(xgboost)
require(methods)
require(data.table)
require(magrittr)
require(Ckmeans.1d.dp)


#=================================================================
# Functions
#=================================================================

# submitting the predictions
#----------------------------------------------------------------
make_submission <- function(sj, iq, m_name){
    submission = read.csv('submission_format.csv')
    predictions = append(sj,iq)
    predictions[predictions<0] = 0 
    submission$total_cases=round(predictions)
    write.csv(submission, m_name, row.names = FALSE)
    }

# tracking the test results
#----------------------------------------------------------------
track_results <- function(res_list, m_name, bind_method){
    if(bind_method==1){
      my_results=as.data.frame(res_list)
      my_results=my_results[2,]
      my_results$model_name=m_name
      my_results=bind_rows(model_comparision, my_results)
      return(my_results)
    }else{
      my_results=as.data.frame(res_list)
      my_results$model_name=m_name
      my_results=bind_rows(model_comparision, my_results)
      return(my_results)
    }
}

# from stackoverflow, getting rid of outliers
# https://stackoverflow.com/questions/10568369/removing-outliers-in-r
#----------------------------------------------------------------
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

#=================================================================
# Load data, and first prep steps
#=================================================================

## Data Loading, handle train and test together
#---------------------------------------------------------------
tr_df = read.csv('dengue_features_train.csv', stringsAsFactors = FALSE)
tr_labels = read.csv('dengue_labels_train.csv', stringsAsFactors = FALSE)
tr_df2=read.csv('dengue_features_train_combo.csv', stringsAsFactors = FALSE)
tst_df = read.csv('dengue_features_test.csv', stringsAsFactors = FALSE)

# convert week_start_date to epoch int, so you have days since 1/1/1970
# makes numeric-only models easier
#---------------------------------------------------------------
tr_df$week_start_date = round(as.numeric(as.POSIXlt(tr_df$week_start_date, format = "%Y-%m-%d", origin = "1970-01-01"))/86400)

tst_df$week_start_date = round(as.numeric(as.POSIXlt(tst_df$week_start_date, format = "%m/%d/%y", origin = "1970-01-01"))/86400)


# Seperate data by city
#---------------------------------------------------------------
sj_tr_df=dplyr::filter(tr_df, city=='sj')
sj_tr_labels = dplyr::filter(tr_labels, city == 'sj')

iq_tr_df=dplyr::filter(tr_df, city=='iq')
iq_tr_labels = dplyr::filter(tr_labels, city == 'iq')

sj_tst_df = dplyr::filter(tst_df, city=='sj')
iq_tst_df = dplyr::filter(tst_df, city=='iq')

aggr(sj_tr_df, prop=T,numbers=T)
aggr(sj_tr_labels, prop=T, numbers=T)

aggr(iq_tr_df, prop=T,numbers=T)
aggr(iq_tr_labels, prop=T, numbers=T)


# So, labels are complete, but the training sets are not.  
# Will need to do some imputation on the training data
# using the 'tidyr' package's fill function
# to carry last observation forward.  First the training, then the test data
#---------------------------------------------------------------

sj_tr_df_imp=na.locf(sj_tr_df, na.rm=FALSE)
sj_tr_df_imp=sj_tr_df_imp[,-1]
# fix up the column types since zoo turns everything to char
ix=1:23
sj_tr_df_imp[ix] = lapply(sj_tr_df_imp[ix], as.numeric)
missing_sj = dim(sj_tr_df_imp %>% filter(!complete.cases(.)))[1]

sj_tst_df_imp=na.locf(sj_tst_df, na.rm=FALSE)
sj_tst_df_imp=sj_tst_df_imp[,-1]
# fix up the column types since zoo turns everything to char
ix=1:23
sj_tst_df_imp[ix]= lapply(sj_tst_df_imp[ix], as.numeric)
sj_test_final=sj_tst_df_imp

iq_tr_df_imp=na.locf(iq_tr_df, na.rm=FALSE)
iq_tr_df_imp=iq_tr_df_imp[,-1]
# fix up the column types since zoo turns everything to char
ix=1:23
iq_tr_df_imp[ix] = lapply(iq_tr_df_imp[ix], as.numeric)
missing_iq = dim(iq_tr_df_imp %>% filter(!complete.cases(.)))[1]

iq_tst_df_imp=na.locf(iq_tst_df, na.rm=FALSE)
iq_tst_df_imp=iq_tst_df_imp[,-1]
# fix up the column types since zoo turns everything to char
ix=1:23
iq_tst_df_imp[ix] = lapply(iq_tst_df_imp[ix], as.numeric)
iq_test_final=iq_tst_df_imp

sj_df= cbind(sj_tr_labels$total_cases, sj_tr_df_imp)
colnames(sj_df)[1]="total_cases"
iq_df = cbind(iq_tr_labels$total_cases, iq_tr_df_imp)
colnames(iq_df)[1]="total_cases"

# clean-up various intermediates
#---------------------------------------------------------------
rm(iq_tr_df)
rm(sj_tr_df)
rm(iq_tr_df_imp)
rm(sj_tr_df_imp)
rm(sj_tst_df_imp)
rm(iq_tst_df_imp)
rm(iq_tst_df)
rm(sj_tst_df)
rm(tr_labels)

#=================================================================
# Data Visualization
#=================================================================

# let's take a graphical look at the data
#---------------------------------------------------------------
 all_cases=rbind(sj_tr_labels, iq_tr_labels)
 ggplot(data=all_cases, (aes(x = total_cases,fill = ..count..))) +
   geom_histogram(bins = 12) + ggtitle('Total Cases Reported') + facet_grid(~city)
 
short_sj = subset(sj_df, select=c(-2,-3,-5))
short_iq=subset(iq_df, select=c(-2,-3,-5))

sj_corList=cor(short_sj, use = "pairwise.complete.obs")
iq_corList=cor(short_iq, use = "pairwise.complete.obs")

{plot.new(); dev.off()}
corrplot(sj_corList, type="lower", method="circle",mar = c(1, 1, 1, 1), col=brewer.pal(n=10,        name="Spectral"),diag=FALSE, main="San Juan")
 corrplot(iq_corList, type="lower", method="circle", mar = c(1, 1, 1, 1), col=brewer.pal(n=10, name="Spectral"),diag=FALSE, main="Iquitos")

# courtsey of the web http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#1.%20Correlation,
# a nifty way to make diverging barchart of the correlationssj
 bar_corr1 = sort(sj_corList[21,-21]) %>%
   as.data.frame %>%
   `names<-`('correlation') %>%
   ggplot(aes(x = reorder(row.names(.), -correlation), y = correlation, fill = correlation)) +
   geom_bar(stat='identity', colour = 'black') + scale_fill_continuous(guide = FALSE) +
   scale_y_continuous(limits =  c(-.60,.60)) + labs(title = 'San Jose Correlations', x = NULL, y = NULL) + coord_flip()

 bar_corr2=sort(iq_corList[21,-21]) %>%
   as.data.frame %>%
   `names<-`('correlation') %>%
   ggplot(aes(x = reorder(row.names(.), -correlation), y = correlation, fill = correlation)) +
   geom_bar(stat='identity', colour = 'black') + scale_fill_continuous(guide = FALSE) + scale_y_continuous(limits =  c(-.60,.60)) + labs(title = 'Iquitos Correlations', x = NULL, y = NULL) + coord_flip()

grid.arrange(bar_corr1, bar_corr2, nrow = 1)
 

# get an overview of the train data
#---------------------------------------------------------------
describe(sj_df)
describe(iq_df)

 
# get rid of excess dataframes
#---------------------------------------------------------------
rm(bar_corr1)
rm(bar_corr2)
rm(iq_corList)
rm(sj_corList)
rm(missing_iq)
rm(missing_sj)
rm(short_iq)
rm(short_sj)
rm(iq_tr_labels)
rm(sj_tr_labels)
gc()


#=================================================================
# setup and final data prep steps
#=================================================================

#---------------------------------------------------------------
# init model_comparision to track how various models are doing
#---------------------------------------------------------------
model_comparision=data.frame(matrix(ncol = 9, nrow = 0))
colnames(model_comparision)<- c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1","Theil's U","model_name")


#----------------------------------------------------------------------------------------
# Make a test and train split on the two data sets for model development and evaluation
#----------------------------------------------------------------------------------------
sj_train=sj_df[1:750,]
sj_test=sj_df[751:936,]
iq_train = iq_df[1:415,]
iq_test=iq_df[416:520,]

# time series with just the total_cases
ts_sj_all=ts(sj_df[,'total_cases'], start=c(1990,04,03), frequency = 52)
ts_iq_all=ts(iq_df[,'total_cases'], start=c(2000,07,01), frequency = 52)
ts_sj_tr= ts(sj_train[,'total_cases'], start=c(1990,04,03), frequency = 52)
ts_sj_tst=ts(sj_test[,'total_cases'], start=c(2004,09,30), frequency = 52)
ts_iq_tr= ts(iq_train[,'total_cases'], start=c(2000,07,01), frequency = 52)
ts_iq_tst=ts(iq_test[,'total_cases'], start=c(2008,06,24), frequency = 52)

# time series with everything
all_ts_sj=ts(sj_df, start=c(1990,04,03), frequency = 52)
all_ts_iq=ts(iq_df, start=c(2000,07,01), frequency = 52)
train_ts_sj=ts(sj_train,start=c(1990,04,03), frequency = 52)
train_ts_iq=ts(iq_train, start=c(2000,07,01), frequency = 52)
test_ts_sj=ts(sj_test,start=c(2004,09,30), frequency = 52)
test_ts_iq=ts(iq_test, start=c(2008,06,24), frequency = 52)

ggtsdisplay(ts_sj_all, main="TS Display for San Juan Data")
ggtsdisplay(ts_iq_all, main = "TS Display for Iquitos Data")

# time series decompostion - San Juan
sj_stl=stl(ts_sj_tr, s.window = 7)
plot(sj_stl, main='San Juan STL Decomposition')
plot(ts_sj_tr, main='San Juan - Red:Seasonal, Blue:Trend', ylab= 'Cases')
lines(sj_stl$time.series[,1], col='red')
lines(sj_stl$time.series[,2], col='blue')

# time series decompostion - Iquitos
iq_stl=stl(ts_iq_tr, s.window = 7)
plot(iq_stl, main='Iquitos STL Decomposition')
plot(ts_iq_tr, main='Iquitos - Blue:Seasonal, Magenta:Trend', ylab= 'Cases')
lines(iq_stl$time.series[,1], col='cyan')
lines(iq_stl$time.series[,2], col='magenta')


# look at stability
diffs_sj=ndiffs(ts_sj_all)
diffs_iq=ndiffs(ts_iq_all)
par(mfrow=c(2,1))
plot(diff(ts_sj_all))
plot(diff(ts_iq_all))
par(mfrow=c(1,1))

rm(diffs_iq)
rm(diffs_sj)

############################################################################################################
# 
#         Best set of models
#
############################################################################################################
#============================================================================================
# XGBoost Model 3
#
# NOTE: XGB4 is the same as XGB3, only the metric used in the hyper-parameter list
# was MAE not RSME.  Curiously, the MAE model scored lower on the competition MAE
# evaluation
#============================================================================================
# find the best number of iterations to run
#---------------------------------------------------------------
n_round <- 100
n_fold <- 10
set.seed(13)
params <- list(objective = "reg:linear",
               eta = 0.2,
               max_depth = 15
)
sj_m = sj_df[,-c(1)]
sj_m = data.matrix(sj_m, rownames.force = F)
target=(sj_df$total_cases)

bst <- xgb.cv(params = params, sj_m, nrounds = n_round, nfold = n_fold, label=target, metrics = 'rmse')
plot(bst$evaluation_log$test_rmse_mean)
n_round <- which((bst$evaluation_log$test_rmse_mean == min((bst$evaluation_log$test_rmse_mean))))

# bst <- xgb.cv(params = params, sj_m, nrounds = n_round, nfold = n_fold, label=target, metrics = 'mae')
# plot(bst$evaluation_log$test_mae_mean)
# n_round <- which((bst$evaluation_log$test_mae_mean == min((bst$evaluation_log$test_mae_mean))))

#train model San Juan
#---------------------------------------------------------------
bst_sj = xgboost(data = sj_m,
                 label = target,
                 eta = 0.2,
                 max_depth = 15,
                 nrounds=n_round,
                 nfold = n_fold,
                 subsample = 0.7,
                 colsample_bytree = 0.6,
                 eval_metric = 'rmse',
#                 eval_metric = 'mae',
                 objective = "reg:linear"
)

pred_sj <- predict(bst_sj, data.matrix(sj_tst_df_imp))

# explore the most important features
#---------------------------------------------------------------
model_sj <- xgb.dump(bst_sj, with_stats = T)
model_sj[1:15]

# Get the feature real names
m_names <- dimnames(sj_m)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(m_names, model = bst_sj)

# Nice graph
xgb.plot.importance(importance_matrix[1:15,], main="San Juan")

# train full model
#---------------------------------------------------------------
bst_sj2 = xgboost(data = sj_m,
                  label = target,
                  eta = 0.2,
                  max_depth = 15,
                  nrounds=n_round,
                  nfold = n_fold,
                  subsample = 0.7,
                  #colsample_bytree = 0.6,
                  eval_metric = 'rmse',
#                  eval_metric = 'mae',
                  objective = "reg:linear"
)

pred_sj2 <- predict(bst_sj2, data.matrix(sj_train))
sj_acc=accuracy(pred_sj2, sj_test$total_cases)
model_comparision <-track_results(sj_acc, "XGB3 SJ",2)
#model_comparision <-track_results(sj_acc, "XGB4 SJ",2)

pred_sj2 <- predict(bst_sj2, data.matrix(sj_test_final))

#===================================================
# Repeat the whole shebang for Iquitos
#===================================================
# find the iterations to run
#---------------------------------------------------------------
n_round <- 100
n_fold <- 10
set.seed(13)
params <- list(objective = "reg:linear",
               eta = 0.2,
               max_depth = 15
)
iq_m = iq_df[,-c(1)]
iq_m = data.matrix(iq_m, rownames.force = F)
target=(iq_df$total_cases)

bst <- xgb.cv(params = params, iq_m, nrounds = n_round, nfold = n_fold, label=target, metrics = 'rmse')
plot(bst$evaluation_log$test_rmse_mean)
n_round <- which((bst$evaluation_log$test_rmse_mean == min((bst$evaluation_log$test_rmse_mean))))

# bst <- xgb.cv(params = params, iq_m, nrounds = n_round, nfold = n_fold, label=target, metrics = 'mae')
# plot(bst$evaluation_log$test_mae_mean)
# n_round <- which((bst$evaluation_log$test_mae_mean == min((bst$evaluation_log$test_mae_mean))))

#train model Iquitos
bst_iq = xgboost(data = iq_m,
                 label = target,
                 eta = 0.3,
                 max_depth = 5,
                 nrounds=n_round,
                 nfold = n_fold,
                 subsample = 0.7,
                 colsample_bytree = 0.6,
                 eval_metric = 'rmse',
#                 eval_metric = 'mae',
                 objective = "reg:linear"
)

pred_iq <- predict(bst_iq, data.matrix(iq_tst_df_imp))

# explore the most important features
model <- xgb.dump(bst_iq, with_stats = T)
model[1:15]

# Get the feature real names
m_names <- dimnames(iq_m)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(m_names, model = bst)

# Nice graph
xgb.plot.importance(importance_matrix[1:15,], main="Iquitos")

# train the full model
#---------------------------------------------------------------
bst_iq2 = xgboost(data = iq_m,
                  label = target,
                  eta = 0.2,
                  max_depth = 15,
                  nrounds=n_round,
                  nfold = n_fold,
                  subsample = 0.7,
                  colsample_bytree = 0.6,
                  eval_metric = 'rmse',
#                  eval_metric = 'mae',
                  objective = "reg:linear"
)

iq_pred2 <- predict(bst_iq2, data.matrix(iq_train))
iq_acc=accuracy(iq_pred2, iq_test$total_cases)
model_comparision <-track_results(iq_acc, "XGB3 IQ",2)
#model_comparision <-track_results(iq_acc, "XGB4 IQ",2)

pred_iq2 <- predict(bst_iq2, data.matrix(iq_test_final))

make_submission(pred_sj2, pred_iq2, "XGB3.csv")
#make_submission(pred_sj2, pred_iq2, "XGB4.csv")

#===============================================
# GLM.NB version 4
#===============================================
sj_glm = glm.nb(formula = total_cases ~ precipitation_amt_mm + reanalysis_specific_humidity_g_per_kg +
                  ndvi_sw + station_avg_temp_c, data = sj_df)

iq_glm = glm.nb(formula = total_cases ~ precipitation_amt_mm + reanalysis_specific_humidity_g_per_kg +
                  ndvi_sw + station_avg_temp_c, data = iq_df)


sj_pred = predict(sj_glm, newdata=sj_train, type="response")
iq_pred= predict(iq_glm, newdata=iq_train, type="response")
sj_acc=accuracy(sj_pred, sj_test$total_cases)
iq_acc=accuracy(iq_pred, iq_test$total_cases)
model_comparision <-track_results(sj_acc, "GLM.NB 4 SJ",2)
model_comparision <-track_results(iq_acc, "GLM.NB 4 IQ",2)
sj_pred = predict(sj_glm, newdata=sj_test_final, type="response")
iq_pred= predict(iq_glm, newdata=iq_test_final, type="response")

make_submission(sj_pred, iq_pred, "GLM_NB_4.csv")

par(mfrow=c(2,1))
plot(residuals(sj_glm), main="San Juan GLM.NB4 residuals")
plot(residuals(iq_glm), main="Iquitos GLM.NB4 residuals")
par(mfrow=c(1,1))

############################################################################################################
# Here endth the ensemble
############################################################################################################




##################################################################################
# Historical archive of the rest of models built
##################################################################################

#-------------------------------------------------------------------------
# let's try building an ETS model, first San Juan then Iquitos
#-------------------------------------------------------------------------
fit_ets_sj= ets(ts_sj_tr, model='ZZZ')
summary(fit_ets_sj)
ets_fcast_sj=forecast(fit_ets_sj)
m_res=accuracy(ets_fcast_sj, ts_sj_tst)
model_comparision <-track_results(m_res, "ETS ZZZ San Juan",1)

fit_ets_iq= ets(ts_iq_tr, model='ZZZ')
summary(fit_ets_iq)
ets_fcast_iq=forecast(fit_ets_iq)
m_res=accuracy(ets_fcast_iq, ts_iq_tst)
model_comparision <-track_results(m_res, "ETS ZZZ Iquitos",1)

par(mfrow=c(2,1))
plot(ets_fcast_sj, main="ETS San Juan")
plot(ets_fcast_iq, main="ETS Iquitos")
par(mfrow=c(1,1))

# OK, not great but to make a first submission, let's re-train on all data
#-------------------------------------------------------------------------
fit_ets_sj_all= ets(ts_sj_all, model='ZZZ')
fit_ets_iq_all= ets(ts_iq_all, model='ZZZ')
predict_sj=forecast(fit_ets_sj_all, h=260)
predict_iq=forecast(fit_ets_iq_all, h=156)
sj_pred=as.numeric(predict_sj$mean)
iq_pred=as.numeric(predict_iq$mean)

#make_submission(sj_pred, iq_pred, 'ETS_Base1.csv')

#clean up
rm(fit_ets_sj_all)
rm(fit_ets_sj)
rm(fit_ets_iq)
rm(fit_ets_iq_all)
rm(ets_fcast_iq)
rm(ets_fcast_sj)
rm(predict_sj)
rm(predict_iq)


#===================================================================================|
# let's try building an stlf ETS model, first San Juan then Iquitos
#===================================================================================|
sj_stl_tr=stlf(ts_sj_tr, h=260, s.window = 'periodic', robust = TRUE)
plot(sj_stl_tr)
iq_stl_tr=stlf(ts_iq_tr, h=156, s.window = 'periodic', robust = TRUE)
sj_res=accuracy(sj_stl_tr, ts_sj_tst)
iq_res=accuracy(iq_stl_tr, ts_iq_tst)
model_comparision <-track_results(sj_res, "STLF SJ",1)
model_comparision <-track_results(iq_res, "STLF IQ",1)

sj_f_stlf=forecast(sj_stl_tr, method="ets")
iq_f_stlf=forecast(iq_stl_tr, method="ets")
sj_acc=accuracy(sj_f_stlf, ts_sj_tst)
iq_acc=accuracy(iq_f_stlf, ts_iq_tst)
model_comparision <-track_results(sj_acc, "Forecast STLF SJ",1)
model_comparision <-track_results(iq_acc, "Forecast STLF IQ",1)

par(mfrow=c(2,1))
plot(sj_f_stlf, main="Forecasted STLF San Juan")
plot(iq_f_stlf, main="Forecasted STLF Iquitos")
par(mfrow=c(1,1))

# looks good enough on test data to try with full data
#-------------------------------------------------------------------------
sj_stl_all=stlf(ts_sj_all, h=260, s.window = 'periodic', robust = TRUE)
iq_stl_all=stlf(ts_iq_all, h=156, s.window = 'periodic', robust = TRUE)
sj_f_stlf=forecast(sj_stl_all, method="ets")
iq_f_stlf=forecast(iq_stl_all, method="ets")
sj_pred=sj_f_stlf$mean
iq_pred=iq_f_stlf$mean
make_submission(sj_pred, iq_pred, "STLF ETS.csv")

par(mfrow=c(2,1))
plot(residuals(sj_f_stlf), type='p', main="San Juan STLF residuals")
plot(residuals(iq_f_stlf), type='p', main="Iquitos STLF residuals")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
Acf(sj_f_stlf$mean)
Acf(iq_f_stlf$mean)
par(mfrow=c(1,1))

#clean up
rm(sj_stl_tr)
rm(iq_stl_tr)
rm(sj_stl_all)
rm(iq_stl_all)
#rm(sj_f_stlf)
#rm(iq_f_stlf)
rm(sj_pred)
rm(iq_pred)


#===================================================================================|
# let's try building an stl model, using quarters as the ts frequency, forecasting 
# trend separately from seasonally adjusted data
#===================================================================================|

# time series with just the total_cases
tsq_sj_all=ts(sj_df[,1], start=c(1990,04,03), frequency = 4)
tsq_iq_all=ts(iq_df[,1], start=c(2000,07,01), frequency = 4)
tsq_sj_tr= ts(sj_train[,1], start=c(1990,04,03), frequency = 4)
tsq_iq_tr= ts(iq_train[,1], start=c(2000,07,01), frequency = 4)
tsq_sj_tst=ts(sj_test[,1], start=c(2004,09,30), frequency = 4)
tsq_iq_tst=ts(iq_test[,1], start=c(2008,06,24), frequency = 4)

sj_stlq_tr=stl(tsq_sj_tr, s.window = 'periodic', robust = TRUE)
plot(sj_stlq_tr)
iq_stlq_tr=stl(tsq_iq_tr, s.window = 'periodic', robust = TRUE)
plot(iq_stlq_tr)

sj_seas=seasadj(sj_stlq_tr)
iq_seas=seasadj(iq_stlq_tr)

plot(sj_seas)
plot(iq_seas)

sj_f_stlq=forecast(ets(sj_seas, damped = TRUE))
iq_f_stlq=forecast(ets(iq_seas, damped = TRUE))

sj_acc=accuracy(sj_f_stlq, tsq_sj_all)
iq_acc=accuracy(iq_f_stlq, tsq_iq_all)
model_comparision <-track_results(sj_acc, "Forecast STL Q SJ",1)
model_comparision <-track_results(iq_acc, "Forecast STL Q IQ",1)

par(mfrow=c(2,1))
plot(sj_f_stlq, main="Forecasted STLF frequency = Q San Juan")
plot(iq_f_stlq, main="Forecasted STLF frequency = Q Iquitos")
par(mfrow=c(1,1))

#now with full data
#-------------------------------------------------------------------------
sj_stlq_tr=stl(tsq_sj_all, s.window = 'periodic', robust = TRUE)
iq_stlq_tr=stl(tsq_iq_all, s.window = 'periodic', robust = TRUE)

sj_seas=seasadj(sj_stlq_tr)
iq_seas=seasadj(iq_stlq_tr)

# make models
sj_fit_stlq=ets(sj_seas, damped = TRUE)
iq_fit_stlq=ets(iq_seas, damped = TRUE)

sj_trend = sj_stlq_tr$time.series[,2]
iq_trend = iq_stlq_tr$time.series[,2]
sj_fit_trend=ets(sj_trend)
iq_fit_trend=ets(iq_trend)

sj_remain = sj_stlq_tr$time.series[,3]
iq_remain = iq_stlq_tr$time.series[,3]
sj_fit_remain=ets(sj_remain)
iq_fit_remain=ets(iq_remain)

# forecast models
sj_fcast=forecast(sj_fit_stlq, h=260)
sj_fcast_t=forecast(sj_fit_trend, h=260)
sj_fcast_r=forecast(sj_fit_remain, h=260)

iq_fcast=forecast(iq_fit_stlq, h=156)
iq_fcast_t=forecast(iq_fit_trend, h=156)
iq_fcast_r=forecast(iq_fit_remain, h= 156)

sj_pred=sj_fcast$mean + sj_fcast_t$mean + sj_fcast_r$mean
iq_pred=iq_fcast$mean + iq_fcast_t$mean + iq_fcast_r$mean

#make_submission(sj_pred, iq_pred, "ETS on Adj data2.csv")

#===================================================================================|
# try adding GARCH
#===================================================================================|
# from Auto ARIMA
# sj Arima ARIMA(1,1,1) 
# iq Arima ARIMA(0,1,2)(0,0,1)[52] 

sj_G<-garchFit(~ garch(1,1), data=sj_remain)
plot(sj_G, which =1)
iq_G<-garchFit(~ garch(1,2), data=iq_remain)
plot(iq_G, which =1)

# figure out how to predict with this
#===================================================================================|
fit_G_sj = garchFit(~arma(1,1)+garch(1, 1), data=sj_remain) 
pred_sj_r=predict(fit_G_sj, n.ahead = 260)

fit_G_iq = garchFit(~arma(1,2)+garch(1, 2), data=iq_remain) 
pred_iq_r=predict(fit_G_iq, n.ahead = 156)

sj_pred=sj_fcast$mean + sj_fcast_t$mean + pred_sj_r$meanForecast
iq_pred=iq_fcast$mean + iq_fcast_t$mean + pred_iq_r$meanForecast

plot(sj_pred)
plot(iq_pred)

make_submission(sj_pred, iq_pred, "GARCH on Resid.csv")


#what happens if add GARCH predictions to the STLF model
#===========================================================
sj_pred=sj_f_stlf$mean + pred_sj_r$meanForecast
iq_pred=iq_f_stlf$mean + pred_iq_r$meanForecast
make_submission(sj_pred, iq_pred, "STLF+GARCH on Resid.csv")

# clean up
rm(iq_G)
rm(sj_G)
rm(tsq_sj_all)
rm(tsq_iq_all)
rm(tsq_sj_tr)
rm(tsq_iq_tr)
rm(tsq_sj_tst)
rm(tsq_iq_tst)
rm(sj_stlq_tr)
rm(iq_stlq_tr)
rm(sj_seas)
rm(iq_seas)
rm(sj_f_stlq)
rm(iq_f_stlq)
rm(sj_fit_stlq)
rm(iq_fit_stlq)
rm(sj_trend)
rm(iq_trend)
rm(sj_fit_trend)
rm(iq_fit_trend)
rm(sj_remain)
rm(iq_remain)
rm(sj_fit_remain)
rm(iq_fit_remain)
rm(sj_fcast)
rm(sj_fcast_t)
rm(sj_fcast_r)
rm(iq_fcast)
rm(iq_fcast_t)
rm(iq_fcast_r)
rm(sj_pred)
rm(iq_pred)
gc()



#===================================================================================|
# let's try building an Auto ARIMA model, first San Juan then Iquitos
#===================================================================================|
sj_arima=auto.arima(ts_sj_tr)
iq_arima=auto.arima(ts_iq_tr)
sj_f=forecast(sj_arima,h=260)
iq_f=forecast(iq_arima, h=156)
sj_acc=accuracy(sj_f, ts_sj_tst)
iq_acc=accuracy(iq_f, ts_iq_tst)
model_comparision <-track_results(sj_acc, "Auto ARIMA SJ",1)
model_comparision <-track_results(iq_acc, "Auto ARIMA IQ",1)

par(mfrow=c(2,1))
plot(sj_f, main="Forecasted Auto Arima San Juan")
plot(iq_f, main="Forecasted Auto Arima Iquitos")
par(mfrow=c(1,1))

# looks good enough on test data to try with full data
#-------------------------------------------------------------------------
sj_arima=auto.arima(ts_sj_all)
iq_arima=auto.arima(ts_iq_all)
sj_f=forecast(sj_arima,h=260)
iq_f=forecast(iq_arima, h=156)

sj_f$method
iq_f$method

sj_pred=sj_f$mean
iq_pred=iq_f$mean
make_submission(sj_pred, iq_pred, "AutoArima.csv")

#=================================================================
# let's try building a hand-crafted ARIMA model
#=================================================================
sj_arima=Arima(ts_sj_tr, order = c(2,1,12), seasonal = c(0,0,0))
iq_arima=Arima(ts_iq_tr,  order = c(2,1,12), seasonal = c(0,0,1))
# sj_arima=Arima(ts_sj_tr, order = c(4,1,12), seasonal = c(0,0,0))
# iq_arima=Arima(ts_iq_tr,  order = c(4,1,12), seasonal = c(0,0,1))
# sj_arima=Arima(ts_sj_tr, order = c(1,1,6), seasonal = c(0,0,0))
# iq_arima=Arima(ts_iq_tr,  order = c(2,1,6), seasonal = c(0,0,1))
sj_f=forecast(sj_arima,h=260)
iq_f=forecast(iq_arima, h=156)
sj_acc=accuracy(sj_f, ts_sj_tst)
iq_acc=accuracy(iq_f, ts_iq_tst)
model_comparision <-track_results(sj_acc, "Hand ARIMA 2,1,12 SJ",1)
model_comparision <-track_results(iq_acc, "Hand ARIMA 2,1,12 IQ",1)

par(mfrow=c(2,1))
plot(sj_f, main="Forecasted Arima(2,1,12)(0,0,0) San Juan")
plot(iq_f, main="Forecasted Arima(2,1,12)(0,0,1) Iquitos")
par(mfrow=c(1,1))

# looks good enough on test data to try with full data
#-------------------------------------------------------------------------
sj_arima=Arima(ts_sj_tr, order = c(2,1,12), seasonal = c(0,0,0))
iq_arima=Arima(ts_iq_tr,  order = c(2,1,12), seasonal = c(0,0,1))
# sj_arima=Arima(ts_sj_tr, order = c(4,1,12), seasonal = c(0,0,0))
# iq_arima=Arima(ts_iq_tr,  order = c(4,1,12), seasonal = c(0,0,1))
# sj_arima=Arima(ts_sj_tr, order = c(1,1,6), seasonal = c(0,0,0))
# iq_arima=Arima(ts_iq_tr,  order = c(2,1,6), seasonal = c(0,0,1))
sj_f=forecast(sj_arima,h=260)
iq_f=forecast(iq_arima, h=156)

sj_f$method
iq_f$method

sj_pred=sj_f$mean
iq_pred=iq_f$mean
#make_submission(sj_pred, iq_pred, "Hand Arima3.csv")
make_submission(sj_pred, iq_pred, "Hand Arima4.csv")
#make_submission(sj_pred, iq_pred, "Hand Arima5.csv")

# clean up
rm(sj_arima)
rm(iq_arima)
rm(sj_f)
rm(iq_f)

#===================================================================================|
# Trying regressors
#===================================================================================|
xreg_sj=as.matrix(cbind(train_ts_sj[,"ndvi_se"], train_ts_sj[,"reanalysis_dew_point_temp_k"]))
xreg_iq=as.matrix(cbind(train_ts_iq[,"ndvi_se"], train_ts_iq[,"reanalysis_dew_point_temp_k"]))

sj_arima=auto.arima(train_ts_sj[,"total_cases"], xreg = xreg_sj)
iq_arima=auto.arima(train_ts_iq[,"total_cases"], xreg = xreg_iq)
sj_x1=auto.arima(train_ts_sj[,"ndvi_se"])
iq_x1=auto.arima(train_ts_iq[,"ndvi_se"])
sj_x2=auto.arima(train_ts_sj[,"reanalysis_dew_point_temp_k"])
iq_x2=auto.arima(train_ts_iq[,"reanalysis_dew_point_temp_k"])

new_xreg_sj=as.matrix(cbind(test_ts_sj[,'ndvi_se'], test_ts_sj[,'reanalysis_dew_point_temp_k']))
new_xreg_iq=as.matrix(cbind(test_ts_iq[,'ndvi_se'], test_ts_iq[,'reanalysis_dew_point_temp_k']))

sj_f=forecast(sj_arima,h=186, xreg=new_xreg_sj)
iq_f=forecast(iq_arima, h=105, xreg=new_xreg_iq)

sj_acc=accuracy(sj_f$mean, all_ts_sj[,'total_cases'])
iq_acc=accuracy(iq_f$mean, all_ts_iq[,'total_cases'])
model_comparision <-track_results(sj_acc, "Auto ARIMA SJ XREG2",2)
model_comparision <-track_results(iq_acc, "Auto ARIMA IQ XREG2",2)

# all data
xreg_sj=as.matrix(cbind(all_ts_sj[,"ndvi_se"], all_ts_sj[,"reanalysis_dew_point_temp_k"]))
xreg_iq=as.matrix(cbind(all_ts_iq[,"ndvi_se"], all_ts_iq[,"reanalysis_dew_point_temp_k"]))

sj_arima=auto.arima(all_ts_sj[,"total_cases"], xreg = xreg_sj)
iq_arima=auto.arima(all_ts_iq[,"total_cases"], xreg = xreg_iq)
sj_x1=auto.arima(all_ts_sj[,"ndvi_se"])
iq_x1=auto.arima(all_ts_iq[,"ndvi_se"])
sj_x2=auto.arima(all_ts_sj[,"reanalysis_dew_point_temp_k"])
iq_x2=auto.arima(all_ts_iq[,"reanalysis_dew_point_temp_k"])

sj_fx1=forecast(sj_x1,h=260)
sj_fx2=forecast(sj_x2,h=260)
iq_fx1=forecast(iq_x1, h=156)
iq_fx2=forecast(iq_x2, h=156)

new_xreg_sj=as.matrix(cbind(sj_fx1$mean, sj_fx2$mean))
new_xreg_iq=as.matrix(cbind(iq_fx1$mean, iq_fx2$mean))

sj_f=forecast(sj_arima,h=260, xreg=new_xreg_sj)
iq_f=forecast(iq_arima, h=156, xreg=new_xreg_iq)

par(mfrow=c(2,1))
plot(sj_f, main="Forecasted Arima XREG2 San Juan")
plot(iq_f, main="Forecasted Arima XREG2 Iquitos")
par(mfrow=c(1,1))

make_submission(sj_f$mean, iq_f$mean, "Auto ARIMA XREG2A.csv")

#clean up
rm(sj_f)
rm(sj_fx1)
rm(sj_fx2)
rm(sj_arima)
rm(iq_f)
rm(iq_fx1)
rm(iq_fx2)
rm(iq_arima)
rm(xreg_sj)
rm(new_xreg_sj)
rm(xreg_iq)
rm(new_xreg_iq)


#=================================================================
# Trying regressors found when running GLM.NB
#=================================================================
xreg_sj=c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm",
            "reanalysis_dew_point_temp_k","ndvi_sw", "ndvi_se", 
            "station_avg_temp_c")
ts_sj_xreg=as.matrix(train_ts_sj[,xreg_sj])

xreg_iq=c("ndvi_se", "reanalysis_dew_point_temp_k", "reanalysis_specific_humidity_g_per_kg",
          "station_min_temp_c")
ts_iq_xreg=as.matrix(train_ts_iq[,xreg_iq])

sj_arima=auto.arima(train_ts_sj[,"total_cases"], xreg = ts_sj_xreg)
iq_arima=auto.arima(train_ts_iq[,"total_cases"], xreg = ts_iq_xreg)
sjf=forecast(sj_arima, xreg = ts_sj_xreg)
iqf=forecast(iq_arima,xreg = ts_iq_xreg)
sj_acc=accuracy(sjf$mean, all_ts_sj[,'total_cases'])
iq_acc=accuracy(iqf$mean, all_ts_iq[,'total_cases'])
model_comparision <-track_results(sj_acc, "Auto ARIMA SJ XREG3",2)
model_comparision <-track_results(iq_acc, "Auto ARIMA IQ XREG3",2)


xreg_sj=c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm",
          "reanalysis_dew_point_temp_k","ndvi_sw", "ndvi_se", 
          "station_avg_temp_c")
ts_sj_xreg=all_ts_sj[,c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm", 
                          "reanalysis_dew_point_temp_k","ndvi_sw", "ndvi_se", "station_avg_temp_c")]


xreg_iq=c("ndvi_se", "reanalysis_dew_point_temp_k", "reanalysis_specific_humidity_g_per_kg",
          "station_min_temp_c")
ts_iq_xreg=all_ts_iq[,c("ndvi_se", "reanalysis_dew_point_temp_k", 
          "reanalysis_specific_humidity_g_per_kg", "station_min_temp_c")]


sj_arima=auto.arima(all_ts_sj[,"total_cases"], xreg = ts_sj_xreg)
iq_arima=auto.arima(all_ts_iq[,"total_cases"], xreg = ts_iq_xreg)
sjf=forecast(sj_arima, xreg = ts_sj_xreg)
iqf=forecast(iq_arima,xreg = ts_iq_xreg)

sj_x1=auto.arima(all_ts_sj[,"reanalysis_specific_humidity_g_per_kg"])
sj_x2=auto.arima(all_ts_sj[,"precipitation_amt_mm"])
sj_x3=auto.arima(all_ts_sj[,"reanalysis_dew_point_temp_k"])
sj_x4=auto.arima(all_ts_sj[,"ndvi_sw"])
sj_x5=auto.arima(all_ts_sj[,"ndvi_se"])
sj_x6=auto.arima(all_ts_sj[,"station_avg_temp_c"])

iq_x1=auto.arima(all_ts_iq[,"ndvi_se"])
iq_x2=auto.arima(all_ts_iq[,"reanalysis_dew_point_temp_k"])
iq_x3=auto.arima(all_ts_iq[,"reanalysis_specific_humidity_g_per_kg"])
iq_x4=auto.arima(all_ts_iq[,"station_min_temp_c"])

sj_fx1=forecast(sj_x1,h=260)
sj_fx2=forecast(sj_x2,h=260)
sj_fx3=forecast(sj_x3,h=260)
sj_fx4=forecast(sj_x4,h=260)
sj_fx5=forecast(sj_x5,h=260)
sj_fx6=forecast(sj_x6,h=260)

iq_fx1=forecast(iq_x1, h=156)
iq_fx2=forecast(iq_x2, h=156)
iq_fx3=forecast(iq_x3, h=156)
iq_fx4=forecast(iq_x4, h=156)

new_xreg_sj=as.matrix(cbind(sj_fx1$mean, sj_fx2$mean, sj_fx3$mean, sj_fx4$mean, sj_fx5$mean, sj_fx6$mean))
new_xreg_iq=as.matrix(cbind(iq_fx1$mean, iq_fx2$mean, iq_fx3$mean, iq_fx4$mean))

sj_f=forecast(sj_arima,h=260, xreg=new_xreg_sj)
iq_f=forecast(iq_arima, h=156, xreg=new_xreg_iq)


par(mfrow=c(2,1))
plot(sj_f, main="Forecasted Arima XREG4 San Juan")
plot(iq_f, main="Forecasted Arima XREG4 Iquitos")
par(mfrow=c(1,1))

make_submission(sj_f$mean, iq_f$mean, "Auto ARIMA XREG4A.csv")
# a lot of work for nothing.

#clean up
rm(sj_f)
rm(sj_x1)
rm(sj_x2)
rm(sj_x3)
rm(sj_x4)
rm(sj_x5)
rm(sj_x6)
rm(sj_fx1)
rm(sj_fx2)
rm(sj_fx3)
rm(sj_fx4)
rm(sj_fx5)
rm(sj_fx6)
rm(sj_arima)
rm(iq_f)
rm(iq_x1)
rm(iq_x2)
rm(iq_x3)
rm(iq_x4)
rm(iq_fx1)
rm(iq_fx2)
rm(iq_fx3)
rm(iq_fx4)
rm(iq_arima)
rm(xreg_sj)
rm(new_xreg_sj)
rm(xreg_iq)
rm(new_xreg_iq)


#======================================================================================================
#======================================================================================================
# regular linear regression models
#======================================================================================================
#======================================================================================================

#====================================================================================
# good old fashioned nb glm
#====================================================================================

# ran glm.nb(total_cases ~., ) to get the significant values, then tailored formulas


sj_glm = glm.nb(formula = total_cases ~ reanalysis_specific_humidity_g_per_kg + precipitation_amt_mm +
                  reanalysis_dew_point_temp_k + weekofyear + ndvi_sw+ ndvi_se +
                  station_avg_temp_c, data = sj_df)
summary(sj_glm)

iq_glm = glm.nb(formula = total_cases~ reanalysis_specific_humidity_g_per_kg +
                  reanalysis_dew_point_temp_k + ndvi_se + station_min_temp_c, data = iq_df)

summary(iq_glm)

sj_pred = predict(sj_glm, newdata=sj_train, type="response")
iq_pred= predict(iq_glm, newdata=iq_train, type="response")
sj_acc=accuracy(sj_pred, sj_test$total_cases)
iq_acc=accuracy(iq_pred, iq_test$total_cases)
model_comparision <-track_results(sj_acc, "GLM.NB SJ",2)
model_comparision <-track_results(iq_acc, "GLM.NB IQ",2)

sj_pred = predict(sj_glm, newdata=sj_test_final, type="response")
iq_pred= predict(iq_glm, newdata=iq_test_final, type="response")

#make_submission(sj_pred, iq_pred, "Good Old NB2.csv")

par(mfrow=c(2,1))
plot(residuals(sj_glm), main="San Juan GLM.NB residuals")
plot(residuals(iq_glm), main="Iquitos GLM.NB residuals")
par(mfrow=c(1,1))


#====================================================================================
# good old fashioned nb glm, no outliers
#====================================================================================
no_outliers=tr_df2
no_outliers$total_cases=remove_outliers(no_outliers$total_cases)
no_outliers[is.na(no_outliers)]=0

# Seperate data by city
sj_tr_df_NO=dplyr::filter(no_outliers, city=='sj')
iq_tr_df_NO=dplyr::filter(no_outliers, city=='iq')

sj_train_NO=sj_tr_df_NO[1:750,]
sj_test_NO=sj_tr_df_NO[751:936,]
iq_train_NO = iq_tr_df_NO[1:415,]
iq_test_NO=iq_tr_df_NO[416:520,]

sj_glm = glm.nb(formula = total_cases ~ reanalysis_specific_humidity_g_per_kg + precipitation_amt_mm +
                  reanalysis_dew_point_temp_k + ndvi_se + 
                  station_min_temp_c, data = sj_tr_df_NO)
summary(sj_glm) 

iq_glm = glm.nb(formula = total_cases~ reanalysis_specific_humidity_g_per_kg +
                  reanalysis_dew_point_temp_k + ndvi_se + station_min_temp_c, data = iq_tr_df_NO)

summary(iq_glm)

sj_pred = predict(sj_glm, newdata=sj_train, type="response")
iq_pred= predict(iq_glm, newdata=iq_train, type="response")
sj_acc=accuracy(sj_pred, sj_test_NO$total_cases)
iq_acc=accuracy(iq_pred, iq_test_NO$total_cases)
model_comparision <-track_results(sj_acc, "GLM.NB No outliers SJ",2)
model_comparision <-track_results(iq_acc, "GLM.NB No Outliers IQ",2)

sj_pred = predict(sj_glm, newdata=sj_test_final, type="response")
iq_pred= predict(iq_glm, newdata=iq_test_final, type="response")

make_submission(sj_pred, iq_pred, "GLM_NB no outliers.csv")

par(mfrow=c(2,1))
plot(residuals(sj_glm), main="San Juan GLM.NB  No outliers residuals")
plot(residuals(iq_glm), main="Iquitos GLM.NB No outliers residuals")
par(mfrow=c(1,1))

#====================================================================================
# good old fashioned nb glm, no outliers, tweaking the variables
#====================================================================================
no_outliers=tr_df2
no_outliers$total_cases=remove_outliers(no_outliers$total_cases)
no_outliers[is.na(no_outliers)]=0

# Seperate data by city
sj_tr_df_NO=dplyr::filter(no_outliers, city=='sj')
iq_tr_df_NO=dplyr::filter(no_outliers, city=='iq')

sj_train_NO=sj_tr_df_NO[1:750,]
sj_test_NO=sj_tr_df_NO[751:936,]
iq_train_NO = iq_tr_df_NO[1:415,]
iq_test_NO=iq_tr_df_NO[416:520,]

sj_glm = glm.nb(formula = total_cases ~ reanalysis_specific_humidity_g_per_kg + precipitation_amt_mm +
                  ndvi_se + station_min_temp_c, data = sj_tr_df_NO)
summary(sj_glm) 

iq_glm = glm.nb(formula = total_cases~ reanalysis_specific_humidity_g_per_kg + precipitation_amt_mm +
                  ndvi_se + station_min_temp_c, data = iq_tr_df_NO)

summary(iq_glm)

sj_pred = predict(sj_glm, newdata=sj_train, type="response")
iq_pred= predict(iq_glm, newdata=iq_train, type="response")
sj_acc=accuracy(sj_pred, sj_test_NO$total_cases)
iq_acc=accuracy(iq_pred, iq_test_NO$total_cases)
model_comparision <-track_results(sj_acc, "GLM.NB No outliers 2 SJ",2)
model_comparision <-track_results(iq_acc, "GLM.NB No Outliers 2 IQ",2)

sj_pred = predict(sj_glm, newdata=sj_test_final, type="response")
iq_pred= predict(iq_glm, newdata=iq_test_final, type="response")

make_submission(sj_pred, iq_pred, "GLM_NB no outliers2.csv")

# par(mfrow=c(2,1))
# plot(residuals(sj_glm), main="San Juan GLM.NB  No outliers residuals")
# plot(residuals(iq_glm), main="Iquitos GLM.NB No outliers residuals")
# par(mfrow=c(1,1))


#====================================================================================
# good old fashioned nb glm, no outliers, all the variables
#====================================================================================
no_outliers=tr_df2
no_outliers$total_cases=remove_outliers(no_outliers$total_cases)
no_outliers[is.na(no_outliers)]=0

# Seperate data by city
sj_tr_df_NO=dplyr::filter(no_outliers, city=='sj')
iq_tr_df_NO=dplyr::filter(no_outliers, city=='iq')

sj_train_NO=sj_tr_df_NO[1:750,]
sj_test_NO=sj_tr_df_NO[751:936,]
iq_train_NO = iq_tr_df_NO[1:415,]
iq_test_NO=iq_tr_df_NO[416:520,]

sj_glm = glm.nb(formula = total_cases ~., data = sj_tr_df_NO)
summary(sj_glm) 

iq_glm = glm.nb(formula = total_cases~ ., data = iq_tr_df_NO)

summary(iq_glm)

sj_pred = predict(sj_glm, newdata=sj_train, type="response")
iq_pred= predict(iq_glm, newdata=iq_train, type="response")
sj_acc=accuracy(sj_pred, sj_test_NO$total_cases)
iq_acc=accuracy(iq_pred, iq_test_NO$total_cases)
model_comparision <-track_results(sj_acc, "GLM.NB No outliers 3 SJ",2)
model_comparision <-track_results(iq_acc, "GLM.NB No Outliers 3 IQ",2)

sj_pred = predict(sj_glm, newdata=sj_test_final, type="response")
iq_pred= predict(iq_glm, newdata=iq_test_final, type="response")

make_submission(sj_pred, iq_pred, "GLM_NB no outliers3.csv")


#====================================================================================
# good old fashioned nb glm with fewer variables
#====================================================================================
# 
# sj_glm = glm.nb(formula = total_cases ~ precipitation_amt_mm + ndvi_sw + station_min_temp_c, data = sj_df)
# summary(sj_glm) 
# 
# iq_glm = glm.nb(formula = total_cases ~ precipitation_amt_mm + ndvi_sw + station_min_temp_c, data = iq_df)
# summary(iq_glm)

sj_glm = glm.nb(formula = total_cases ~ precipitation_amt_mm + reanalysis_specific_humidity_g_per_kg +
                  ndvi_sw + station_avg_temp_c, data = sj_df)

iq_glm = glm.nb(formula = total_cases ~ precipitation_amt_mm + reanalysis_specific_humidity_g_per_kg +
                  ndvi_sw + station_avg_temp_c, data = iq_df)


sj_pred = predict(sj_glm, newdata=sj_train, type="response")
iq_pred= predict(iq_glm, newdata=iq_train, type="response")
sj_acc=accuracy(sj_pred, sj_test$total_cases)
iq_acc=accuracy(iq_pred, iq_test$total_cases)
# model_comparision <-track_results(sj_acc, "GLM.NB short SJ",2)
# model_comparision <-track_results(iq_acc, "GLM.NB short IQ",2)
model_comparision <-track_results(sj_acc, "GLM.NB 4 SJ",2)
model_comparision <-track_results(iq_acc, "GLM.NB 4 IQ",2)
sj_pred = predict(sj_glm, newdata=sj_test_final, type="response")
iq_pred= predict(iq_glm, newdata=iq_test_final, type="response")

#make_submission(sj_pred, iq_pred, "GLM_NB_3.csv")

make_submission(sj_pred, iq_pred, "GLM_NB_4.csv")

#====================================================================================
# Try of diff'ed data
#====================================================================================
diff_sj=diff(all_ts_sj)
diff_iq=diff(all_ts_iq)
tr_diff_sj=diff(train_ts_sj)
tr_diff_iq=diff(train_ts_iq)
tst_diff_sj=diff(test_ts_sj)
tst_diff_iq=diff(test_ts_iq)

sj_glm = glm.nb(formula = total_cases ~ ., data = diff_sj)
summary(sj_glm) 

iq_glm = glm.nb(formula = total_cases ~ ., data = diff_iq)
summary(iq_glm)

sj_pred = predict(sj_glm, newdata=tr_diff_sj, type="response")
iq_pred= predict(iq_glm, newdata=tr_diff_iq, type="response")
sj_acc=accuracy(sj_pred, sj_test$total_cases)
iq_acc=accuracy(iq_pred, iq_test$total_cases)
model_comparision <-track_results(sj_acc, "GLM.NB diffed SJ",2)
model_comparision <-track_results(iq_acc, "GLM.NB diffed IQ",2)

sj_pred = predict(sj_glm, newdata=sj_test_final, type="response")
iq_pred= predict(iq_glm, newdata=iq_test_final, type="response")

make_submission(sj_pred, iq_pred, "GLM_NB_diffed.csv")



#============================================================================
# good old fashioned glm
#============================================================================
# ran glm(total_cases ~., ) to get the significant values, then tailored formulas

#sj_glm = glm(formula = total_cases ~ ., data = sj_df)
sj_glm = glm(formula = total_cases ~ reanalysis_specific_humidity_g_per_kg +
                 reanalysis_max_air_temp_k + reanalysis_avg_temp_k + weekofyear + ndvi_sw+ ndvi_se +
                 station_avg_temp_c, data = sj_df)
summary(sj_glm)

#iq_glm = glm(formula = total_cases~ ., data = iq_df)
iq_glm = glm(formula = total_cases~ reanalysis_specific_humidity_g_per_kg + ndvi_se +
               station_min_temp_c, data = iq_df)

summary(iq_glm)


sj_pred = predict(sj_glm, newdata=sj_train, type="response")
iq_pred= predict(iq_glm, newdata=iq_train, type="response")
sj_acc=accuracy(sj_pred, sj_test$total_cases)
iq_acc=accuracy(iq_pred, iq_test$total_cases)
model_comparision <-track_results(sj_acc, "GLM reduced SJ",2)
model_comparision <-track_results(iq_acc, "GLM reduced IQ",2)

sj_pred = predict(sj_glm, newdata=sj_test_final, type="response")
iq_pred= predict(iq_glm, newdata=iq_test_final, type="response")

make_submission(sj_pred, iq_pred, "GLM reduced.csv")


par(mfrow=c(2,1))
plot(residuals(sj_glm), main="San Juan GLM residuals")
plot(residuals(iq_glm), main="Iquitos GLM residuals")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
Acf(sj_pred)
Acf(iq_pred)
par(mfrow=c(1,1))

#*************************************************************************************************************
#*************************************************************************************************************
#   Machine Learning techniques
#*************************************************************************************************************
#*************************************************************************************************************

#=================================================================
# trying alternate parameters XGB parameters
#=================================================================

# find the best number of iterations to run
n_round <- 100
n_fold <- 10
set.seed(13)
params <- list(objective = "reg:linear",
               eta = 0.1,
               max_depth = 20
)
sj_m = sj_df[,-c(1)]
sj_m = data.matrix(sj_m, rownames.force = F)
target=(sj_df$total_cases)

bst <- xgb.cv(params = params, sj_m, nrounds = n_round, nfold = n_fold, label=target, metrics = 'rmse')
plot(bst$evaluation_log$test_rmse_mean)
n_round <- which((bst$evaluation_log$test_rmse_mean == min((bst$evaluation_log$test_rmse_mean))))

#train model San Juan
bst_sj = xgboost(data = sj_m,
                 label = target,
                 eta = 0.1,
                 max_depth = 20,
                 nrounds=n_round,
                 nfold = n_fold,
                 subsample = 0.7,
                 colsample_bytree = 0.6,
                 eval_metric = 'rmse',
                 objective = "reg:linear"
)

pred_sj <- predict(bst_sj, data.matrix(sj_tst_df_imp))

# explore the most important features
model_sj <- xgb.dump(bst_sj, with_stats = T)
model_sj[1:15]

# Get the feature real names
m_names <- dimnames(sj_m)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(m_names, model = bst_sj)

# Nice graph
xgb.plot.importance(importance_matrix[1:15,], main="San Juan")

# # reduce features (large data sets)
# #----------------------------------------------------------
# keep <- importance_matrix[1:15,]
# keep <- subset(keep, select = c(Feature))
# keepList <- as.list(as.data.frame(split(keep, seq(nrow(keep)))))
# sj_new <-  data.matrix(sj_df[,names(sj_df) %in% keepList])
sj_new=sj_m

bst_sj2 = xgboost(data = sj_new,
                  label = target,
                  eta = 0.1,
                  max_depth = 20,
                  nrounds=n_round,
                  nfold = n_fold,
                  subsample = 0.7,
                  #colsample_bytree = 0.6,
                  eval_metric = 'rmse',
                  objective = "reg:linear"
)

pred_sj2 <- predict(bst_sj2, data.matrix(sj_train))
sj_acc=accuracy(pred_sj2, sj_test$total_cases)
model_comparision <-track_results(sj_acc, "XGB5 alt param SJ",2)


pred_sj2 <- predict(bst_sj2, data.matrix(sj_test_final))

#=================================================================
# Repeat the whole shebang for Iquitos
#=================================================================
# find the iterations to run
n_round <- 100
n_fold <- 10
set.seed(13)
params <- list(objective = "reg:linear",
               eta = 0.1,
               max_depth = 20
)
iq_m = iq_df[,-c(1)]
iq_m = data.matrix(iq_m, rownames.force = F)
target=(iq_df$total_cases)

bst <- xgb.cv(params = params, iq_m, nrounds = n_round, nfold = n_fold, label=target, metrics = 'rmse')
plot(bst$evaluation_log$test_rmse_mean)
n_round <- which((bst$evaluation_log$test_rmse_mean == min((bst$evaluation_log$test_rmse_mean))))

#train model Iquitos
bst_iq = xgboost(data = iq_m,
                 label = target,
                 eta = 0.1,
                 max_depth = 20,
                 nrounds=n_round,
                 nfold = n_fold,
                 subsample = 0.7,
                 colsample_bytree = 0.6,
                 eval_metric = 'rmse',
                 objective = "reg:linear"
)

pred_iq <- predict(bst_iq, data.matrix(iq_train))

# explore the most important features
model <- xgb.dump(bst_iq, with_stats = T)
model[1:15]

# Get the feature real names
m_names <- dimnames(iq_m)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(m_names, model = bst)

# Nice graph
xgb.plot.importance(importance_matrix[1:15,], main="Iquitos")

# reduce to top 5 features
#----------------------------------------------------------
# keep <- importance_matrix[1:15,]
# keep <- subset(keep, select = c(Feature))
# keepList <- as.list(as.data.frame(split(keep, seq(nrow(keep)))))
# iq_new <-  data.matrix(iq_df[,names(iq_df) %in% keepList])
iq_new=iq_m

bst_iq2 = xgboost(data = iq_new,
                  label = target,
                  eta = 0.1,
                  max_depth = 20,
                  nrounds=n_round,
                  nfold = n_fold,
                  subsample = 0.7,
                  colsample_bytree = 0.6,
                  eval_metric = 'rmse',
                  objective = "reg:linear"
)

iq_pred2 <- predict(bst_iq2, data.matrix(iq_train))
iq_acc=accuracy(iq_pred2, iq_test$total_cases)
model_comparision <-track_results(iq_acc, "XGB5 alt param IQ",2)

pred_iq2 <- predict(bst_iq2, data.matrix(iq_test_final))
make_submission(pred_sj2, pred_iq2, "XGB5 alt params.csv")



#=================================================================
# trying gridSearch to pick parameters
#=================================================================
#From https://www.kaggle.com/silverstone1903/xgboost-grid-search-r
#---------------------------------------------------------------

#San Juan

sj_m = sj_df[,-c(1)]
sj_m = data.matrix(sj_m, rownames.force = F)
target=(sj_df$total_cases)

searchGridSubCol <- expand.grid(subsample = c(0.5, 0.6, 0.7), 
                                colsample_bytree = c(0.5, 0.6),
                                max_depth = c(4, 5,10, 12,13,14,15),
                                min_child = seq(1), 
                                eta = c(0.1, 0.05, 0.01)
)

ntrees <- 100

system.time(
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentMinChild <- parameterList[["min_child"]]
    xgboostModelCV <- xgb.cv(data =  sj_m, label=target, nrounds = ntrees, nfold = 10, showsd = TRUE, 
                             metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                             "objective" = "reg:linear", "max.depth" = currentDepth, "eta" = currentEta,      
                             "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate,
                             print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree",
                             early_stopping_rounds = 10)
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    rmse <- tail(xvalidationScores$test_rmse_mean, 1)
    trmse <- tail(xvalidationScores$train_rmse_mean,1)
    output <- return(c(rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, 
                       currentEta, currentMinChild))
    
  }))

output <- as.data.frame(t(rmseErrorsHyperparameters))
head(output)
varnames <- c("TestRMSE", "TrainRMSE", "SubSampRate", "ColSampRate", "Depth", "eta", "currentMinChild")
names(output) <- varnames
head(output)
write.csv(output, "xgb_gridsearch_sj.csv")

# and now Iquitos, in case they are different
iq_m = iq_df[,-c(1)]
iq_m = data.matrix(iq_m, rownames.force = F)
target=(iq_df$total_cases)

searchGridSubCol <- expand.grid(subsample = c(0.5, 0.6, 0.7), 
                                colsample_bytree = c(0.5, 0.6),
                                max_depth = c(4, 5,10, 12,13,14,15),
                                min_child = seq(1), 
                                eta = c(0.1, 0.05, 0.01)
)

ntrees <- 100

system.time(
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentMinChild <- parameterList[["min_child"]]
    xgboostModelCV <- xgb.cv(data =  iq_m, label=target, nrounds = ntrees, nfold = 10, showsd = TRUE, 
                             metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                             "objective" = "reg:linear", "max.depth" = currentDepth, "eta" = currentEta,      
                             "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate,
                             print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree",
                             early_stopping_rounds = 10)
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    rmse <- tail(xvalidationScores$test_rmse_mean, 1)
    trmse <- tail(xvalidationScores$train_rmse_mean,1)
    output <- return(c(rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, 
                       currentEta, currentMinChild))
    
  }))

output <- as.data.frame(t(rmseErrorsHyperparameters))
head(output)
varnames <- c("TestRMSE", "TrainRMSE", "SubSampRate", "ColSampRate", "Depth", "eta", "currentMinChild")
names(output) <- varnames
head(output)
write.csv(output, "xgb_gridsearch_iq.csv")


#---------------------------------------------------------------

# find the best number of iterations to run
n_round <- 100
n_fold <- 10
set.seed(13)
params <- list(objective = "reg:linear",
               eta = 0.05,
               max_depth = 15
)
sj_m = sj_df[,-c(1)]
sj_m = data.matrix(sj_m, rownames.force = F)
target=(sj_df$total_cases)

bst <- xgb.cv(params = params, sj_m, nrounds = n_round, nfold = n_fold, label=target, metrics = 'rmse')
plot(bst$evaluation_log$test_rmse_mean)
n_round <- which((bst$evaluation_log$test_rmse_mean == min((bst$evaluation_log$test_rmse_mean))))

#train model San Juan
bst_sj = xgboost(data = sj_m,
                 label = target,
                 eta = 0.05,
                 max_depth = 15,
                 nrounds=n_round,
                 nfold = n_fold,
                 subsample = 0.7,
                 colsample_bytree = 0.5,
                 eval_metric = 'rmse',
                 objective = "reg:linear"
)

pred_sj <- predict(bst_sj, data.matrix(sj_tst_df_imp))


bst_sj2 = xgboost(data = sj_m,
                  label = target,
                  eta = 0.05,
                  max_depth = 15,
                  nrounds=n_round,
                  nfold = n_fold,
                  subsample = 0.7,
                  colsample_bytree = 0.5,
                  eval_metric = 'rmse',
                  objective = "reg:linear"
)

pred_sj2 <- predict(bst_sj2, data.matrix(sj_train))
sj_acc=accuracy(pred_sj2, sj_test$total_cases)
model_comparision <-track_results(sj_acc, "XGB grid param SJ",2)


pred_sj2 <- predict(bst_sj2, data.matrix(sj_test_final))

#====================================================================
# Repeat the whole shebang for Iquitos
#====================================================================
# find the iterations to run
n_round <- 100
n_fold <- 10
set.seed(13)
params <- list(objective = "reg:linear",
               eta = 0.1,
               max_depth = 4
)
iq_m = iq_df[,-c(1)]
iq_m = data.matrix(iq_m, rownames.force = F)
target=(iq_df$total_cases)

bst <- xgb.cv(params = params, iq_m, nrounds = n_round, nfold = n_fold, label=target, metrics = 'rmse')
plot(bst$evaluation_log$test_rmse_mean)
n_round <- which((bst$evaluation_log$test_rmse_mean == min((bst$evaluation_log$test_rmse_mean))))

#train model Iquitos
bst_iq = xgboost(data = iq_m,
                 label = target,
                 eta = 0.1,
                 max_depth = 4,
                 nrounds=n_round,
                 nfold = n_fold,
                 subsample = 0.7,
                 colsample_bytree = 0.5,
                 eval_metric = 'rmse',
                 objective = "reg:linear"
)

pred_iq <- predict(bst_iq, data.matrix(iq_train))


bst_iq2 = xgboost(data = iq_m,
                  label = target,
                  eta = 0.1,
                  max_depth = 20,
                  nrounds=n_round,
                  nfold = n_fold,
                  subsample = 0.7,
                  colsample_bytree = 0.6,
                  eval_metric = 'rmse',
                  objective = "reg:linear"
)

iq_pred2 <- predict(bst_iq2, data.matrix(iq_train))
iq_acc=accuracy(iq_pred2, iq_test$total_cases)
model_comparision <-track_results(iq_acc, "XGB grid param IQ",2)

pred_iq2 <- predict(bst_iq2, data.matrix(iq_test_final))
make_submission(pred_sj2, pred_iq2, "XGBGrid alt params.csv")

#======================================================================================================
#======================================================================================================
# try a neural net
#======================================================================================================
#======================================================================================================
VARselect(ts_sj_all)
VARselect(ts_iq_all)

sj_nn=nnetar(ts_sj_tr)
iq_nn=nnetar(ts_iq_tr)
sj_f=forecast(sj_nn,h=186)
iq_f=forecast(iq_nn, h=105)
sj_acc=accuracy(sj_f, ts_sj_tst)
iq_acc=accuracy(iq_f, ts_iq_tst)
model_comparision <-track_results(sj_acc, "Nnetar SJ",1)
model_comparision <-track_results(iq_acc, "Nnetar IQ",1)

# sj_nn=nnetar(ts_sj_all, 10, p=2, size = 3)
# iq_nn=nnetar(ts_iq_all, 5, p=3, size=3)

sj_nn=nnetar(ts_sj_all, 10, p=1, size = 12)
iq_nn=nnetar(ts_iq_all, 5, p=2, size=12)

sj_f=forecast(sj_nn,h=260)
iq_f=forecast(iq_nn, h=156)

par(mfrow=c(2,1))
plot(sj_f)
plot(iq_f)
par(mfrow=c(1,1))

sj_pred=sj_f$mean
iq_pred=iq_f$mean
#make_submission(sj_pred, iq_pred, "NNetars2.csv")
make_submission(sj_pred, iq_pred, "NNetars2 12 nodes.csv")

#==================================================
# try a NN where I've twiddled the lags
#==================================================
sj_nn=nnetar(ts_sj_tr)
iq_nn=nnetar(ts_iq_tr)
sj_f=forecast(sj_nn,h=186)
iq_f=forecast(iq_nn, h=105)
sj_acc=accuracy(sj_f, ts_sj_tst)
iq_acc=accuracy(iq_f, ts_iq_tst)
model_comparision <-track_results(sj_acc, "Nnetar3 lagged SJ",1)
model_comparision <-track_results(iq_acc, "Nnetar3 lagged IQ",1)

sj_nn=nnetar(ts_sj_all, 10, p=13, size = 3)
iq_nn=nnetar(ts_iq_all, 5, p=13, size=3)
sj_f=forecast(sj_nn,h=260)
iq_f=forecast(iq_nn, h=156)

par(mfrow=c(2,1))
plot(sj_f)
plot(iq_f)
par(mfrow=c(1,1))

sj_pred=sj_f$mean
iq_pred=iq_f$mean
make_submission(sj_pred, iq_pred, "NNetars3.csv")

#==================================================
# look at the effect of scaling the data
#==================================================
sj_scaled = scale(ts_sj_tr)
iq_scaled=scale(ts_iq_tr)
tst_sj_scaled=scale(ts_sj_tst)
tst_iq_scaled=scale(ts_iq_tst)
ssj_nn=nnetar(scaled_sj)
iq_nn=nnetar(scaled_tr)
sj_f=forecast(sj_nn,h=186)
iq_f=forecast(iq_nn, h=105)
sj_acc=accuracy(sj_f, tst_sj_scaled)
iq_acc=accuracy(iq_f, tst_iq_scaled)
model_comparision <-track_results(sj_acc, "Nnetar Scaled SJ",1)
model_comparision <-track_results(iq_acc, "Nnetar Scaled IQ",1)


sj_scaled_all = scale(ts_sj_all)
iq_scaled_all=scale(ts_iq_all)
sj_nn=nnetar(sj_scaled_all)
iq_nn=nnetar(iq_scaled_all)
sj_f=forecast(sj_nn,h=260)
iq_f=forecast(iq_nn, h=156)

par(mfrow=c(2,1))
plot(sj_f)
plot(iq_f)
par(mfrow=c(1,1))

sj_pred=sj_f$mean
iq_pred=iq_f$mean
make_submission(sj_pred, iq_pred, "NNetars scaled.csv")

#==================================================
# Let's try external regressors
#==================================================
xreg_sj=c("precipitation_amt_mm","reanalysis_dew_point_temp_k","ndvi_sw", "ndvi_se", 
          "station_avg_temp_c")
ts_sj_xreg=sj_train[,c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm", 
                        "reanalysis_dew_point_temp_k","ndvi_sw", "ndvi_se", "station_avg_temp_c")]
tst_sj_xreg=sj_test[,c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm", 
                       "reanalysis_dew_point_temp_k","ndvi_sw", "ndvi_se", "station_avg_temp_c")]


xreg_iq=c("ndvi_se", "reanalysis_dew_point_temp_k", "reanalysis_specific_humidity_g_per_kg",
          "station_min_temp_c")
ts_iq_xreg=iq_train[,c("ndvi_se", "reanalysis_dew_point_temp_k", 
                        "reanalysis_specific_humidity_g_per_kg", "station_min_temp_c")]
tst_iq_xreg=iq_test[,c("ndvi_se", "reanalysis_dew_point_temp_k", 
                       "reanalysis_specific_humidity_g_per_kg", "station_min_temp_c")]


sj_nn=nnetar(ts_sj_tr, 10, p=13, size = 3, xreg = ts_sj_xreg)
iq_nn=nnetar(ts_iq_tr, 5, p=13, size=3, xreg = ts_iq_xreg)
sj_f=forecast(sj_nn,h=186, xreg=tst_sj_xreg)
iq_f=forecast(iq_nn, h=105, xreg=tst_iq_xreg)

sj_acc=accuracy(sj_f, ts_sj_tst)
iq_acc=accuracy(iq_f, ts_iq_tst)
model_comparision <-track_results(sj_acc, "Nnetar Xreg SJ",1)
model_comparision <-track_results(iq_acc, "Nnetar Xreg IQ",1)

sj_x1=auto.arima(all_ts_sj[,"reanalysis_specific_humidity_g_per_kg"])
sj_x2=auto.arima(all_ts_sj[,"precipitation_amt_mm"])
sj_x3=auto.arima(all_ts_sj[,"reanalysis_dew_point_temp_k"])
sj_x4=auto.arima(all_ts_sj[,"ndvi_sw"])
sj_x5=auto.arima(all_ts_sj[,"ndvi_se"])
sj_x6=auto.arima(all_ts_sj[,"station_avg_temp_c"])

iq_x1=auto.arima(all_ts_iq[,"ndvi_se"])
iq_x2=auto.arima(all_ts_iq[,"reanalysis_dew_point_temp_k"])
iq_x3=auto.arima(all_ts_iq[,"reanalysis_specific_humidity_g_per_kg"])
iq_x4=auto.arima(all_ts_iq[,"station_min_temp_c"])

sj_fx1=forecast(sj_x1,h=260)
sj_fx2=forecast(sj_x2,h=260)
sj_fx3=forecast(sj_x3,h=260)
sj_fx4=forecast(sj_x4,h=260)
sj_fx5=forecast(sj_x5,h=260)
sj_fx6=forecast(sj_x6,h=260)

iq_fx1=forecast(iq_x1, h=156)
iq_fx2=forecast(iq_x2, h=156)
iq_fx3=forecast(iq_x3, h=156)
iq_fx4=forecast(iq_x4, h=156)

new_xreg_sj=as.matrix(cbind(sj_fx1$mean, sj_fx2$mean, sj_fx3$mean, sj_fx4$mean, sj_fx5$mean, sj_fx6$mean))
new_xreg_iq=as.matrix(cbind(iq_fx1$mean, iq_fx2$mean, iq_fx3$mean, iq_fx4$mean))

sj_nn=nnetar(ts_sj_all, 10, p=13, size = 3, xreg = new_xreg_sj)
iq_nn=nnetar(ts_iq_all, 5, p=13, size=3, xreg = new_xreg_iq)
sj_f=forecast(sj_nn, h=260, xreg = new_xreg_sj)
iq_f=forecast(iq_nn, h=156, xreg = new_xreg_iq)

par(mfrow=c(2,1))
plot(sj_f)
plot(iq_f)
par(mfrow=c(1,1))

sj_pred=sj_f$mean
iq_pred=iq_f$mean
make_submission(sj_pred, iq_pred, "NNetars xreg.csv")

#==================================================
# Let's try external regressors, this time paying attention to correlations
#==================================================
xreg_sj=c("precipitation_amt_mm","reanalysis_dew_point_temp_k","ndvi_sw", "station_avg_temp_c")
ts_sj_xreg=sj_train[,c("precipitation_amt_mm", "reanalysis_dew_point_temp_k","ndvi_sw", "station_avg_temp_c")]
tst_sj_xreg=sj_test[,c("precipitation_amt_mm", "reanalysis_dew_point_temp_k","ndvi_sw", "station_avg_temp_c")]


xreg_iq=c("ndvi_se", "reanalysis_dew_point_temp_k", "station_min_temp_c","ndvi_sw", "station_avg_temp_c")
ts_iq_xreg=iq_train[,c("precipitation_amt_mm", "reanalysis_dew_point_temp_k","ndvi_sw", "station_avg_temp_c")]    
tst_iq_xreg=iq_test[,c("precipitation_amt_mm", "reanalysis_dew_point_temp_k","ndvi_sw", "station_avg_temp_c")]

sj_nn=nnetar(ts_sj_tr, 10, p=13, size = 3, xreg = ts_sj_xreg)
iq_nn=nnetar(ts_iq_tr, 5, p=13, size=3, xreg = ts_iq_xreg)
sj_f=forecast(sj_nn,h=186, xreg=tst_sj_xreg)
iq_f=forecast(iq_nn, h=105, xreg=tst_iq_xreg)

sj_acc=accuracy(sj_f, ts_sj_tst)
iq_acc=accuracy(iq_f, ts_iq_tst)
model_comparision <-track_results(sj_acc, "Nnetar Xreg cor SJ",1)
model_comparision <-track_results(iq_acc, "Nnetar Xreg cor IQ",1)

sj_x1=auto.arima(all_ts_sj[,"precipitation_amt_mm"])
sj_x2=auto.arima(all_ts_sj[,"reanalysis_dew_point_temp_k"])
sj_x3=auto.arima(all_ts_sj[,"ndvi_sw"])
sj_x4=auto.arima(all_ts_sj[,"station_avg_temp_c"])

iq_x1=auto.arima(all_ts_iq[,"precipitation_amt_mm"])
iq_x2=auto.arima(all_ts_iq[,"reanalysis_dew_point_temp_k"])
iq_x3=auto.arima(all_ts_iq[,"ndvi_sw"])
iq_x4=auto.arima(all_ts_iq[,"station_avg_temp_c"])

sj_fx1=forecast(sj_x1,h=260)
sj_fx2=forecast(sj_x2,h=260)
sj_fx3=forecast(sj_x3,h=260)
sj_fx4=forecast(sj_x4,h=260)

iq_fx1=forecast(iq_x1, h=156)
iq_fx2=forecast(iq_x2, h=156)
iq_fx3=forecast(iq_x3, h=156)
iq_fx4=forecast(iq_x4, h=156)

new_xreg_sj=as.matrix(cbind(sj_fx1$mean, sj_fx2$mean, sj_fx3$mean, sj_fx4$mean))
new_xreg_iq=as.matrix(cbind(iq_fx1$mean, iq_fx2$mean, iq_fx3$mean, iq_fx4$mean))

sj_nn=nnetar(ts_sj_all, 10, p=13, size = 3, xreg = new_xreg_sj)
iq_nn=nnetar(ts_iq_all, 5, p=13, size=3, xreg = new_xreg_iq)
sj_f=forecast(sj_nn, h=260, xreg = new_xreg_sj)
iq_f=forecast(iq_nn, h=156, xreg = new_xreg_iq)

par(mfrow=c(2,1))
plot(sj_f)
plot(iq_f)
par(mfrow=c(1,1))

sj_pred=sj_f$mean
iq_pred=iq_f$mean
make_submission(sj_pred, iq_pred, "NNetars xreg corr.csv")

#==================================================
# Let's try fewer external regressors
#==================================================
xreg_sj=c("precipitation_amt_mm","station_min_temp_c")
ts_sj_xreg=sj_train[,c("precipitation_amt_mm","station_min_temp_c")]
tst_sj_xreg=sj_test[,c("precipitation_amt_mm","station_min_temp_c")]


xreg_iq=c("precipitation_amt_mm","station_min_temp_c")
ts_iq_xreg=iq_train[,c("precipitation_amt_mm","station_min_temp_c")]
tst_iq_xreg=iq_test[,c("precipitation_amt_mm","station_min_temp_c")]


sj_nn=nnetar(ts_sj_tr, 10, p=6, size = 3, xreg = ts_sj_xreg)
iq_nn=nnetar(ts_iq_tr, 5, p=6, size=3, xreg = ts_iq_xreg)
sj_f=forecast(sj_nn,h=186, xreg=tst_sj_xreg)
iq_f=forecast(iq_nn, h=105, xreg=tst_iq_xreg)

sj_acc=accuracy(sj_f, ts_sj_tst)
iq_acc=accuracy(iq_f, ts_iq_tst)
model_comparision <-track_results(sj_acc, "Nnetar Xreg2 SJ",1)
model_comparision <-track_results(iq_acc, "Nnetar Xreg2 IQ",1)

xreg_sj=c("precipitation_amt_mm","station_min_temp_c")
ts_sj_xreg=sj_df[,c("precipitation_amt_mm","station_min_temp_c")]

xreg_iq=c("precipitation_amt_mm","station_min_temp_c")
ts_iq_xreg=iq_df[,c("precipitation_amt_mm","station_min_temp_c")]

sj_x1=auto.arima(all_ts_sj[,"precipitation_amt_mm"])
sj_x2=auto.arima(all_ts_sj[,"station_min_temp_c"])

iq_x1=auto.arima(all_ts_iq[,"precipitation_amt_mm"])
iq_x2=auto.arima(all_ts_iq[,"station_min_temp_c"])

sj_fx1=forecast(sj_x1,h=260)
sj_fx2=forecast(sj_x2,h=260)

iq_fx1=forecast(iq_x1, h=156)
iq_fx2=forecast(iq_x2, h=156)

new_xreg_sj=as.matrix(cbind(sj_fx1$mean, sj_fx2$mean))
new_xreg_iq=as.matrix(cbind(iq_fx1$mean, iq_fx2$mean))

sj_nn=nnetar(ts_sj_all, 10, p=6, size = 3, xreg = ts_sj_xreg)
iq_nn=nnetar(ts_iq_all, 5, p=6, size=3, xreg = ts_iq_xreg)
sj_f=forecast(sj_nn, h=260, xreg = new_xreg_sj)
iq_f=forecast(iq_nn, h=156, xreg = new_xreg_iq)

par(mfrow=c(2,1))
plot(sj_f)
plot(iq_f)
par(mfrow=c(1,1))

sj_pred=sj_f$mean
iq_pred=iq_f$mean
make_submission(sj_pred, iq_pred, "NNetars xreg2A.csv")

#==================================================
# Let's try diff'ed data
#==================================================
diff_sj=diff(all_ts_sj)
diff_iq=diff(all_ts_iq)
tr_diff_sj=diff(train_ts_sj)
tr_diff_iq=diff(train_ts_iq)
tst_diff_sj=diff(test_ts_sj)
tst_diff_iq=diff(test_ts_iq)

sj_nn=nnetar(tr_diff_sj[,'total_cases'], scale.inputs = FALSE)
iq_nn=nnetar(tr_diff_iq[,'total_cases'], scale.inputs = FALSE)
sj_f=forecast(sj_nn,h=186)
iq_f=forecast(iq_nn, h=105)
sj_acc=accuracy(sj_f, ts_sj_tst)
iq_acc=accuracy(iq_f, ts_iq_tst)
model_comparision <-track_results(sj_acc, "Nnetar4 diff SJ",1)
model_comparision <-track_results(iq_acc, "Nnetar4 diff IQ",1)

sj_nn=nnetar(ts_sj_all,10, p=13, size = 3,  scale.inputs = FALSE)
iq_nn=nnetar(ts_iq_all, 5, p=13, size=3,scale.inputs = FALSE)
sj_f=forecast(sj_nn,h=260)
iq_f=forecast(iq_nn, h=156)

par(mfrow=c(2,1))
plot(sj_f)
plot(iq_f)
par(mfrow=c(1,1))

sj_pred=sj_f$mean
iq_pred=iq_f$mean
make_submission(sj_pred, iq_pred, "NNetars4.csv")

#=================================================================
# try removing outliers
#=================================================================
no_outliers=tr_df2
no_outliers$total_cases=remove_outliers(no_outliers$total_cases)
no_outliers[is.na(no_outliers)]=0

# Seperate data by city
sj_tr_df_NO=dplyr::filter(no_outliers, city=='sj')
iq_tr_df_NO=dplyr::filter(no_outliers, city=='iq')

sj_train_NO=sj_tr_df_NO[1:750,]
sj_test_NO=sj_tr_df_NO[751:936,]
iq_train_NO = iq_tr_df_NO[1:415,]
iq_test_NO=iq_tr_df_NO[416:520,]

# time series with just the total_cases
ts_sj_all_no=ts(sj_tr_df_NO[,'total_cases'], start=c(1990,04,03), frequency = 52)
ts_iq_all_no=ts(iq_tr_df_NO[,'total_cases'], start=c(2000,07,01), frequency = 52)
ts_sj_tr_no= ts(sj_train_NO[,'total_cases'], start=c(1990,04,03), frequency = 52)
ts_sj_tst_no=ts(sj_test_NO[,'total_cases'], start=c(2004,09,30), frequency = 52)
ts_iq_tr_no= ts(iq_train_NO[,'total_cases'], start=c(2000,07,01), frequency = 52)
ts_iq_tst_no=ts(iq_test_NO[,'total_cases'], start=c(2008,06,24), frequency = 52)

sj_nn=nnetar(ts_sj_tr_no)
iq_nn=nnetar(ts_iq_tr_no)
sj_f=forecast(sj_nn,h=186)
iq_f=forecast(iq_nn, h=105)
sj_acc=accuracy(sj_f,ts_sj_all_no)
iq_acc=accuracy(iq_f, ts_iq_all_no)
model_comparision <-track_results(sj_acc, "Nnetar no outliers SJ",1)
model_comparision <-track_results(iq_acc, "Nnetar no outliers IQ",1)

sj_nn=nnetar(ts_sj_all_no, 10, p=2, size = 10)
iq_nn=nnetar(ts_sj_all_no, 5, p=3, size=10)
sj_f=forecast(sj_nn,h=260)
iq_f=forecast(iq_nn, h=156)

par(mfrow=c(2,1))
plot(sj_f)
plot(iq_f)
par(mfrow=c(1,1))

sj_pred=sj_f$mean
iq_pred=iq_f$mean
make_submission(sj_pred, iq_pred, "NNetars no outliers.csv")

#======================================================================================================
#======================================================================================================
# try a non-time series neural net
#======================================================================================================
#======================================================================================================
library(neuralnet)

# scale the data
sj_tr_scaled = scale(sj_train)
sj_tst_scaled = scale(sj_test)
iq_tr_scaled = scale(iq_train)
iq_tst_scaled = scale(iq_test)
sj_scaled = scale(sj_df)
iq_scaled = scale(iq_df)
sj_final=scale(sj_test_final)
iq_final=scale(iq_test_final)


# fit neural network
set.seed(13)
sj_NN = neuralnet(total_cases~reanalysis_specific_humidity_g_per_kg +precipitation_amt_mm +
          ndvi_sw+station_min_temp_c, sj_tr_scaled, hidden = 3 , linear.output = T, stepmax = 1e+07 )

iq_NN = neuralnet(total_cases~reanalysis_specific_humidity_g_per_kg +precipitation_amt_mm +
          +ndvi_sw+station_avg_temp_c, iq_tr_scaled, hidden = 3 , linear.output = T, stepmax = 1e+07)

# plot neural network
plot(sj_NN)
plot(iq_NN)

sj_results <- compute(sj_NN, sj_tst_scaled[, c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm",
                                                 "ndvi_sw","station_min_temp_c")])
iq_results <- compute(iq_NN, iq_tst_scaled[, c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm",
                                               "ndvi_sw","station_min_temp_c")])

sj_pred = sj_results$net.result
iq_pred=iq_results$net.result
sj_acc=accuracy(sj_test$total_cases, sj_pred)
iq_acc=accuracy(iq_test$total_cases, iq_pred)
model_comparision <-track_results(sj_acc, "NeuralNet SJ",2)
model_comparision <-track_results(iq_acc, "NeuralNet IQ",2)

#train full data
sj_NN = neuralnet(total_cases~reanalysis_specific_humidity_g_per_kg +precipitation_amt_mm +
                    ndvi_sw+station_min_temp_c, sj_scaled, hidden = 3 , linear.output = T, stepmax = 1e+07 )

iq_NN = neuralnet(total_cases~reanalysis_specific_humidity_g_per_kg +precipitation_amt_mm +
                    +ndvi_sw+station_avg_temp_c, iq_scaled, hidden = 3 , linear.output = T, stepmax = 1e+07)

sj_results <- compute(sj_NN, sj_final[, c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm",
                                               "ndvi_sw","station_min_temp_c")])
iq_results <- compute(iq_NN, iq_final[, c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm",
                                               "ndvi_sw","station_min_temp_c")])

sj_pred = sj_results$net.result
iq_pred=iq_results$net.result
make_submission(sj_pred, iq_pred, "NeuralNet not ts.csv")


#======================================================================================================
# non-scaled data
#======================================================================================================

# fit neural network
set.seed(13)
sj_NN = neuralnet(total_cases~reanalysis_specific_humidity_g_per_kg +precipitation_amt_mm +
                    ndvi_sw+station_min_temp_c, sj_train, hidden = 3 , linear.output = T, stepmax = 1e+07 )

iq_NN = neuralnet(total_cases~reanalysis_specific_humidity_g_per_kg +precipitation_amt_mm +
                    +ndvi_sw+station_avg_temp_c, iq_train, hidden = 3 , linear.output = T, stepmax = 1e+07)

# plot neural network
plot(sj_NN)
plot(iq_NN)

sj_results <- compute(sj_NN, sj_test[, c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm",
                                               "ndvi_sw","station_min_temp_c")])
iq_results <- compute(iq_NN, sj_test[, c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm",
                                               "ndvi_sw","station_min_temp_c")])

sj_pred = sj_results$net.result
iq_pred=iq_results$net.result
# sj_acc=accuracy(sj_pred, sj_df$total_cases)
# iq_acc=accuracy(iq_pred, iq_df$total_cases)
# model_comparision <-track_results(sj_acc, "NeuralNet no scale SJ",2)
# model_comparision <-track_results(iq_acc, "NeuralNet no scale IQ",2)

#train full data
sj_NN = neuralnet(total_cases~reanalysis_specific_humidity_g_per_kg +precipitation_amt_mm +
                    ndvi_sw+station_min_temp_c, sj_df, hidden = 3 , linear.output = T, stepmax = 1e+07 )

iq_NN = neuralnet(total_cases~reanalysis_specific_humidity_g_per_kg +precipitation_amt_mm +
                    +ndvi_sw+station_avg_temp_c, iq_df, hidden = 3 , linear.output = T, stepmax = 1e+08)

sj_results <- compute(sj_NN, sj_test_final[, c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm",
                                          "ndvi_sw","station_min_temp_c")])
iq_results <- compute(iq_NN, iq_test_final[, c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm",
                                          "ndvi_sw","station_min_temp_c")])

sj_pred = sj_results$net.result
iq_pred=iq_results$net.result
make_submission(sj_pred, iq_pred, "NeuralNet no scale.csv")

#======================================================================================================
# normalized data
#======================================================================================================
#Max-Min Normalization from http://www.michaeljgrogan.com/neural-network-modelling-neuralnet-r/

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# scale the data
sj_tr_scaled = as.data.frame(lapply(sj_train, normalize))
sj_tst_scaled = as.data.frame(lapply(iq_train, normalize))
iq_tr_scaled = as.data.frame(lapply(sj_test, normalize))
iq_tst_scaled = as.data.frame(lapply(iq_test, normalize))
sj_scaled = as.data.frame(lapply(sj_df, normalize))
iq_scaled = as.data.frame(lapply(iq_df, normalize))
sj_final=as.data.frame(lapply(sj_test_final, normalize))
iq_final=as.data.frame(lapply(iq_test_final, normalize))


# fit neural network
set.seed(13)
sj_NN = neuralnet(total_cases~reanalysis_specific_humidity_g_per_kg +precipitation_amt_mm +
                    ndvi_sw+station_min_temp_c, sj_tr_scaled, hidden = 3 , linear.output = T, stepmax = 1e+07 )

iq_NN = neuralnet(total_cases~reanalysis_specific_humidity_g_per_kg +precipitation_amt_mm +
                    +ndvi_sw+station_avg_temp_c, iq_tr_scaled, hidden = 3 , linear.output = T, stepmax = 1e+07)

# plot neural network
# plot(sj_NN)
# plot(iq_NN)

sj_results <- compute(sj_NN, sj_tst_scaled[, c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm",
                                               "ndvi_sw","station_min_temp_c")])
iq_results <- compute(iq_NN, iq_tst_scaled[, c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm",
                                               "ndvi_sw","station_min_temp_c")])

sj_pred = sj_results$net.result
iq_pred=iq_results$net.result
sj_acc=accuracy(sj_test$total_cases, sj_pred)
iq_acc=accuracy(iq_test$total_cases, iq_pred)
model_comparision <-track_results(sj_acc, "NeuralNet min_max Norm SJ",2)
model_comparision <-track_results(iq_acc, "NeuralNet min_max Norm IQ",2)

#train full data
sj_NN = neuralnet(total_cases~reanalysis_specific_humidity_g_per_kg +precipitation_amt_mm +
                    ndvi_sw+station_min_temp_c, sj_scaled, hidden = 3 , linear.output = T, stepmax = 1e+07 )

iq_NN = neuralnet(total_cases~reanalysis_specific_humidity_g_per_kg +precipitation_amt_mm +
                    +ndvi_sw+station_avg_temp_c, iq_scaled, hidden = 3 , linear.output = T, stepmax = 1e+07)

sj_results <- compute(sj_NN, sj_final[, c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm",
                                          "ndvi_sw","station_min_temp_c")])
iq_results <- compute(iq_NN, iq_final[, c("reanalysis_specific_humidity_g_per_kg","precipitation_amt_mm",
                                          "ndvi_sw","station_min_temp_c")])

sj_pred = sj_results$net.result
iq_pred=iq_results$net.result
make_submission(sj_pred, iq_pred, "NeuralNet min_max Norm.csv")  #all 0's??!


