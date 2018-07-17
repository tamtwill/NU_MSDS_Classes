##################################################################################
# NWU Capstone 498
# 
# Table for Four Project team
# Advisor: Don Wedding 
# 
# Team: Tom Alig, Sheela Rao, Catherine Tolley, Tamara Williams
# 
# Script Author: Tamara Williams
# 
# This script uses an input file which contains the base Kaggle data, joined to the
# weather data from Hunter McGushion's kernal 
# (https://www.kaggle.com/huntermcgushion/exhaustive-weather-eda-file-overview)
# 
# The data are then additionally augmented by adding cluster info and the is.golden.week
# flag feature
##################################################################################

# For the sake of good programming hygiene, start with a clean workspace
#-------------------------------------------------------------------
# clear Workspace, then clear console
rm(list=ls())
cat("\014")

# Get location script, and set to working directory
#-------------------------------------------------------------------
working.dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(working.dir)
data.path = # your path
out.path = # your path
train.file = "final_train_dataset.csv"
test.file = "final_test_dataset.csv"


# include required packages
#-------------------------------------------------------------------
library(reshape2)
library(dplyr)
library(zoo)
library(magrittr)
library(xgboost)
library(lubridate)


###############################################################################
######                       Read and prep data block                ##########
###############################################################################

# get the data
df=read.csv(paste0(data.path,train.file),header=T,stringsAsFactors = T)
test.df = read.csv(paste0(data.path,test.file), header = T, stringsAsFactors = T)

# get Kaggle competition file to extract store ids you do NOT submit
submit_only_id=read.csv(paste0(data.path,"sample_submission 3.csv"),header=T, stringsAsFactors = FALSE) 

df[is.na(df)] = 0
test.df[is.na(test.df)] = 0

# move visitors to position 1 for easy of processing
refcols  =  c("visitors", "is.golden.week","air_store_id", "visit_date","clus.avail","clus.loc",
              "clus.res","clus.vol", "holiday.name","is.weekend" )
df = df[, c(refcols, setdiff(names(df), refcols))]

refcols  =  c("is.golden.week","air_store_id", "visit_date","clus.avail","clus.loc",
              "clus.res","clus.vol", "holiday.name","is.weekend")
test.df = test.df[, c(refcols, setdiff(names(test.df), refcols))]

df$visit_date = as.Date(df$visit_date)
test.df$visit_date = as.Date(test.df$visit_date)

# dropping columns which are not useful
df = df[-c(13,14)]
test.df = test.df[-c(12,13)]  

df.overlap = df[df$visit_date >= "2017-04-12",]
df.minus = df[df$visit_date < "2017-04-12",]
tmp = df.overlap[-c(1)]
df.chart.it = rbind(tmp, test.df)

#====================================================================================
# Function to output the forecast in the right format for Kaggle
#====================================================================================
make_submission <- function (my_forecast,m_name){
  print("entered function")
  #  my_forecast <- pred_df
  
  new_sub_id = paste0(test.df$air_store_id,"_", test.df$visit_date)
  submission = cbind(new_sub_id, my_forecast)
  
  names(submission) = c('id', 'visitors')
  submission$visitors = pmax(submission$visitors,0)
  submission$visitors = round(submission$visitors,0)
  
  # get rid of the id's NOT in the submission file
  print("starting the extraction of good ids")
  good_id = submit_only_id$id
  all_id = submission$id
  submission = submission[submission$id %in% good_id, ]
  
  #getNow <- as.integer(now())
  submissionFile = paste0(out.path,"TW_", m_name, ".csv")
  write.csv(submission, submissionFile, quote=FALSE, row.names = FALSE)
  
}

make_test <- function (my_forecast,m_name){
  print("entered function")
  #  my_forecast <- pred_df
  
  new_sub_id = test.df$air_store_id
  new_date = test.df$visit_date
  submission = cbind(new_sub_id, new_date, my_forecast)
  
  names(submission) = c('id', 'visit_date', 'visitors')
  submission$visitors = pmax(submission$visitors,0)
  #  submission$visitors = round(submission$visitors,0)
  
  # get rid of the id's NOT in the submission file
  print("starting the extraction of good ids")
  good_id = submit_only_id$id
  all_id = submission$id
  submission = submission[submission$id %in% good_id, ]
  
  #getNow <- as.integer(now())
  submissionFile = paste0(out.path,"TW_", m_name, ".csv")
  write.csv(submission, submissionFile, quote=FALSE, row.names = FALSE)
  
}

#====================================================================================
#  Setup and train the model
#====================================================================================
set.seed(13)

df_m = df[,-c(1)]
df_m = data.matrix(df_m, rownames.force = F)

test_m = data.matrix(test.df, rownames.force = F)

target=(df$visitors)

#train model 
#---------------------------------------------------------------
bst_df = xgboost(data = df_m,
                 label = target,
                 eta = 0.3,
                 max_depth = 15, 
                 nrounds = 35,
                 nfold = 10,
                 subsample = 0.7,
                 colsample_bytree = 0.6,
                 gamma = .2,
                 min_child = 1,
                 eval_metric = 'rmse',
                 objective = "reg:linear"
)

my.pred =  predict(bst_df, test_m)
pred_df = as.data.frame(my.pred)

make_submission(pred_df, "your-file-name")

# make the charting data so you have a 10-day overlap to show
# actual versus predicted.  
#
# Do the actual combining of predicted and actual 
# data in Excel, it's faster.

test_m2 = data.matrix(df.chart.it, rownames.force = F)
chart.pred =  predict(bst_df, test_m2)
tmp = df.chart.it[c('air_store_id', 'visit_date')]
colnames(tmp) = c()
make.chart.data = cbind(tmp, chart.pred)
write.csv(make.chart.data, paste0(data.path, "predicts_for_chart.csv"), quote=FALSE, row.names = FALSE)

