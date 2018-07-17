######## Predict 498,
######## Submitted by: Tamara Williams
########
######## 



# For the sake of good programming hygiene, start clean
# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Get location script, and set to working directory
#-------------------------------------------------------------------
working.dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(working.dir)
data.path = "/Users/tamtwill/gitRepo/capstone_raw_data/"
out.path = "/Users/tamtwill/gitRepo/capstone_raw_data/submissions/"

# include required packages
#-------------------------------------------------------------------
library(reshape2)
library(dplyr)
require(forecastxgb)
library(fpp)
library(lubridate)
require(methods)
require(data.table)
require(magrittr)
require(Ckmeans.1d.dp)

#___________________________________________________________________________________
make_submission  =  function (my_forecast,m_name){
  print("entered function")
#  my_forecast  =  full.results
  
  
  # get the date range for the submission
  tmp_date = as.Date(df$visit_date, format = "%Y-%m-%d")
  train = (max(tmp_date)) + days(1) 
  day_tmp  =  as.character.Date(seq(train, as.Date("2017-05-31"), by = "day"))
  id_tmp  =  colnames(df)
  
  k=1
  len_id  =  dim(my_forecast)[1]
  len_day =  length(day_tmp)
  for(i in seq(1,len_id, by=39)){
    for(j in 1:39){
      store=my_forecast$id[i]
      my_forecast$new_id[k] = as.character(paste0(store,'_',day_tmp[j]))
      print(paste0("Current k: ", k))
      k=k+1
      next #j
    }
    next  #i
  }
  
  tmp_forecast = my_forecast[c(3,2,1)]
  
  submission  =  tmp_forecast[-c(3)]
  names(submission) = c('id', 'visitors')

  # get rid of the id's NOT in the submission file
  print("starting the extraction of good ids")
  good_id  =  as.vector(submit_only_id$id)
  
  submission =  unique(submission[submission$id %in% good_id,])
  
  #getNow  =  as.integer(now())
  submissionFile  = paste0(out.path,"TW_", m_name, ".csv", sep="")
  write.csv(submission, submissionFile, quote=FALSE, row.names = FALSE)
  
}
#**********************************************


###############################################################################
######                       Read and prep data block                ##########
###############################################################################

# get the data
df=read.csv(paste0(data.path,"all_visits_weather_geo.csv"),header=T, stringsAsFactors = FALSE)
submit_only_id=read.csv(paste0(data.path,"sample_submission 3.csv"),header=T, stringsAsFactors = FALSE)

# move visitors to position 1
refcols  =  c("visitors", "visit_date", "air_store_id")
df = df[, c(refcols, setdiff(names(df), refcols))]

# drop some unneeded columns
df = df[ ,-c(13:15)]

# convert various columns to better type
cols.to.factors <- c("air_genre_name", "air_area_name", "day_of_week")
df %<>% mutate_at(cols.to.factors, funs(factor(.)))
df$visit_date = as.Date(df$visit_date)
df[is.na(df)] = 0

#train <- dcast(df, visit_date~air_store_id, value.var="visitors", fun.aggregate=sum)

#======================================================================================================
#======================================================================================================
#======================================================================================================
# find the iterations to run
set.seed(13)
n_rounds = 100
n_folds = 10

# air = train[,-c(1)]
# air = train
# air_ts = ts(air, start=c(2016,1,1), frequency=365)

          
#air_m = data.matrix(air_m, rownames.force = T)
target=(df$visitors)
iter_store = unique(df$air_store_id)

# i = 1
# this.store=df %>% filter(df$air_store_id == iter_store[i])
# air = this.store[,-c(3)]
# air_ts = ts(air, start=c(2016,1,1), frequency=1)
# x = as.numeric(this.store$visitors)


full.results7 = data.frame()
for(i in 1:829){  #829
  print(i)
  this.store=df %>% filter(df$air_store_id == iter_store[i])
  air = this.store[,-c(3)]
  air_ts = ts(air, start=c(2016,1,1), frequency=1)
  x = as.numeric(this.store$visitors)
  

  #train model store[i]
  bst = xgbar(y = air_ts,
                max_depth = 1,
                nrounds=n_rounds,
                nrounds_method = 'cv'
  )

    predict_this = forecast(bst, h=39)
    
    predicted_results = data.frame()
    tmp_predict = predict_this$newx[1:39,7]
    tmp.stores = data.frame()
    
    for (j in 1:39){
      get.stores = iter_store[i]
      tmp.stores = rbind(tmp.stores, get.stores)
    }
    predicted_results = cbind(tmp.stores, tmp_predict)
    colnames(predicted_results) = c('id','visits')
full.results7 = rbind(predicted_results, full.results7)
}

make_submission(full.results7, "ts_xgb_lag7")

# make submission for lag 1
#-----------------------------------------------------
full.results1 = data.frame()
for(i in 1:829){  #829
  print(i)
  this.store=df %>% filter(df$air_store_id == iter_store[i])
  air = this.store[,-c(3)]
  air_ts = ts(air, start=c(2016,1,1), frequency=1)
  x = as.numeric(this.store$visitors)
 
  predicted_results = data.frame()
  tmp_predict = predict_this$newx[1:39,1]
  tmp.stores = data.frame()
  
  for (j in 1:39){
    get.stores = iter_store[i]
    tmp.stores = rbind(tmp.stores, get.stores)
  }
  predicted_results = cbind(tmp.stores, tmp_predict)
  colnames(predicted_results) = c('id','visits')
full.results1 = rbind(predicted_results, full.results1)
}

make_submission(full.results1, "ts_xgb_lag1")

# average/ensemble Lag 1 and lag 7
#-----------------------------------------------------
tmp = cbind(full.results1, full.results7$visits)
tmp$avg = rowMeans(tmp[-1])
tmp = tmp[-c(2,3)]
colnames(tmp) = c('id','visits')
make_submission(tmp, "ts_xgb_lag1e7")
