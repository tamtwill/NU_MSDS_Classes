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
require(xgboost)
library(lubridate)
require(methods)
require(data.table)
require(magrittr)
require(Ckmeans.1d.dp)
require(caret)

#___________________________________________________________________________________
# Function to output the forecast in the right format for Kaggle
#___________________________________________________________________________________
make_submission <- function (my_forecast,m_name){
  print("entered function")
#  my_forecast <- pred_df
  
   new_sub_id = paste0(test.df$air_store_id,"_", test.df$visit_date)
   submission = cbind(new_sub_id, my_forecast)

  names(submission)<-c('id', 'visitors')
  submission$visitors <- round(submission$visitors,0)
  
  # get rid of the id's NOT in the submission file
  print("starting the extraction of good ids")
  good_id = submit_only_id$id
  all_id = submission$id
  submission = submission[submission$id %in% good_id, ]
  
  #getNow <- as.integer(now())
  submissionFile <-paste("TW_", m_name, ".csv", sep="")
  write.csv(submission, submissionFile, quote=FALSE, row.names = FALSE)
  
}
#**********************************************

###############################################################################
######                       Read and prep data block                ##########
###############################################################################

# get the data
df=read.csv(paste0(data.path,"all_visits_w_weather.csv"),header=T, stringsAsFactors = FALSE)
test.df = read.csv("test_visits_w_weather.csv", header = T, stringsAsFactors = T)
submit_only_id=read.csv(paste0(data.path,"sample_submission 3.csv"),header=T, stringsAsFactors = FALSE)



# move visitors to position 1
refcols  =  c("visitors", "air_store_id", "visit_date")
df = df[, c(refcols, setdiff(names(df), refcols))]

# convert various columns to better type
cols.to.factors <- c("air_store_id", "air_genre_name", "air_area_name", "day_of_week", "station_id")
df %<>% mutate_at(cols.to.factors, funs(factor(.)))
df$visit_date = as.Date(df$visit_date)
df[is.na(df)] = 0

#train <- dcast(df, visit_date~air_store_id, value.var="visitors", fun.aggregate=sum)

#======================================================================================================
#======================================================================================================
#======================================================================================================

# find the best number of iterations to run
#---------------------------------------------------------------
n_round <- 100
n_fold <- 10
set.seed(13)
params <- list(objective = "reg:linear",
               eta = 0.2,
               max_depth = 15
)
df_m = df[,-c(1,13,14)]
df_m = data.matrix(df_m, rownames.force = F)
test.df = test.df[, -c(12,13)]
test_m = data.matrix(test.df, rownames.force = F)

target=(df$visitors)

#---------------------------------------------------------------
# grid search parameters
#-------------------------------------------------------------
searchGridSubCol <- expand.grid(subsample = c(0.5, 0.6), 
                                max_depth = c(2,4,6,8,10,14),
                                subsample = c(0.4, 0.5, 0.6, 0.7, 0.8),
                                colsample_bytree = c(0.4, 0.5, 0.6, 0.7, 0.8),
                                gamma = c(.2, .4, .6, .8, 1.0),
                                min_child = seq(1), 
                                eta = c(0.1, 0.01,0.05,0.001)
)


rmseErrorsHyperparameters = apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentMinChild <- parameterList[["min_child"]]
    currentGamma <- parameterList[["gamma"]]
    xgboostModelCV <- xgb.cv(data =  df_m, nrounds = n_round, nfold = n_fold, showsd = TRUE, 
                             metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                             "objective" = "reg:linear", "max.depth" = currentDepth, "eta" = currentEta,                                      "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                             , print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree",
                             early_stopping_rounds = 10, label = target)
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    rmse <- tail(xvalidationScores$test_rmse_mean, 1)
    trmse <- tail(xvalidationScores$train_rmse_mean,1)
    output <- return(c(rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild))
    
  })

output <- as.data.frame(t(rmseErrorsHyperparameters))
head(output)
varnames <- c("TestRMSE", "TrainRMSE", "SubSampRate", "ColSampRate", "Depth", "eta", "currentMinChild")
names(output) <- varnames
head(output)
write.csv(output, "xgb_gridsearch.csv")

#--------------------------------------------------------------
#train model 
#--------------------------------------------------------------

bst <- xgb.cv(params = params, df_m, nrounds = n_round, nfold = n_fold, label=target, metrics = 'rmse')
plot(bst$evaluation_log$test_rmse_mean)
n_round <- which((bst$evaluation_log$test_rmse_mean == min((bst$evaluation_log$test_rmse_mean))))

bst_df = xgboost(data = df_m,
                 label = target,
                 eta = etas,
                 max_depth = 15,
                 nrounds=n_round,
                 nfold = n_fold,
                 subsample = subsamples,
                 colsample_bytree = colsample_bytrees,
                 eval_metric = 'rmse',
                 objective = "reg:linear",
                 tuneGrid = xgb_grid
)

my.pred =  predict(bst_df, test_m)
pred_df = as.data.frame(my.pred)


# explore the most important features
#---------------------------------------------------------------
model_df <- xgb.dump(bst_df, with_stats = T)
model_df[1:15]

# Get the feature real names
m_names <- dimnames(df_m)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(m_names, model = bst_df)

# Nice graph
xgb.plot.importance(importance_matrix[1:15,], main="Restaurant importance")

make_submission(pred_df, "air_plain_xgb")

