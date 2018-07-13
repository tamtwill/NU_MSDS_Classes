#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct  3 18:55:42 2017
@author: Tamara Williams

Assignment 5 for Predict 422
Topic: Decision Trees and Random Forests

"""


# initialize various system variables
RANDOM_SEED = 13
SET_FIT_INTERCEPT=True

# set the number of folds for cross-validation
N_FOLDS = 10

# import the packages we will need 
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression, Ridge, Lasso, ElasticNet
from sklearn.ensemble import RandomForestRegressor, BaggingRegressor,\
 AdaBoostRegressor, GradientBoostingRegressor, ExtraTreesRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import KFold
from scipy.stats import skew


def move_last_to_first(df):
    # moves the last column in the dataframe to the first postion in the DF
    # no, I did not crib this from Stackoverflow
    cols = df.columns.tolist()
    cols = cols[-1:] + cols[:-1]
    return df[cols]

np.set_printoptions(formatter={'float': '{: 0.3f}'.format})

# get the data, assumes it is in the same directory
boston_df = pd.read_csv("boston.csv")


# Get a view of the data
#--------------------------------------------------
print("Quick view of the data, head and tail")
print(boston_df.head(),'\n')
print(boston_df.tail(), '\n\n\n')

print("Show summary statisitcs for the data")
print(boston_df.describe(), '\n\n\n')

print("Basic type and completeness info")
print(boston_df.info(), '\n\n\n')


# dropping the 'neighborhood' column per instructions for the 
# regression methods
boston_df = boston_df.drop('neighborhood', axis = 1)


# verify the 'neighborhood' column is gone
print(boston_df.columns)

# move the last column, the target, to the first position in the dataframe
df1=move_last_to_first(boston_df)
print('Shape of the data', df1.shape)
orig_data = df1.copy()
orig_data = orig_data.as_matrix()
col_names = df1.columns.values

# scale the df1 data
scale_it = StandardScaler()
print(scale_it.fit(df1))
# verify results are what you expected
print(scale_it.mean_)
print(scale_it.scale_)
m1_data = scale_it.fit_transform(df1)
print("Shape of model 1 data:", m1_data.shape)

fig = plt.figure()
plt.title("Distribution of reponse variable")
ax = sns.distplot(df1.mv)
plt.savefig('mv-dist.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()

print (skew(df1.mv))


# Build and cross-validate regression models
#--------------------------------------------------
#----- Setup the list of linear regression models to look at

reg_methods = ['Linear_Regression', 'Ridge_Regression', 'Lasso_Regression', 
          'ElasticNet_Regression', 'Bagging Ensemble', 
          'Random Forest', 'AdaBoost','Gradient Boost 1.0','Gradient Boost .1', 
          'Extra Trees']

regress_list = [LinearRegression(fit_intercept = SET_FIT_INTERCEPT), 
               Ridge(alpha = 1, solver = 'cholesky', 
                     fit_intercept = SET_FIT_INTERCEPT, 
                     normalize = False, 
                     random_state = RANDOM_SEED),
               Lasso(alpha = 0.1, max_iter=10000, tol=0.01, 
                     fit_intercept = SET_FIT_INTERCEPT, 
                     random_state = RANDOM_SEED),
               ElasticNet(alpha = 0.1, l1_ratio = 0.5, 
                    max_iter=10000, tol=0.01, 
                    fit_intercept = SET_FIT_INTERCEPT, 
                    normalize = False, 
                    random_state = RANDOM_SEED),
               BaggingRegressor(DecisionTreeRegressor(random_state=RANDOM_SEED, max_features='log2'), 
                    n_estimators=100,max_samples=100, bootstrap=True, 
                    n_jobs=-1, random_state=RANDOM_SEED),
               RandomForestRegressor(n_estimators=100, max_leaf_nodes=12, bootstrap=True,
                    n_jobs=-1, random_state=RANDOM_SEED, max_features='log2'),
               AdaBoostRegressor(DecisionTreeRegressor(max_depth=5), 
                    n_estimators=100, learning_rate=0.5, random_state=RANDOM_SEED),
               GradientBoostingRegressor(max_depth=5, n_estimators=100, 
                    learning_rate=1.0, random_state=RANDOM_SEED, max_features='log2'),
               GradientBoostingRegressor(max_depth=5, n_estimators=100, 
                    learning_rate=0.1, random_state=RANDOM_SEED, max_features='log2'),
               ExtraTreesRegressor(n_estimators=100, criterion='mse', max_depth=5, 
                    min_samples_split=2, min_samples_leaf=1, max_features='log2', 
                    bootstrap=True, random_state=RANDOM_SEED)
              ]
         
   

def eval_model(df):

    # array to hold results
    cross_val_res = np.zeros((N_FOLDS, len(reg_methods)))
    r2_val_res = np.zeros((N_FOLDS, len(reg_methods)))
        
    k_folds = KFold(n_splits=N_FOLDS, shuffle=False, random_state=RANDOM_SEED)

    fold_index = 0
    for train_index, test_index in k_folds.split(df):
        print("Index is:", fold_index,"________________")
        # set up the split between train and test
        # relies on response being in df[0]
        X_train = df[train_index, 1:df.shape[1]]
        X_test = df[test_index, 1:df.shape[1]]
        y_train = df[train_index, 0]
        y_test = df[test_index, 0]   
        print('Shape of input data for this fold:','Data Set: (Observations, Variables)')
        print('X_train:', X_train.shape)
        print('X_test:',X_test.shape)
        print('y_train:', y_train.shape)
        print('y_test:',y_test.shape)
    
        
        method_index = 0
        for model_name, method in zip(reg_methods, regress_list):
            print("\n\n\nRegression model evaluation for model:", model_name)
            print("Scikit_Learn method:", method)
            method.fit(X_train,y_train)
            
            # run the eval on this fold
            y_test_predict = method.predict(X_test)
            r2_val = r2_score(y_test, y_test_predict) #
            print("R-squared is:", r2_val)
            fold_method_res = np.sqrt(mean_squared_error(y_test, y_test_predict))
            print(method.get_params(deep=True))
            print('Root mean-squared error:', fold_method_res)
            cross_val_res[fold_index, method_index] = fold_method_res
            r2_val_res[fold_index, method_index] = r2_val
            
            method_index += 1
      
        fold_index += 1
    
    cross_val_res_df = pd.DataFrame(cross_val_res)
    cross_val_res_df.columns = reg_methods
    r2_val_res_df = pd.DataFrame(r2_val_res)
    r2_val_res_df.columns = reg_methods

    res=cross_val_res_df.mean()
    r2=r2_val_res_df.mean()
    
    tmp = pd.concat([res, r2], axis=1)
    tmp.columns = ['RSME', 'R2']
   

    return res, r2, tmp
    print ("***********************************")


#**************************************************
# Run the build and evaluate model loop
#**************************************************
# do evaluation with the original response variable
#print ("\n\n\n************ SUMMARY REGRESSION RESULTS USING SCALED DATA ")
m1_res, m1_r2, m1_tmp = eval_model(m1_data)

# do evaluation with log(response) as target variable
#print ("\n\n\n************ SUMMARY REGRESSION RESULTS USING RAW DATA ")
#orig_res = eval_model(orig_data)



# Output results of cross-validation for comparison
#--------------------------------------------------

print ("\n\n\n************ SUMMARY REGRESSION RESULTS USING SCALED DATA ")
print('Average results from ', N_FOLDS, '-fold cross-validation\n',
      'in standardized units (mean 0, standard deviation 1)\n', sep = '')     

print (m1_tmp.sort_values('RSME'))


#print ("\n\n\n************ SUMMARY REGRESSION RESULTS USING RAW DATA ")
#print('Average results from ', N_FOLDS, '-fold cross-validation\n',
#      '\nMethod               Root mean-squared error', sep = '')     
#
#pretty_out = orig_res.map('{:,.5f}'.format)
#print(pretty_out.sort_values())

########################################################################
# get the feature importance for the models that have it
# skip Gradient Boost 1.0, it is not successful in the RSME value
# so it is not needed here
########################################################################
def get_importance(df):  
   
   reg_methods = ['Random Forest', 'AdaBoost','Gradient Boost .1','Extra Trees']
    
   regress_list = [RandomForestRegressor(n_estimators=100, max_leaf_nodes=12, 
                    n_jobs=-1, random_state=RANDOM_SEED, max_features='log2'),
               AdaBoostRegressor(DecisionTreeRegressor(max_depth=5), 
                    n_estimators=100, learning_rate=0.5, 
                    random_state=RANDOM_SEED),
               GradientBoostingRegressor(max_depth=5, n_estimators=100, 
                    learning_rate=0.1, random_state=RANDOM_SEED, max_features='log2'),
               ExtraTreesRegressor(n_estimators=100, criterion='mse', max_depth=5, 
                    min_samples_split=2, min_samples_leaf=1, max_features='log2', 
                    bootstrap=True, random_state=RANDOM_SEED)
                  ]
   feature_list = np.delete(col_names,0)
         
#def get_importance(df):  
    
   X_train = df[:, 1:13]
   y_train = df[:, 0]
           
   method_index = 0
   for model_name, method in zip(reg_methods, regress_list):
       method.fit(X_train,y_train)
       feature_import = np.round(method.feature_importances_,4)
       array_stack = np.column_stack([feature_list, feature_import])
       pretty_array = array_stack[np.argsort(array_stack[:, 1])]
       print('\n----------------------------------------------')
       print('Feature importance for method', model_name, '\n')
       print((pretty_array[::-1]), '\n')
       method_index += 1
   return pretty_array
    
print ("\n\n\n************ FEATURE IMPORTANCE SCALED DATA ***********************")
    
get_importance(m1_data)

#print ("\n\n\n************ FEATURE IMPORTANCE RAW DATA ***********************")
#    
#get_importance(orig_data)
#      


