#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct  3 18:55:42 2017
@author: Tamara Williams

Assignment 3 for Predict 422
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
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import KFold
from scipy.stats import skew

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

# dropping the 'neighborhood' column per instructions
boston_df = boston_df.drop('neighborhood', axis = 1)

# verify the 'neighborhood' column is gone
print(boston_df.columns)

def move_last_to_first(df):
    # moves the last column in the dataframe to the first postion in the DF
    cols = df.columns.tolist()
    cols = cols[-1:] + cols[:-1]
    return df[cols]

# move the last column, the target, to the first position in the dataframe
df1=move_last_to_first(boston_df)
print('Shape of the data', df1.shape)

# create a copy of the data to see effective of using Log(response)
df2 = df1.copy()

# scale the df1 data
scale_it = StandardScaler()
print(scale_it.fit(df1))
# verify results are what you expected
print(scale_it.mean_)
print(scale_it.scale_)
m1_data = scale_it.fit_transform(df1)
print("Shape of model 1 data:", m1_data.shape)

# replace variable mv with log(mv) in second dataframe
df2['log_mv'] = np.log(df2['mv'])
df2.drop('mv', axis = 1)
df2 = move_last_to_first(df2)
# scale the data
scale_it = StandardScaler()
print(scale_it.fit(df2))
# verify results are what you expected
print(scale_it.mean_)
print(scale_it.scale_)
m_log_data = scale_it.fit_transform(df2)
print("Shape of model 2, log(mv) data:", m_log_data.shape)

fig = plt.figure()
plt.title("Distribution of reponse variable")
ax = sns.distplot(df1.mv)
plt.savefig('mv-dist.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()

fig = plt.figure()
plt.title("Distribution of reponse variable")
ax = sns.distplot(df2.log_mv)
plt.savefig('mv-log-dist.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()

skew(df1.mv)
skew(df2.log_mv)



# Build and cross-validate models
#--------------------------------------------------

#----- Setup the list of models to look at
#----- Code below from class evaluate-regression-models sample

#reg_methods = ['ElasticNet']
#
#regress_list = [ElasticNet(alpha = 0.1, l1_ratio = 0.5, 
#                          max_iter=10000, tol=0.01, 
#                          fit_intercept = SET_FIT_INTERCEPT, 
#                          normalize = False, 
#                          random_state = RANDOM_SEED)]   

reg_methods = ['Linear_Regression', 'Ridge_Regression', 'Lasso_Regression', 
          'ElasticNet_Regression'] 

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
                          random_state = RANDOM_SEED)]
          

def eval_model(df):
   
    # array to hold results
    cross_val_res = np.zeros((N_FOLDS, len(reg_methods)))
    
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
            print("The fitted intercept =", method.intercept_)
            print("The fitted coefficients are:", method.coef_)
            
            # run the eval on this fold
            y_test_predict = method.predict(X_test)
            print("R-squared is:", r2_score(y_test, y_test_predict))
            fold_method_res = np.sqrt(mean_squared_error(y_test, y_test_predict))
            print(method.get_params(deep=True))
            print('Root mean-squared error:', fold_method_res)
            cross_val_res[fold_index, method_index] = fold_method_res
            method_index += 1
      
        fold_index += 1
    
    cross_val_res_df = pd.DataFrame(cross_val_res)
    cross_val_res_df.columns = reg_methods
    
    
    print('\n----------------------------------------------')
    print('Average results from ', N_FOLDS, '-fold cross-validation\n',
          'in standardized units (mean 0, standard deviation 1)\n',
          '\nMethod               Root mean-squared error', sep = '')     
    print(cross_val_res_df.mean())  
    res=cross_val_res_df.mean()
    
    return res
    print ("***********************************")


#**************************************************
# Run the build and evaluate model loop
#**************************************************
# do evaluation with the original response variable
m1_res = eval_model(m1_data)

# do evaluation with log(response) as target variable
m_log_res = eval_model(m_log_data)




# Output results of cross-validation for comparison
#--------------------------------------------------

print ("\n\n\n************ SUMMARY RESULTS FOR ORIGINAL VERSUS LOG RESPONSE VARIABLES ***********************")

print('\n------------------Original Response variable results ----------------------------')
print('Average results from ', N_FOLDS, '-fold cross-validation\n',
      'in standardized units (mean 0, standard deviation 1)\n',
      '\nMethod               Root mean-squared error', sep = '')     
print(m1_res) 
 
print('\n------------------Taking Log of Response variable results ----------------------------')
print('Average results from ', N_FOLDS, '-fold cross-validation\n',
      'in standardized units (mean 0, standard deviation 1)\n',
      '\nMethod               Root mean-squared error', sep = '')     
print(m_log_res)  


