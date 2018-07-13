#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 24 17:19:43 2017

@author: Tamara Williams

Random Forest Classifier for the MNIST data set

Predict 422
Assignment 6, part 1
"""



# import the packages we will need 
import time
import numpy as np
import pandas as pd
import seaborn as sns

import matplotlib.pyplot as plt
#from matplotlib.backends.backend_pdf import PdfPages  # plot to pdf files

#from sklearn.pipeline import Pipeline
from sklearn.ensemble import RandomForestClassifier
#from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import KFold
from sklearn.metrics import precision_recall_fscore_support, confusion_matrix


# initialize various system variables
RANDOM_SEED = 13
SET_FIT_INTERCEPT=True

# set the number of folds for cross-validation
N_FOLDS = 10
#N_FOLDS = 2


# get the MNIST dataset 
#----------------------------------------------------------
from sklearn.datasets import fetch_mldata
mnist = fetch_mldata('MNIST original')
mnist

# seed value for random number generators to obtain reproducible results
RANDOM_SEED = 1
RANDOM_SEED_MODEL = 9999

# define arrays from the complete data set
mnist_X, mnist_y = mnist['data'], mnist['target']

# show stucture of numpy arrays
# 70,000 observations, 784 explanatory variables/features
# features come from 28x28 pixel displays
# response is a single digit 0 through 9
print('\n Structure of explanatory variable array:', mnist_X.shape)
print('\n Structure of response array:', mnist_y.shape)

# summarize the sequential structure of the MNIST data
# target/label and index values for the observations
# the first 60 thousand observations are often used as training data        
# they cover the ten digits... aranged in order... that is, zeros come
# before ones, ones before twos, and so on
# but the observed digit frequencies are unequal    
# examine the frequency distributions for the digits using pandas DataFrame
# the first 60 thousand observations are often used as training data    
mnist_y_0_59999_df = pd.DataFrame({'label': mnist_y[0:59999,]}) 
print('\nFrequency distribution for 60,000 observations (for model building)')
print(mnist_y_0_59999_df['label'].value_counts(ascending = True))   
train_y = np.r_[mnist_y[0:59999,]]
train_X = np.r_[mnist_X[0:59999,]]
train_data = np.concatenate((train_y.reshape(-1, 1), train_X), axis = 1)

# check on shape of the training data array
print('\nShape of training_data:', train_data.shape)

# the last 10000 observations cover the ten digits
# these are often used as test data
# digits are arranged in order but the frequencies are unequal     
mnist_y_60000_69999_df = pd.DataFrame({'label': mnist_y[60000:69999,]}) 
print('\nFrequency distribution for last 10,000 observations (holdout sample)')
print(mnist_y_60000_69999_df['label'].value_counts(ascending = True))  

test_y = np.r_[mnist_y[60000:69999,]]
test_X = np.r_[mnist_X[60000:69999,]]
test_data = np.concatenate((test_y.reshape(-1, 1), test_X), axis = 1) 

# check on shape of the test data array
print('\nShape of test_data:', test_data.shape)


# shuffle the rows because MNIST data rows have a sequence
# with lower digits coming before higher digits
# shuffle is by the first index, which is the rows
np.random.seed(RANDOM_SEED)
np.random.shuffle(train_data)

np.random.seed(RANDOM_SEED)
np.random.shuffle(test_data)


## build a random forest model
##----------------------------------------------------------------
#names = ["Random Forest"]
#classifiers = [RandomForestClassifier(n_estimators=10, max_leaf_nodes=12, bootstrap=True,
#                    n_jobs=-1, random_state=RANDOM_SEED, max_features='sqrt'),
#
#]
#
#
#print('\nProgress with' + str(N_FOLDS) + '-fold cross-validation')
#
## set up numpy array for storing results
#col_head = ["F1 Score", "Precision", "Recall"]
#cv_results = np.zeros((N_FOLDS, len(names)))
#cv_pr = np.zeros((N_FOLDS, len(names)))
#cv_rc = np.zeros((N_FOLDS, len(names)))
#eval_scores = []
#total_time = 0
#
#kf = KFold(n_splits = N_FOLDS, shuffle=False, random_state = RANDOM_SEED)
## check the splitting process by looking at fold observation counts
#index_for_fold = 0  # fold count initialized 
#
#
#for train_index, test_index in kf.split(train_data):
#    print('\nFold index:', index_for_fold,
#          '------------------------------------------')
##   the structure of modeling data for this study has the
##   response variable coming first and explanatory variables later          
##   so 1:model_data.shape[1] slices for explanatory variables
##   and 0 is the index for the response variable    
#    X_train = train_data[train_index, 1:train_data.shape[1]]
#    X_test = train_data[test_index, 1:train_data.shape[1]]
#    y_train = train_data[train_index, 0]
#    y_test = train_data[test_index, 0]   
#    print('\nShape of input data for this fold:',
#          '\nData Set: (Observations, Variables)')
#    print('X_train:', X_train.shape)
#    print('X_test:',X_test.shape)
#    print('y_train:', y_train.shape)
#    print('y_test:',y_test.shape)
#
#    index_for_method = 0  # initialize
#
#    for name, clf in zip(names, classifiers):
#        print('\nClassifier evaluation for:', name)
#        print('  Scikit Learn method:', clf)
#        start_time = time.time()
#        clf.fit(X_train, y_train)  # fit on the train set for this fold
#        # evaluate on the test set for this fold
#        y_test_predict = clf.predict(X_test)
#        p_r_fs = precision_recall_fscore_support(y_test, y_test_predict, average = 'macro')
#        end_time = time.time() - start_time
#        l_val = list(p_r_fs)
#        eval_scores.append(l_val)
#        print ("Precision, recall and f-score", p_r_fs)
#        
#        #print graphical version of results
#        save_name = "Per_fold"+str(index_for_fold)
#        show_conf_mat(y_test, y_test_predict, save_name)
#        total_time = total_time + end_time                             
#        index_for_method += 1
#  
#    index_for_fold += 1
#    
#
#cv_eval_df = pd.DataFrame(np.array(eval_scores))
#
#print('\n----------------------------------------------')
#print('Average results from ', N_FOLDS, '-fold cross-validation\n',
#      '\nMethod', name, '\n')  
#
#fs_mean = cv_eval_df[0].mean()
#pr_mean = cv_eval_df[1].mean()
#rc_mean = cv_eval_df[2].mean()



m_name = ["Random Forest"]
clf = RandomForestClassifier(n_estimators=10, max_leaf_nodes=12, bootstrap=True,
                    n_jobs=-1, random_state=RANDOM_SEED, max_features='sqrt')
    
start_time = time.time()
clf.fit(train_X, train_y)  
y_test_predict = clf.predict(test_X)
p_r_fs = precision_recall_fscore_support(test_y, y_test_predict, average = 'macro')
end_time = time.time() - start_time
l_val = list(p_r_fs)
fs = p_r_fs[0]
pr = p_r_fs[1]
rc = p_r_fs[2]

print ("Precision     Recall         F-score")
print ('---------------------------------------')
print (fs, pr, rc)


#print graphical version of results for 95% level
cv_y = []
for  p in range(len(y_test_predict)):
    if y_test_predict[p] >= .95:
        cv_y.append(1)
    else:
        cv_y.append(0)

conf_mat=confusion_matrix(test_y, y_test_predict)
#print(conf_mat)

# visualize results
fig = plt.figure()
ax = fig.add_subplot(111)
sns.heatmap(conf_mat, annot = True, annot_kws={'size': 9}, fmt = 'd', cmap="YlGnBu")# font size
plt.ylabel('Actual')
plt.xlabel('Predicted')
plt.title("Random Forest Actual versus Predicted")
plt.savefig("rf_conf_mat", 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()


print ("Time taken:", end_time)
