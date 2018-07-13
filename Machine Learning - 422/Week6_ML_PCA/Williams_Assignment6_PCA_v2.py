#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 25 15:37:11 2017

@author: Tamara Williams

Random Forest Classifier for the MNIST data set

Predict 422
Assignment 6, part 2
"""
import time
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
from sklearn import preprocessing
from datetime import datetime

from sklearn.ensemble import RandomForestClassifier
#from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import KFold
from sklearn.metrics import precision_recall_fscore_support, confusion_matrix

# Global Variables
# seed value for random number generators to obtain reproducible results
RANDOM_SEED = 1
RANDOM_SEED_MODEL = 9999
datapath = '/Users/tamtwill/NorthwesternU_MSPA/Classes/Machine Learning - 422/Week6_ML/'


# set the number of folds for cross-validation
N_FOLDS = 10
#N_FOLDS = 2


# get the MNIST dataset 
#----------------------------------------------------------
#commented out since mldata.org seemed to be permanently offline
#
#from sklearn.datasets import fetch_mldata
#mnist = fetch_mldata('MNIST original')


"""
using alternative data source from data.world
"""



mnist_data = pd.read_csv(datapath+'mnist_data.csv')
mnist_target = pd.read_csv(datapath+'mnist_target.csv')
mnist = pd.DataFrame(np.column_stack((mnist_target, mnist_data)))
    

train_data = mnist.loc[0:59999,:]
train_y = mnist_target.loc[0:59999]
train_X = mnist_data.loc[0:59999,:]

test_data = mnist.loc[60000:69999,:]
test_y = test_data.loc[:,0]
test_X = test_data.loc[:,1:785]

 
# Random forest using raw data
m_name = ["Random Forest"]
clf = RandomForestClassifier(n_estimators=10, max_leaf_nodes=12, bootstrap=True,
                    n_jobs=-1, random_state=RANDOM_SEED, max_features='sqrt')
    
start_time = time.time()
clf.fit(train_X, train_y)  
y_test_predict = clf.predict(test_X)
p_r_fs = precision_recall_fscore_support(test_y, y_test_predict, average = 'macro')
end_time = time.time() - start_time
l_val = list(p_r_fs)
fs = round(p_r_fs[0],3)
pr = round(p_r_fs[1],3)
rc = round(p_r_fs[2],3)

print ("Precision     Recall         F-score")
print ('---------------------------------------')
print (fs,"       ", pr,"       ", rc)


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



# now, do PCA and use those results
#---------------------------------------------------------
# define X and y values for the data set
train_X, train_y = mnist.loc[:,1:785], mnist.loc[:,0]

# drop the response variable and scale
scaled_data=preprocessing.scale(train_X)
num_var = scaled_data.shape[1]

pca = PCA()  
#pca.fit_transform(scaled_data)

# start the timing loop for getting PCA components
#---------------------------------------------------------
start_time = time.time()
pca.fit_transform(train_X)
end_time = time.time()

##amount of variance each PC explains an the cummulative variance explained
var_ex = np.round(pca.explained_variance_ratio_, decimals = 3)*100
cumm_var = np.cumsum(np.round(pca.explained_variance_ratio_, decimals = 3)*100)
print ('\n')

for i in range(0,num_var-1):
    if cumm_var[i] < 95.00:
        print ("PC %i accounts for %g%% of variation; cummulative variation is: %g%%"\
        %(i+1, var_ex[i], cumm_var[i]))
        last_i = i

total_time = end_time - start_time
print("------------------------------------------------")
print ("Time to fit/find Principle Components:", total_time)

# plot the results on a scree plot
fig = plt.figure()
ax = fig.add_subplot(111)
fig.suptitle('Scree Plot', fontsize=14)
plt.ylabel('% of Variance Explained')
plt.xlabel('Principal Component')
#plt.plot(var_ex, label = "% Explained Variance per PC")
#ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.plot(cumm_var, label = "% Cummulative Explained Variance")
#ax.get_lines()[1].set_markerfacecolor('maroon')
start, end = ax.get_xlim()
ax.xaxis.set_ticks(np.arange(1, num_var, 50.0))  
plt.xticks(rotation=90)
handles, labels = ax.get_legend_handles_labels()
ax.legend(handles, labels)
plt.show()

# compute full set of principal components (scores)
pca_scores = pca.fit_transform(train_X)

# add principal component scores to the original data frame
df_pca = mnist_target
for i in range(0, last_i):
    name= 'pc'+str(i)
    df_pca[name] = pca_scores[:,i]
    
    
    
# fix the data
#----------------------------------------------
# split into test and training sets
pca_data = df_pca.as_matrix()


#pca_X, pca_y = mnist_X['data'], mnist_y['target']
train_data = pca_data[0:59999,:]
train_y = train_data[:,0]
train_X = train_data[:,1:137]

pca_y_0_59999_df = pd.DataFrame({'label': train_y[0:59999,]}) 
print('\nFrequency distribution for 60,000 observations (for model building)')
print(pca_y_0_59999_df['label'].value_counts(ascending = True))   


# the last 10000 observations cover the ten digits
# these are often used as test data
# digits are arranged in order but the frequencies are unequal     
pca_y_60000_69999_df = pd.DataFrame({'label': pca_data[60000:69999,0]}) 
print('\nFrequency distribution for last 10,000 observations (holdout sample)')
print(pca_y_60000_69999_df['label'].value_counts(ascending = True))  

test_data = pca_data[60000:69999,:]
test_y = test_data[:,0]
test_X = test_data[:,1:137]

# check on shape of the test data array
print('\nShape of test_data:', test_data.shape)
print ('\n\n')


# shuffle the rows because MNIST data rows have a sequence
# with lower digits coming before higher digits
# shuffle is by the first index, which is the rows
np.random.seed(RANDOM_SEED)
np.random.shuffle(train_data)

np.random.seed(RANDOM_SEED)
np.random.shuffle(test_data)

 
# Random forest using PCA 
m_name = ["Random Forest"]
clf = RandomForestClassifier(n_estimators=10, max_leaf_nodes=12, bootstrap=True,
                    n_jobs=-1, random_state=RANDOM_SEED, max_features='sqrt')
    
start_time = time.time()
clf.fit(train_X, train_y)  
y_test_predict = clf.predict(test_X)
p_r_fs = precision_recall_fscore_support(test_y, y_test_predict, average = 'macro')
end_time = time.time() - start_time
l_val = list(p_r_fs)
fs = round(p_r_fs[0],3)
pr = round(p_r_fs[1],3)
rc = round(p_r_fs[2],3)

print ("Precision     Recall         F-score")
print ('---------------------------------------')
print (fs,"        ", pr,"        ", rc)


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
