##############################################################################
# 
# Assignment 4
# Submitted by: Tamara Williams
# Topic: Classification - SVM
# 
##############################################################################

# Initialize the dataframe

# seed value for random number generators to obtain reproducible results
RANDOM_SEED = 13

# import base packages into the namespace for this program
import numpy as np
import pandas as pd

# use the full data set after development is complete with the smaller data set
# bank = pd.read_csv('bank-full.csv', sep = ';')  # start with smaller data set

# initial work with the smaller data set
bank = pd.read_csv('bank.csv', sep = ';')  # start with smaller data set
# examine the shape of original input data
print(bank.shape)

# check for incomplete records
bank.isnull().values.any()

# drop observations with missing data, if any
bank.dropna()
# examine the shape of input data after dropping missing data
print(bank.shape)

# look at the list of column names, note that y is the response
list(bank.columns.values)

# look at the beginning of the DataFrame
bank.head()

# mapping function to convert text no/yes to integer 0/1
convert_to_binary = {'no' : 0, 'yes' : 1}

# define binary variable for having credit in default
default = bank['default'].map(convert_to_binary)

# define binary variable for having a mortgage or housing loan
housing = bank['housing'].map(convert_to_binary)

# define binary variable for having a personal loan
loan = bank['loan'].map(convert_to_binary)

# define continuous variable for account balance
balance= bank['balance']

# define response variable to use in the model
response = bank['response'].map(convert_to_binary)

# check for incomplete records
bank.isnull().values.any()

# gather three explanatory variables and response into a numpy array 
# here we use .T to obtain the transpose for the structure we want
model_data = np.array([np.array(default), np.array(housing), np.array(loan),
                       np.array(balance), np.array(response)]).T

# examine the shape of model_data, which we will use in subsequent modeling
print(model_data.shape)

# the rest of the program should set up the modeling methods
# and evaluation within a cross-validation design

##############################################################################


import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LogisticRegression
from sklearn.naive_bayes import BernoulliNB
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC
from sklearn.model_selection import KFold
from sklearn.metrics import roc_auc_score


# brief visualization of the data
plt.figure(figsize=(8, 6))
plt.figure(1)
plt.suptitle("Counts of the variable values")
plt.subplot(221)
sns.countplot(bank['response'])
plt.grid(True)

plt.subplot(222)
sns.countplot(bank['default'])
plt.grid(True)

plt.subplot(223)
sns.countplot(bank['loan'])
plt.grid(True)

plt.subplot(224)
sns.countplot(bank['housing'])
plt.grid(True)
plt.subplots_adjust(top=0.75, bottom=0.05, left=0.10, right=0.95, hspace=0.5,wspace=0.5)
plt.savefig('count_of_vars.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()

plt.figure()
fig,ax = plt.subplots(1)
sns.distplot(bank['balance'])
plt.grid(True)
ax.get_yaxis().set_visible(False)
plt.savefig('dist_of_bal.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()

count_1 = bank['response'].value_counts()
print ("Count each of the response variables is:\n", count_1, '\n')


# Look at the response distribution to see if 0 and 1 have roughly same frequency
bank[bank['response'] == 1].sum()

corr = bank.corr()

plt.figure()
sns.heatmap(corr, cmap="YlGnBu", center = 0, square=True, linewidths=.5, cbar_kws={'shrink':.5}, 
        annot = True, annot_kws={'size': 9}, fmt = '.3f')
plt.title("Correlations between variables")
plt.savefig('corr_of_vars.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()

# First, the Logistic Regression Model
#-------------------------------------------
names = ["Naive_Bayes", "Logistic_Regression"]
classifiers = [BernoulliNB(alpha=1.0, binarize=0.5, 
                           class_prior = [0.5, 0.5], fit_prior=False), 
               LogisticRegression()]


# ten-fold cross-validation employed here
N_FOLDS = 10

# set up numpy array for storing results
cv_results = np.zeros((N_FOLDS, len(names)))

kf = KFold(n_splits = N_FOLDS, shuffle=False, random_state = RANDOM_SEED)
# check the splitting process by looking at fold observation counts
index_for_fold = 0  # fold count initialized 
for train_index, test_index in kf.split(model_data):
    print('\nFold index:', index_for_fold,
          '------------------------------------------')
#   note that 0:model_data.shape[1]-1 slices for explanatory variables
#   and model_data.shape[1]-1 is the index for the response variable    
    X_train = model_data[train_index, 0:model_data.shape[1]-1]
    X_test = model_data[test_index, 0:model_data.shape[1]-1]
    y_train = model_data[train_index, model_data.shape[1]-1]
    y_test = model_data[test_index, model_data.shape[1]-1]   
    print('\nShape of input data for this fold:',
          '\nData Set: (Observations, Variables)')
    print('X_train:', X_train.shape)
    print('X_test:',X_test.shape)
    print('y_train:', y_train.shape)
    print('y_test:',y_test.shape)

    index_for_method = 0  # initialize
    for name, clf in zip(names, classifiers):
        print('\nClassifier evaluation for:', name)
        print('  Scikit Learn method:', clf)
        clf.fit(X_train, y_train)  # fit on the train set for this fold
        # evaluate on the test set for this fold
        y_test_predict = clf.predict_proba(X_test)
        fold_method_result = roc_auc_score(y_test, y_test_predict[:,1]) 
        print('Area under ROC curve:', fold_method_result)
        cv_results[index_for_fold, index_for_method] = fold_method_result
        index_for_method += 1
  
    index_for_fold += 1

cv_results_df1 = pd.DataFrame(cv_results)
cv_results_df1.columns = names

print('\n----------------------------------------------')
print('Average results from ', N_FOLDS, '-fold cross-validation\n',
      '\nMethod                 Area under ROC Curve', sep = '')     
print(cv_results_df1.mean())   


#=======================================================================

sample_weight1 = model_data[:,0]*2
names = ["Logistic_Regression", "Linear_SVM_C=0.01", "Linear_SVM_C=0.1", "Linear_SVM_C=1",\
         "Linear_SVM_C=10","RBF_SVR_C=1", "Poly_SVR_C=1"]
classifiers = classifiers = [LogisticRegression(),
Pipeline([
        ("scaler", StandardScaler()),
        ("svc1", SVC(kernel='linear', probability=True, C=0.01, random_state=13)),
    ]),
Pipeline([
        ("scaler", StandardScaler()),
        ("svc2", SVC(kernel='linear', probability=True, C=0.1, random_state=13)),
    ]),
Pipeline([
        ("scaler", StandardScaler()),
        ("svc3", SVC(kernel='linear', probability=True, C=1, random_state=13)),
    ]),
Pipeline([
        ("scaler", StandardScaler()),
        ("svc4", SVC(kernel='linear', probability=True, C=10, random_state=13)),
    ]),
Pipeline([
        ("scaler", StandardScaler()),
        ("svc5", SVC(kernel='rbf', probability=True, C=1, random_state=13)),
    ]),
Pipeline([
        ("scaler", StandardScaler()),
        ("svc6", SVC(kernel="poly", degree=3, probability=True,coef0=1, C=1))
    ]),
    ]
                       
# ten-fold cross-validation employed here
N_FOLDS = 10

# set up numpy array for storing results
cv_results = np.zeros((N_FOLDS, len(names)))

kf = KFold(n_splits = N_FOLDS, shuffle=False, random_state = RANDOM_SEED)
# check the splitting process by looking at fold observation counts
index_for_fold = 0  # fold count initialized 
for train_index, test_index in kf.split(model_data):
    print('\nFold index:', index_for_fold,
          '------------------------------------------')
#   note that 0:model_data.shape[1]-1 slices for explanatory variables
#   and model_data.shape[1]-1 is the index for the response variable    
    X_train = model_data[train_index, 0:model_data.shape[1]-1]
    X_test = model_data[test_index, 0:model_data.shape[1]-1]
    y_train = model_data[train_index, model_data.shape[1]-1]
    y_test = model_data[test_index, model_data.shape[1]-1]   
    print('\nShape of input data for this fold:',
          '\nData Set: (Observations, Variables)')
    print('X_train:', X_train.shape)
    print('X_test:',X_test.shape)
    print('y_train:', y_train.shape)
    print('y_test:',y_test.shape)

    index_for_method = 0  # initialize
    for name, clf in zip(names, classifiers):
        print('\nClassifier evaluation for:', name)
        print('  Scikit Learn method:', clf)
        clf.fit(X_train, y_train)  # fit on the train set for this fold
        # evaluate on the test set for this fold
        y_test_predict = clf.predict_proba(X_test)
        fold_method_result = roc_auc_score(y_test, y_test_predict[:,1]) 
        print('Area under ROC curve:', fold_method_result)
        cv_results[index_for_fold, index_for_method] = fold_method_result
        index_for_method += 1
  
    index_for_fold += 1

cv_results_df2 = pd.DataFrame(cv_results)
cv_results_df2.columns = names

print('\n----------------------------------------------')
print('Average results from ', N_FOLDS, '-fold cross-validation\n',
      '\nMethod                 Area under ROC Curve', sep = '')     
print(cv_results_df2.mean())   


#*****************************
print('\n----------------------------------------------')
print('Average results from Linear Models ', N_FOLDS, '-fold cross-validation\n',
      '\nMethod                 Area under ROC Curve', sep = '')     
print(cv_results_df1.mean())   

print('\n----------------------------------------------')
print('Average results from SVMs ', N_FOLDS, '-fold cross-validation\n',
      '\nMethod                 Area under ROC Curve', sep = '')     
print(cv_results_df2.mean())   




#########################################################################
## per the discussion I missed, going back and adding code to fit a
## logistic regression model and then test with synthetic data I created
## of the 8 possible values for the 3 variables
#########################################################################

df_test = pd.read_csv('svm_test_data.csv', sep = ',') 
#print (df_test.describe())

for name, clf in zip(names, classifiers):
    print('\nClassifier test predictions results for:', name)
    y_test = clf.predict(df_test)
    y_test_predict = clf.predict_proba(df_test)
    best = np.argmax(y_test_predict[:,1])
    print ("Best test row for response = ", best, '\n')
    print ("Predicted probability of a yes", y_test_predict[best,1],'\n') 
    print("Profile most likely respondant based on this model:\n")
    print (df_test.iloc[best], '\n\n')
    index_for_method += 1

