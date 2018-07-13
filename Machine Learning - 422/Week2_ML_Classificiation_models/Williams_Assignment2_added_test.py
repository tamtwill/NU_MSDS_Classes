##############################################################################
# 
# Assignment 2
# Submitted by: Tamara Williams
# Topic: Classification
# Notes: Jumpstart code provided as part of the assigment, student's work
#   follows the jumpstart block
#
##############################################################################

# --------------- START OF JUMPSTART CODE ----------------------------
# Jump-Start for the Bank Marketing Study
# as described in Marketing Data Science: Modeling Techniques
# for Predictive Analytics with R and Python (Miller 2015)
# jump-start code revised by Thomas W. Milller (2017/09/26)

# Scikit Learn documentation for this assignment:
# http://scikit-learn.org/stable/auto_examples/classification/
#   plot_classifier_comparison.html
# http://scikit-learn.org/stable/modules/generated/
#   sklearn.naive_bayes.BernoulliNB.html#sklearn.naive_bayes.BernoulliNB.score
# http://scikit-learn.org/stable/modules/generated/
#   sklearn.linear_model.LogisticRegression.html
# http://scikit-learn.org/stable/modules/model_evaluation.html 
# http://scikit-learn.org/stable/modules/generated/
#  sklearn.model_selection.KFold.html

# seed value for random number generators to obtain reproducible results
RANDOM_SEED = 1

# import base packages into the namespace for this program
import numpy as np
import pandas as pd

# use the full data set after development is complete with the smaller data set
# bank = pd.read_csv('bank-full.csv', sep = ';')  # start with smaller data set

# initial work with the smaller data set
bank = pd.read_csv('bank.csv', sep = ';')  # start with smaller data set
# examine the shape of original input data
print(bank.shape)

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

# define response variable to use in the model
response = bank['response'].map(convert_to_binary)

# gather three explanatory variables and response into a numpy array 
# here we use .T to obtain the transpose for the structure we want
model_data = np.array([np.array(default), np.array(housing), np.array(loan), 
    np.array(response)]).T

# examine the shape of model_data, which we will use in subsequent modeling
print(model_data.shape)

# the rest of the program should set up the modeling methods
# and evaluation within a cross-validation design
# --------------- END OF JUMPSTART CODE ----------------------------


##############################################################################

# ---------------START OF STUDENT CODE -----------------------------
#-------------------------------------------------------------------

import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import cross_val_score, cross_val_predict
from sklearn.metrics import confusion_matrix, f1_score
from sklearn.naive_bayes import BernoulliNB
from sklearn.metrics import roc_curve, precision_recall_curve

#---------------- FROM THE TEXTBOOK 
def plot_roc_curve(fpr, tpr, label=None):
    plt.plot(fpr, tpr, linewidth=2, label=label)
    plt.plot([0, 1], [0, 1], 'k--')
    plt.axis([0, 1, 0, 1])
    plt.xlabel('False Positive Rate', fontsize=16)
    plt.ylabel('True Positive Rate', fontsize=16)
#--------

header = ["default", "housing", "loan", "response"]
bank_df = pd.DataFrame(model_data, columns = header)


# brief visualization of the data
plt.figure(figsize=(8, 6))
plt.figure(1)
plt.suptitle("Counts of the variable values")
plt.subplot(221)
sns.countplot(bank_df['response'])
plt.grid(True)

plt.subplot(222)
sns.countplot(bank_df['default'])
plt.grid(True)

plt.subplot(223)
sns.countplot(bank_df['loan'])
plt.grid(True)

plt.subplot(224)
sns.countplot(bank_df['housing'])
plt.grid(True)
plt.subplots_adjust(top=0.75, bottom=0.05, left=0.10, right=0.95, hspace=0.5,wspace=0.5)
plt.savefig('count_of_vars.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()

count_1 = bank_df['response'].value_counts()
print ("Count each of the response variables is:\n", count_1, '\n')


# Look at the response distribution to see if 0 and 1 have roughly same frequency
bank_df[bank_df['response'] == 1].sum()

corr = bank_df.corr()

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
#train_df, test_df = train_test_split(bank_df, test_size=.2, random_state=13)


# set up the x and y variables
x_all = bank_df.values[:, 0:3]
y_all = bank_df["response"]
#x_train=train_df.values[:, 0:3]
#y_train=train_df["response"]
#x_test=test_df.values[:, 0:3]
#y_test=test_df["response"]


# assignment says "use all complete records" so I am use all_x and all_y,
# rather than the test/train split
log_reg = LogisticRegression(penalty='l1')
log_reg.fit(x_all,y_all)

# get cross-validated scores for the training data
roc_scores=cross_val_score(log_reg, x_all,y_all, scoring = "roc_auc", cv=10)
print("Scores for the Logistic Regression are:")
print("ROC AUC scores are:", roc_scores)
print("Average ROC score for Logistic Regression is:", np.round(np.mean(roc_scores),3), '\n')
acc_scores=cross_val_score(log_reg, x_all,y_all, scoring = "accuracy", cv=10)
print("Accuracy scores are:", acc_scores)
print("Average accuracy score for Logistic Regression is:", np.round(np.mean(acc_scores),3), '\n')


# display an ROC curve
y_log = cross_val_predict(log_reg, x_all, y_all, cv=5, method="predict_proba")
# hack to work around issue #9589 introduced in Scikit-Learn 0.19.0
if y_log.ndim == 2:
    y_log = y_log[:, 1]
fpr, tpr, _ = roc_curve(y_all, y_log)
plt.figure(figsize=(8, 6))
plt.title("Logistic Regression ROC Curve")
plot_roc_curve(fpr, tpr)
plt.savefig('roc_curve_LR.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()

# display a PR curve
y_log = cross_val_predict(log_reg, x_all, y_all, cv=5, method="predict_proba")
# hack to work around issue #9589 introduced in Scikit-Learn 0.19.0
if y_log.ndim == 2:
    y_log = y_log[:, 1]
pr, rc, _ = precision_recall_curve(y_all, y_log)
plt.figure(figsize=(8, 6))
plt.title("Logistic Regression Precision/Recall Curve")
plt.plot(rc, pr)
plt.savefig('pr_curve_LR.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()


# get the cross-validated predictions, and see what happens with a .05 cut-off
predicted_y = cross_val_predict(log_reg, x_all, y_all, cv=10, method="predict_proba")

cv_y = []
for  p in range(len(predicted_y)):
    if predicted_y[p][0] >= .88:
        cv_y.append(1)
    else:
        cv_y.append(0)

labels = ["is pos", "is neg"]
conf_mat=confusion_matrix(y_all, cv_y)
#print(conf_mat)

# visualize results
fig = plt.figure()
ax = fig.add_subplot(111)
sns.heatmap(conf_mat, annot = True, annot_kws={'size': 9}, fmt = 'd', cmap="YlGnBu")# font size
plt.ylabel('Actual')
plt.xlabel('Predicted')
plt.title("Confusion matrix Logistic Regression")
plt.savefig('conf_LR.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()

# compute F1 score for fun and education
f1s=f1_score(y_all, cv_y)
print("F1 score for Logistic Regression is:", np.round((f1s), 3))
print('\n', '\n')

# Second, the Naive Bayes Model
#-------------------------------------------

# assignment says "use all complete records" so I am use all_x and all_y,
# rather than the test/train split
bernNB = BernoulliNB()
bernNB.fit(x_all,y_all)


# get cross-validated scores for the training data
roc_scores=cross_val_score(bernNB, x_all,y_all, scoring = "roc_auc", cv=10)
print("Scores for the Naive Bayes Regression are:")
print("ROC AUC scores are:", roc_scores)
print("Average ROC score for Naive Bayes is:", np.round(np.mean(roc_scores),3), '\n')
acc_scores=cross_val_score(bernNB, x_all,y_all, scoring = "accuracy", cv=10)
print("Accuracy scores are:", acc_scores)
print("Average accuracy score for Naive Bayes is:", np.round(np.mean(acc_scores),3), '\n')

y_NB = cross_val_predict(bernNB, x_all, y_all, cv=10, method="predict_proba")
# hack to work around issue #9589 introduced in Scikit-Learn 0.19.0
if y_NB.ndim == 2:
    y_NB = y_NB[:, 1]
fpr, tpr, _ = roc_curve(y_all, y_NB)
plt.figure(figsize=(8, 6))
plt.title("Naive Bayes Regression ROC Curve")
plot_roc_curve(fpr, tpr)
plt.savefig('roc_curve_NB.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()
plt.show()

# get the cross-validated predictions, and see what happens with a .05 cut-off
predicted_y = cross_val_predict(bernNB, x_all, y_all, cv=10, method="predict_proba")

cv_y = []
for  p in range(len(predicted_y)):
    if predicted_y[p][0] >= .88:
        cv_y.append(1)
    else:
        cv_y.append(0)
        

labels = ["is pos", "is neg"]
conf_mat=confusion_matrix(y_all, cv_y)
#print(conf_mat)

# visualize results
fig = plt.figure()
ax = fig.add_subplot(111)
sns.heatmap(conf_mat, annot = True, annot_kws={'size': 9}, fmt = 'd', cmap="YlGnBu")# font size
plt.title('Confusion Matrix')
plt.ylabel('Actual')
plt.xlabel('Predicted')  
plt.title("Confusion matrix Naive Bayes Regression")
plt.savefig('conf_NB.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()

# compute F1 score for fun and education
f1s = f1_score(y_all, cv_y)
print("F1 score for Naive Bayes is:", np.round((f1s), 3))

########################################################################
# per the discussion I missed, going back and adding code to fit a
# logistic regression model and then test with synthetic data I created
# of the 8 possible values for the 3 variables
########################################################################

df_test = pd.read_csv('test_data.csv', sep = ',') 
df_test.describe()


# gather three explanatory variables and response into a numpy array 
# here we use .T to obtain the transpose for the structure we want
test_data = np.array([np.array(default), np.array(housing), np.array(loan)]).T


test_pred = log_reg.predict(df_test)
test_pred_prob =log_reg.predict_proba(df_test)

print(test_pred,'\n')
print(test_pred_prob)


test_NB = bernNB.predict(df_test)
test_NB_prob = bernNB.predict_proba(df_test)

print(test_NB, '\n')
print(test_NB_prob)


max(test_NB_prob[:,1])



