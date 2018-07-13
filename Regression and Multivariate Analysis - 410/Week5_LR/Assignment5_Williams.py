#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Tue Jul 18 16:40:01 2017

@author: tamtwill
"""

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import scipy.stats as stats
import operator

from statsmodels.formula.api import ols
from sklearn.feature_selection import SelectKBest
from sklearn.linear_model import Lasso, LinearRegression, RidgeCV
from sklearn.preprocessing import StandardScaler
from sklearn.feature_selection import RFE



# read in the data
df=pd.read_csv('/Users/tamtwill/NorthwesternU_MSPA/410 - Regression/Week5_LR/chatterjee-hadi-gasoline.csv', sep = ',')
df
df_x = df.drop('mpg', axis=1)
df_y = df['mpg']

#----------------------
# EDA
#----------------------

#look at basic info on the frame, check for missing values and types
print df.info()

print df.describe()

# look at shape of the data
plt.figure
fig=sns.distplot(df['mpg'])
fig.set(title='MPG')
fig.set(yticklabels=[])
plt.show()

fig = plt.figure()
fig = sns.boxplot(df_x, df_y, orient = 'v')
fig.set_title('MPG')
plt.show()

fig = plt.figure(figsize=(12,12))
ax = sns.pairplot(df)
plt.show()

# as a faster alternative to pairplots, make a correlation plot
corr = df.corr()
#screen top half to get a triangle
top = np.zeros_like(corr, dtype=np.bool)
top[np.triu_indices_from(top)] = True

fig, ax = plt.subplots(figsize=(10,8))
sns.heatmap(corr, mask=top, cmap='coolwarm', center = 0, square=True, 
           linewidths=.5, cbar_kws={'shrink':.5}, 
           annot = True,annot_kws={"size": 12})

#----------------------
# Full Model
#----------------------
# get the columns and make the full model

all_col = list(df.columns.values)
all_col.pop(0)
form1 = '+'.join(all_col)
form1 = 'mpg~' + form1
full_mod = ols(formula = form1, data = df).fit()
print ''
print full_mod.summary()
print ''

fig = plt.figure()
ax = fig.add_subplot(111)
fig.suptitle('Full Model', fontsize=14)
stats.probplot(full_mod.resid, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

fig, ax = plt.subplots()
plt.scatter(full_mod.fittedvalues, full_mod.resid)
fig.suptitle('Full Model', fontsize=14)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for Full Model', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()

#----------------------
# trying Lasso
#----------------------
# using Lasso regression to do feature selection after normalizing variables
scaler = StandardScaler()
X = scaler.fit_transform(df_x)

names = all_col
  
lasso = Lasso(alpha=.1)
lasso.fit(X, df_y)
y_pred = lasso.predict(df_x)
  
lasso_dict = dict((name, value) for (name, value) in zip(all_col, lasso.coef_[0:len(df_x.columns)]))

print '----------- Lasso Results --------------'
print''
print "Lasso coefficients:", lasso_dict
print ''

short_lst = {name for (name,value) in lasso_dict.items() if abs(value) >= .9}
form2 = '+'.join(short_lst)
form2 = 'mpg~'+form2

# using OLS module for consistent comparision across LM models
lasso_mod = ols(formula=form2, data=df).fit()
print lasso_mod.summary()
print ''

fig = plt.figure()
ax = fig.add_subplot(111)
fig.suptitle('Lasso Model', fontsize=14)
stats.probplot(lasso_mod.resid, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

fig, ax = plt.subplots()
plt.scatter(lasso_mod.fittedvalues, lasso_mod.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for Lasso Model', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()

### ===========================================================================
###     Material in the appendix
### ===========================================================================

#----------------------
# trying RidgeCV
#----------------------
print '----------- Ridge Results --------------'
# using Ridge regression with cross-validation 
# to do feature selection after normalizing variables
scaler = StandardScaler()
x = scaler.fit_transform(df_x)
names = all_col
  
ridge = RidgeCV(alphas=(.001, .005, .01, .05, .1, .5, 1.0, 2.0, 10.0), cv=10)
ridge.fit(x, df_y)
y_pred = ridge.predict(df_x)
  
ridge_dict = dict((name, value) for (name, value) in zip(all_col, ridge.coef_[0:len(df_x.columns)]))

print''
print "RidgeCV coefficients:", ridge_dict
print ''


short_lst4 = {(name) for (name,value) in ridge_dict.items() if abs(value) >=.90}
form4 = '+'.join(short_lst4)
form4 = 'mpg~'+form4

# using OLS module for consistent comparision across LM models
ridge_mod = ols(formula=form4, data=df).fit()
print ridge_mod.summary()
print ''


fig = plt.figure()
ax = fig.add_subplot(111)
fig.suptitle('Ridge Model', fontsize=14)
stats.probplot(ridge_mod.resid, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

fig, ax = plt.subplots()
plt.scatter(ridge_mod.fittedvalues, ridge_mod.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for Ridge Model', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()

#----------------------
# trying RFE
#----------------------
print '----------- RFE Results --------------'

#rank all features, i.e continue the elimination until the last one
rfe_mod = RFE(LinearRegression(), n_features_to_select=1)

rfe_mod.fit(df_x, df_y)
names = df_x.columns.values
print ''
print sorted(zip(map(lambda x: round(x, 4), rfe_mod.ranking_), names))
print ''
rfe_mod.support_


short_lst3 = sorted(zip(map(lambda x: round(x, 4), rfe_mod.ranking_), names))
rfe_dict = dict((name, value) for (name, value) in zip(all_col, rfe_mod.support_))
short_lst3 = {name for (name,value) in rfe_dict.items() if value == True}
form3 = '+'.join(short_lst3)
form3 = 'mpg~'+form3

rfe_ols = ols(formula=form3, data=df).fit()
print ''
print rfe_ols.summary()
print ''

fig = plt.figure()
ax = fig.add_subplot(111)
fig.suptitle('RFE Model', fontsize=14)
stats.probplot(rfe_ols.resid, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

fig, ax = plt.subplots()
plt.scatter(rfe_ols.fittedvalues, rfe_ols.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for Recursive Feature Elimination Model', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()


#----------------------
# trying SelectKBest
#----------------------
kbest_mod = SelectKBest()
kbest_mod.fit(df_x, df_y)

#short_lst4 = sorted(zip(map(lambda x: round(x, 4), kbest_mod.scores_), names), reverse=True)
kbest_dict = dict((name, value) for (name, value) in zip(all_col, kbest_mod.scores_[0:len(df_x.columns)]))
sorted_best = sorted(kbest_dict.items(), key=operator.itemgetter(1), reverse=True)

print '----------- KBest Results --------------'
short_lst4=[]
for i in range(0, 5):
    short_lst4.append(sorted_best[i][0])
    form4 = '+'.join(short_lst4)
    form4 = 'mpg~'+form4
    
    kbest_mod = ols(formula=form4, data=df).fit()
    print ''
    print kbest_mod.summary()
    print ''

fig = plt.figure()
ax = fig.add_subplot(111)
fig.suptitle('KBest (5) Model', fontsize=14)
stats.probplot(rfe_ols.resid, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

fig, ax = plt.subplots()
plt.scatter(kbest_mod.fittedvalues, kbest_mod.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for Select K Best Model', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()



#----------------------
# trying Sequential Backward Selection
#----------------------
from mlxtend.feature_selection import SequentialFeatureSelector as SFS
lr = LinearRegression()
X = df_x.as_matrix()
Y = df_y.as_matrix()
back_step = SFS(lr, 
           k_features=5, 
           forward=False, 
           floating=False, 
           scoring='r2',
           cv=0)

back_step = back_step.fit(X,Y)
back_step.k_feature_idx_

bs_mod = ols('mpg~displacement+horsepower+compression_ratio+transmission_speeds+width' , data=df).fit()
print ''
print bs_mod.summary()
print ''

fig = plt.figure()
ax = fig.add_subplot(111)
fig.suptitle('Backward Selection Model', fontsize=14)
stats.probplot(bs_mod.resid, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

fig, ax = plt.subplots()
plt.scatter(bs_mod.fittedvalues, bs_mod.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for Backward Selection Model', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()
