#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Tue Jul 18 16:40:01 2017
@author: Tamara Williams
"""

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import scipy.stats as stats
from sklearn.decomposition import PCA
from sklearn import preprocessing
import statsmodels.formula.api as sm

# since doing these more than once, let's make some functions
def cor_chart(df_corr):
    corr=df_corr.corr()
    #screen top half to get a triangle
    top = np.zeros_like(corr, dtype=np.bool)
    top[np.triu_indices_from(top)] = True

    fig, ax = plt.subplots(figsize=(8,8))
    sns.heatmap(corr, mask=top, cmap='coolwarm', center = 0, square=True, 
           linewidths=.5, cbar_kws={'shrink':.5}, 
           annot = True,annot_kws={"size": 12})
    print '\n'
    print corr
    print '\n'
    
def res_charts(model, mod_name):    
    fig = plt.figure()
    ax = fig.add_subplot(111)
    fig.suptitle(mod_name+' Model', fontsize=14)
    stats.probplot(model.resid, dist="norm", plot=plt, )
    ax.get_lines()[0].set_markerfacecolor('steelblue')
    plt.show()
    
    fig, ax = plt.subplots()
    plt.scatter(model.fittedvalues, model.resid)
    plt.ylabel('Residuals')
    plt.xlabel('Predicted')
    fig.suptitle('Residual Plot for '+mod_name+' Model', fontsize=14)
    plt.axhline(linewidth=4, color='r')
    ax.tick_params(labelbottom='off')  
    plt.show()


# read in the data
df=pd.read_csv('/Users/tamtwill/NorthwesternU_MSPA/410 - Regression/Week6_LR/chatterjee-hadi-gasoline.csv', sep = ',')

# correlation matrix, visualization and values for subset of data
cor_chart(df)



# remove the response var "mpg"
pca_df = df.drop('mpg', axis=1)

# now scale the data for actual use
scaled_data=preprocessing.scale(pca_df)
num_var = len(pca_df.columns)

pca = PCA()  
pca.fit_transform(scaled_data)

# find the amount of variance each PC explains 
# and the cummulative variance explained
var_ex = np.round(pca.explained_variance_ratio_, decimals = 3)*100
cumm_var = np.cumsum(np.round(pca.explained_variance_ratio_, decimals = 3)*100)
eig_val = np.round(pca.explained_variance_, decimals = 3)
print '\n'
for i in range(0,num_var):
    print "PC %i accounts for %g%% of variation; cummulative variation is: %g%%"\
    %(i+1, var_ex[i], cumm_var[i])

print '\n' 
for i in range(0,num_var):
    print "PC %i has an eigen value of %g" %(i+1, eig_val[i])

# plot the results, basically a scree plot plus cummulative variance explained
fig = plt.figure()
ax = fig.add_subplot(111)
fig.suptitle('Scree and Cummulative Variance Plot', fontsize=14)
plt.ylabel('% of Variance Explained')
plt.xlabel('Principal Component')
plt.plot(var_ex, label = "% Explained Variance per PC")
#ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.plot(cumm_var, label = "% Cummulative Explained Variance")
#ax.get_lines()[1].set_markerfacecolor('maroon')
start, end = ax.get_xlim()
ax.xaxis.set_ticks(np.arange(1, num_var, 1.0))  
handles, labels = ax.get_legend_handles_labels()
ax.legend(handles, labels)
plt.show()

# compute full set of principal components (scores)
pca_scores = pca.fit_transform(scaled_data)

# add principal component scores to the original data frame
df_pca=df
df_pca['pc1'] = pca_scores[:,0]
df_pca['pc2'] = pca_scores[:,1]
df_pca['pc3'] = pca_scores[:,2]
df_pca['pc4'] = pca_scores[:,3]
df_pca['pc5'] = pca_scores[:,4]
df_pca['pc6'] = pca_scores[:,5]
df_pca['pc7'] = pca_scores[:,6]
df_pca['pc8'] = pca_scores[:,7]
df_pca['pc9'] = pca_scores[:,8]
df_pca['pc10'] = pca_scores[:,9]
df_pca['pc11'] = pca_scores[:,10]

# reduce data to just the 3 PCs we will look at
pcr_mod_data = df_pca.loc[:,['mpg','pc1','pc2', 'pc3']]

## Let's look at a scatterplot matrix for the selected subset of data
ax = sns.pairplot(pcr_mod_data, diag_kind='kde')
plt.show()

# heatmap for PCs
tmp = pcr_mod_data.drop('mpg', axis=1)
cor_chart(tmp)

# Using Statsmodel regression
ols_mod1 = sm.ols('mpg ~ pc1', data=pcr_mod_data).fit()
print '\n'
print ols_mod1.summary()
print '\n'
res_charts(ols_mod1, "PC1")

ols_mod3 = sm.ols('mpg ~ pc1 + pc2 + pc3', data=pcr_mod_data).fit()
print '\n'
print ols_mod3.summary()
res_charts(ols_mod3, "PC1 + PC2 + PC3")

# Kaiser's Rule
ols_mod2 = sm.ols('mpg ~ pc1+pc2', data=df_pca).fit()
print '\n'
print ols_mod2.summary()
res_charts(ols_mod2, "PC1 + PC2 - Kaiser's Rule")

#ols_mod9 = sm.ols('mpg ~ pc1+pc2+pc3+pc4+pc5+pc6+pc7+pc8+pc9', data=df_pca).fit()
#print '\n'
#print ols_mod9.summary()
#res_charts(ols_mod9, "PC1 thru PC9 Inclusive")
