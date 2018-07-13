#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Wed Aug  2 11:35:07 2017

@author: Tamara Williams

"""

import os
os.chdir('/Users/tamtwill/NorthwesternU_MSPA/410 - Regression/Week7_LR') 

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA, FactorAnalysis
from sklearn import preprocessing

# since sklearn lacks factor rotation import the factor_rotation module from 
# https://github.com/mvds314/factor_rotation 
import factor_rotation as fr


def corr_chart(df_corr):
    corr=df_corr.corr()
    #screen top half to get a triangle
    top = np.zeros_like(corr, dtype=np.bool)
    top[np.triu_indices_from(top)] = True
    fig=plt.figure()
    fig, ax = plt.subplots(figsize=(12,12))
    sns.heatmap(corr, mask=top, cmap='coolwarm', center = 0, square=True, 
           linewidths=.5, cbar_kws={'shrink':.5}, 
           annot = True,annot_kws={"size": 10})
    plt.xticks(rotation=90)
    plt.yticks(rotation=45)
    plt.show()
    print '\n'
    print "Correlation Matrix"
    print np.round(corr, decimals = 3)
    print '\n'

np.set_printoptions(precision=4, threshold = 5000)
study_data = pd.read_csv("time_use_1976.csv")

print ''
print '----- Summary of Input Data -----'
print ''

# show variable names
print ''
print 'Variable names: ', study_data.columns
print ''
data_names = pd.Series(study_data.columns)

# show descriptive statistics
pd.set_option('display.max_columns', None)  # do not limit output
print ''
print study_data.describe(), '\n'
print ''


# show a portion of the beginning of the DataFrame
print study_data.head(), '\n'

#tmp = study_data.drop(study_data.columns[[2,3,4]], axis=1)
use_time = study_data.drop(study_data.columns[[0,1,2,3,4]], axis=1)
data_names = data_names.drop([0,1,2,3,4])
data_names = data_names.reset_index(drop=True)
time_names = data_names.loc[0:9]


# Looking at time-use only
ax = sns.pairplot(use_time)
plt.show()

corr_chart(use_time)


#---------------------------------------------
#----- Principal Component Analysis -----
#---------------------------------------------

# to work with standard scores for all pca variables, scale the data
pca_data = preprocessing.scale(use_time.as_matrix())
pca = PCA()
pca.fit(pca_data)

#amount of variance each PC explains an the cummulative variance explained
num_var = pca.n_components_
var_ex = np.round(pca.explained_variance_ratio_, decimals = 3)*100
cumm_var = np.cumsum(np.round(pca.explained_variance_ratio_, decimals = 3)*100)
print '\n'
for i in range(0,num_var):
    print "PC %i accounts for %g%% of variation; cummulative variation is: %g%%"\
    %(i+1, var_ex[i], cumm_var[i])
    

## note that principal components analysis corresponds
## to finding eigenvalues and eigenvectors of the correlation matrix
#pca_data_cormat = np.corrcoef(pca_data.T)
#eigenvalues, eigenvectors = np.linalg.eig(pca_data_cormat)
#print eigenvalues)
#print eigenvectors)
#print 'Linear algebra demonstration: Proportion of variance explained: ',
#    eigenvalues/eigenvalues.sum())


# show the scree plot for the pricipal component analysis
exp_variance= pca.explained_variance_ratio_
plt.plot(np.arange(len(exp_variance)), exp_variance)
plt.title('PCA Proportion of Total Variance', fontsize=14) 
plt.ylabel('% of Variance Explained')
plt.xlabel('Principal Component')


# provide partial listing of variable loadings on principal components
# transpose for variables by components listing
pca_loadings = pca.components_.T
print ''
tmp = pd.DataFrame(pca_loadings)
tmp['use'] = time_names
print tmp,'\n'
#print pca_loadings
print ''


# provide full formatted listing of loadings for first five components
# print loadings while rounding to three digits 
# and suppress printing of very small numbers
# but do not suppress printing of zeroes
np.set_printoptions(precision = 3)
print''
print "PCA Un-rotated Loadings\n"
tmp = pd.DataFrame(pca_loadings[:,0:5])
tmp['use'] = time_names
print tmp,'\n'
#print pca_loadings[:,0:5]
print''

# compute full set of principal components (scores)
C = pca.transform(pca_data)

# add first five principal component scores to the original data frame
study_data['pca1'] = C[:,0]
study_data['pca2'] = C[:,1]
study_data['pca3'] = C[:,2]
study_data['pca4'] = C[:,3]
study_data['pca5'] = C[:,4] 
 
pca_scores = study_data.loc[:,['pca1','pca2', 'pca3', 'pca4', 'pca5']]
corr_chart(pca_scores)

# show scatter plot matrix for the principal component scores
# with kernel density estimate on the diagonal
sns.pairplot(pca_scores, kind='scatter', diag_kind ='kde')


#---------------------------------------------
#----- Rotate Principal Component Analysis -----'
#---------------------------------------------
#reduce the data to 2 PCs and rotate
pca_short = pca_data[:,0:2]
pca_short_load = pca_loadings[:,0:2]

varimax_loadings, varimax_rotation = fr.rotate_factors(pca_short_load,'varimax')

# show the loadings of the PCA on the varimax-rotated factors
# for the unrotated maximum likelihood solution
# print loadings while rounding to three digits 
# and suppress printing of very small numbers
# but do not suppress printing of zeroes
np.set_printoptions(precision = 3, suppress = False, formatter={'float': '{: 0.3f}'.format})
print ''
print 'Varimax loadings PC1 & PC2: '
tmp = pd.DataFrame(varimax_loadings)
tmp['use'] = time_names
print tmp,'\n'
#print varimax_loadings,'\n'

# show the difference between the original and Varimax loadings
tmp1 = varimax_loadings - pca_short_load
np.set_printoptions(precision = 3)
print ''
print 'Difference between Rotated and Original loadings '
tmp = pd.DataFrame(tmp1)
tmp['use'] = time_names
print tmp,'\n'
#print tmp1,'\n'


# the proportion of variance explained 
# may be computed directly from the loadings matrix
# sum the columns of the loadings matrix to obtain
varimax_loadings_squared = np.square(varimax_loadings)
varimax_variance_explained = varimax_loadings_squared.sum(axis=0)
print ''
print 'Varimax PCA Variance Explained: '
print varimax_variance_explained,'\n'


# uniqueness represents the proportion of variable variance 
# that is unique to the variable, not shared with other variables
# communality represents the proportion of variable variance
# that is common to the factor analytic solution
# 1 - uniqueness 
varimax_variable_communalities = varimax_loadings_squared.sum(axis=1)
varimax_variable_uniquenesses = 1 - varimax_variable_communalities
print 'Varimax PCA variable uniquenesses: ', varimax_variable_uniquenesses,'\n'
print 'Varimax PCA variable communalities: ', varimax_variable_communalities,'\n'


#---------------------------------------------
#----- Factor Analysis (Unrotated) -----'
#---------------------------------------------
 
# suppose we think five factors will be sufficient 
# we begin with an unrotated orthogonal solution
# unrotated factor analysis with five factors 
# maximum likelihood estimation is employed
# for best results set tolerance low and max iterations high
fa = FactorAnalysis(n_components = 5, tol=1e-8, max_iter=1000000)  

# work with the standardized data matrix we created with pca_data  
# here we obtain the unrotated solution
fa.fit(pca_data)

# retrieve the factor loadings as an array of arrays
# transpose for variables by factors listing of loadings
fa_loadings = fa.components_.T

# take a reduced set of factors, since the assignment seems to just want us 
# to look at 2 or 3
fa_loadings_short = fa_loadings[:,0:2]

np.set_printoptions(precision = 3, suppress = False, formatter={'float': '{: 0.3f}'.format})
print ''
print "FA Loadings\n"
tmp = pd.DataFrame(fa_loadings_short)
tmp['use'] = time_names
print tmp,'\n'


# compute full set of factor scores
F = fa.transform(pca_data)

# add factor scores to the original data frame
study_data['fa1'] = F[:,0]
study_data['fa2'] = F[:,1]
#study_data['fa3'] = F[:,2]
#study_data['fa4'] = F[:,3]
#study_data['fa5'] = F[:,4] 
 
# explore relationships between pairs of factors
# scikit learn provides orthogonal factors
#fa_scores = study_data.loc[:,['fa1','fa2', 'fa3', 'fa4', 'fa5']]
fa_scores = study_data.loc[:,['fa1','fa2']]
fa_cormat = np.corrcoef(fa_scores.as_matrix().transpose()).round(decimals=3)
print '\n'
print "Feature Analysis correlation matrix\n"
print fa_cormat
print''

# show scatter plot matrix for the factor scores
# with kernel density estimate on the diagonal
#scatter_matrix(fa_scores, alpha=0.2, figsize=(6, 6), diagonal='kde')
sns.pairplot(fa_scores, kind='scatter', size = 2, diag_kind ='kde')

fa_loadings_squared = np.square(fa_loadings_short)
fa_variance_explained = fa_loadings_squared.sum(axis=0)
print ''
print "Factor Analysis variance explained"
print fa_variance_explained
print ''


fa_variable_communalities = fa_loadings_squared.sum(axis=1)
fa_variable_uniquenesses = 1 - fa_variable_communalities
print ''
print 'FA Variable uniquenesses: ', fa_variable_uniquenesses
print 'FA Variable communalities: ', fa_variable_communalities
print ''
print ''

#---------------------------------------------
#----- Factor Analysis (Varimax-Rotated) -----
#---------------------------------------------

# use factor_rotation module to obtain varimax rotation
# where varimax_rotation is a matrix representing a linear transformation
varimax_loadings, varimax_rotation = fr.rotate_factors(fa_loadings_short,'varimax')

np.set_printoptions(precision = 3, suppress = True, formatter={'float': '{: 0.3f}'.format})

print ''
print 'Varimax Rotated FA factor loadings: '
tmp = pd.DataFrame(varimax_loadings)
tmp['use'] = time_names
print tmp,'\n'

# demonstrate how the varimax-rotated loadings are obtained
# as the dot product of the original unrotated loadings matrix
# and the varimax transition matrix (rotation matrix)
#print 'Varimax FA loadings: '
#print fa_loadings.dot(varimax_rotation),'\n'

varimax_loadings_squared = np.square(varimax_loadings)
varimax_variance_explained = varimax_loadings_squared.sum(axis=0)
print ''
print 'Varimax Rotated FA Variance Explained: '
print varimax_variance_explained,'\n'


varimax_variable_communalities = varimax_loadings_squared.sum(axis=1)
varimax_variable_uniquenesses = 1 - varimax_variable_communalities
print ''
print 'FA Rotated Varimax variable uniquenesses: ', varimax_variable_uniquenesses
print 'FA Rotated Varimax variable communalities: ', varimax_variable_communalities

#---------------------------------------------
# Rotation with alternate pivot matrix
#---------------------------------------------
# create a new rotation matrix.  Let's put the commonalities on the diagonal
# and a 0 everywhere else
new_pivot = np.identity(10)
new_pivot = new_pivot*varimax_variable_communalities
new_pivot_2 = new_pivot[0:2,0:2]

# use the dot-product method to get the loadings by multiplying new_pivot_5
# by the original unrotated loadings.
fa_alt = fa_loadings_short.dot(new_pivot_2)

fa_alt = pd.DataFrame(fa_alt)
fa_alt['use'] = time_names
print ''
print "Loadings after rotation via alternate matrix"
print fa_alt

