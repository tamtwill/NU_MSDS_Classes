# From Principal Components to Factor Analysis (Python)

# programmed by Thomas W. Miller, July 2017, revised August 2017

# prepare for Python version 3x features and functions
from __future__ import division, print_function
from future_builtins import ascii, filter, hex, map, oct, zip

# import packages for this example
import pandas as pd  # DataFrame operations 
# special plotting methods
from pandas.tools.plotting import scatter_matrix    
import numpy as np  # arrays and math functions
import matplotlib.pyplot as plt  # static plotting
from sklearn import preprocessing
from sklearn.decomposition import PCA, FactorAnalysis

# sklearn does not have factor rotation methods
# these we obtained from https://github.com/mvds314/factor_rotation
# stored as a local repository in the jump-start-factor-analysis directory 
import factor_rotation as fr

# previously obtained data from public-domain source at Stanford University
# 240 student participants' self-ratings on 32 personality characteristics
# a review of these data suggests that student survey participants were
# given an adjective check-list with instructions to self-rate such as:
# "Rate the extent to which each adjective describes you. Use a 
# 1-to-9 scale, where 1 means 'very much unlike me' and 
# 9 means 'very much like me.' " 
# source: http://www.stanford.edu/class/psych253/data/personality0.txt
# in previous work with R, we created a csv file from these data 

# create Pandas DataFram from the student data
# define a pandas DataFrame
student_data = pd.read_csv("student_data.csv")

# for fun, consider adding your own data to the student_data
# data frame by self-rating the 32 adjectives on a 1-to-9 scale ...
# this would provide 241 observations
# for example...
# my_data_dict = {"distant":1, "talkative":5, "careless":1, "hardworking":8, 
#   "anxious":2, "agreeable":6, "tense":1, "kind":7, "opposing":3, "relaxed":5,
#   "disorganized":4, "outgoing":5, "approving":3, "shy":1, "disciplined":5, 
#   "harsh":1, "persevering":9, "friendly":7, "worrying":3, "responsive":6,
#   "contrary":2, "sociable":6, "lazy":1, "cooperative":8, "quiet":3,   
#   "organized":6, "critical":5, "lax":2, "laidback":5, "withdrawn":1,
#   "givingup":1, "easygoing":6}
# my_data_frame = pd.DataFrame(my_data_dict, index = [0])
# student_data = pd.concat([student_data, my_data_frame])

print('')
print('----- Summary of Input Data -----')
print('')

# show the object is a DataFrame
print('Object type: ', type(student_data))

# show number of observations in the DataFrame
print('Number of observations: ', len(student_data))

# show variable names
print('Variable names: ', student_data.columns)

# show descriptive statistics
pd.set_option('display.max_columns', None)  # do not limit output
print(student_data.describe())

# show a portion of the beginning of the DataFrame
print(student_data.head())

print('')
print('----- Principal Component Analysis -----')
print('')

# work with standard scores for all pca variables
# standard scores have zero mean and unit standard deviation
pca_data = preprocessing.scale(student_data.as_matrix())
pca = PCA()
pca.fit(pca_data)

# show summary of pca solution
pca_explained_variance = pca.explained_variance_ratio_
print('Proportion of variance explained:', pca_explained_variance)

# note that principal components analysis corresponds
# to finding eigenvalues and eigenvectors of the correlation matrix
pca_data_cormat = np.corrcoef(pca_data.T)
eigenvalues, eigenvectors = np.linalg.eig(pca_data_cormat)
print(eigenvalues)
print(eigenvectors)
print('Linear algebra demonstration: Proportion of variance explained: ',
    eigenvalues/eigenvalues.sum())

# show the scree plot for the pricipal component analysis
plt.bar(np.arange(len(pca_explained_variance)), pca_explained_variance, 
    color = 'grey', alpha = 0.5, align = 'center')
plt.title('PCA Proportion of Total Variance')    

# provide partial listing of variable loadings on principal components
# transpose for variables by components listing
pca_loadings = pca.components_.T
print(pca_loadings) 

# provide full formatted listing of loadings for first five components
# print loadings while rounding to three digits 
# and suppress printing of very small numbers
# but do not suppress printing of zeroes
np.set_printoptions(precision = 3, suppress = True,
    formatter={'float': '{: 0.3f}'.format})
print(pca_loadings[:,0:5])  

# compute full set of principal components (scores)
C = pca.transform(pca_data)

# add first five principal component scores to the original data frame
student_data['pca1'] = C[:,0]
student_data['pca2'] = C[:,1]
student_data['pca3'] = C[:,2]
student_data['pca4'] = C[:,3]
student_data['pca5'] = C[:,4] 
 
# explore relationships between pairs of principal components
# working with the first five components only
pca_scores = student_data.loc[:,['pca1','pca2', 'pca3', 'pca4', 'pca5']]
pca_model_cormat = \
    np.corrcoef(pca_scores.as_matrix().transpose()).round(decimals=3)
print(pca_model_cormat)
 
# show scatter plot matrix for the principal component scores
# with kernel density estimate on the diagonal
scatter_matrix(pca_scores, alpha=0.2, figsize=(6, 6), diagonal='kde')

# note that there has been much psychological research 
# about what are called the big five factors of perosnality:
# extraversion, agreeableness, conscientiousness, neuroticism, openness
#
# some personality researchers have focused on only two factors:
# extraversion/introversion and neuroticism
# or on three factors, adding psychoticism

print('')
print('----- Principal Component Analysis (Five-Component Rotated) -----')
print('')

# use factor_rotation module to obtain varimax rotation
# where pca_varimax_rotation is a matrix representing a linear transformation
pca_varimax_loadings, pca_varimax_rotation = \
    fr.rotate_factors(pca_loadings[:,0:5],'varimax')

# show the loadings of the variables on the varimax-rotated factors
# for the unrotated maximum likelihood solution
# print loadings while rounding to three digits 
# and suppress printing of very small numbers
# but do not suppress printing of zeroes
np.set_printoptions(precision = 3, suppress = True,
    formatter={'float': '{: 0.3f}'.format})
print('PCA varimax factor loadings: \n',pca_varimax_loadings)  

# demonstrate how the varimax-rotated loadings are obtained
# as the dot product of the original unrotated loadings matrix
# and the varimax transition matrix (rotation matrix)
print(pca_loadings[:,0:5].dot(pca_varimax_rotation))

# the proportion of variance explained 
# may be computed directly from the loadings matrix
# sum the columns of the loadings matrix to obtain
pca_varimax_loadings_squared = np.square(pca_varimax_loadings)
pca_varimax_variance_explained = pca_varimax_loadings_squared.sum(axis=0)
print('PCA varimax proportion of variance explained \n',
    pca_varimax_variance_explained)

# uniqueness represents the proportion of variable variance 
# that is unique to the variable, not shared with other variables
# communality represents the proportion of variable variance
# that is common to the factor analytic solution
# 1 - uniqueness 
# rotation of the factor axes does not affect variable communalities
# or uniquenesses
pca_varimax_variable_communalities = pca_varimax_loadings_squared.sum(axis=1)
pca_varimax_variable_uniquenesses = 1 - pca_varimax_variable_communalities
print('PCA varimax variable uniquenesses: \n', pca_varimax_variable_uniquenesses)
print('PCA varimax variable communalities: \n', pca_varimax_variable_communalities)

print('')
print('----- Factor Analysis (Unrotated) -----')
print('')
 
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

# show the loadings of the variables on the factors
# for the unrotated maximum likelihood solution
# print loadings while rounding to three digits 
# and suppress printing of very small numbers
# but do not suppress printing of zeroes
np.set_printoptions(precision = 3, suppress = True,
    formatter={'float': '{: 0.3f}'.format})
print(fa_loadings)  

# compute full set of factor scores
F = fa.transform(pca_data)

# add factor scores to the original data frame
student_data['fa1'] = F[:,0]
student_data['fa2'] = F[:,1]
student_data['fa3'] = F[:,2]
student_data['fa4'] = F[:,3]
student_data['fa5'] = F[:,4] 
 
# explore relationships between pairs of factors
# scikit learn provides orthogonal factors
fa_scores = student_data.loc[:,['fa1','fa2', 'fa3', 'fa4', 'fa5']]
fa_cormat = \
    np.corrcoef(fa_scores.as_matrix().transpose()).round(decimals=3)
print(fa_cormat)
# show scatter plot matrix for the factor scores
# with kernel density estimate on the diagonal
scatter_matrix(fa_scores, alpha=0.2, figsize=(6, 6), diagonal='kde')

# the proportion of variance explained 
# may be computed directly from the loadings matrix
# sum the columns of the loadings matrix to obtain
fa_loadings_squared = np.square(fa_loadings)
fa_variance_explained = fa_loadings_squared.sum(axis=0)
print('Unrotated factor analysis proportion of variance explained: \n',
    fa_variance_explained)

# uniqueness represents the proportion of variable variance 
# that is unique to the variable, not shared with other variables
# communality represents the proportion of variable variance
# that is common to the factor analytic solution
# 1 - uniqueness 
fa_variable_communalities = fa_loadings_squared.sum(axis=1)
fa_variable_uniquenesses = 1 - fa_variable_communalities
print('Unrotated factor analysis variable uniquenesses: \n', 
    fa_variable_uniquenesses)
print('Unrotated factor analysis variable communalities: \n', 
    fa_variable_communalities)

print('')
print('----- Factor Analysis (Varimax-Rotated) -----')
print('')

# note that unrotated solutions are often difficult to interpret
# so we employ an orthogonal rotation called varimax
# that is, we rotate factor axes while maintaining their
# orthogonality (factor scores remain uncorrelated),
# and we do it in a way that maximizes the sum of the
# variances of the factor loadings....
# this has the effect of moving individual loadings
# in the direction of plus/minus one or zero, so a variable
# is either strongly associated with a factor or not....
# when loadings of variables on factors are either
# plus/minus one or zero, it is easier to interpret
# the factor analytic solution

# use factor_rotation module to obtain varimax rotation
# where varimax_rotation is a matrix representing a linear transformation
varimax_loadings, varimax_rotation = fr.rotate_factors(fa_loadings,'varimax')

# show the loadings of the variables on the varimax-rotated factors
# for the unrotated maximum likelihood solution
# print loadings while rounding to three digits 
# and suppress printing of very small numbers
# but do not suppress printing of zeroes
np.set_printoptions(precision = 3, suppress = True,
    formatter={'float': '{: 0.3f}'.format})
print('Varimax factor loadings: \n',varimax_loadings)  

# demonstrate how the varimax-rotated loadings are obtained
# as the dot product of the original unrotated loadings matrix
# and the varimax transition matrix (rotation matrix)
print(fa_loadings.dot(varimax_rotation))

# the proportion of variance explained 
# may be computed directly from the loadings matrix
# sum the columns of the loadings matrix to obtain
varimax_loadings_squared = np.square(varimax_loadings)
varimax_variance_explained = varimax_loadings_squared.sum(axis=0)
print('Varimax-rotated factor analysis proportion of variance explained \n', 
    varimax_variance_explained)

# uniqueness represents the proportion of variable variance 
# that is unique to the variable, not shared with other variables
# communality represents the proportion of variable variance
# that is common to the factor analytic solution
# 1 - uniqueness 
# notice that rotation of the factor axes does not 
# affect variable communalities or uniquenesses
varimax_variable_communalities = varimax_loadings_squared.sum(axis=1)
varimax_variable_uniquenesses = 1 - varimax_variable_communalities
print('Factor analysis varimax variable uniquenesses: \n', 
    varimax_variable_uniquenesses)
print('Factor analysis varimax variable communalities: \n', 
    varimax_variable_communalities)

# try interpreting the varimax-rotated solution...
# to what extent does it match up with the big five
# personality factors...
# if the factors do not match up with the big five,
# try naming the identified factors yourself

# naming of factors is as much art as science...
# refer to the matrix of factor loadings and
# note the variables that have the highest
# positive and negative loadings on each factor...
# then come up with a words that describe these variables

# note that scores on factors are not fully determined by 
# a factor analytic solution... factor scores are "indeterminate"  
# this means that there is an infinite number of ways of assigning 
# factor scores for any given set of factor loadings...
# this is yet another reason for statisticians to decry 
# factor analytic methods, choosing to stick with unrotated 
# principal components analysis.  



     