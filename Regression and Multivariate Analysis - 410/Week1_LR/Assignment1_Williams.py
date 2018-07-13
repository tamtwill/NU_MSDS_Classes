#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Wed Jun 21 09:53:17 2017

@author: tamtwill
"""

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.sankey import Sankey
import numpy as np
import statsmodels.api as sm
from statsmodels.formula.api import ols
import scipy.stats as sp

df = pd.read_csv('/Users/tamtwill/NorthwesternU_MSPA/410 - Regression/Week1_LR/ames_housing_data.csv', sep = ",")
obs0 = len(df)
sankey0 = "Starting number = "+ str(obs0)

# drop the non-residential properties first
resid_only = df[((df['Zoning'] == 'RL') |(df['Zoning'] == 'RM') | 
        (df['Zoning'] == 'RH')|(df['Zoning'] == 'RP'))]
obs1 = len(resid_only)
sankey1 = "Keep zoned residential only leaves " + str(obs1)
drop1 = obs0 - obs1
sandrop1 = "Dropped" + str(drop1)

# keep only single family detatched
family1 = resid_only[(resid_only['BldgType'] == '1Fam')]
obs2 = len(family1)
sankey2 = "Keep only Single Family homes leaves " + str(obs2)
drop2 = obs1 - obs2

# keep normal sales, getting rid of all the weird sale types
norm_only = family1[(family1['SaleCondition'] == 'Normal')]
obs3 = len(norm_only)
sankey3 = "Keep only normal sales leaves " + str(obs3)
drop3 = obs2 - obs3

# make a sankey chart showing the criteria and remaining number of observations
# for the data waterfall
sankey = Sankey(unit=None)
# first diagram, indexed by prior=0
sankey.add(flows=[1, -1],
        orientations=[0,-1],
        labels=[sankey0, sankey1])
# second diagram indexed by prior=1
sankey.add(flows=[1, -1],
        orientations=[0,0], 
        labels=[' ', sankey2],
        prior=0,
        connect=(1, 0))
 # second diagram indexed by prior=1
sankey.add(flows=[1, -1],
        orientations=[0,0],
        labels=[' ', sankey3],
        prior=1,
        connect=(1, 0))
sankey.finish()

new_df = norm_only

# check for abnormal sale prices, like 0, or negative
print "Maximum Sales Price", max(new_df.SalePrice)
print "Mimimum Sales Price", min(new_df.SalePrice)
tmp = new_df[(new_df['SalePrice'] == 755000)]
print tmp


# pick 20 columns to Data Quality check
df_20 = new_df[['LotArea', 'Utilities','Neighborhood','HouseStyle','BsmtFinSF2',
    'OverallQual','OverallCond','YearBuilt','YearRemodel','TotalBsmtSF',
    'Heating','FullBath','HalfBath','BedroomAbvGr','Fireplaces','GarageCars',
    'PoolArea', 'Fence', 'GrLivArea','YrSold','SalePrice']]
df_20.dtypes

# look at the one really big sale price
tmp = new_df[(new_df['SalePrice'] == 755000)]
print tmp

# Data quality check on the 20
# first, are the types as expected
df_20.dtypes

# let's look at the summary data for the 20
print df_20.describe()
# looking at the .info(), to see if there are missing values
print df_20.info()


# look at a boxplot of SalePrice
plt.figure()
ax = sns.boxplot(x="SalePrice", orient = 'v', data=new_df)
plt.show()

# and look at the distribution of SalePrice
plt.figure
ax=sns.distplot(new_df['SalePrice'], kde=False)
ax.set(title='Sales Prices')
plt.show()
print 'Skew = ', sp.skew(new_df['SalePrice'])
print 'Kurtosis = ', sp.kurtosis(new_df['SalePrice'])

# pick 10 features, try looking at a pairs plot for inspiration, split df into 2
# for readability of output
df_20_1 = new_df[['LotArea', 'Utilities','TotRmsAbvGrd','HouseStyle','BsmtFinSF2',
    'OverallQual','OverallCond','YearBuilt','YearRemodel','TotalBsmtSF','SalePrice']]
#plt.figure()
ax = sns.pairplot(df_20_1)
plt.show()

df_20_2 = new_df[['Heating','FullBath','HalfBath','BedroomAbvGr','Fireplaces','GarageCars',
    'PoolArea', 'Fence', 'GrLivArea','YrSold','SalePrice']]
#plt.figure()
ax = sns.pairplot(df_20_2)
plt.show()

# do some plotting for lot size and sales price, these value wide differences
# between min and max
plt.figure()
ax = sns.boxplot(x="LotArea", y="Neighborhood", orient = 'h', data=df_20)
ax.set_title('Lot Sizes')
plt.show()


plt.figure()
ax = sns.boxplot(x="SalePrice", y="Neighborhood", orient = 'h', data=df_20)
ax.set_title('Sale Prices')
plt.show()

# based on posible correlations in pairplot, pull out likely columns
df_10 = new_df[['YearBuilt','YearRemodel','TotalBsmtSF','OverallQual','GrLivArea',
    'BedroomAbvGr','GarageCars','FullBath','TotRmsAbvGrd','Fireplaces', 'SalePrice']]
print df_10.describe()
print df_10.info()

df_10_1 = new_df[['YearBuilt','YearRemodel','TotalBsmtSF','OverallQual','GrLivArea',
    'SalePrice']]
    
df_10_2 = new_df[['BedroomAbvGr','GarageCars','FullBath','TotRmsAbvGrd',
    'Fireplaces', 'SalePrice']]
    
    
#plt.figure()
ax = sns.pairplot(df_10_1)
plt.show()

#plt.figure()
ax = sns.pairplot(df_10_2)
plt.show()

# OK, nenver used Python for statistics, so based on what I can find, this seems 
# to be the closest anova function to what I'm used to in R
my_lm = ols('SalePrice~ YearBuilt+ YearRemodel+TotalBsmtSF+OverallQual+GrLivArea+BedroomAbvGr+GarageCars+FullBath+TotRmsAbvGrd+Fireplaces', 
    data = new_df).fit()
print(sm.stats.anova_lm(my_lm, typ=2))


df_3 = new_df[['TotalBsmtSF','OverallQual','GrLivArea', 'SalePrice']]
#plt.figure()
ax  = sns.pairplot(df_3)
plt.show()

# compute log od SalePrice and add to dataframe
df_3['logSale'] = np.log(df_3.SalePrice)

#plt.figure()
ax  = sns.pairplot(df_3)
plt.show()

# chart fitted regression lines for the 3 variables selected
#plt.figure()
ax = sns.lmplot( x='SalePrice', y='TotalBsmtSF', data = df_3)
plt.show()
ax = sns.lmplot( x='SalePrice', y='OverallQual', data = df_3)
plt.show()
ax = sns.lmplot( x='SalePrice', y='GrLivArea', data = df_3)
plt.show()



# look at the Pair grid for SalePrice and log(SalePrice)
sns.set(style="white")
ax = sns.PairGrid(df_3, diag_sharey=False)
ax.map_lower(sns.kdeplot, cmap="Blues_d")
ax.map_upper(plt.scatter)
ax.map_diag(sns.kdeplot, lw=3)

ax=sns.regplot(x='logSale', y='OverallQual', data=df_3)
plt.show()
ax=sns.regplot(x='logSale', y='GrLivArea', data=df_3)
plt.show()
ax=sns.regplot(x='logSale', y='TotalBsmtSF', data=df_3)
plt.show()

plt.close()

