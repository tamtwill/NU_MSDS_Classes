#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Wed Jun 28 09:53:17 2017

@author: tamtwill
"""

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.sankey import Sankey
import numpy as np
from sklearn.linear_model import LinearRegression
import statsmodels.api as sm
from statsmodels.formula.api import ols

df = pd.read_csv('/Users/tamtwill/NorthwesternU_MSPA/410 - Regression/Week1_LR/ames_housing_data.csv', sep = ",")
obs0 = len(df)
sankey0 = "Starting number = "+ str(obs0)

# drop the non-residential properties first
resid_only = df[((df['Zoning'] == 'RL') |(df['Zoning'] == 'RM') | 
        (df['Zoning'] == 'RH')|(df['Zoning'] == 'RP'))]
obs1 = len(resid_only)
sankey1 = "Drop all properties NOT zoned residential,  # remaining = " + str(obs1)
drop1 = obs0 - obs1
sandrop1 = "Dropped" + str(drop1)

# keep only single family detatched
family1 = resid_only[(resid_only['BldgType'] == '1Fam')]
obs2 = len(family1)
sankey2 = "Drop all properties NOT Single Family,  # remaining = " + str(obs2)
drop2 = obs1 - obs2

# keep normal sales, getting rid of all the weird sale types
norm_only = family1[(family1['SaleCondition'] == 'Normal')]
obs3 = len(norm_only)
sankey3 = "Drop all sales where condidtions are NOT normal,  # remaining = " + str(obs3)
drop3 = obs2 - obs3

# make a sankey chart showing the criteria and remaining number of observations
# for the data waterfall
fig = plt.figure(figsize=(8, 12))
ax = fig.add_subplot(1, 1, 1, xticks=[], yticks=[],
            title="Waterfall of dropped observations")
obs = [obs0, obs1, obs2, obs3]
labels = ["Drop all properties NOT zoned residential", "Drop all properties NOT Single Family",
          "Drop all sales where condidtions are NOT normal", "Remaining Observations"]
colors = ["#25EE46", "#2ADCB1", "#2ADCDC", "#20A6EE"]

sankey = Sankey(ax=ax, scale=0.0015, offset=0.3)
for input_obs, output_obs, label, prior, color in zip(obs[:-1], obs[1:], 
            labels, [None, 0, 1, 2, 3], colors):
    if prior != 1:
        sankey.add(flows=[input_obs, -output_obs, output_obs - input_obs],
                orientations=[0, 0, 1],
                patchlabel=label,
                labels=['', None, 'dropped'],
              prior=prior,
              connect=(1, 0),
                pathlengths=[0, 0, 2],
              trunklength=10.,
              rotation=-90,
                  facecolor=color)
    else:
        sankey.add(flows=[input_obs, -output_obs, output_obs - input_obs],
                orientations=[0, 0, 1],
                patchlabel=label,
                labels=['', labels[-1], 'dropped'],
              prior=prior,
              connect=(1, 0),
                pathlengths=[0, 0, 10],
              trunklength=10.,
              rotation=-90,
                  facecolor=color)
diagrams = sankey.finish()
for diagram in diagrams:
    diagram.text.set_fontweight('bold')
    diagram.text.set_fontsize('10')
    for text in diagram.texts:
        text.set_fontsize('10')
ylim = plt.ylim()
plt.ylim(ylim[0]*1.05, ylim[1])
plt.show()


df_houses = norm_only

# check for abnormal sale prices, like 0, or negative
print "Maximum Sales Price", max(df_houses.SalePrice)
print "Mimimum Sales Price", min(df_houses.SalePrice)
tmp = df_houses[(df_houses['SalePrice'] == 755000)]
print tmp

# Common sense suggests house prices will be most impacted by the size of the
# house, so let's restrict our view to continuous size related features
df_size = df_houses[['BsmtFinSF1', 'BsmtFinSF2','TotalBsmtSF','GrLivArea',
    'FirstFlrSF','SecondFlrSF', 'GarageArea','PoolArea','WoodDeckSF',
    'OpenPorchSF','EnclosedPorch','ThreeSsnPorch','ScreenPorch','MiscVal','SalePrice']]
    
df_size_1 = df_size[['BsmtFinSF1', 'BsmtFinSF2','TotalBsmtSF','GrLivArea',
    'FirstFlrSF','SecondFlrSF', 'GarageArea','SalePrice']]

df_size_2 = df_size.iloc[:, 7:15]

ax = sns.pairplot(df_size_1)
plt.show()

ax = sns.pairplot(df_size_2)
plt.show()

df_final2 = df_houses[['TotalBsmtSF','GrLivArea','SalePrice']]
ax = sns.pairplot(df_final2)
plt.show()

# fit the regression line for above grade living area v saleprice and plot
# -----------------------------------------------------------------------------
x=df_final2['SalePrice']
y=df_final2['GrLivArea']
X = x[:, np.newaxis]
X.shape
model = LinearRegression(fit_intercept = True)
model.fit(X,y)
print "Coefficient, Above Grade Living Area", model.coef_
print "Intercept, Above Grade Living Area",model.intercept_
print ''

fig = plt.figure()
xfit=np.linspace(-1, 800000)
Xfit = xfit[:, np.newaxis]
yfit = model.predict(Xfit)
plt.scatter(x,y)
plt.scatter(xfit, yfit)
fig.suptitle('Above Grade Living Area v Sales Prices')
plt.show()

fig = plt.figure()
ax = sns.residplot(x='SalePrice',y = 'GrLivArea', data = df_final2)
ax.set(title='Residuals')
plt.axhline(linewidth=4, color='r')
plt.show()

my_lm = ols('SalePrice~ GrLivArea',data = df_final2)
results = my_lm.fit()
print results.summary()
print ''



# fit the regression line for Basement v saleprice and plot
# -----------------------------------------------------------------------------
x=df_final2['SalePrice']
y=df_final2['TotalBsmtSF']
X = x[:, np.newaxis]
X.shape
model = LinearRegression(fit_intercept = True)
model.fit(X,y)
print "Coefficient, Total Basement Area", model.coef_
print "Intercept, Total Basement Area", model.intercept_
print ''

fig = plt.figure()
xfit=np.linspace(-1, 800000)
Xfit = xfit[:, np.newaxis]
yfit = model.predict(Xfit)
plt.scatter(x,y)
plt.scatter(xfit, yfit)
fig.suptitle('Size of Basement v Sales Prices')
plt.show()

fig = plt.figure()
ax = sns.residplot(x='SalePrice',y = 'TotalBsmtSF', data = df_final2)
ax.set(title='Residuals')
plt.axhline(linewidth=4, color='r')
plt.show()

my_lm = ols('SalePrice~ TotalBsmtSF',data = df_final2).fit()
print my_lm.summary()
print ''



# multiple regression model
my_lm = ols(formula = 'SalePrice~ TotalBsmtSF+GrLivArea',data = df_final2).fit()
print my_lm.summary()

fig = plt.figure()
plt.scatter(df_final2['SalePrice'],my_lm.resid)
plt.xlabel('SalePrice')
plt.ylabel('Residuals')
plt.axhline(linewidth=4, color='r')
plt.show()

# -----------------------------------------------------------------------------
# Repeating above using log(SalePrice) in place of SalePrice
# good coding hygiene says this should probably be a function, 
# since duplicating code is bad, but I'm still feeling wretched, 
# so, copying and pasting and feeling badly about it
# -----------------------------------------------------------------------------

# create log(SalePrice)  column
df_log = df_houses[['TotalBsmtSF','GrLivArea','SalePrice']]
df_log['logSale'] = np.log(df_log.SalePrice)
df_log = df_log[['TotalBsmtSF','GrLivArea','logSale']]
ax = sns.pairplot(df_log)
plt.show()

# fit the regression line for above grade living area v logSale and plot
# -----------------------------------------------------------------------------
x=df_log['logSale']
y=df_log['GrLivArea']
X = x[:, np.newaxis]
X.shape
model = LinearRegression(fit_intercept = True)
model.fit(X,y)
print "Coefficient, Above Grade Living Area", model.coef_
print "Intercept, Above Grade Living Area",model.intercept_
print ''

fig = plt.figure()
xfit=np.linspace(10,15)
Xfit = xfit[:, np.newaxis]
yfit = model.predict(Xfit)
plt.scatter(x,y)
plt.scatter(xfit, yfit)
fig.suptitle('Above Grade Living Area v log of Sales Prices')
plt.show()

fig = plt.figure()
ax = sns.residplot(x='logSale',y = 'GrLivArea', data = df_log)
ax.set(title='Residuals')
plt.axhline(linewidth=4, color='r')
plt.show()

my_lm = ols('logSale~ GrLivArea',data = df_log)
results = my_lm.fit()
print results.summary()
print ''

# fit the regression line for Basement v logSale and plot
# -----------------------------------------------------------------------------
x=df_log['logSale']
y=df_log['TotalBsmtSF']
X = x[:, np.newaxis]
X.shape
model = LinearRegression(fit_intercept = True)
model.fit(X,y)
print "Coefficient, Total Basement Area", model.coef_
print "Intercept, Total Basement Area", model.intercept_
print ''

fig = plt.figure()
xfit=np.linspace(10, 15)
Xfit = xfit[:, np.newaxis]
yfit = model.predict(Xfit)
plt.scatter(x,y)
plt.scatter(xfit, yfit)
fig.suptitle('Size of Basement v log of Sales Prices')
plt.show()

fig = plt.figure()
ax = sns.residplot(x='logSale',y = 'TotalBsmtSF', data = df_log)
ax.set(title='Residuals')
plt.axhline(linewidth=4, color='r')
plt.show()

my_lm = ols('logSale~ TotalBsmtSF',data = df_log).fit()
print my_lm.summary()
print ''


# multiple regression model
my_lm = ols(formula = 'logSale~ TotalBsmtSF+GrLivArea',data = df_log).fit()
print my_lm.summary()

fig = plt.figure()
plt.scatter(df_log['logSale'],my_lm.resid)
plt.xlabel('logSale')
plt.ylabel('Residuals')
plt.axhline(linewidth=4, color='r')
plt.show()

