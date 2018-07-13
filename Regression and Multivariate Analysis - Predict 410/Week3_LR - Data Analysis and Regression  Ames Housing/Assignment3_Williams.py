#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Tues Ju1 04 11:01:38 2017

@author: tamtwill
"""

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.sankey import Sankey
import numpy as np
from sklearn.metrics import mean_absolute_error
from statsmodels.formula.api import ols
import scipy.stats as stats

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
                pathlengths=[0, 0, 2],
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

# fit the regression line for above grade living area v saleprice and plot
# -----------------------------------------------------------------------------
x=df_houses['GrLivArea']
y=df_houses['SalePrice']

my_lm = ols('SalePrice~ GrLivArea',data = df_houses)
results = my_lm.fit()
print results.summary()
print ''

fig = plt.figure()
fig.suptitle('Data and fitted regression line - SalePrice v Living Area', fontsize=14)
fig= sns.regplot(x,y, line_kws = {'color':'red'})
plt.show()


fig = plt.figure()
ax = fig.add_subplot(111)
fig = sns.residplot(x='SalePrice',y = 'GrLivArea', data = df_houses, )
fig.set(xticklabels=[])
ax.set_ylabel('Residuals')    
ax.set_xlabel('Predicted')
plt.axhline(linewidth=4, color='r')
plt.show()

my_predicts = results.fittedvalues
my_res = results.resid
my_res.describe()

fig = plt.figure()
fig.suptitle('QQ Plot Living Area Residuals', fontsize=14)
ax = fig.add_subplot(111)
qqp = stats.probplot(my_res, dist="norm", plot=plt, );
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

# fit the regression line for Basement v saleprice and plot
# -----------------------------------------------------------------------------
x=df_houses['TotalBsmtSF']
y=df_houses['SalePrice']
my_lm = ols('SalePrice~ TotalBsmtSF',data = df_houses).fit()
print my_lm.summary()
print ''

fig = plt.figure()
fig.suptitle('Data and fitted regression line - SalePrice v Bassement Area', fontsize=14)
fig= sns.regplot(x,y, line_kws = {'color':'red'})
plt.show()

my_predicts = my_lm.fittedvalues
my_res = my_lm.resid
my_res.describe()

fig = plt.figure()
ax = fig.add_subplot(111)
fig.suptitle('QQ Plot Basement Size Residuals', fontsize=14)
stats.probplot(my_res, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

fig = plt.figure()
ax = fig.add_subplot(111)
fig = sns.residplot(x='SalePrice',y = 'TotalBsmtSF', data = df_houses)
fig.set(xticklabels=[])
ax.set_ylabel('Residuals')    
ax.set_xlabel('Predicted')
plt.axhline(linewidth=4, color='r')
plt.show()



# multiple regression model
# -----------------------------------------------------------------------------
my_lm1 = ols(formula = 'SalePrice~ TotalBsmtSF+GrLivArea',data = df_houses).fit()
print my_lm1.summary()

#fig = plt.figure()
fig, ax = plt.subplots()
plt.scatter(df_houses['SalePrice'],my_lm1.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for SalePrice~TotalBsmtSF+GrLivArea (MR1)', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()


my_predicts1 = my_lm1.fittedvalues
my_res1 = my_lm1.resid
my_res1.describe()
mult_mae1 = mean_absolute_error(df_houses['SalePrice'], my_predicts1)
print ''
print "Multiple Regression #1, response variable = SalePrice, MAE = ", mult_mae1
print ''

fig = plt.figure()
fig.suptitle('QQ Plot MR1 Residuals', fontsize=14)
ax = fig.add_subplot(111)
stats.probplot(my_res1, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

# Neighborhood accuracy section
# -----------------------------------------------------------------------------

# Create dataframe with residuals and neighborhood, by appending residuals to 
# df_houses, adding the residuals and finding the difference between actual and 
# predicted SalePrice

df_res = df_houses.copy(deep=True)
df_res = df_res[['SalePrice', 'Neighborhood', 'GrLivArea']]
df_res['Residuals'] = my_res
df_res['Abs_res'] = df_res['Residuals'].abs()
#df_res['Diff'] = df_res['SalePrice'] - df_res['Predicted']


fig = plt.figure()
fig = sns.boxplot(x="Residuals", y="Neighborhood", orient = 'h', data=df_res)
fig.set_title('Sale Price Residuals')
plt.show()

grouped_df = df_res.groupby(['Neighborhood'])
grouped_df.describe()

# Sum all the SF for each Neighborhood
tot_SF = df_res['GrLivArea'].groupby([df_res['Neighborhood']]).sum()

# Sum all the prices paid in each neighborhood
tot_pr = df_res['SalePrice'].groupby([df_res['Neighborhood']]).sum() 

# Find price per SF by dividing sum of all prices by sum of all area by neighborhood
# Note to self - access neighborhood name as pr_per-SF.loc['name']
pr_per_SF = tot_pr.div(tot_SF) 

# count the number of houses in each neighborhood
houses =   df_res['SalePrice'].groupby([df_res['Neighborhood']]).count() 

# get the total of the absolute value of the residuals for each neighborhood
tot_abs_res = df_res['Abs_res'].groupby([df_res['Neighborhood']]).sum()


# Compute the MAE as the sum of abs(y minus y-hat) over n, where y - y_hat is the 
# same as the residual.  So, for each neighborhood, get 
#  sum of abs(residuals))/count of houses. 

i = 0
mae_list = []
neighborhood_list = []
sf_list = []
for neighborhood, group in df_res.groupby('Neighborhood'):
    n=neighborhood
    neighborhood_list.append(n)
    mae = round(tot_abs_res[i]/houses[i], 2)
    mae_list.append(mae)
    sf_list.append(round(pr_per_SF[i], 2))
    i+=1
    
# make a dataframe out of the SF and MAE values
df_mae = pd.DataFrame(neighborhood_list)
df_mae['ppsf'] = pd.Series(sf_list, index = df_mae.index)
df_mae['mae'] = pd.Series(mae_list, index = df_mae.index)
df_mae.rename(columns = {0:'Neighborhood'}, inplace = True)
    
# per item 4 on the assignment, plot df_mae
sort_mae = df_mae.sort(['Neighborhood'])
fig = plt.figure()
my_marks = ['x','o','v','^','<', 'X','o','v','^','<', 'x','o','v','^','<', 'x','o','v','^','<','X']
fig = sns.lmplot(x='ppsf', y="mae", data=sort_mae, hue='Neighborhood', markers=my_marks, fit_reg=False)
# some code from stackoverflow to get the legend out of the way
for ax in fig.axes.flat:
    box = ax.get_position()
    ax.set_position([box.x0,box.y0,box.width*0.85,box.height])
    ax.set_ylabel('MAE')    
    ax.set_xlabel('Price per Square Foot')
sns.plt.show()



# create groups based on price per sq ft
# -----------------------------------------------------------------------------
df_mae_sorted = df_mae.sort(columns='ppsf')
i=0
bin_list = []
for neighborhood in df_mae['Neighborhood']:
    if df_mae.loc[i]['ppsf'] <= 102.00 :
        bin_list.append(1)
    elif  102.00 < df_mae.loc[i]['ppsf'] <= 118.00 :
        bin_list.append(2)
    elif  118.00 < df_mae.loc[i]['ppsf'] <= 134.00 :
        bin_list.append(3)
    elif 134.00 < df_mae.loc[i]['ppsf'] <= 150.00 :
        bin_list.append(4)
    else:
        bin_list.append(5)
    i+=1
df_mae['n_bins'] = pd.Series(bin_list, index = df_mae.index)

# new multiple regression model
# -----------------------------------------------------------------------------
# add the data just calculated back into the main dataframe by leveraging merge
# on Neighborhood
target_cols = ['Neighborhood', 'ppsf', 'mae', 'n_bins']
df_merge = df_houses.merge(df_mae[target_cols], on='Neighborhood', how='left')

       
my_lm1 = ols(formula = 'SalePrice~ TotalBsmtSF+GrLivArea+ppsf+mae+n_bins',data = df_merge).fit()
print my_lm1.summary()
print ''
print ''


my_lm2 = ols(formula = 'SalePrice~ TotalBsmtSF*n_bins+GrLivArea*n_bins+ppsf*n_bins*mae',data = df_merge).fit()
print my_lm2.summary()

my_predicts2 = my_lm2.fittedvalues
my_res2 = my_lm2.resid
my_res2.describe()
mult_mae2 = mean_absolute_error(df_houses['SalePrice'], my_predicts2)
print ''
print "Multiple Regression #2, response variable = SalePrice, MAE = ", mult_mae2
print ''


# new multiple regression model comparing Y versus log(Y)
# -----------------------------------------------------------------------------

# create log(SalePrice)  column
df_log = df_merge[['TotalBsmtSF','GrLivArea','SalePrice', 'GarageArea','BsmtFinSF1',
    'OverallQual','Neighborhood', 'ppsf', 'mae', 'n_bins']]
df_log['logSale'] = np.log(df_log.SalePrice)

my_lm3 = ols(formula = 'SalePrice~ TotalBsmtSF+GrLivArea+n_bins+mae+GarageArea+BsmtFinSF1+OverallQual',data = df_merge).fit()
print my_lm3.summary()


#fig = plt.figure()
fig, ax = plt.subplots()
plt.scatter(df_houses['SalePrice'],my_lm3.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot (MR3): Response = SalePrice', fontsize=14)
ax.tick_params(labelbottom='off') 
plt.axhline(linewidth=4, color='r')
plt.show()

my_predicts3 = my_lm3.fittedvalues
my_res3 = my_lm3.resid

mult_mae3 = mean_absolute_error(df_houses['SalePrice'], my_predicts3)
print ''
print "Multiple Regression #3, response variable = SalePrice, MAE = ", mult_mae3
print ''

fig = plt.figure()
ax=fig.add_subplot(111)
fig.suptitle('QQ Plot Multiple Regression Residuals (MR3): Response = SalePrice', fontsize=14)
stats.probplot(my_res3, dist="norm", plot=plt)
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

# now, with log(SalePrice) as the response variable
my_lm4 = ols(formula = 'logSale~ TotalBsmtSF+GrLivArea+n_bins+mae+GarageArea+BsmtFinSF1+OverallQual',data = df_log).fit()
print my_lm4.summary()

#fig = plt.figure()
fig, ax = plt.subplots()
plt.scatter(df_log['logSale'],my_lm4.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot (MR4): Response = log(SalePrice)', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off') 
plt.show()

my_predicts4 = my_lm4.fittedvalues
my_res4 = my_lm4.resid
my_res4.describe()
mult_mae4 = mean_absolute_error(df_log['logSale'], my_predicts4)
print ''
print "Multiple Regression #4, response variable = log(SalePrice), MAE = ", mult_mae4
print ''


fig = plt.figure()
ax = fig.add_subplot(111)
fig.suptitle('QQ Plot Multiple Regression Residuals (MR4): Response = log(SalePrice)', fontsize=14)
stats.probplot(my_res4, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

# new multiple regression model trying sqrt(Y)
# -----------------------------------------------------------------------------

# create sqrt(SalePrice)  column
df_sr = df_merge[['TotalBsmtSF','GrLivArea','SalePrice', 'GarageArea','BsmtFinSF1',
    'OverallQual','Neighborhood', 'ppsf', 'mae', 'n_bins']]
df_sr['sqrtSale'] = np.sqrt(df_sr.SalePrice)

my_lm5 = ols(formula = 'sqrtSale~ TotalBsmtSF+GrLivArea+n_bins+mae+GarageArea+BsmtFinSF1+OverallQual',data = df_sr).fit()
print ' '
print my_lm5.summary()
print ' '

#fig = plt.figure()
fig, ax = plt.subplots()
plt.scatter(df_log['logSale'],my_lm5.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot (MR5): Response = sqrt(SalePrice)', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off') 
plt.show()

my_predicts5 = my_lm5.fittedvalues
my_res5 = my_lm5.resid
my_res5.describe()
mult_mae5 = mean_absolute_error(df_sr['sqrtSale'], my_predicts5)
print ''
print "Multiple Regression #5, response variable = sqrt(SalePrice), MAE = ", mult_mae5
print ''


fig = plt.figure()
ax = fig.add_subplot(111)
fig.suptitle('QQ Plot Multiple Regression Residuals (MR5): Response = sqrt(SalePrice)', fontsize=14)
stats.probplot(my_res5, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

