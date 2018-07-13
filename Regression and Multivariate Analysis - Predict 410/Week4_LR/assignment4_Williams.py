import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.sankey import Sankey
import numpy as np
#from sklearn.metrics import mean_absolute_error
from statsmodels.formula.api import ols
import scipy.stats as stats

sns.set_palette('colorblind')

df_start = pd.read_csv('/Users/tamtwill/NorthwesternU_MSPA/410 - Regression/Week4_LR/two_months_salary.csv', sep = ",")
obs0 = len(df_start)
sankey0 = "Starting number = "+ str(obs0)

# stone selection
df1= df_start[(df_start['carat'] >= .70) & (df_start['carat'] <= 2.0)] 
obs1 = len(df1)
sankey1 = "Drop all stones less than .70 carat and greater than or equal to 2.0 carats,  # remaining = " + str(obs1)
drop1 = obs0 - obs1
sandrop1 = "Dropped" + str(drop1)

df1 = df1[(df1['clarity'] < 8.0)]
obs2 = len(df1)
sankey2 = "Drop all stones with calrity less than SI2, # reamaining = "+str(obs2)
drop2 = obs1 - obs2

# drop "K" and below color because I can tell 
df1 = df1[(df1['color'] <= 8)]
obs3 = len(df1)
sankey3 = "Drop stones that have a 'K', 'L' or 'M' color,  # remaining = " + str(obs3)
drop3 = obs2 - obs3

# make a sankey chart showing the criteria and remaining number of observations
# for the data waterfall
fig = plt.figure(figsize=(6, 8))
ax = fig.add_subplot(1, 1, 1, xticks=[], yticks=[],
            title="Waterfall of dropped observations")
obs = [obs0, obs1, obs2, obs3]
labels = ["Drop all stones less than .70 and more than 2.0 carats", "Drop all stones with Clarity less than SI2",
          "Drop all stone with color equal or less than 'K' rating", "Remaining numberof stones"]
colors = ["#25EE46", "#2ADCB1", "#2ADCDC", "#20A6EE"]

sankey = Sankey(ax=ax, scale=0.0015, offset=0.5)
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
              trunklength=5.,
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
              trunklength=5.,
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


# EDA
#---------------------------------------
# Data quality check
# first, are the types as expected
df1.dtypes

# let's look at the summary data
print df1.price.describe()
print df1.carat.describe()

# looking at the .info(), to see if there are missing values
print df1.info()

# look at shape of the data
plt.figure
fig=sns.distplot(df1['price'], kde=False)
fig.set(title='Prices')
fig.set(yticklabels=[])
plt.show()

print 'Skew =', stats.skew(df1['price'])
print 'Kurtosis =', stats.kurtosis(df1['price'])

# look at a boxplot of price 
plt.figure()
fig = sns.boxplot(x="price", data=df1)
fig.set_title('Price')
plt.show()

## Let's look at a Pairplot
ax = sns.pairplot(df1)
plt.show()

# simple linear regression model
#---------------------------------------
slm = ols('price~ carat',data = df1).fit()
print slm.summary()
print ''

fig = plt.figure()
fig.suptitle('Data and fitted regression line - price v carat', fontsize=14)
fig= sns.regplot(df1['price'],df1['carat'], line_kws = {'color':'red'})
plt.show()

my_predicts = slm.fittedvalues
my_res = slm.resid
my_res.describe()

fig = plt.figure()
ax = fig.add_subplot(111)
stats.probplot(slm.resid, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

fig = plt.figure()
ax = fig.add_subplot(111)
fig = sns.residplot(x='price',y = 'carat', data = df1)
fig.set(xticklabels=[])
ax.set_ylabel('Residual')    
ax.set_xlabel('Price')
plt.axhline(linewidth=4, color='r')
plt.show()

fig, ax = plt.subplots()
plt.scatter(slm.fittedvalues, slm.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for price~carat (SLM)', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()


# multiple linear regression models
#-------------------------------------------------------------------------
#----------
mr1c = ols('price~ carat+color+clarity+cut+channel',data = df1).fit()
print mr1c.summary()
print ''

fig = plt.figure()
ax = fig.add_subplot(111)
stats.probplot(mr1c.resid, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

fig, ax = plt.subplots()
plt.scatter(mr1c.fittedvalues, mr1c.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for price~carat+color+clarity+channel (MR1)', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()



# MR2
#------------------------------------------
df_trans = df1
df_trans['log_price'] = np.log(df_trans.price)

mr2 = ols('log_price~ carat+color+clarity+cut+channel',data = df_trans).fit()
print mr2.summary()
print ''

fig = plt.figure()
ax = fig.add_subplot(111)
stats.probplot(mr2.resid, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()


fig, ax = plt.subplots()
plt.scatter(mr2.fittedvalues, mr2.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for log(price)~carat+color+clarity+cut+channel (MR2)', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()


#-----------------------------------
# effect of adding a quadratic term
#-----------------------------------

mr1_2 = ols('price~ np.power(carat,2)+color+clarity+cut+channel',data = df_trans).fit()
print mr1_2.summary()
print ''

fig, ax = plt.subplots()
plt.scatter(mr1_2.fittedvalues, mr1_2.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for price~carat**2+color+clarity+cut+channel (MR1_2)', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()

fig = plt.figure()
ax = fig.add_subplot(111)
stats.probplot(mr1_2.resid, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()


### ===========================================================================
###     Material in the appendix
### ===========================================================================

# finding the right predictors
#------------------------------------------

mr1 = ols('price~ carat+color',data = df1).fit()
print mr1.summary()
print ''

# my_predicts1 = mr1.fittedvalues
# my_res1 = mr1.resid
# my_res1.describe()

fig, ax = plt.subplots()
plt.scatter(mr1.fittedvalues,mr1.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for price~carat+color (MR1)', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()

fig = plt.figure()
fig.suptitle('QQ Plot MR1 Residuals', fontsize=14)
ax = fig.add_subplot(111)
stats.probplot(mr1.resid, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

#------  MR1a
mr1a = ols('price~ carat+color+clarity',data = df1).fit()
print mr1a.summary()
print ''

# my_predicts1a = mr1a.fittedvalues
# my_res1a = mr1a.resid
# my_res1a.describe()

fig, ax = plt.subplots()
plt.scatter(mr1a.fittedvalues,mr1a.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for price~carat+color+clarity (MR1a)', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()

fig = plt.figure()
fig.suptitle('QQ Plot MR1a Residuals', fontsize=14)
ax = fig.add_subplot(111)
stats.probplot(mr1a.resid, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

#---------- MR1b 
mr1b = ols('price~ carat+color+clarity+cut',data = df1).fit()
print mr1b.summary()
print ''
# 
# my_predicts1b = mr1b.fittedvalues
# my_res1b = mr1b.resid
# my_res1b.describe()

fig, ax = plt.subplots()
plt.scatter(mr1b.fittedvalues,mr1b.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for price~carat+color+clarity+cut (MR1b)', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()

fig = plt.figure()
fig.suptitle('QQ Plot MR1b Residuals', fontsize=14)
ax = fig.add_subplot(111)
stats.probplot(mr1b.resid, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

# -------------- MR1F
#convert the clarity into 4 dummy variables, for F
# VV, V and S
i=0
for i in range(0, len(df1)-1):
    if df1.iloc[i]['clarity'] <=2:
        df1['F'] = 1
        df1['VV'] = 0
        df1['V']  = 0
        df1['S'] = 0
    elif (df1.iloc[i]['clarity'] > 2 and df1.iloc[i]['clarity'] <=4):
        df1['F'] = 0
        df1['VV'] = 1
        df1['V']  = 0
        df1['S'] = 0
    elif (df1.iloc[i]['clarity'] > 4 and df1.iloc[i]['clarity'] <=6):
        df1['F'] = 0
        df1['VV'] = 0
        df1['V'] = 1
        df1['S'] = 0
    elif (df1.iloc[i]['clarity'] > 6):
        df1['F'] = 0
        df1['VV'] = 0
        df1['V'] = 0
        df1['S'] = 1
        
mr1F = ols('price~ carat+color+F+VV+V+S+cut+channel',data = df1).fit()
print mr1F.summary()
print ''

fig = plt.figure()
ax = fig.add_subplot(111)
stats.probplot(mr1F.resid, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

fig, ax = plt.subplots()
plt.scatter(mr1F.fittedvalues, mr1F.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for price~carat+color+F+VV+V+S+channel (MR1)', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()

    


# ------------ Store has minor effect
mr1d = ols('price~ carat+color+clarity+cut+channel+store',data = df1).fit()
print mr1d.summary()
print ''

my_predictsD = mr1d.fittedvalues
my_resD = mr1d.resid
my_resD.describe()

fig, ax = plt.subplots()
plt.scatter(df1['price'],mr1d.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for sqrt_price~carat+color+clarity+cut+channel+store (MR1d)', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()

fig = plt.figure()
fig.suptitle('QQ Plot mr1d Residuals', fontsize=14)
ax = fig.add_subplot(111)
stats.probplot(my_resD, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()

#-----------------------------------
# effect of adding quadratic terms
#-----------------------------------
mr2_2 = ols('price~ np.power(carat,4)+np.power(color,1)+np.power(clarity,1)+np.power(cut, 2)+ np.power(channel,1)+ np.power(store,2)',data = df_trans).fit()
print mr2_2.summary()
print ''


fig, ax = plt.subplots()
plt.scatter(mr2_2.fittedvalues,mr2_2.resid)
plt.ylabel('Residuals')
plt.xlabel('Predicted')
fig.suptitle('Residual Plot for price~carat^4 + color + clarity + cut^2 + channel + store^2 (MR2)', fontsize=14)
plt.axhline(linewidth=4, color='r')
ax.tick_params(labelbottom='off')  
plt.show()

fig = plt.figure()
ax = fig.add_subplot(111)
stats.probplot(mr2_2.resid, dist="norm", plot=plt, )
ax.get_lines()[0].set_markerfacecolor('steelblue')
plt.show()