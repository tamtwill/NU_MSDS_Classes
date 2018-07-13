# Tamara Williams
# Unit01 bonus bingo recreation of R assignment in Python 

# seed value for random number generators to obtain reproducible results
RANDOM_SEED = 1

# import base packages into the namespace for this program
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.formula.api as sm
from sklearn.feature_selection import RFE
from sklearn.linear_model import LogisticRegression
from statsmodels.stats.outliers_influence import variance_inflation_factor

import os

os.chdir('/Users/tamtwill/NorthwesternU_MSPA/Classes/Generalized Linear Models - 411/Unit 01/Assignment1') 

# initial work with the smaller data set
moneyball = pd.read_csv('moneyball.csv', sep = ',')

# clean the na's for charing purposes
na_moneyball = moneyball.copy()
na_moneyball=na_moneyball.fillna(0)

# examine the shape of input data 
print(moneyball.describe())

# show the histogram and boxplot
plt.figure(figsize=(8, 6))
plt.figure(1)
plt.suptitle("Target Wins")
plt.subplot(2,2,1)
plt.hist(na_moneyball['TARGET_WINS'])
plt.grid(True)

plt.subplot(2,2,2)
plt.boxplot(na_moneyball['TARGET_WINS'])
plt.grid(True)
plt.show()


# create plots for other statistics
# ------------- batting - hits and doubles
plt.figure(figsize=(8, 6))
plt.figure(1)
plt.subplot(221)
plt.title("Histogram of Hits")
plt.hist(na_moneyball['TEAM_BATTING_H'],color = "maroon")
plt.grid(True)

plt.subplot(222)
plt.title("Histogram of Doubles")
plt.hist(na_moneyball['TEAM_BATTING_2B'], color = "teal")
plt.grid(True)

plt.subplot(223)
plt.title("Boxplots of Hits")
sns.boxplot(na_moneyball['TEAM_BATTING_H'], color = "maroon", orient = 'v')
plt.grid(True)

plt.subplot(224)
plt.title("Boxplot of Doubles")
sns.boxplot(na_moneyball['TEAM_BATTING_2B'], color = "teal", orient = 'v')
plt.grid(True)
plt.subplots_adjust(top=0.75, bottom=0.05, left=0.10, right=0.95, hspace=0.5,wspace=0.5)
plt.show()


# ------------- triples and homeruns
plt.figure(figsize=(8, 6))
plt.figure(1)
plt.subplot(221)
plt.title("Histogram of Hits")
plt.hist(na_moneyball['TEAM_BATTING_3B'],color = "maroon")
plt.grid(True)

plt.subplot(222)
plt.title("Histogram of Doubles")
plt.hist(na_moneyball['TEAM_BATTING_HR'], color = "tan")
plt.grid(True)

plt.subplot(223)
plt.title("Boxplots of Hits")
sns.boxplot(na_moneyball['TEAM_BATTING_3B'], color = "maroon", orient = 'v')
plt.grid(True)

plt.subplot(224)
plt.title("Boxplot of Doubles")
sns.boxplot(na_moneyball['TEAM_BATTING_HR'], color = "tan", orient = 'v')
plt.grid(True)
plt.subplots_adjust(top=0.75, bottom=0.05, left=0.10, right=0.95, hspace=0.5,wspace=0.5)
plt.show()

# ------------- walks, strikeouts and HBP
plt.figure(figsize=(8, 6))
plt.figure(1)
plt.subplot(231)
plt.title("Histogram of Walk")
plt.hist(na_moneyball['TEAM_BATTING_BB'],color = "maroon")
plt.grid(True)

plt.subplot(232)
plt.title("Histogram of Strikeouts")
plt.hist(na_moneyball['TEAM_BATTING_SO'], color = "teal")
plt.grid(True)

plt.subplot(233)
plt.title("Histogram of HBP")
plt.hist(na_moneyball['TEAM_BATTING_HBP'],color = "tan")
plt.grid(True)


plt.subplot(234)
plt.title("Boxplots of Walks")
sns.boxplot(na_moneyball['TEAM_BATTING_BB'], color = "maroon", orient = 'v')
plt.grid(True)

plt.subplot(235)
plt.title("Boxplot of Strikeouts")
sns.boxplot(na_moneyball['TEAM_BATTING_SO'], color = "teal", orient = 'v')
plt.grid(True)

plt.subplot(236)
plt.title("Boxplots of HBP")
sns.boxplot(na_moneyball['TEAM_BATTING_HBP'], color = "tan", orient = 'v')
plt.grid(True)
plt.subplots_adjust(top=0.75, bottom=0.05, left=0.10, right=0.95, hspace=0.5,wspace=0.5)
plt.show()

# ------------- steal and CS
plt.figure(figsize=(8, 6))
plt.figure(1)
plt.subplot(221)
plt.title("Histogram of Steals")
plt.hist(na_moneyball['TEAM_BASERUN_SB'],color = "maroon")
plt.grid(True)

plt.subplot(222)
plt.title("Histogram of CS")
plt.hist(na_moneyball['TEAM_BASERUN_CS'], color = "tan")
plt.grid(True)

plt.subplot(223)
plt.title("Boxplots of Steals")
sns.boxplot(na_moneyball['TEAM_BASERUN_SB'], color = "maroon", orient = 'v')
plt.grid(True)

plt.subplot(224)
plt.title("Boxplot of CS")
sns.boxplot(na_moneyball['TEAM_BASERUN_CS'], color = "tan", orient = 'v')
plt.grid(True)
plt.subplots_adjust(top=0.75, bottom=0.05, left=0.10, right=0.95, hspace=0.5,wspace=0.5)
plt.show()


# ------------- Hits and Homeruns
plt.figure(figsize=(8, 8))
plt.figure(1)
plt.subplot(221)
plt.title("Histogram of Hits Against")
plt.hist(na_moneyball['TEAM_PITCHING_H'],color = "maroon")
plt.grid(True)

plt.subplot(222)
plt.title("Histogram of HR against")
plt.hist(na_moneyball['TEAM_PITCHING_HR'], color = "teal")
plt.grid(True)

plt.subplot(223)
plt.title("Boxplots of Hits Against")
sns.boxplot(na_moneyball['TEAM_PITCHING_H'], color = "maroon", orient = 'v')
plt.grid(True)

plt.subplot(224)
plt.title("Boxplot of HR against")
sns.boxplot(na_moneyball['TEAM_PITCHING_HR'], color = "teal", orient = 'v')
plt.grid(True)
plt.subplots_adjust(top=0.75, bottom=0.05, left=0.10, right=0.95, hspace=0.5,wspace=0.5)
plt.show()

# ------------- Walks and strikeouts
plt.figure(figsize=(8, 8))
plt.figure(1)
plt.subplot(221)
plt.title("Histogram of Walks Allowed")
plt.hist(na_moneyball['TEAM_PITCHING_BB'],color = "maroon")
plt.grid(True)

plt.subplot(222)
plt.title("Histogram of Strikeouts")
plt.hist(na_moneyball['TEAM_PITCHING_SO'], color = "tan")
plt.grid(True)

plt.subplot(223)
plt.title("Boxplots of Walks allowed")
sns.boxplot(na_moneyball['TEAM_PITCHING_BB'], color = "maroon", orient = 'v')
plt.grid(True)

plt.subplot(224)
plt.title("Boxplot of strikeouts")
sns.boxplot(na_moneyball['TEAM_PITCHING_SO'], color = "tan", orient = 'v')
plt.grid(True)
plt.subplots_adjust(top=0.75, bottom=0.05, left=0.10, right=0.95, hspace=0.5,wspace=0.5)
plt.show()


# ------------- Double plays and Error
plt.figure(figsize=(8, 8))
plt.figure(1)
plt.subplot(221)
plt.title("Histogram of Double Plays")
plt.hist(na_moneyball['TEAM_FIELDING_DP'],color = "maroon")
plt.grid(True)

plt.subplot(222)
plt.title("Histogram of Errors Committed")
plt.hist(na_moneyball['TEAM_FIELDING_E'], color = "teal")
plt.grid(True)

plt.subplot(223)
plt.title("Boxplots of Double Plays")
sns.boxplot(na_moneyball['TEAM_FIELDING_DP'], color = "maroon", orient = 'v')
plt.grid(True)

plt.subplot(224)
plt.title("Boxplot of Errors Committed")
sns.boxplot(na_moneyball['TEAM_FIELDING_E'], color = "teal", orient = 'v')
plt.grid(True)
plt.subplots_adjust(top=0.75, bottom=0.05, left=0.10, right=0.95, hspace=0.5,wspace=0.5)
plt.show()

# --------- scatterplot
## Let's look at a Pairplot
ax = sns.pairplot(na_moneyball[['TARGET_WINS', 'TEAM_BATTING_H', 'TEAM_BATTING_2B',
       'TEAM_BATTING_3B', 'TEAM_BATTING_HR', 'TEAM_BATTING_BB',
       'TEAM_BATTING_SO']], diag_kind="kde")
plt.show()

# --------- scatterplot
## Let's look at a Pairplot
ax = sns.pairplot(na_moneyball[['TARGET_WINS','TEAM_BASERUN_CS', 'TEAM_BASERUN_SB']], diag_kind="kde")
plt.show()

# --------- scatterplot
## Let's look at a Pairplot
ax = sns.pairplot(na_moneyball[['TARGET_WINS','TEAM_PITCHING_H', 'TEAM_PITCHING_HR',
       'TEAM_PITCHING_BB', 'TEAM_PITCHING_SO']], diag_kind="kde")
plt.show()


# fix the missing values
moneyball.fillna(moneyball.mean(), inplace = True)
moneyball.describe()

#Straighten Relationships
moneyball['TEAM_BATTING_1B'] = (moneyball['TEAM_BATTING_H'] - moneyball['TEAM_BATTING_HR'] - moneyball['TEAM_BATTING_3B'] - moneyball['TEAM_BATTING_2B'])
moneyball['log_TEAM_BATTING_1B'] = np.log(moneyball['TEAM_BATTING_1B'])
moneyball['log_TEAM_BATTING_3B'] = np.log(moneyball['TEAM_BATTING_3B'])
moneyball['log_TEAM_BASERUN_SB'] = np.log(moneyball['TEAM_BASERUN_SB'])
moneyball['log_TEAM_BASERUN_CS'] = np.log(moneyball['TEAM_BASERUN_CS'])

moneyball[moneyball['TEAM_FIELDING_E'] > 500] = 500
moneyball['sqrt_TEAM_PITCHING_HR'] = np.sqrt((moneyball['TEAM_PITCHING_HR']))
moneyball['SB_PCT'] = moneyball['TEAM_BASERUN_SB']/(1.0*moneyball['TEAM_BASERUN_SB']+moneyball['TEAM_BASERUN_CS'])

moneyball.describe()


#Remove bad data from data set
#-----------------------------
moneyball2 = moneyball.copy()
moneyball2 = moneyball[(moneyball.TARGET_WINS >= 21) & (moneyball.TARGET_WINS <= 120)]
moneyball2 = moneyball[moneyball.TEAM_PITCHING_H < 2000]
moneyball2.describe()


#################### Part 3: Model Creation ############################################
#---------------------------------------------------------------------------------------
#Function for Mean Square Error Calculation
def my_mse(sm): 
  mse = np.mean(sm*sm)
  print ("MSE =",mse,'\n')

olsmodel = sm.ols ('TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B +\
                TEAM_BATTING_HR + TEAM_BATTING_H + TEAM_BATTING_BB + TEAM_BATTING_SO +\
                TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_HR + TEAM_PITCHING_BB +\
                TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP +\
                log_TEAM_BATTING_1B +log_TEAM_BATTING_3B + log_TEAM_BASERUN_SB +\
                SB_PCT + log_TEAM_BASERUN_CS + sqrt_TEAM_PITCHING_HR', data = moneyball)

results = olsmodel.fit()
print(results.summary())

vif = pd.DataFrame()
vif["VIF Factor"] = [variance_inflation_factor(moneyball.values, i) for i in range(moneyball.shape[1])]
vif["features"] = moneyball.columns
print ("VIF values\n", vif)
tmp_e = results.resid
my_mse(tmp_e)


# closest I can get to teh R stepwise functions
tmpx = moneyball.copy()
tmpx = tmpx.drop('TARGET_WINS', axis = 1)
tmpx = tmpx.values
#tmpx=tmpx.reshape(-1, 1)
tmpy=moneyball['TARGET_WINS']
tmpy =tmpy.values
#tmpx=tmpx.reshape(-1, 1)

# create a base classifier used to evaluate a subset of attributes
method = LogisticRegression()
# create the RFE model and select 3 attributes
rfe = RFE(method, 5)
rfe = rfe.fit(tmpx, tmpy)
names = moneyball.columns.values
names= np.delete(names, 1)

# summarize the selection of the attributes, see which to keep 
names = moneyball.columns.values
names= np.delete(names, 1)
m_vars = rfe.support_
print(rfe.support_)
print(rfe.ranking_)
print(rfe.get_params)
mask = np.stack((names, m_vars), axis = -1)
print ("Keep the true variables\n", mask)



print(tmpx)

model3 = sm.ols ('TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B +\
                   TEAM_BATTING_3B + TEAM_BATTING_HR + TEAM_BASERUN_SB +\
                   TEAM_BASERUN_CS + TEAM_FIELDING_E + TEAM_FIELDING_DP +\
                   TEAM_PITCHING_SO + TEAM_PITCHING_BB', data = moneyball2)


