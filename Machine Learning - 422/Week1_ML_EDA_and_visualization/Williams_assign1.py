# Assignment 1 
# Predict 422, Fall 2017
# Submitted by Tamara Williams
#-------------------------------


# NOTE: rplot was deprecated in 0.19, use seaborn instead, sample needs update
#import pandas.tools.rplot as rplot  # trellis/lattice plotting  
#from pandas.tools.plotting import scatter_matrix    

import pandas as pd  # data frame operations
import numpy as np  # arrays and math functions
import matplotlib.pyplot as plt  # static plotting
import seaborn as sns  # pretty plotting, including heat map
from sklearn.cluster import KMeans
from sklearn import preprocessing
from sklearn import metrics  # for silhouette coefficient
from collections import OrderedDict  # to create DataFrame with ordered columns
from statsmodels.formula.api import ols
from sklearn.decomposition import PCA

# correlation heat map setup for seaborn
def corr_chart(df_corr,file_name):
    corr=df_corr.corr()
    #screen top half to get a triangle
    top = np.zeros_like(corr, dtype=np.bool)
    top[np.triu_indices_from(top)] = True
    fig=plt.figure()
    fig, ax = plt.subplots(figsize=(12,12))
    sns.heatmap(corr, mask=top, cmap='coolwarm', 
        center = 0, square=True, 
        linewidths=.5, cbar_kws={'shrink':.5}, 
        annot = True, annot_kws={'size': 9}, fmt = '.3f')           
    plt.xticks(rotation=90) # rotate variable labels on columns (x axis)
    plt.yticks(rotation=0) # use horizontal variable labels on rows (y axis)
    plt.title('Correlation Heat Map')   
    plt.savefig(file_name, 
        bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
        orientation='portrait', papertype=None, format=None, 
        transparent=True, pad_inches=0.25, frameon=None)      

np.set_printoptions(precision=3)

# read in comma-delimited text file, creating a pandas DataFrame object
valid_survey_input = pd.read_csv('mspa-survey-data.csv')

# use the RespondentID as label for the rows... the index of DataFrame
valid_survey_input.set_index('RespondentID', drop = True, inplace = True)

# examine the structure of the DataFrame object
print('\nContents of initial survey data ---------------')

# could use len() or first index of shape() to get number of rows/observations
print('\nNumber of Respondents =', len(valid_survey_input)) 

# show the column/variable names of the DataFrame
# note that RespondentID is no longer present
print(valid_survey_input.columns)

# abbreviated printing of the first five rows of the data frame
print(pd.DataFrame.head(valid_survey_input)) 

# shorten the variable/column names for software preference variables
survey_df = valid_survey_input.rename(index=str, columns={'Personal_JavaScalaSpark': 'My_Java',
    'Personal_JavaScriptHTMLCSS': 'My_JS',
    'Personal_Python': 'My_Python',
    'Personal_R': 'My_R',
    'Personal_SAS': 'My_SAS',
    'Professional_JavaScalaSpark': 'Prof_Java',
    'Professional_JavaScriptHTMLCSS': 'Prof_JS',
    'Professional_Python': 'Prof_Python',
    'Professional_R': 'Prof_R',
    'Professional_SAS': 'Prof_SAS',
    'Industry_JavaScalaSpark': 'Ind_Java',
    'Industry_JavaScriptHTMLCSS': 'Ind_JS',
    'Industry_Python': 'Ind_Python',
    'Industry_R': 'Ind_R',
    'Industry_SAS': 'Ind_SAS'})

# define subset DataFrame for analysis of software preferences 
software_df = survey_df.loc[:, 'My_Java':'Ind_SAS']
                     
# scatter plot example
fig, axis = plt.subplots()
axis.set_xlabel('Personal Preference for R')
axis.set_ylabel('Personal Preference for Python')
plt.title('R and Python Perferences')
scatter_plot = axis.scatter(survey_df['My_R'], 
    survey_df['My_Python'],
    facecolors = 'none', 
    edgecolors = 'blue') 
plt.show()
plt.savefig('plot-scatter-r-python.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  

# examine intercorrelations among software preference variables
# with correlation matrix/heat map
corr_chart(df_corr = software_df, file_name = 'plot-corr-map.pdf') 

# descriptive statistics for software preference variables
print('\nDescriptive statistics for survey data ---------------')
print(software_df.describe())

# descriptive statistics for one variable
print('\nDescriptive statistics for courses completed ---------------')
print(survey_df['Courses_Completed'].describe())

#-------------------------------------------------------------------------
#--------------------- Language Exploration ------------------------------
#-------------------------------------------------------------------------

# get column lables, this is often useful to have
features = valid_survey_input.columns.values

# chart mean level of interest by language
avg_interest = software_df.mean()
lang_interest = pd.DataFrame({'language':avg_interest.index, 'mean interest':avg_interest.values}).sort_values(by='mean interest', ascending=False)
fig, axis = plt.subplots()
axis.set_xlabel('Language')
axis.set_ylabel('Average Interest')
plt.title('Mean Language Perferences')
ax = sns.barplot(x=lang_interest['mean interest'],y=lang_interest['language'], data=lang_interest)
plt.savefig('barplot_sw_interest.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()

# let aggregate the language families and plot that view of language interest
lang_fam = valid_survey_input.copy()
for index, rows in lang_fam.iterrows():
    lang_fam['any_web'] = lang_fam['Personal_JavaScriptHTMLCSS']+\
    lang_fam['Professional_JavaScriptHTMLCSS']+\
        lang_fam['Industry_JavaScriptHTMLCSS']
    lang_fam['any_R'] = lang_fam['Personal_R']+\
        lang_fam['Professional_R']+ lang_fam['Industry_R']
    lang_fam['any_SAS']= lang_fam['Personal_SAS']+\
        lang_fam['Industry_SAS']+lang_fam['Professional_SAS']
    lang_fam['any_java'] = lang_fam['Personal_JavaScalaSpark']+\
        lang_fam['Professional_JavaScalaSpark']+lang_fam['Industry_JavaScalaSpark']
    lang_fam['any_python'] = lang_fam['Personal_Python']+\
        lang_fam['Professional_Python']+ lang_fam['Industry_Python']
drop_list = ['Personal_JavaScalaSpark', 'Personal_JavaScriptHTMLCSS',
       'Personal_Python', 'Personal_R', 'Personal_SAS',
       'Professional_JavaScalaSpark', 'Professional_JavaScriptHTMLCSS',
       'Professional_Python', 'Professional_R', 'Professional_SAS',
       'Industry_JavaScalaSpark', 'Industry_JavaScriptHTMLCSS',
       'Industry_Python', 'Industry_R', 'Industry_SAS',
       'Python_Course_Interest', 'Foundations_DE_Course_Interest',
       'Analytics_App_Course_Interest', 'Systems_Analysis_Course_Interest',
       'Courses_Completed', 'PREDICT400', 'PREDICT401', 'PREDICT410',
       'PREDICT411', 'PREDICT413', 'PREDICT420', 'PREDICT422',
       'PREDICT450', 'PREDICT451', 'PREDICT452', 'PREDICT453',
       'PREDICT454', 'PREDICT455', 'PREDICT456', 'PREDICT457',
       'OtherPython', 'OtherR', 'OtherSAS', 'Other', 'Graduate_Date']
lang_fam_short = lang_fam.drop(drop_list, axis=1)
lang_interest = lang_fam_short.mean()
lang_interest = pd.DataFrame({'language':lang_interest.index,
    'mean aggregate interest':lang_interest.values}).sort_values(by='mean aggregate interest', ascending=False)
        
# chart level of interest by language
fig, axis = plt.subplots()
axis.set_xlabel('Language Family')
axis.set_ylabel(' Aggregate Interest')
plt.title('Aggregated Language Family Perferences')
ax = sns.barplot(x=lang_interest['mean aggregate interest'],y=lang_interest['language'], data=lang_interest)
plt.savefig('barplot_agg_sw_interest.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()


# examine correlations of language family preference and class interest
drop_list = ['Personal_JavaScalaSpark', 'Personal_JavaScriptHTMLCSS',
       'Personal_Python', 'Personal_R', 'Personal_SAS',
       'Professional_JavaScalaSpark', 'Professional_JavaScriptHTMLCSS',
       'Professional_Python', 'Professional_R', 'Professional_SAS',
       'Industry_JavaScalaSpark', 'Industry_JavaScriptHTMLCSS',
       'Industry_Python', 'Industry_R', 'Industry_SAS',
       'Courses_Completed', 'PREDICT400', 'PREDICT401', 'PREDICT410',
       'PREDICT411', 'PREDICT413', 'PREDICT420', 'PREDICT422',
       'PREDICT450', 'PREDICT451', 'PREDICT452', 'PREDICT453',
       'PREDICT454', 'PREDICT455', 'PREDICT456', 'PREDICT457',
       'OtherPython', 'OtherR', 'OtherSAS', 'Other', 'Graduate_Date']
lang_fam_corr = lang_fam.drop(drop_list, axis=1)
corr_chart(df_corr = lang_fam_corr, file_name = 'plot_corr_lang_class.pdf') 


# get descriptive statistics for the interest level in new classes
new_classes = valid_survey_input.filter(['Python_Course_Interest', 'Foundations_DE_Course_Interest',
       'Analytics_App_Course_Interest', 'Systems_Analysis_Course_Interest'])
print('\nDescriptive statistics for new class data ---------------')
print(new_classes.describe())

# visualizing new class interest
plt.figure()
sns.boxplot(data = new_classes,orient="h", palette="Set2")
plt.title('New Class Interest')
plt.savefig('boxplot_class_interest.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()


#-------------------------------------------------------------------------
#--------------------- PCA Exploration -----------------------------------
#-------------------------------------------------------------------------
# now scale the data for actual use
tmp = valid_survey_input.copy()
tmp = tmp.loc[:,'Personal_JavaScalaSpark':'Courses_Completed']
tmp = tmp.fillna(0)
scaled_data = preprocessing.scale(tmp)   

num_var = len(tmp.columns)

pca = PCA()  
pca.fit_transform(scaled_data)

# find the amount of variance each PC explains 
# and the cummulative variance explained
var_ex = np.round(pca.explained_variance_ratio_, decimals = 3)*100
cumm_var = np.cumsum(np.round(pca.explained_variance_ratio_, decimals = 3)*100)
eig_val = np.round(pca.explained_variance_, decimals = 3)
print('\n')


#for i in range(0,num_var):
#    print("PC %i accounts for %g%% of variation; cummulative variation is: %g%%"\
#    %(i+1, var_ex[i], cumm_var[i]))

#print('\n') 
#for i in range(0,num_var):
#    print ("PC %i has an eigen value of %g" %(i+1, eig_val[i]))

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
plt.savefig('scree_plot.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()


# compute full set of principal components (scores)
pca_scores = pca.fit_transform(scaled_data)

# add principal component scores to the original data frame
df_pca=tmp
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
df_pca['pc12'] = pca_scores[:,11]
df_pca['pc13'] = pca_scores[:,12]
df_pca['pc14'] = pca_scores[:,13]
df_pca['pc15'] = pca_scores[:,14]
df_pca['pc16'] = pca_scores[:,15]
df_pca['pc17'] = pca_scores[:,16]
df_pca['pc18'] = pca_scores[:,17]
df_pca['pc19'] = pca_scores[:,18]

# reduce data to just the 3 PCs we will look at
pcr_mod_data = df_pca.loc[:,['Courses_Completed','pc1','pc2', 'pc3']]

## Let's look at a scatterplot matrix for the selected subset of data
ax = sns.pairplot(pcr_mod_data, diag_kind='kde')
plt.savefig('PCA_scatter.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  
plt.show()


pca1 = ols('Courses_Completed ~ pc1+pc2+pc3', data = df_pca).fit()  
print (pca1.summary())
print ('')


#-------------------------------------------------------------------------
#--------------------- OLS Exploration -----------------------------------
#-------------------------------------------------------------------------
r_model = ols('any_R~Python_Course_Interest+Foundations_DE_Course_Interest+\
    Analytics_App_Course_Interest+Systems_Analysis_Course_Interest', data = lang_fam).fit()  
print (r_model.summary())
print ('')

sas_model = ols('any_SAS~Python_Course_Interest+Foundations_DE_Course_Interest+\
    Analytics_App_Course_Interest+Systems_Analysis_Course_Interest', data = lang_fam).fit()  
print (sas_model.summary())
print ('')

python_model = ols('any_python~Python_Course_Interest+Foundations_DE_Course_Interest+\
    Analytics_App_Course_Interest+Systems_Analysis_Course_Interest', data = lang_fam).fit()  
print (python_model.summary())
print ('')

web_model = ols('any_web~Python_Course_Interest+Foundations_DE_Course_Interest+\
    Analytics_App_Course_Interest+Systems_Analysis_Course_Interest', data = lang_fam).fit()  
print (web_model.summary())
print ('')

java_model = ols('any_java~Python_Course_Interest+Foundations_DE_Course_Interest+\
    Analytics_App_Course_Interest+Systems_Analysis_Course_Interest', data = lang_fam).fit()  
print (java_model.summary())
print ('')


analytics_class = ols('Analytics_App_Course_Interest~Personal_JavaScalaSpark+\
       Personal_JavaScriptHTMLCSS+Personal_Python+Personal_R+Personal_SAS+\
       Professional_JavaScalaSpark+Professional_JavaScriptHTMLCSS+\
       Professional_Python+Professional_R+Professional_SAS+\
       Industry_JavaScalaSpark+Industry_JavaScriptHTMLCSS+\
       Industry_Python+Industry_R+Industry_SAS+\
       Python_Course_Interest+Foundations_DE_Course_Interest+\
       Systems_Analysis_Course_Interest+\
       Courses_Completed', data=lang_fam).fit()
print (analytics_class.summary())
print ('')

python_class = ols('Python_Course_Interest~Personal_JavaScalaSpark+Personal_JavaScriptHTMLCSS+\
       Personal_Python+Personal_R+Personal_SAS+\
       Professional_JavaScalaSpark+Professional_JavaScriptHTMLCSS+\
       Professional_Python+Professional_R+Professional_SAS+\
       Industry_JavaScalaSpark+Industry_JavaScriptHTMLCSS+\
       Industry_Python+Industry_R+Industry_SAS+\
       Foundations_DE_Course_Interest+\
       Analytics_App_Course_Interest+Systems_Analysis_Course_Interest+\
       Courses_Completed', data=lang_fam).fit()
print (python_class.summary())
print ('')


foundation_class = ols('Foundations_DE_Course_Interest~Personal_JavaScalaSpark+\
       Personal_JavaScriptHTMLCSS+Personal_Python+Personal_R+Personal_SAS+\
       Professional_JavaScalaSpark+Professional_JavaScriptHTMLCSS+\
       Professional_Python+Professional_R+Professional_SAS+\
       Industry_JavaScalaSpark+Industry_JavaScriptHTMLCSS+\
       Industry_Python+Industry_R+Industry_SAS+\
       Python_Course_Interest+\
       Analytics_App_Course_Interest+Systems_Analysis_Course_Interest+\
       Courses_Completed', data=lang_fam).fit()
print (foundation_class.summary())
print ('')

systems_class = ols('Systems_Analysis_Course_Interest~Personal_JavaScalaSpark+\
       Personal_JavaScriptHTMLCSS+Personal_Python+Personal_R+Personal_SAS+\
       Professional_JavaScalaSpark+Professional_JavaScriptHTMLCSS+\
       Professional_Python+Professional_R+Professional_SAS+\
       Industry_JavaScalaSpark+Industry_JavaScriptHTMLCSS+\
       Industry_Python+Industry_R+Industry_SAS+\
       Python_Course_Interest+\
       Analytics_App_Course_Interest+Foundations_DE_Course_Interest+\
       Courses_Completed', data=lang_fam).fit()
print (systems_class.summary())
print ('')


#-------------------------------------------------------------------------
#--------------------- Clustering  Exploration ---------------------------
#-------------------------------------------------------------------------
def look_at_clusters(df):
    # evaluate a solution based on the silhouette coefficient
    cluster_data = df
    measures_list=[]
    print ('')
    for nclusters in range(2,13): # search between 2 and 12 clusters/segments
        kmeans = KMeans(n_clusters = nclusters, n_init = 2, random_state = 13)
        kmeans.fit(cluster_data)
        segment = kmeans.predict(cluster_data)  # cluster ids for variables
        tmp = metrics.silhouette_score(cluster_data, segment, metric='euclidean')
        measures_list.append(tmp)
        print('nclusters: %i, silhouette coefficient: %f' %(nclusters,tmp))
    print ('\n')
    
    max_score = max(measures_list)
    best_n = measures_list.index(max_score) + 2
    print ('\n')
    print ("highest silhouette score is %f for nclusters = %i" %(max_score, best_n))
    print ('\n')
    
    print('')
    print('----- Solution for Best N Clusters  as determined by Silhouette -----')
    print('')
    
    best_cluster_data = np.asmatrix(norm_data.copy())
    interest_lvl= features[0:19]
    best_cluster_data.colnames = interest_lvl
    kmeans = KMeans(n_clusters = best_n, n_init = 2, random_state = 13)
    kmeans.fit(best_cluster_data)
    segment = kmeans.predict(best_cluster_data)  # cluster index
    
    cluster_t = best_cluster_data.T
    kmeans.fit(cluster_t)
    cluster = kmeans.predict(cluster_t)
    kmeans_res = pd.DataFrame(OrderedDict([('cluster', cluster),('interest', interest_lvl)]))
    
    # show results
    print ('')
    print ('cluster membership by interest levels')
    for cluster_id in sorted(kmeans_res.cluster.unique()):
        tmp = (kmeans_res.loc[kmeans_res['cluster']==cluster_id])
        tmp = tmp.to_string(index=False)
        print (tmp)
        print('')


tmp = valid_survey_input.copy()
tmp = tmp.loc[:,'Personal_JavaScalaSpark':'Systems_Analysis_Course_Interest']
tmp = tmp.fillna(0)
norm_data = preprocessing.scale(tmp)        

look_at_clusters(norm_data)

# let's compress the language variations to just any_[language]
# to see how that effects the interest in new classes
agg_data = valid_survey_input.copy()
agg_data = agg_data.loc[:,'Personal_JavaScalaSpark':'Courses_Completed']
agg_data = agg_data.fillna(0)
for index, rows in agg_data.iterrows():
    agg_data['any_web'] = agg_data['Personal_JavaScriptHTMLCSS']+agg_data['Professional_JavaScriptHTMLCSS']+\
        agg_data['Industry_JavaScriptHTMLCSS']
    agg_data['any_R'] = agg_data['Personal_R']+agg_data['Professional_R']+ agg_data['Industry_R']
    agg_data['any_SAS']= agg_data['Personal_SAS']+agg_data['Industry_SAS']+agg_data['Professional_SAS']
    agg_data['any_java'] = agg_data['Personal_JavaScalaSpark']+agg_data['Professional_JavaScalaSpark']+agg_data['Industry_JavaScalaSpark']
    agg_data['any_python'] = agg_data['Personal_Python']+agg_data['Professional_Python']+ agg_data['Industry_Python']

drop_lst=['Personal_JavaScalaSpark', 'Personal_JavaScriptHTMLCSS',
       'Personal_Python', 'Personal_R', 'Personal_SAS',
       'Professional_JavaScalaSpark', 'Professional_JavaScriptHTMLCSS',
       'Professional_Python', 'Professional_R', 'Professional_SAS',
       'Industry_JavaScalaSpark', 'Industry_JavaScriptHTMLCSS',
       'Industry_Python', 'Industry_R', 'Industry_SAS']
agg_data = agg_data.drop(drop_lst, axis = 1)

look_at_clusters(agg_data)

