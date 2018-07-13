#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Wed Aug  2 11:35:07 2017

@author: Tamara Williams
"""

import os
os.chdir('/Users/tamtwill/NorthwesternU_MSPA/410 - Regression/Week8_LR')

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans, AgglomerativeClustering
from sklearn import preprocessing
from sklearn import metrics  # for silhouette coefficient
from sklearn.metrics.pairwise import euclidean_distances
from collections import OrderedDict  # to create DataFrame with ordered columns
from scipy.cluster.hierarchy import dendrogram
import seaborn as sns; sns.set()

# dendrogram plot for sklearn.  Scipy and sklearn hierarchy clusters don't
# come out the same, so you have to tailor the graph if using sklearn methods
# from the jump-start code: this method contributed by Matt Kallada at
# https://github.com/scikit-learn/scikit-learn/pull/3464/files
def plot_dendrogram(model, **kwargs):
    # Children of hierarchical clustering
    children = model.children_
    # Distances between each pair of children
    # Since we don't have this information,
    # we can use a uniform one for plotting
    distance = np.arange(children.shape[0])
    # The number of observations contained in each cluster level
    no_of_observations = np.arange(2, children.shape[0]+2)
    # Create linkage matrix and then plot the dendrogram
    linkage_matrix = \
        np.column_stack([children, distance, no_of_observations]).astype(float)
    # Plot the corresponding dendrogram
    dendrogram(linkage_matrix, **kwargs)


# read the data in
study_data = pd.read_csv("time_use_1976_rev.csv")
study_names = pd.Series(study_data.columns)

n_time_clusters = 4
n_demo_clusters = 2

print ('')
print ('----- Summary of Input Data -----')
print ('')

# show a portion of the beginning of the DataFrame
print (study_data.head())
print ('')

time_use = study_data.drop(study_data.columns[[0,1,2,3,4]], axis=1)
activity = list(time_use.columns)

# show descriptive statistics
pd.set_option('display.max_columns', None)  # do not limit output
print ('')
print (time_use.describe())
print ('')


#------------------------------------------------------------------------------
# computing KMean clusters, start by normalizing data, then transpose the data
# finally perform cluster analysis
#------------------------------------------------------------------------------
norm_time = preprocessing.scale(time_use)

#transpose the data for use in the cluster ananlysis
time_cluster_data = norm_time.T

# evaluate a solution based on the silhouette coefficient
time_cluster_data = norm_time
measures_list=[]
print ('')
for nclusters in range(2,11): # search between 2 and 10 clusters/segments
    kmeans = KMeans(n_clusters = nclusters, n_init = 25, random_state = 13)
    kmeans.fit(time_cluster_data)
    segment = kmeans.predict(time_cluster_data)  # cluster ids for variables
    tmp = metrics.silhouette_score(time_cluster_data, segment, metric='euclidean')
    measures_list.append(tmp)
    print('nclusters: %i, silhouette coefficient: %f' %(nclusters,tmp))
print ('\n')

max_score = max(measures_list)
best_n = measures_list.index(max_score) + 2
print ('\n')
print ("highest silhouette score is %f for nclusters = %i" %(max_score, best_n))
print ('\n')

#****************************
# Compute Euclidean distances between the time data, the books all talk about
# Euclidean distance, but it isn't clear how to interpret it in the context
# of this assignment
print ('\n')
print ("Euclidean distances between normalized time use data points")
dist_array=pd.DataFrame(euclidean_distances(norm_time, norm_time))
print (dist_array.head())
print ('\n')
#****************************

print('')
print('----- Solution for Best N Clusters  as determined by Silhouette -----')
print('')

best_cluster_data = pd.DataFrame(preprocessing.scale(time_use))
best_cluster_data.columns = activity
kmeans = KMeans(n_clusters = n_time_clusters, n_init = 25, random_state = 13)
kmeans.fit(best_cluster_data)
segment = kmeans.predict(best_cluster_data)  # cluster index

#****************************
## Example plot of first 2 variables as example of how to visualize K-Means
label = kmeans.labels_
centroids = kmeans.cluster_centers_
my_marks = ('<','s', 'P','D','*','H','x', 'v','d', '>')
fig = plt.figure(figsize=(8, 8))
ax=fig.add_subplot(111)
plt.scatter(norm_time[:,0], norm_time[:,1], c=label, s=100, cmap=plt.cm.cool)
for x, y, m in zip(centroids[:,0],centroids[:,1], my_marks):
    ax.scatter([x],[y], marker=m, s=100, color = "black")
title_string = "Center of Time Clusters, n=10"
plt.title(title_string, fontsize=14)
plt.ylabel('Variable 2')
plt.xlabel('Variable 1')
plt.show()
#****************************


# transpose the data for use in the cluster ananlysis anp
# print variable by cluster
time_cluster_t = best_cluster_data.T
kmeans.fit(time_cluster_t)
cluster = kmeans.predict(time_cluster_t)
time_kmeans_res = pd.DataFrame(OrderedDict([('cluster', cluster),
                ('variable', activity)]))

# show results
print ('')
print ('cluster membership by variable for K-Means time use')
for cluster_id in sorted(time_kmeans_res.cluster.unique()):
    tmp = (time_kmeans_res.loc[time_kmeans_res['cluster']==cluster_id])
    tmp = tmp.to_string(index=False)
    print tmp
print('')

# create pandas DataFrame for summarizing the cluster analysis results
# using OrderedDict to preserve the order of column names
time_kmeans_res = pd.DataFrame(OrderedDict([('observation',
    range(0,len(best_cluster_data))),('segment', segment)]))


# to interpret the results of the segmentation
# we can review the original ratings data for the six clusters/segments
# merge/join the segment information with the original student data
time_segmentation_data = time_kmeans_res.join(best_cluster_data)

# try printing the means for attributes within each segment
print ('\n')
for segment_id in sorted(time_segmentation_data.segment.unique()):
    print('')
    print('Attribute means for segment: %i' %segment_id)
    this_time_segment_data = time_segmentation_data[ \
        time_segmentation_data.segment == segment_id]
    attributes = this_time_segment_data.loc[:,'professional':'leisure'].mean()
    print(attributes.values)
    print ('\n')


#------------------------------------------------------------------------------
# computing the hierarchical clusters - activities
#------------------------------------------------------------------------------
print ('---------computing the hierarchical clusters - activities ---------')
best_cluster_data = pd.DataFrame(preprocessing.scale(time_use))
best_cluster_data.columns = activity

variable_cluster_data = best_cluster_data.T


hierarch = AgglomerativeClustering(n_clusters=n_time_clusters, affinity='euclidean',
                                   compute_full_tree='auto', linkage='ward')

segment = hierarch.fit_predict(best_cluster_data)  # cluster index

# create pandas DataFrame for summarizing the cluster analysis results
# using OrderedDict to preserve the order of column names
time_hierarch_res = pd.DataFrame(OrderedDict([('cluster', cluster),
                ('variable', activity)]))

# show results
print ('')
print ('cluster membership by variable for Hierarchy time use')
for cluster_id in sorted(time_hierarch_res.cluster.unique()):
    tmp = (time_hierarch_res.loc[time_hierarch_res['cluster']==cluster_id])
    tmp = tmp.to_string(index=False)
    print tmp
print('')

# to interpret the results of the segmentation
# we can review the original ratings data for the clusters/segments
# merge/join the segment information with the original student data
time_segmentation_data = time_hierarch_res.join(best_cluster_data)

# try printing the means for attributes within each segment
for cluster_id in sorted(time_segmentation_data.cluster.unique()):
    print('')
    print('Attribute means for segment: %i' %segment_id)
    this_time_cluster_data = time_segmentation_data[ \
        time_segmentation_data.cluster == cluster_id]
    attributes = this_time_cluster_data.loc[:,'professional':'leisure'].mean()
    print(attributes.values)
    print ('\n')

#------------------------------------------------------------------------------
# create the dendrogram - this uses scipy and gives different results
# than the sklearn hierarchy
#------------------------------------------------------------------------------
#
## generate the linkage matrix
#Z_time = linkage(time_use, method='complete', metric='euclidean')
#plt.figure(figsize=(12, 10))
#plt.title('Hierarchical Clustering Time Use Dendrogram')
#plt.xlabel('Observation Group')
#plt.ylabel('Distance')
#dendrogram(Z_time,
#    leaf_rotation=90.,
#    leaf_font_size=12.,
#    labels = ['maus','waus','wnaus','mnsus','wnsus','msus','wsus','mawe','wawe',
#     'wnawe','mnswe','wnswe','mswe','wswe','mayu','wayu','wnayu','mnsyu',
#     'wnsyu','msyu','wsyu','maea','waea','wnaea','mnsea','wnsea','msea','wsea']
#    )
#plt.show()

#------------------------------------------------------------------------------
# create the dendrogram - for sklearn using the code from the jumpstart
#------------------------------------------------------------------------------

# compute the full tree with all observations
ward = AgglomerativeClustering(linkage='ward',
    n_clusters = variable_cluster_data.shape[0],
    compute_full_tree = True)
ward_full_tree = ward.fit(variable_cluster_data)

# use variable names for the cluster labels
cluster_labels = map(lambda x: time_use.columns[x], ward_full_tree.labels_)

# plot the full hierarchical tree with variable labels
plot_dendrogram(ward_full_tree, labels = cluster_labels,leaf_rotation=90.)
plt.show()

# using n_time_clusters from silhouette metrics as number of clusters
ward_n_time_clusters = AgglomerativeClustering(linkage='ward',
    n_clusters = n_time_clusters,
    compute_full_tree = False)
ward_n_time_clusters_tree = ward_n_time_clusters.fit(variable_cluster_data)
# get cluster ids for variables
ward_n_time_clusters_cluster = ward_n_time_clusters.fit_predict(variable_cluster_data)

# create pandas DataFrame for summarizing the cluster analysis results
variable_ward_solution = \
    pd.DataFrame(OrderedDict([('cluster', ward_n_time_clusters_cluster),('variable', activity)]))

# print results of variable clustering one cluster at a time
print ('cluster membership by variable for hierarchy cluster by time use variables')
for cluster_id in sorted(set(variable_ward_solution.cluster)):
    tmp = (variable_ward_solution.loc[variable_ward_solution['cluster'] == \
        cluster_id])
    tmp = tmp.to_string(index = False)
    print tmp
print ('')

print ('')
print ('*******************************************************')
print "demographic data"
print ('*******************************************************')
print ('')
#******************************************************************************
# repeat the process from above for the demographic data
#******************************************************************************
demog_data = study_data.drop(study_data.columns[[5,6,7,8,9,10,11,12,13,14]], axis=1)
demog_var = demog_data.columns

# show descriptive statistics
pd.set_option('display.max_columns', None)  # do not limit output
print ('')
print (demog_data.describe())
print ('')

#------------------------------------------------------------------------------
# encoding the catagory vars in the demographic data
# so you can run calculations
#------------------------------------------------------------------------------
df_tmp=study_data.copy()
df_tmp = df_tmp.drop(group)
num_data=df_tmp
tmpC = num_data
tmpC.gender=pd.Categorical(tmpC.gender)
tmpC.country=pd.Categorical(tmpC.country)
tmpC.professional_work_status =pd.Categorical(tmpC.professional_work_status)
tmpC.marital_status=pd.Categorical(tmpC.marital_status)


num_data['gender'] = tmpC.gender.cat.codes
num_data['country'] = tmpC.country.cat.codes
num_data['professional_work_status'] = tmpC.professional_work_status.cat.codes
num_data['marital_status'] = tmpC.marital_status.cat.codes

demog_data = num_data.iloc[:,0:5]


#------------------------------------------------------------------------------
# computing KMean clusters, start by normalizing data, then transpose the data
# finally perform cluster analysis
#------------------------------------------------------------------------------
#demog_cluster_data = preprocessing.scale(demog_data)
demog_cluster_data  = demog_data

#transpose the data for use in the cluster ananlysis
rotate_demog=demog_cluster_data.T

# evaluate a solution based on the silhouette coefficient
measures_list=[]
print ('')
for nclusters in range(2,6): # search between 2 and 5 clusters/segments
    kmeans = KMeans(n_clusters = nclusters, n_init = 25, random_state = 13)
    kmeans.fit(demog_cluster_data)
    segment = kmeans.predict(demog_cluster_data)  # cluster ids for variables
    tmp = metrics.silhouette_score(demog_cluster_data, segment, metric='euclidean')
    measures_list.append(tmp)
    print('nclusters: %i, silhouette coefficient: %f' %(nclusters,tmp))

max_score = max(measures_list)
best_n = measures_list.index(max_score) + 2
print ('')
print ("highest silhouette score is %f for nclusters = %i" %(max_score, best_n))
print ('')


print('')
print('----- Solution for Best N Clusters -----')
print('')

best_cluster_data = pd.DataFrame(preprocessing.scale(demog_data))
best_cluster_data.columns = demog_var
kmeans = KMeans(n_clusters = n_demo_clusters, n_init = 25, random_state = 13)
kmeans.fit(best_cluster_data)
segment = kmeans.predict(best_cluster_data)  # cluster index

# transpose the data for use in the cluster ananlysis anp
# print variable by cluster
kmeans.fit(rotate_demog)
cluster = kmeans.predict(rotate_demog)
demog_kmeans_res = pd.DataFrame(OrderedDict([('cluster', cluster),('variable', demog_var)]))

# show results
print ('')
print ('cluster membership by variable for K-Means time use')
for cluster_id in sorted(demog_kmeans_res.cluster.unique()):
    tmp = (demog_kmeans_res.loc[demog_kmeans_res['cluster']==cluster_id])
    tmp = tmp.to_string(index=False)
    print tmp
print('')

# create pandas DataFrame for summarizing the cluster analysis results
# using OrderedDict to preserve the order of column names
demog_kmeans_res = pd.DataFrame(OrderedDict([('observation',
    range(0,len(best_cluster_data))),('segment', segment)]))


# merge/join the segment information with the original student data
demog_segmentation_data = demog_kmeans_res.join(best_cluster_data)

# evaluate the clustering solution using the silhouette coefficient
print('Highest Silhouette coefficient for the k-means solution: %f'%(metrics.silhouette_score(best_cluster_data, segment, metric = 'euclidean')))
print ('')

# try printing the means for attributes within each segment
print ('\n')
print ('Attributes - K-Means')
for segment_id in sorted(demog_segmentation_data.segment.unique()):
    print('Attribute means for segment: %i' %segment_id)
    this_demog_segment_data = demog_segmentation_data[demog_segmentation_data.segment == segment_id]
    attributes = this_demog_segment_data.loc[:,'group':'country'].mean()
    print(attributes.values)
    print ('\n')


#------------------------------------------------------------------------------
# computing the hierarchical clusters - demographics
#------------------------------------------------------------------------------
best_cluster_data = pd.DataFrame(preprocessing.scale(demog_data))
best_cluster_data.columns = demog_var

variable_cluster_data = best_cluster_data.T

hierarch = AgglomerativeClustering(n_clusters=n_demo_clusters, affinity='euclidean',
                                   compute_full_tree='auto', linkage='ward')

segment = hierarch.fit_predict(best_cluster_data)  # cluster index



# create pandas DataFrame for summarizing the cluster analysis results
# using OrderedDict to preserve the order of column names
demog_hierarch_res = pd.DataFrame(OrderedDict([('cluster', cluster),
                ('variable', demog_var)]))

# show results
print ('')
print ('cluster membership by variable for Hierarchy time use')
for cluster_id in sorted(demog_hierarch_res.cluster.unique()):
    tmp = (demog_hierarch_res.loc[demog_hierarch_res['cluster']==cluster_id])
    tmp = tmp.to_string(index=False)
    print tmp
print('')


# to interpret the results of the segmentation
# we can review the original ratings data for the six clusters/segments
# merge/join the segment information with the original student data
demog_segmentation_data = demog_hierarch_res.join(best_cluster_data)

# try printing the means for attributes within each segment
print('Attribute info - Hierarchical clusters')
for cluster_id in sorted(demog_segmentation_data.cluster.unique()):
    print('Attribute means for segment: %i' %segment_id)
    this_demog_cluster_data = demog_segmentation_data[demog_segmentation_data.cluster == cluster_id]
    attributes = this_demog_cluster_data.loc[:,'group':'country'].mean()
    print(attributes.values)
    print ('\n')


# evaluate the clustering solution using the silhouette coefficient
print ('\n')
print('Silhouette coefficient for the hierarchy solution: %f '
      %(metrics.silhouette_score(best_cluster_data, segment, metric = 'euclidean')))
print ('\n')


##------------------------------------------------------------------------------
## create the dendrogram
##------------------------------------------------------------------------------

# compute the full tree with all observations
ward = AgglomerativeClustering(linkage='ward',
    n_clusters = variable_cluster_data.shape[0],
    compute_full_tree = True)
ward_full_tree = ward.fit(variable_cluster_data)

# use variable names for the cluster labels
cluster_labels = map(lambda x: demog_data.columns[x], ward_full_tree.labels_)

# plot the full hierarchical tree with variable labels
plot_dendrogram(ward_full_tree, labels = cluster_labels,leaf_rotation=90.)
plt.show()

# using n_demo_clusters from silhouette metrics as number of clusters
ward_n_demo_clusters = AgglomerativeClustering(linkage='ward',
    n_clusters = n_demo_clusters,
    compute_full_tree = False)
ward_n_demo_clusters_tree = ward_n_demo_clusters.fit(variable_cluster_data)

# get cluster ids for variables
ward_n_demo_clusters_cluster = ward_n_demo_clusters.fit_predict(variable_cluster_data)

# create pandas DataFrame for summarizing the cluster analysis results
variable_ward_solution = \
    pd.DataFrame(OrderedDict([('cluster', ward_n_demo_clusters_cluster),('variable', demog_var)]))

# print results of variable clustering one cluster at a time
# BUG
# =============================================================================
# there is some sort of bug here, the clusters printed out by this routinue
# don't match the ones iterated "by hand"
# =============================================================================
print ('cluster membership by variable for hierarchy cluster by demographic variables')
for cluster_id in sorted(set(variable_ward_solution.cluster)):
    tmp = (variable_ward_solution.loc[variable_ward_solution['cluster'] == \
        cluster_id])
    tmp = tmp.to_string(index = False)
    print tmp
print ('')

