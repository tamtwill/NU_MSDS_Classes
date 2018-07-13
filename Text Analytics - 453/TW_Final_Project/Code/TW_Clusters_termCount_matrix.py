#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat May 12 15:10:10 2018

@author: Tamara Williams

"""
from __future__ import print_function

import pandas as pd
import os
import numpy as np
from matplotlib import pyplot as plt
from sklearn.feature_extraction.text import TfidfTransformer
#import mpld3
#import codecs



#For the purposes of this walkthrough, imagine that I have 2 primary lists:
#'dsi_title': the titles of the articles in their rank order
#'dsi_text': the dsi_list of the articles titles to the 'titles' order


working_dir = "/Users/tamtwill/NorthwesternU_MSPA/Classes/Text Analytics - 453/DSI_text_only_version"
rtv_path = "/Users/tamtwill/NorthwesternU_MSPA/Classes/Text Analytics - 453/TTW Corpus/"
chart_dir = "/Users/tamtwill/NorthwesternU_MSPA/Classes/Text Analytics - 453/Charts"
#rtv_name = 'RTV_input_matrix.csv'
#rtv_name = 'RTV_input_matrix2.csv'
rtv_name = 'RTV_input_matrix7.csv'

os.chdir(working_dir)
dsi_title = os.listdir(working_dir)
# get rid of the .DS_Store Apple hides in your folders
try:
    dsi_title.remove('.DS_Store')
except:
    print(".DS_Store not found")
    dsi_title = os.listdir(working_dir)
# create short for of the DSI names
dsi_short=[]
for item in dsi_title:
    tmp = item.split('_',1)
    dsi_short.append(tmp[0])

# try reading the input matrix directly
rtv_terms = pd.read_csv(rtv_path+rtv_name, sep = ',')  
terms = rtv_terms.iloc[:, 0].tolist()
terms = pd.DataFrame(terms)
doc_names = list(rtv_terms.columns.values)
doc_names.pop(0)

doc_full_names = np.column_stack((doc_names, dsi_title))

# make the matrix transpose
rtv_m = pd.read_csv(rtv_path+rtv_name, sep = ',')  
rtv_matrix = rtv_m.set_index('DSI_num').transpose()
rtv_matrix = rtv_matrix.fillna(0)

## get transposed  matrix is in samples x features order
#rtv_matrix = pd.read_csv(rtv_path+'RTV_input_matrix_T.csv', sep = ',')  
print(rtv_matrix.shape)

feature_matrix = np.array(rtv_matrix)
# if this lines gives errors, go check the input file for spaces, you have to get rid of them
num_matrix = feature_matrix.astype('float')


# create the tf-idf matrix
vect_trans = TfidfTransformer()
tfidf_m = vect_trans.fit_transform(num_matrix)
print(tfidf_m.shape)


from sklearn.metrics.pairwise import cosine_similarity
dist = 1 - cosine_similarity(tfidf_m)
print(dist)

from sklearn.cluster import KMeans
num_clusters = 5
km = KMeans(n_clusters=num_clusters, n_init = 50, random_state = 13, algorithm = 'full')
km.fit(tfidf_m)
clusters = km.labels_.tolist()


articles= {'dsi_doc': dsi_title, 'cluster': clusters}
doc_frame = pd.DataFrame.from_dict(articles)
doc_frame['cluster'].value_counts() #number of articles per cluster (clusters from 0 to 4)


grouped = doc_frame['dsi_doc'].groupby(doc_frame['cluster']) #groupby cluster for aggregation purposes

print("Top terms per cluster:")
print()

#sort cluster centers by proximity to centroid
order_centroids = km.cluster_centers_.argsort()[:, ::-1] 
order_centroids = order_centroids[:,1:]

# number of words to get
n_words = 5

for i in range(num_clusters):
    print("Cluster %d words:" % i, end='')
    print (i)
    
    for ind in order_centroids[i, :n_words]:
        try:
            print(' %s' % terms.loc[ind].values, end=',')
        except:
            print()
    print() #add whitespace
    print() #add whitespace
    
    print("Cluster %d titles:" % i)
    sort_frame = doc_frame.sort_values(['cluster'])
    getmask = sort_frame['cluster'] == i
    tmp = sort_frame[getmask]['dsi_doc']
    
    print(tmp.to_string(index = False))
         
    print() #add whitespace
    print() #add whitespace
    
print()


import os  # for os.path.basename

from sklearn.manifold import MDS

MDS()

# convert two components as we're plotting points in a two-dimensional plane
# "precomputed" because we provide a distance matrix
# we will also specify `random_state` so the plot is reproducible.
mds = MDS(n_components=2, dissimilarity="precomputed", random_state=1)

pos = mds.fit_transform(dist)  # shape (n_components, n_samples)

xs, ys = pos[:, 0], pos[:, 1]
print()
print()


# show word grouping
#set up colors per clusters using a dict
cluster_colors = {0: '#1b9e77', 1: '#d95f02', 2: '#7570b3', 3: '#e7298a', 4: '#66a61e'} ##, 5: '#00FFFF'}

#set up cluster names using a dict
cluster_names = {0:  'C0',
                 1:  'C1', 
                 2:  'C2',
                 3:  'C3',
                 4:  'C4'} ##, 5:  'five'}


#create data frame that has the result of the MDS plus the cluster numbers and titles
df = pd.DataFrame(dict(x=xs, y=ys, label=clusters, title=dsi_title)) 
#df = pd.DataFrame(dict(x=xs, y=ys, label=clusters, title=dsi_short)) 

#group by cluster
groups = df.groupby('label')


# set up plot
fig, ax = plt.subplots(figsize=(17, 9)) # set size
ax.margins(0.05) # Optional, just adds 5% padding to the autoscaling

#iterate through groups to layer the plot
#note that I use the cluster_name and cluster_color dicts with the 'name' lookup to return the appropriate color/label
for name, group in groups:
    ax.plot(group.x, group.y, marker='o', linestyle='', ms=12, 
            label=cluster_names[name], color=cluster_colors[name], 
            mec='none')
    ax.set_aspect('auto')
    ax.tick_params(\
        axis= 'x',         # changes apply to the x-axis
        which='both',      # both major and minor ticks are affected
        bottom=False,      # ticks along the bottom edge are off
        top=False,         # ticks along the top edge are off
        labelbottom=False)
    ax.tick_params(\
        axis= 'y',         # changes apply to the y-axis
        which='both',      # both major and minor ticks are affected
        left=False,        # ticks along the bottom edge are off
        top=False,         # ticks along the top edge are off
        labelleft=False)
    
ax.legend(numpoints=1)  #show legend with only 1 point

#add label in x,y position with the label as the title
for i in range(len(df)):
    ax.text(df.iloc[i]['x'], df.iloc[i]['y'], df.iloc[i]['title'], size=8)  
   
#plt.show() #show the plot

#uncomment the below to save the plot if need be
save_to = os.path.join(chart_dir, 'cluster_members_chart2.png')
#plt.savefig(save_to, dpi=200) #save figure as ward_clusters



# heirarchy clusters
from scipy.cluster.hierarchy import ward, dendrogram

linkage_matrix = ward(dist) #define the linkage_matrix using ward clustering pre-computed distances

fig, ax = plt.subplots(figsize=(5, 10)) # set size
ax = dendrogram(linkage_matrix, orientation="right", labels=dsi_title);

plt.tick_params(\
    axis= 'x',         # changes apply to the x-axis
    which='both',      # both major and minor ticks are affected
    bottom=False,      # ticks along the bottom edge are off
    top=False,         # ticks along the top edge are off
    labelbottom=False,
    labelsize = 10)

#plt.tight_layout() #show plot with tight layout

#uncomment below to save figure
save_to = os.path.join(chart_dir, 'ward_clusters_chart2.png')
#plt.savefig(save_to, dpi=200) #save figure as ward_clusters
