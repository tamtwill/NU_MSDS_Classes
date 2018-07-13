#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat May 12 15:10:10 2018

@author: Tamara Williams

based on work from http://brandonrose.org/clustering
"""
from __future__ import print_function

import pandas as pd
import docx
import nltk
import re
import os
import numpy as np




#For the purposes of this walkthrough, imagine that I have 2 primary lists:
#'dsi_title': the titles of the articles in their rank order
#'dsi_text': the dsi_list of the articles titles to the 'titles' order


working_dir = "/Users/tamtwill/NorthwesternU_MSPA/Classes/Text Analytics - 453/DSI_text_only_version"
chart_dir = "/Users/tamtwill/NorthwesternU_MSPA/Classes/Text Analytics - 453/Charts"
os.chdir(working_dir)
dsi_title = os.listdir(working_dir)
# get rid of the .DS_Store Apple hides in your folders
try:
    dsi_title.remove('.DS_Store')
except:
    print(".DS_Store not found")


stopwords = nltk.corpus.stopwords.words('english')

def fetchDocx(fname):
    doc = docx.Document(fname)
    text= []
    for p in doc.paragraphs:
        text.append(p.text)
    return '\n'.join(text)

article_text = []
# read in doc file(s)
for item in dsi_title:
    dsi_words = fetchDocx(item)
    article_text.append(dsi_words)


# load nltk's SnowballStemmer as variabled 'stemmer'
from nltk.stem.snowball import SnowballStemmer
stemmer = SnowballStemmer("english")

# definition for a tokenizer and stemmer which returns the set of stems in the text that it is passed
def tokenize_and_stem(text):
    # first tokenize by sentence, then by word to ensure that punctuation is caught as it's own token
    tokens = [word for sent in nltk.sent_tokenize(text) for word in nltk.word_tokenize(sent)]
    filtered_tokens = []
    # filter out any tokens not containing letters (e.g., numeric tokens, raw punctuation)
    for token in tokens:
         if re.search('[a-zA-Z]', token):
             if (len(token) > 1) & (token != "'s"):   #only way I can find to get rid of 's and single letter terms 
                 filtered_tokens.append(token)
    stems = [stemmer.stem(t) for t in filtered_tokens]
    return stems


def tokenize_only(text):
    # first tokenize by sentence, then by word to ensure that punctuation is caught as it's own token
    tokens = [word.lower() for sent in nltk.sent_tokenize(text) for word in nltk.word_tokenize(sent)]
    filtered_tokens = []
    # filter out any tokens not containing letters (e.g., numeric tokens, raw punctuation)
    for token in tokens:
       if re.search('[a-zA-Z]', token):
           if (len(token) > 1) & (token != "'s"):
               filtered_tokens.append(token)
    return filtered_tokens


# create short for of the DSI names
dsi_short=[]
for item in dsi_title:
    tmp = item.split('_',1)
    dsi_short.append(tmp[0])


#not super pythonic, no, not at all.
#use extend so it's a big flat list of vocab
totalvocab_stemmed = []
totalvocab_tokenized = []
for i in article_text:
    allwords_stemmed = tokenize_and_stem(i) #for each item in 'synopses', tokenize/stem
    totalvocab_stemmed.extend(allwords_stemmed) #extend the 'totalvocab_stemmed' list
    
    allwords_tokenized = tokenize_only(i)
    totalvocab_tokenized.extend(allwords_tokenized)


vocab_frame = pd.DataFrame({'words': totalvocab_tokenized}, index = totalvocab_stemmed)
print ('there are ' + str(vocab_frame.shape[0]) + ' items in vocab_frame')
print (vocab_frame.head())




from sklearn.feature_extraction.text import TfidfVectorizer

#define vectorizer parameters
#tfidf_vectorizer = TfidfVectorizer(max_df=0.2, max_features=1000,
#                                 min_df=2, stop_words='english',
#                                 use_idf=True, tokenizer=tokenize_and_stem, ngram_range=(1,2))

tfidf_vectorizer = TfidfVectorizer(stop_words='english',tokenizer=tokenize_and_stem )

tfidf_matrix = tfidf_vectorizer.fit_transform(article_text) #fit the vectorizer to article_text list
#tfidf_matrix = tfidf_matrix.astype(int)

print(tfidf_matrix.shape)

terms = tfidf_vectorizer.get_feature_names()

from sklearn.metrics.pairwise import cosine_similarity
dist = 1 - cosine_similarity(tfidf_matrix)
print(dist)

from sklearn.cluster import KMeans
num_clusters = 5
km = KMeans(n_clusters=num_clusters)
km.fit(tfidf_matrix)
clusters = km.labels_.tolist()

articles= {'dsi_doc': dsi_title, 'synopsis':article_text, 'cluster': clusters}
doc_frame = pd.DataFrame.from_dict(articles)
#doc_frame = pd.DataFrame.from_dict(articles, index = ['cluster'], columns = ['dsi_doc', 'synopsis'])
doc_frame['cluster'].value_counts() #number of articles per cluster (clusters from 0 to 4)


grouped = doc_frame['dsi_doc'].groupby(doc_frame['cluster']) #groupby cluster for aggregation purposes

print("Top terms per cluster:")
print()
#sort cluster centers by proximity to centroid
order_centroids = km.cluster_centers_.argsort()[:, ::-1] 

# number of words to get
n_words = 5

for i in range(num_clusters):
    print("Cluster %d words:" % i, end='')
    print (i)
    
    for ind in order_centroids[i, :n_words]:
        #print(' %s' % vocab_frame.loc[terms[ind].split(' ')].values.tolist()[0][0].encode('utf-8', 'ignore'), end=',')
        print(' %s' % vocab_frame.loc[terms[ind].split(' ')].values.tolist()[0][0], end=',')
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
import matplotlib.pyplot as plt


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
cluster_colors = {0: '#1b9e77', 1: '#d95f02', 2: '#7570b3', 3: '#e7298a', 4: '#66a61e'}

#set up cluster names using a dict
cluster_names = {0:  'C0',
                 1:  'C1', 
                 2:  'C2',
                 3:  'C3',
                 4:  'C4'
                 }


#create data frame that has the result of the MDS plus the cluster numbers and titles
df = pd.DataFrame(dict(x=xs, y=ys, label=clusters, title=dsi_short)) 

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
save_to = os.path.join(chart_dir, 'cluster_members_chart.png')
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



#uncomment below to save figure
save_to = os.path.join(chart_dir, 'ward_clusters_chart.png')
#plt.savefig(save_to, dpi=200) #isn't working not sure why


# ------------- LDA ------------------------
# strip any proper names from a text...unfortunately right now this is yanking the first word from a sentence too.
import string

def strip_proppers(text):
    # first tokenize by sentence, then by word to ensure that punctuation is caught as it's own token
    tokens = [word for sent in nltk.sent_tokenize(text) for word in nltk.word_tokenize(sent) if word.islower()]
    return "".join([" "+i if not i.startswith("'") and i not in string.punctuation else i for i in tokens]).strip()



# strip any proper nouns (NNP) or plural proper nouns (NNPS) from a text
from nltk.tag import pos_tag

def strip_proppers_POS(text):
    tagged = pos_tag(text.split()) #use NLTK's part of speech tagger
    non_propernouns = [word for word,pos in tagged if pos != 'NNP' and pos != 'NNPS']
    return non_propernouns

from gensim import corpora, models, similarities 

#remove proper names

preprocess = [strip_proppers(doc) for doc in article_text]

#tokenize
tokenized_text = [tokenize_and_stem(text) for text in preprocess]

#remove stop words
texts = [[word for word in text if word not in stopwords] for text in tokenized_text]

#create a Gensim dictionary from the texts
dictionary = corpora.Dictionary(texts)

#remove extremes (similar to the min/max df step used when creating the tf-idf matrix)
dictionary.filter_extremes(no_below=1, no_above=0.8)

#convert the dictionary to a bag of words corpus for reference
corpus = [dictionary.doc2bow(text) for text in texts]

numtopics=5

lda = models.LdaModel(corpus, num_topics=numtopics, 
                            id2word=dictionary, 
                            update_every=5, 
                            chunksize=10000, 
                            passes=100)


lda.show_topics()


topics_matrix = lda.show_topics(formatted=False, num_words=20)
topics_matrix = np.array(topics_matrix, object)

topic_words = topics_matrix[:,:]
for i in range(numtopics):
    print("topic: ", i)
    tmp = topics_matrix[i,1]
    print([word[0] for word in tmp])
    print()