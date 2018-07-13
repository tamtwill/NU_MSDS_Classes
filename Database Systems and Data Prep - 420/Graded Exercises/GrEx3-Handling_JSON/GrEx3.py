# -*- coding: utf-8 -*-

## Graded Exercise 3
## Tamara Williams

import pandas as pd
import json
import gc
import pprint
pp = pprint.PrettyPrinter(indent=4)

##-------- Part the first ---------------

with open('100506.json') as input_file:
    j_dict =json.load(input_file)

# iterate thru the json to get all the Ratings under Review    
unique_keys = set()
item_list = []
for x in j_dict['Reviews']:
    item_list.append(x['Ratings'])
    for rating_key in x['Ratings']:
        unique_keys.add(rating_key)
#print item_list

#pp.pprint unique_keys

df_scores = pd.DataFrame(item_list, columns=unique_keys)

## Convert columns to be numeric, not object in type, otherwise the 'describe' 
## method will return counts not averages
df_scores[['Check in / front desk','Overall','Value','Sleep Quality','Rooms',
    'Location', 'Service', 'Cleanliness', 
    'Business service (e.g., internet access)']]=df_scores[['Check in / front desk',
    'Overall','Value','Sleep Quality','Rooms','Location', 'Service', 
    'Cleanliness', 'Business service (e.g., internet access)']].apply(pd.to_numeric)
#print df_scores

## Copy the original dataframe, get rid of unneeded columns so it can be
## joined to 'scores', resulting it the final frame as specified by the problem
df_origin = pd.DataFrame(j_dict['Reviews'])
df_tmp = df_origin.copy()
df_tmp.drop(df_tmp.columns[[1,2,4,6]], axis=1, inplace=True)
#print df_tmp

df_ratings = df_tmp.join(df_scores)
#print df_ratings

## per problem statement, we are indexing on review name, otherwise I'd never 
## drop ReviewID.  Drop everything except Author, Date and Content to create
## the comments frame
df_comments = df_origin.copy()
df_comments.drop(df_comments.columns[[1,4,5,6]], axis=1, inplace=True)
print df_comments.head()

## Clean up all the intermediate dataframes, since Python's all done in memory
## practice goo hygiene
del df_tmp, df_scores, df_origin
gc.collect()

df_stats = df_ratings.describe()
#print df_stats
df_stats.drop(df_stats.index[[0,2,4,5,6]], inplace=True)
print df_stats

# pickle and save 
df_stats.to_pickle('stats.pkl')
df_ratings.to_pickle('ratings.pkl')
df_ratings.to_csv('ratings.csv', header=True, index=False, encoding='utf-8')
df_comments.to_csv('review_comments.csv', header=True, index=False, encoding='utf-8')

##-------- Part the Second ---------------
import glob
import re

## Get the list of JSON files from a specific directory, here the directory
## 'hotel_files' which is a subdirectory of local

file_list = glob.iglob('./hotel_files/*.json')
row_count = 0

##initialize a clean dataframe
df_hotels = pd.DataFrame()

## iterate over every file in the list from the directory.  Get the JSON blob,
## iterate over it column-wise.  For each row-column cell, run the JSON thru
## RegEx to clean the HTML tags out, then save the to dataframe
for f in file_list:
    with open(f) as input_file:
        try:
            tmp = json.load(input_file)
            col_list = []
            for x in tmp['HotelInfo']:
                value = str(tmp['HotelInfo'][x])
                clean_val = re.sub('<[^>]*>', '', value)
                df_hotels.set_value(row_count, x, clean_val)
        except ValueError:
            print "File", f, "is not a valid JSON file"
    row_count+=1
  

## Save the HotelInfo dataframe
df_hotels.to_csv('hotel_info.csv', header=True, index=False, encoding='utf-8')            
df_hotels.to_pickle('hotels.pkl')
        