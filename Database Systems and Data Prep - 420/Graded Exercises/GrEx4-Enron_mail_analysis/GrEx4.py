#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Sun May 14 16:53:39 2017
@author: tamara_williams

NOTE: to be able to checkin the Enron full data, I had to split the JSON into 3 files and compress them
to run this, you will have to un-do that and make one big file again.
"""
import pandas as pd
import json
import sys
import re
import pprint
import logging

logging.basicConfig(filename='enron.log',filemode='w',level=logging.DEBUG)
pp = pprint.PrettyPrinter(indent=4)

# Start with a JSON file dowloaded from SSCC ES server, via the sample code
# provided by instructor.  That code not shown here.  Open the file and load 
# contents to a dict object
mail_list = []
with open('enron_data_5000.json') as input_file:
#with open('enron_data.json') as input_file:
    j_dict =json.load(input_file)

# initialize var to track key errors, not all mail has "to" (as in to:"undisclosed
# recipents") and a few are missing headers
#key_error_count = 0

# iterate over the messages, extracting the relevant parts and saving those to 
# a new list for further processing
for msg in j_dict:
#    pprint.pprint(msg)
    try:
        msg_ID = msg['headers']['Message-ID']  
        msg_date = msg['headers']['Date']
        msg_from = msg['headers']['From']
        msg_to_tmp = msg['headers']['To'].split()
        for msg_to in msg_to_tmp:
            mail_list.append({'msg_ID':msg_ID,'address_from':msg_from,'address_to':msg_to.strip(','),'date':msg_date})
    except KeyError as e:
        # Some messages have no "to". For example, those to "undisclosed 
        # recipients". Headers are also sometimes missing.  These situtations 
        # throw KeyErrors; capture to log for evaluation 
        logging.info("KeyError initial loop, missing key %s in msg_ID %s", e, msg_ID)
#        print ("KeyError seen: missing key %s in msg_ID %s" %(e, msg_ID))
#        key_error_count +=1
    except:
        logging.debug("Unexpected error: %s", sys.exc_info()[0])    

df_headers = pd.DataFrame(mail_list)

# Convert the NaN to blanks so RegEx is happy 
df_headers = df_headers.fillna('')

# create 2 empty dataframes, loop over the extracted data, and split mails 
# into the 'to' and 'from' Ken Lay buckets.  RegEx looks for all permutations
# of starts with 'k' and ends with 'lay@', just checking 'lay' matches a 
# 'vampireslayer' you don't want
j = 0
k = 0
df_from_kl = pd.DataFrame(columns=['msg_ID', 'send_alias', 'to_person','date'])
df_to_kl = pd.DataFrame(columns=['msg_ID', 'to_alias', 'from_person'])
for i in range(0, len(df_headers)):
    if re.match(r"^k+\S{0,}lay@", df_headers['address_from'][i]):
        df_from_kl.loc[j] = [df_headers['msg_ID'][i], df_headers['address_from'][i], df_headers['address_to'][i],df_headers['date'][i]]
        j+=1
    if re.match(r"^k+\S{0,}lay@", df_headers['address_to'][i]):  
        df_to_kl.loc[k] = [df_headers['msg_ID'][i], df_headers['address_to'][i], df_headers['address_from'][i]]
        k+=1

# clean the data before counting
df_from_kl.drop_duplicates() 
df_to_kl.drop_duplicates()     

df_from_kl.to_csv('test_from.csv', header=True, sep=',', index_label = None) 
df_to_kl.to_csv('test_to.csv', header=True, sep=',', index_label = None) 

print "Total number of mails to/from Ken Lay =", len(df_to_kl)+len(df_from_kl),'\n'
sent_alias_count = df_from_kl.groupby('send_alias').size()
print "Summary of aliases used to send mail", sent_alias_count,'\n'
to_alias_count = df_to_kl.groupby('to_alias').size()
print "Summary of aliases used to get mail", to_alias_count,'\n'

to_kl_count = len(df_to_kl)
from_kl_count= len(df_from_kl)
print "Number of mails to Ken Lay = ", to_kl_count, '\n'
print "Number of mails from Ken Lay = ", from_kl_count,'\n'

#Group and sort to get the counts by person for sending and recieving
max_mails = df_from_kl.groupby('to_person').size()
max_mails = max_mails.sort_values(ascending=False)
print "most frequent recipient of mail from Ken Lay", max_mails.iloc[0:1], '\n'

max_mails = df_to_kl.groupby('from_person').size()
max_mails = max_mails.sort_values(ascending=False)
print "most frequent sender of mail to Ken Lay", max_mails.iloc[0:1],'\n'

# Extract the date column, and then delete the timezone adjustment which isn't
# recognized by python 2.7 strptime %z formatter
# parse to get datetime obj, and compare to target datetime obj
df_date = pd.DataFrame(df_from_kl['date'])
df_date.to_csv('test_date.csv', header=True, sep=',') 
from dateutil.parser import parse
from datetime import datetime

#clean_date = [] 
before = 0 
after = 0
for i in df_date['date']:
    try:
        tmp = parse(i[0:25])
        if tmp <= datetime(2001,12,1,11,15,59):
            before +=1
        else:
            after +=1
#        clean_date.append(tmp)
    except:
        logging.debug("Unexpected error: %s", sys.exc_info()[0]) 

print "Ken Lay sent %i mails before the filing, and %i mails after the filing \n"% (before,after)

# get the series of all IDs from step 4, the 'to' & 'from" Ken Lay & make list
# then iterate and get the body of message
id_list = (df_from_kl['msg_ID'].append(df_to_kl['msg_ID'])).tolist()
aa_count = 0
for msg in j_dict: 
    try:
        tmp = str(msg['headers']['Message-ID'])
        if tmp in id_list:
            body = str(msg['body'])
            if "Arthur Andersen" in body:
                aa_count +=1
    except KeyError as e:
        logging.info("KeyError Arthur Andersen loop, missing key %s in msg_ID %s", e, msg_ID)
    except:
        logging.debug("Unexpected error: %s", sys.exc_info()[0])        
        
print "Found", aa_count,"mail(s) from or to Ken Lay mentioning Arthur Andersen"

import pickle
df_to_kl.to_pickle('to_KenLay.pkl')
df_from_kl.to_pickle('from_KenLay.pkl')
df_headers.to_pickle('parsed_mail_list.pkl')

# For completeness, let's take a peek at the error log # to see what sort of info is in it
from itertools import islice
     with open('enron.log', 'r') as l:
         head = list(islice(l, 5))
     pprint.pprint(head)