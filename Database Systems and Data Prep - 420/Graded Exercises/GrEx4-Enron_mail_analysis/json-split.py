#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jul 13 12:17:02 2018

@author: Tamara Williams
"""


#!/usr/bin/env python 
# based on  http://stackoverflow.com/questions/7052947/split-95mb-json-array-into-smaller-chunks
# splits the full enron data files into a size Github accepts for check-in

import json

with open('enron_data.json','r') as infile:
    o = json.load(infile)
    chunkSize = 90000
    for i in range(0, len(o), chunkSize):
        with open('enron_data_split' + '_' + str(i//chunkSize) + '.json', 'w') as outfile:
            json.dump(o[i:i+chunkSize], outfile)