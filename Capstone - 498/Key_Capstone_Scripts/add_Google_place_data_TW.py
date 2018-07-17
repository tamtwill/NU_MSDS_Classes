#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Apr 27 11:48:06 2018
@author: Tamara Williams

this script takes a file of lat/long pairs as input and queries Google Places for
specified items within the given distance range.  Script based on samples at the
Google developer website.  See the Google Place API for more information: 
https://developers.google.com/places/web-service/intro

"""

import requests
import json
import pandas as pd


key = 'AIzaSyDRc-ORARiw3zE5HYTqdYMFyE0B-FM9M8Y'
url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json'

# read in the locations to query on
df = pd.read_csv('./id_loc.csv', sep = ",")

def build_url_parm(loc):
    print(loc)
    sloc = str(loc)
    pars_subs = 'language=en&location='+sloc+'&radius=500&sensor=false&types=subway_station|train_station&key='+key
    pars_tourist = 'language=en&location='+sloc+'&radius=500&sensor=false&types=museum|zoo&key='+key   
    pars_night = 'language=en&location='+sloc+'&radius=500&sensor=false&types=night_club|movie_theater|shopping_mall|bar&key='+key    
    return [pars_subs, pars_tourist, pars_night]; 
            
    

new_df_list = []

# iterate over the list and build the query for each of the various items
for r, rows in df.iterrows():
    print(r)
    ps, pt, pn = build_url_parm(df.iloc[r].Loc)
    
    rs = requests.get(url=url, params=ps)
    subway_data = json.loads(rs.text)
    
    rt = requests.get(url=url, params=pt)
    tourist_data = json.loads(rt.text)
    
    rn = requests.get(url=url, params=pn)
    nightlife_data = json.loads(rn.text)
    
    i = 0
    subway_stops = []
    while True:
        try:
            tmp_s = (subway_data['results'][i]['name'])
            subway_stops.append(tmp_s)
            i +=1
        except IndexError:
            break     
    stop_set = set(subway_stops)
    stop_count = len(stop_set)
    
    tourist_attractions = []
    j = 0
    while True:
        try:
            tmp_t = (tourist_data['results'][j]['name'])
            tourist_attractions.append(tmp_t)
            j +=1
        except IndexError:
            break     
    attraction_set = set(tourist_attractions)
    attraction_count = len(attraction_set)
    
    
    nightlife = []
    k = 0
    while True:
        try:
            tmp_n = (nightlife_data['results'][k]['name'])
            nightlife.append(tmp_n)
            k +=1
        except IndexError:
            break     
    nightlife_set = set(nightlife)
    nightlife_count = len(nightlife_set)
    
    id = rows.air_store_id
    new_df_row = [id, stop_count, attraction_count, nightlife_count]
    new_df_list.append(new_df_row)

enhanced_data = pd.DataFrame(new_df_list)
enhanced_data.columns = ('air_store_id','subwayStops','touristAct', 'nightLife')
enhanced_data.to_csv("geo_data_TW.csv", header = True, index = False)

weather_visits_df = pd.read_csv('./all_visits_w_weather2.csv', sep = ",")
visits = pd.merge(weather_visits_df, enhanced_data, on = 'air_store_id')
visits.to_csv("all_visits_weather_geo_2.csv", header = True, index = False)

weather_res_df = pd.read_csv('./all_res_w_weather2.csv', sep = ",")
reser = pd.merge(weather_res_df, enhanced_data, on = 'air_store_id')
reser.to_csv("all_res_weather_geo_2.csv", header = True, index = False)