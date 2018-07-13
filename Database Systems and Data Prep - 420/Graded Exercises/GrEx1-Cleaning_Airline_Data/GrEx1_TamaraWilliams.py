#!/usr/bin/env python2
# -*- coding: utf-8 -*-

## Graded Exercise 1
## Tamara Williams
## Created on Fri Mar 31 12:14:22 2017

import pandas as pd
import re
import pickle
import shelve

## For each of the given data files (airlines, airports, routes and
## airport_codes), read into a dataframe.  
## Using an empty string to fill missing data.  
## No path variable or relatives paths are used - assumes data is in local path

dfAirlines = pd.read_csv("airlines.dat", sep = ",", names = ("AirlineID",
                    "Name", "Alias", "IATA", "ICAO", "CallSign",
                    "Country", "Active"))
dfAirlines = dfAirlines.fillna('')
print dfAirlines.head(3),"\n"

dfAirports = pd.read_csv("airports.dat", sep = ",", names = ("AirportID", 
                "Name", "City", "Country", "AirportCode", "ICAO", "Lat", 
                "Long", "Alt", "TimeZone", "DST", "Olson_TZ"))
dfAirports = dfAirports.fillna('')
print dfAirports.head(3),"\n"

dfRoutes = pd.read_csv("routes.dat", sep = ",", names = ("Airline", "AirlineID",
        "Src", "SrcID", "Dest", "DestID", "Codeshare", "Stops", "Equip"))
dfRoutes = dfRoutes.fillna('')
print dfRoutes.head(3), "\n"

dfAirportCodes = pd.read_csv("airports_codes.txt", sep = "\t", na_values = '\N')
dfAirportCodes.columns = ["AirportCode", "City_Country", "WorldAreaCode"]
dfAirportCodes.fillna('')
print dfAirportCodes.head(3), "\n"


## For each dataframe above, create a new dataframe with the duplicates
## removed.  Specify the columns to compare on, making sure the 
## ID column is not included.  Compare the before and after dataframe
## lengths to determine the number of dropped rows.

before = len(dfAirlines)
noDupsAL = dfAirlines.drop_duplicates(["Name", "Alias", "IATA", "ICAO","CallSign","Country", "Active"])
after = len(noDupsAL)
numDups = before - after
print "Number of duplicate Airline records =", numDups, "\n"

before = len(dfAirports)
noDupsAP = dfAirports.drop_duplicates(["Name", "City", "Country","AirportCode", "ICAO", "Lat", "Long", "Alt",
                                       "TimeZone", "DST", "Olson_TZ"])
after = len(noDupsAP)
numDups = before - after
print "Number of duplicate Airport records =", numDups, "\n"

before = len(dfRoutes)
noDupsRoute = dfRoutes.drop_duplicates(["Airline", "AirlineID","Src", "SrcID", "Dest", "DestID", "Codeshare",
                                        "Stops", "Equip"])
after = len(noDupsRoute)
numDups = before - after
print "Number of duplicated Route records =", numDups, "\n"

before = len(dfAirportCodes)
noDupsCodes = dfAirportCodes.drop_duplicates(["AirportCode", "City_Country", "WorldAreaCode"])
after = len(noDupsCodes)
numDups = before - after
print "Number of duplicate Airport Code records =", numDups, "\n"



## getting the data types for dataframe member via the df.dtypes command
## and printing the results
airlinesTypes = dfAirlines.dtypes
airportsTypes = dfAirports.dtypes
routesTypes = dfRoutes.dtypes
codesTypes = dfAirportCodes.dtypes
print "Airlines dataframe"
print airlinesTypes, "\n"
print "Airports dataframe"
print airportsTypes,"\n"
print "Routes dataframe"
print routesTypes, "\n"
print "Airport Codes dataframe"
print codesTypes, '\n'



## my definition of a defunct airline is one which has no routes
## get the list of all airline IDs in the airlines.dat file, and compare to the
## airline column in the routes.dat file.  Any airline not in routes.dat is
## considered as defunct.  There are some bogus values in the Route Airline column
## so, handle the NANs and pattern match for a good Airline code,
## count the elements in the noRoutes set to get the number of defunct airlines

allAirlines = dfAirlines['IATA'].unique()
hasRoute = dfRoutes['Airline'].unique()
noRoutes = list(set(allAirlines)- set(hasRoute))
defunct = 0
for rt in noRoutes:
    goodID = re.search('\w\w', rt)
    if goodID:
        defunct += 1
print "There are", defunct, "defunct airlines \n"


## get the list of source airports in the route dataframe, and compare to the 
## list of airpot codes from the airport_codes txt file.  Any source airport
## not in the code list will be counted as a phantom flight.
codeList = list(dfAirportCodes.AirportCode)
srcList = list(dfRoutes.Src)

phantomCount = 0
for i in srcList:
    if i not in codeList:
#        print "missing", i
        phantomCount +=1
print "There are", phantomCount,"flights without a valid starting airport", '\n'

## pickle each of the 4 dataframes used above
## stored in local working directory
print "----------Pickle test - round trip each dataframe-----------", '\n'
dfAirlines.to_pickle('Airlines.pkl')
testPickle = pd.read_pickle('Airlines.pkl')
print testPickle.head(3),"\n"
dfAirports.to_pickle('Airports.pkl')
testPickle = pd.read_pickle('Airports.pkl')
print testPickle.head(3),"\n"
dfRoutes.to_pickle('Routes.pkl')
testPickle = pd.read_pickle('Routes.pkl')
print testPickle.head(3),"\n"
dfAirportCodes.to_pickle('AirportCodes.pkl')
testPickle = pd.read_pickle('AirportCodes.pkl')
print testPickle.head(3),"\n"