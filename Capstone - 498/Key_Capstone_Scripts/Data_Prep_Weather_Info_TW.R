######## NWU Capstone 498
########
######## Table for Four Project team
######## Advisor: Don Wedding 
######## 
######## Author: Tamara Williams

######## This is built using Hunter McGushion's kernal and data as a starting point
######## (https://www.kaggle.com/huntermcgushion/exhaustive-weather-eda-file-overview)
######## specifically, the scraped weather data and the air_store_info_with_nearest_active_station.csv
######## files are from his Kaggle kernal share.
########



# For the sake of good programming hygiene, start with a clean workspace
#-------------------------------------------------------------------
# clear Workspace, then clear console
rm(list=ls())
cat("\014")

# Get location script, and set to working directory
#-------------------------------------------------------------------
working.dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(working.dir)
data.path = "/Users/tamtwill/gitRepo/capstone_raw_data/"
weather.data.path = "/Users/tamtwill/gitRepo/capstone_raw_data/rrv-weather-data/"


# include required packages
#-------------------------------------------------------------------
library(reshape2)
library(dplyr)
library(lubridate)


###############################################################################
######                       Read and prep data block                ##########
###############################################################################

# read the data for the stations closest to restaurants
weather.station.df=read.csv(paste0(weather.data.path,"air_store_info_with_nearest_active_station.csv"),header=T, 
                            stringsAsFactors = FALSE)


# get the data for the master lists of reservations and visits
master.visit.df = read.csv(paste0(working.dir,"/all_visit_list.csv"),header=T)
master.res.df = read.csv(paste0(working.dir, "/all_reservation_list.csv"), header=T)

visit.weather.stations = merge(master.visit.df, weather.station.df)
res.weather.stations = merge(master.res.df, weather.station.df)

# get list of weather stations that map to stores
# also used list to pull relevant csv files into weather data folder
# done by hand, easier than doing it programatically
station.list = unique(sort(weather.station.df$station_id))

full.res.weather.df = data.frame()
full.visit.weather.df = data.frame()
num_stations = length(station.list)

for (i in 1:num_stations) {
  this.station = station.list[i]
  print (this.station)
  station.info = read.csv(paste0(working.dir,'/targeted_weather_data/',this.station,'.csv'))
  station.info[is.na(station.info)] = 0
  # fix up calendar_date format to match what we have in the main DF
  station.info$calendar_date = as.POSIXct(station.info$calendar_date, orders = c('ymd'))
  tmp.res.weather = res.weather.stations[res.weather.stations$station_id  == this.station,]
  # make this POSIX too
  tmp.res.weather$visitDate = as.POSIXct(tmp.res.weather$visitDate)
  tmp.combo.weather = left_join(tmp.res.weather, station.info, by = c('visitDate' = 'calendar_date'))
  full.res.weather.df = rbind (full.res.weather.df, tmp.combo.weather)

  tmp.visit.weather = visit.weather.stations[visit.weather.stations$station_id  == this.station,]
  # make this POSIX too
  tmp.visit.weather$visit_date = as.POSIXct(tmp.visit.weather$visit_date)
  tmp.combo.weather = left_join(tmp.visit.weather, station.info, by = c('visit_date' = 'calendar_date'))
  full.visit.weather.df = bind_rows(full.visit.weather.df, tmp.combo.weather)

}


# save so you can just load directly in the future
write.csv(full.res.weather.df, "all_res_w_weather3.csv", row.names = FALSE)
write.csv(full.visit.weather.df, "all_visits_w_weather3.csv", row.names = FALSE)



