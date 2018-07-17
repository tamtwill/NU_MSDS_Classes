######## NWU Capstone 498
########
######## Table for Four Project team
######## Advisor: Don Wedding 
########
######## Team: Tom Alig, Sheela Rao, Catherine Tolley, Tamara Williams
######## 
######## Author: Tamara Williams

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

# include required packages
#-------------------------------------------------------------------
library(reshape2)
library(dplyr)
library(lubridate)


###############################################################################
######                       Read and prep data block                ##########
###############################################################################

# get the data
air.info.df=read.csv(paste0(data.path,"air_store_info.csv"),header=T, stringsAsFactors = FALSE)
day.of.week = read.csv(paste0(data.path,"predict_date_info.csv"),header=T, stringsAsFactors = FALSE)

i = 1
new_df = data.frame()
for (id in air.info.df$air_store_id){
  print (id)
  for (i.day in day.of.week$calendar_date){
    print (i.day)
    tmp = cbind(id, i.day)
    colnames(tmp) = c('air_store_id','calendar_date')
    new_df= rbind(new_df, tmp)
    i = i + 1
    print(i)
    next
  }
  next
}

new_df$air_store_id = as.character(new_df$air_store_id)
new_df$calendar_date = as.character(new_df$calendar_date)

new_df = merge(new_df, air.info.df)
write.csv(new_df, "test_visit_step1.csv", row.names = FALSE)

# join day of week and the visit data, delete calendar data
all.info = merge(x = new_df, y = day.of.week)
all.info$visit_date =  mdy(all.info$calendar_date)
all.info = all.info[-1]

# do some date fix up of visit info
tmp = as.character(all.info$visit_date)
air_split = t(as.data.frame(strsplit(tmp,"-")))
all.info$Year=air_split[,1]
all.info$Month=air_split[,2]
all.info$Day=air_split[,3]

rm(air_split)

# get rid of superfluous columns, rename and reorder
all.info = all.info[c(1,8,2,3,4,5,6,7,9,10,11)]

# do some fix up on the area values for the visits, split out Prefecture as first thing before a space
tmp.area = colsplit(all.info$air_area_name," ",c("Prefecture","Zone"))
all.info = cbind(all.info, tmp.area)

write.csv(all.info, "test_visit_step2.csv", row.names = FALSE)

#air.info=read.csv("test_visit_step2.csv",header=T, stringsAsFactors = FALSE)


#-----------------------------------------------------------------------------
#data.path = "/Users/tamtwill/gitRepo/capstone_raw_data/"
weather.data.path = "/Users/tamtwill/gitRepo/capstone_raw_data/rrv-weather-data/"

# read the data for the stations closest to restaurants
weather.station.df=read.csv(paste0(weather.data.path,"air_store_info_with_nearest_active_station.csv"),header=T, 
                            stringsAsFactors = FALSE)


# get the data for the master lists of reservations and visits
#master.visit.df = read.csv(paste0(working.dir,"/test_visit_step2.csv"),header=T)
master.visit.df = all.info


visit.weather.stations = merge(x = master.visit.df, y = weather.station.df)

# get list of weather stations that map to stores
# also used list to pull relevant csv files into weather data folder
# done by hand, easier than doing it programatically
station.list = unique(sort(weather.station.df$station_id))

full.visit.weather.df = data.frame()

for(station in station.list){
  print(station)
  station.info = read.csv(paste0(working.dir,'/targeted_weather_data/',station,'.csv'))
  # fix up calendar_date format to match what we have in the main DF
  station.info$calendar_date = as.Date(station.info$calendar_date, orders = c('ymd'))
  tmp.visit.weather = visit.weather.stations[visit.weather.stations$station_id  == station,]
  
  tmp.combo.weather = merge(x = tmp.visit.weather, y = station.info, by.x =  'visit_date', by.y = 'calendar_date')
  full.visit.weather.df = bind_rows(full.visit.weather.df, tmp.combo.weather)
}

refcols  =  c("air_store_id", "visit_date")
full.visit.weather.df = full.visit.weather.df[, c(refcols, setdiff(names(full.visit.weather.df), refcols))]

# save so you can just load directly in the future
write.csv(full.visit.weather.df, "test_visits_w_weather2.csv", row.names = FALSE)

full.visit.weather.df[is.na(full.visit.weather.df)] = 0
write.csv(full.visit.weather.df, "test_visits_w_weather3.csv", row.names = FALSE)

