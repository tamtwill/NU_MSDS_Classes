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


###############################################################################
######                       Read and prep data block                ##########
###############################################################################

# get the data
air.visit.df=read.csv(paste0(data.path,"air_visit_data.csv"),header=T, stringsAsFactors = FALSE)
air.res.df=read.csv(paste0(data.path,"air_reserve.csv"),header=T, stringsAsFactors = FALSE)
air.info.df=read.csv(paste0(data.path,"air_store_info.csv"),header=T, stringsAsFactors = FALSE)
hpg.res.df=read.csv(paste0(data.path,"hpg_reserve.csv"),header=T, stringsAsFactors = FALSE)
hpg.info.df=read.csv(paste0(data.path,"hpg_store_info.csv"),header=T, stringsAsFactors = FALSE)

day.of.week = read.csv(paste0(data.path,"date_info.csv"),header=T, stringsAsFactors = TRUE)
# join day of week and the visit data
air.visit.df = merge(x = air.visit.df, y = day.of.week, by.x = "visit_date", by.y = "calendar_date")
day.of.week$DoW = factor(day.of.week$day_of_week, levels= c("Sunday", "Monday","Tuesday", "Wednesday", 
                                                        "Thursday", "Friday", "Saturday"))  # to make charts print nicely

#write.csv(weekday.df, paste0(data.path,"weekday_data.csv")) 

join.info=read.csv(paste0(data.path,"store_id_relation.csv"), header = T, stringsAsFactors = FALSE)

# check all the dates in the range are there in the visit data
all.dates = seq(as.Date('2016-1-1'), as.Date('2017-4-22'), by=1)
df.dates = as.Date(unique(air.visit.df$visit_date))
setequal(all.dates, df.dates)
all.dates.in.range = as.data.frame(all.dates)
colnames(all.dates.in.range) = ('visit_date')

# do some date fix up of visit info
air_split =t(as.data.frame(strsplit(air.visit.df$visit_date,'-')))
air.visit.df$Year=air_split[,1]
air.visit.df$Month=air_split[,2]
air.visit.df$Day=air_split[,3]
air.visit.df$visit_date = as.Date(air.visit.df$visit_date)
rm(air_split)

# merge air restuarant info with the visit data
all.air.visit.df = merge(air.visit.df, air.info.df, by = 'air_store_id')

# map hpg reservations to the air store id, then get the info for those reservations
hpg.res.map.it = left_join(hpg.res.df, join.info, by = 'hpg_store_id', all.x = TRUE)
hpg.res.map.it = hpg.res.map.it[complete.cases(hpg.res.map.it), ]
all.hpg.info.res = merge(x = hpg.res.map.it, y = hpg.info.df, by = 'hpg_store_id')
hpg.mapped.res = merge(x = all.hpg.info.res, y = air.info.df, by = c('air_store_id'))

# for each air reservation add in the other data
all.air.info.res = merge(x = air.res.df, y = air.info.df, by.x = 'air_store_id', by.y = 'air_store_id')

# split reservation datetime into separate pieces
all.air.info.res$visit_datetime = as.POSIXct(all.air.info.res$visit_datetime, format = "%Y-%m-%d %H:%M:%S")
all.air.info.res$reserve_datetime = as.POSIXct(all.air.info.res$reserve_datetime, format = "%Y-%m-%d %H:%M:%S")

all.air.info.res$visitDate = format(as.POSIXct((all.air.info.res$visit_datetime)),format = "%Y-%m-%d") 
all.air.info.res$visitTime = format(as.POSIXct((all.air.info.res$visit_datetime)),format = "%H:%M:%S") 

all.air.info.res$resDate = format(as.POSIXct((all.air.info.res$reserve_datetime)),format = "%Y-%m-%d") 
all.air.info.res$resTime = format(as.POSIXct((all.air.info.res$reserve_datetime)),format = "%H:%M:%S") 

# NOTE: the reserve time is when the reservation was made, not when it is for
# visit time is when the people are suposed to show up
all.air.info.res$deltaDays = difftime(all.air.info.res$visit_datetime, all.air.info.res$reserve_datetime, units = c("days"))


# do some fix up on the area values, split out Prefecture as first thing before a space
tmp.area = colsplit(all.air.info.res$air_area_name," ",c("Prefecture","Zone"))
all.air.info.res.Prefecture = cbind(all.air.info.res, tmp.area)

# do some fix up on the area values for the visits, split out Prefecture as first thing before a space
tmp.area = colsplit(all.air.visit.df$air_area_name," ",c("Prefecture","Zone"))
all.air.visit.df = cbind(all.air.visit.df, tmp.area)


# repeat for hpg reservations
# split reservation datetime into separate pieces
all.hpg.info.res$visit_datetime = as.POSIXct(all.hpg.info.res$visit_datetime, format = "%Y-%m-%d %H:%M:%S")
all.hpg.info.res$reserve_datetime = as.POSIXct(all.hpg.info.res$reserve_datetime, format = "%Y-%m-%d %H:%M:%S")

all.hpg.info.res$visitDate = format(as.POSIXct((all.hpg.info.res$visit_datetime)),format = "%Y-%m-%d")
all.hpg.info.res$visitTime = format(as.POSIXct((all.hpg.info.res$visit_datetime)),format = "%H:%M:%S") 

all.hpg.info.res$resDate = format(as.POSIXct((all.hpg.info.res$reserve_datetime)),format = "%Y-%m-%d")
all.hpg.info.res$resTime = format(as.POSIXct((all.hpg.info.res$reserve_datetime)),format = "%H:%M:%S") 

all.hpg.info.res$deltaDays = difftime(all.hpg.info.res$visit_datetime, all.hpg.info.res$reserve_datetime, units = c("days"))


# do some fix up on the area values, split out Prefecture as first thing before a space
tmp.area = colsplit(all.hpg.info.res$hpg_area_name," ",c("Prefecture","Zone"))
all.hpg.info.res.Prefecture = cbind(all.hpg.info.res, tmp.area)

# combine all reservations
master.res.list = bind_rows(all.air.info.res.Prefecture, all.hpg.info.res.Prefecture)

# save so you can just load directly in the future
write.csv(master.res.list, "all_reservation_list.csv", row.names = FALSE)
write.csv(all.air.visit.df, "all_visit_list.csv", row.names = FALSE)



