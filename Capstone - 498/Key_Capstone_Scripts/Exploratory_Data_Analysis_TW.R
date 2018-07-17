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
library(psych)
library(VIM)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(fpp)



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
day.of.week$DoW = factor(day.of.week$day_of_week, levels= c("Sunday", "Monday","Tuesday", "Wednesday", 
                                                        "Thursday", "Friday", "Saturday"))  # to make charts print nicely

join.info=read.csv(paste0(data.path,"store_id_relation.csv"), header = T, stringsAsFactors = FALSE)
submit.only.id=read.csv(paste0(data.path,"sample_submission.csv"),header=T, stringsAsFactors = FALSE)

# count of unique IDs in the various files
air.stores = length(unique(air.info.df$air_store_id))
air.res = length(unique(air.res.df$air_store_id))
hpg.stores = length(unique(hpg.info.df$hpg_store_id))
hpg.res = length(unique(hpg.res.df$hpg_store_id))

# merge the files to produce a dataframe for all reservations
# as expected there are more reservations, than stores, so make sure to handle
# that in the merge
all.hpg.res.data = left_join(hpg.res.df, hpg.info.df, by = c('hpg_store_id'))
all.air.res.data = left_join(air.res.df, air.info.df, by = c('air_store_id'))

hpg.to.air = right_join(join.info, all.hpg.res.data, by = c('hpg_store_id'))
mixed.reservations.df = full_join(all.air.res.data, hpg.to.air, by = c('air_store_id','visit_datetime', "reserve_datetime", "reserve_visitors"))
# in the full merge we could have gotten dups if there are reservations in both systems, so, de-dup
all.reservations.df = unique(mixed.reservations.df)

# count stores with both reservation types 
combo.stores = length(unique(all.reservations.df$air_store_id))
all.air.visit.info = left_join(air.visit.df, air.info.df, by = c('air_store_id'))

# join day of week and the visit data
weekday.df = merge(x = air.visit.df, y = day.of.week, by.x = "visit_date", by.y = "calendar_date")
write.csv(weekday.df, paste0(data.path,"weekday_data.csv")) 

# check all the dates in the range are there
all.dates = seq(as.Date('2016-1-1'), as.Date('2017-4-22'), by=1)
df.dates = as.Date(unique(air.visit.df$visit_date))
setequal(all.dates, df.dates)
all.dates.in.range = as.data.frame(all.dates)
colnames(all.dates.in.range) = ('visit_date')

# get the list of unique store ids
stores=unique(air.visit.df$air_store_id)

# looking for patterns of missing data with VIM
# get the first instance
this.store = air.visit.df %>% filter(air.visit.df$air_store_id == stores[1])
this.store$visit_date = as.Date(this.store$visit_date)
df.visits = right_join(this.store, all.dates.in.range, by ="visit_date" )
df.visits$air_store_id[is.na(df.visits$air_store_id)] =stores[1]
df.collate = df.visits
# now get the whole list
for(i in 1:829){
  print(i)
  this.store=air.visit.df %>% filter(air.visit.df$air_store_id == stores[1])
  this.store$visit_date = as.Date(this.store$visit_date)
  df.visits = right_join(this.store, all.dates.in.range, by ="visit_date" )
  df.visits$air_store_id[is.na(df.visits$air_store_id)] =stores[1]
  df.collate=df.visits
  df.collate = rbind(df.collate, df.visits)
}

aggr_plot = aggr(df.collate, col=c('dodgerblue4','dodgerblue3'), numbers=TRUE, sortVars=TRUE, labels=names(df.collate), cex.axis=1.2, 
                 cex.lab = 1.7, gap=1, ylab=c("Histogram of missing data","Pattern"))


# do some fix up on the dates, may be useful later
ar_split =t(as.data.frame(strsplit(air.visit.df$visit_date,'-')))
air.visit.df$Year=ar_split[,1]
air.visit.df$Month=ar_split[,2]
air.visit.df$Day=ar_split[,3]
air.visit.df$visit_date = as.Date(air.visit.df$visit_date)
rm(ar_split)

# split reservation datetime into separate pieces
all.reservations.df$visitDate = as.Date(all.reservations.df$visit_datetime)
all.reservations.df$visitTime = format(as.POSIXct(all.reservations.df$visit_datetime) ,format = "%H:%M:%S") 
all.reservations.df$resDate = as.Date(all.reservations.df$visit_datetime)
all.reservations.df$resTime = format(as.POSIXct(all.reservations.df$visit_datetime) ,format = "%H:%M:%S") 

#----------- for reservations
all.res.by.split.date = all.reservations.df
all.res.by.split.date$visitDate = as.character(all.res.by.split.date$visitDate)
all.res.by.split.date$resDate = as.character(all.res.by.split.date$resDate)
ar_split =t(as.data.frame(strsplit(all.res.by.split.date$visitDate,'-')))
all.res.by.split.date$vYear=ar_split[,1]
all.res.by.split.date$vMonth=ar_split[,2]
all.res.by.split.date$vDay=ar_split[,3]
ar_split =t(as.data.frame(strsplit(all.res.by.split.date$resDate,'-')))
all.res.by.split.date$rYear=ar_split[,1]
all.res.by.split.date$rMonth=ar_split[,2]
all.res.by.split.date$rDay=ar_split[,3]

# do some fix up on the area values, split out city as first thing before a space
tmp.area = colsplit(all.air.res.data$air_area_name," ",c("City","Neighborhood"))
all.air.res.data = cbind(all.air.res.data, tmp.area)

# do some fix up on the area values, split out city as first thing before a space
tmp.area = colsplit(all.air.visit.info$air_area_name," ",c("City","Neighborhood"))
all.air.visit.info = cbind(all.air.visit.info, tmp.area)

# merge reservations and visits to see how the 2 compare
all.res.agg.by.date = aggregate(reserve_visitors ~ air_store_id + resDate, data = all.reservations.df, FUN = sum)
combo.visits.res = merge(x = air.visit.df, y = all.res.agg.by.date, by = c('air_store_id','visit_date'), by.y = c('air_store_id', 'resDate'))

# save so you can just load directly in the future
write.csv(all.reservations.df, paste0(data.path,"all_reservations_info.csv"))
write.csv(all.air.res.data, paste0(data.path,"all_air_data.csv"))
write.csv(all.air.visit.info, paste0(data.path,"all_air_visit_info.csv"))
write.csv(all.hpg.res.data, paste0(data.path,"all_hpg_data.csv")) 
write.csv(air.visit.df, paste0(data.path,"air_visit_df.csv")) 
write.csv(combo.visits.res, paste0(data.path,"combo_visits_res.csv")) 


# clean up, since R holds everything memory
# rm(hpg.to.air)
# rm(hpg.res.df)
# rm(hpg.info.df)
# rm(air.res.df)
# rm(air.info.df)
# rm(day.of.week)
# rm(join.info)
# rm(mixed.reservations.df)
# rm(tmp.area)
# gc()



#******************************************************************************
######                    Visualize the data graphically             ##########
#******************************************************************************

# let's graph the data to see if there are any linear model concerns
tmp = weekday.df
tmp$date = as.Date(tmp$visit_date)
all.visitors.by.date = aggregate(x= tmp[c("visitors")],FUN = sum, by = list(tmp$date))

qplot(Group.1, visitors, data=all.visitors.by.date, geom="line") +
  geom_line(col='royalblue4') +
  labs(x="Date", y="Total Visitors") +
  ggtitle("Visitor Counts by Date") + 
  theme(aspect.ratio = 1/3) +
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20)) + 
  theme(text = element_text(size = 10)) +
  theme(title = element_text(size = 30))

# Look at the monthly aggregates
visits.by.month = aggregate(x= air.visit.df[c("visitors")],FUN = sum, by = list(air.visit.df$Month))
ggplot(visits.by.month, aes(x = Group.1 ,y = visitors)) + 
  ggtitle('Total Visitors per Month') + 
  geom_col(aes(fill = Group.1)) + labs(x = "Month", y = "Visitors") +
  scale_fill_discrete(name = "Month") + scale_fill_hue(l = 30) +
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20)) + 
  theme(text = element_text(size = 10)) +
  theme(title = element_text(size = 30))

ggplot(air.visit.df, aes(x = Month ,y = visitors)) + 
  ggtitle('Boxplot of Visitors per Month') +
  geom_boxplot(aes(color = Month)) + 
  labs(x = "Month", y = "Visitors") +
  scale_color_hue(l = 30) +
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20)) + 
  theme(text = element_text(size = 10)) +
  theme(title = element_text(size = 30))

# try a log plot for readability
ggplot(air.visit.df,aes(x = Month, y = log(visitors))) + 
  ggtitle('Boxplot of log(Visitors) per Month') + 
  geom_boxplot(aes(color = Month), notch = TRUE) + 
  scale_color_hue(l = 30) +
  labs(x = "Month", y = "log(Visitors)") +
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20)) + 
  theme(text = element_text(size = 10)) +
  theme(title = element_text(size = 30))

#---- reservations
ggplot(all.res.by.split.date, aes(x = vMonth ,y = visitors)) + 
  ggtitle('Boxplot of Visitors per vMonth') +
  geom_boxplot(aes(color = vMonth)) + 
  labs(x = "Month", y = "Visitors") +
  scale_color_hue(l = 30) +
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20)) + 
  theme(text = element_text(size = 10)) +
  theme(title = element_text(size = 30))

ggplot(all.res.by.split.date, aes(x = rMonth ,y = visitors)) + 
  ggtitle('Boxplot of Visitors per rMonth') +
  geom_boxplot(aes(color = rMonth)) + 
  labs(x = "Month", y = "Visitors") +
  scale_color_hue(l = 30) +
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20)) + 
  theme(text = element_text(size = 10)) +
  theme(title = element_text(size = 30))

#-------------------------

# look at the aggregates by day of the week
visits.by.day = aggregate(x = weekday.df[c("visitors")],FUN = sum, by = list(weekday.df$DoW))
ggplot(visits.by.day, aes(x = Group.1 ,y = visitors)) + 
  ggtitle('Plot of Total Visitors by Day of the Week') +
  geom_col(aes(fill = Group.1)) + labs(x = "Day", y = "Visitors") +
  scale_fill_discrete(name = "Day of Week") + scale_fill_hue(l = 30) +
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20)) + 
  theme(text = element_text(size = 10)) +
  theme(legend.text = element_text(size = 15)) +
  theme(title = element_text(size = 30))

# look at the aggregates by holiday
visits.on.holidays = aggregate(x = weekday.df[c("visitors")],FUN = sum, by = list(weekday.df$DoW, weekday.df$holiday_flg))
visits.on.holidays$h_flag = as.factor(visits.on.holidays$Group.2)
ggplot(visits.on.holidays, aes(x = Group.1, y = visitors)) + 
  ggtitle('Plot of Total Visitors Showing Holiday Counts') +
  geom_col(aes(fill = h_flag)) + labs(x = "Day", y = "Total # of Visitors") +
  scale_fill_discrete(name = "Is Holiday", labels = c("No", "Yes"), l=30) + 
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20)) + 
  theme(text = element_text(size = 10)) +
  theme(legend.text = element_text(size = 15)) +
  theme(title = element_text(size = 30))

                      
# reservations by food type plot
reservations.by.food = aggregate(reserve_visitors ~ air_genre_name, data = all.air.res.data, FUN = sum)
ggplot(reservations.by.food, aes(x = air_genre_name, y = reserve_visitors)) + 
  ggtitle('Plot of Total Reservations by Type of Food') + labs(x = 'Food', y = "Total Visits") +
  geom_col(aes(fill = air_genre_name)) + scale_fill_discrete(name = "Type of Food", l = 30) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(axis.text = element_text(size = 10)) + 
  theme(axis.title = element_text(size = 20)) + 
  theme(text = element_text(size = 10)) +
  theme(legend.text = element_text(size = 15)) +
  theme(title = element_text(size = 30))

# visitors by food type plot
visits.by.food = aggregate(visitors ~ air_genre_name, data = all.air.visit.info, FUN = sum)
ggplot(visits.by.food, aes(x = air_genre_name, y = visitors)) + 
  ggtitle('Plot of Total Visitors by Type of Food') + labs(x = 'Food', y = "Total Visits") +
  geom_col(aes(fill = air_genre_name)) + scale_fill_discrete(name = "Type of Food", l = 30) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text = element_text(size = 10)) + 
  theme(axis.title = element_text(size = 20)) + 
  theme(text = element_text(size = 10)) +
  theme(legend.text = element_text(size = 15)) +
  theme(title = element_text(size = 30))


# reservations by city 
reservations.by.city = aggregate(reserve_visitors ~ City, data = all.air.res.data, FUN = sum)
ggplot(reservations.by.city, aes(x = City, y = reserve_visitors)) + 
  ggtitle('Total Reservations by City') + scale_fill_hue(l = 30) +
  geom_col(aes(fill = City)) + labs(x = "City", y = "Total # of Visitors Resevered") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text = element_text(size = 10)) + 
  theme(axis.title = element_text(size = 20)) + 
  theme(text = element_text(size = 10)) +
  theme(legend.text = element_text(size = 15)) +
  theme(title = element_text(size = 30))


# visits by city 
visits.by.city = aggregate(visitors ~ City, data = all.air.visit.info, FUN = sum)
ggplot(visits.by.city, aes(x = City, y = visitors)) + 
  ggtitle('Total Visits by City') + scale_fill_hue(l = 30) +
  geom_col(aes(fill = City)) + labs(x = "City", y = "Total # of Visitors") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text = element_text(size = 10)) + 
  theme(axis.title = element_text(size = 20)) + 
  theme(text = element_text(size = 10)) +
  theme(legend.text = element_text(size = 15)) +
  theme(title = element_text(size = 30))


# reservations by city by neighborhood plot
reservations.by.area = aggregate(reserve_visitors ~ City + Neighborhood, data = all.air.res.data, FUN = sum)
ggplot(reservations.by.area, aes(x = City, y = reserve_visitors)) + 
  ggtitle('Total Reservations by City by Neighborhood') +
  geom_col(aes(fill = Neighborhood)) + labs(x = "Area", y = "Total # of Visitors Resevered") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_hue(l = 30) +
  theme(axis.text = element_text(size = 10)) + 
  theme(axis.title = element_text(size = 18)) + 
  theme(title = element_text(size = 25))



# Visitors versus reservations plot
ggplot(combo.visits.res, aes(x = visit_date, y = visitors)) + 
  ggtitle('Plot of Total vs Reserved Visitors by Day') +
  geom_line(aes(y = visitors, colour = 'Visitors')) + 
  geom_line(aes(y = reserve_visitors, colour = 'Reservations')) +  
  scale_color_manual(name="Guest Type",values=c("skyblue", "royalblue4")) +
  labs(x = "Date", y = "Total # of Visitors") +
  theme(axis.text = element_text(size = 10)) + 
  theme(axis.title = element_text(size = 20)) + 
  theme(text = element_text(size = 10)) +
  theme(legend.text = element_text(size = 15)) +
  theme(title = element_text(size = 30))

# output all these aggregation so use in Tableau so we have consistent looking 
# graphics
write.csv(reservations.by.area, paste0(data.path,"reservations_by_area.csv")) 
write.csv(visits.by.city, paste0(data.path,"visits_by_city.csv")) 
write.csv(reservations.by.city, paste0(data.path,"reservations_by_city.csv")) 
write.csv(visits.by.food, paste0(data.path,"visits_by_food.csv")) 
write.csv(reservations.by.food, paste0(data.path,"reservations_by_food.csv")) 
write.csv(visits.on.holidays, paste0(data.path,"visits_on_holidays.csv")) 
write.csv(visits.by.day, paste0(data.path,"visits_by_day.csv")) 
write.csv(visits.by.month, paste0(data.path,"visits_by_month.csv")) 




# clean-up
rm(visits.on.holidays)
rm(visits.by.day)
rm(visits.by.month)
rm(tmp)
rm(reservations.by.food)
gc()



##############################################################################
#  Run *AFTER* adding weather and metro data
#  data augmentation is handled in a separate file 
##############################################################################

df=read.csv(paste0(data.path,"all_visits_weather_geo_TW.csv"),header=T, stringsAsFactors = TRUE)

# convert factors to numerics
indx = sapply(df, is.factor)
tmp = df
tmp[indx] = lapply(df[indx], function(x) as.numeric(x))
tmp[is.na(tmp)] = 0
df_new = tmp


#-------------------------------------------------------------------
# Make Corrplots
#-------------------------------------------------------------------

cor.tmp = cor(tmp)
corrplot(cor.tmp)
corrplot(cor.tmp,type="lower", order="hclust")
corrplot.mixed(cor.tmp,order = "AOE", upper = "ellipse", lower = "number", upper.col = col4, 
               lower.col = col4,tl.cex = 1.5, cl.cex = 1.5)


# with significance testing
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

cor.sig <- cor.mtest(tmp)
corrplot(cor.tmp, type="lower", order="hclust", p.mat = cor.sig, sig.level = 0.01, tl.cex = .75, cl.cex = .75)

# just against visitors
tmp2 = cor(tmp$visitors,tmp)

