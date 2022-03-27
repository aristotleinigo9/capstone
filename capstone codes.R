## Google Data Analytics Capstone Project
## Data Used: March 2021 to Februar 2022

install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")

library("tidyverse")
library("janitor")
library("lubridate")

df2103<-read.csv("202103-divvy-tripdata.csv")
df2104<-read.csv("202104-divvy-tripdata.csv")
df2105<-read.csv("202105-divvy-tripdata.csv")
df2106<-read.csv("202106-divvy-tripdata.csv")
df2107<-read.csv("202107-divvy-tripdata.csv")
df2108<-read.csv("202108-divvy-tripdata.csv")
df2109<-read.csv("202109-divvy-tripdata.csv")
df2110<-read.csv("202110-divvy-tripdata.csv")
df2111<-read.csv("202111-divvy-tripdata.csv")
df2112<-read.csv("202112-divvy-tripdata.csv")
df2201<-read.csv("202201-divvy-tripdata.csv")
df2202<-read.csv("202202-divvy-tripdata.csv")

glimpse(df2103)
glimpse(df2104)
glimpse(df2105)
glimpse(df2106)
glimpse(df2107)
glimpse(df2108)
glimpse(df2109)
glimpse(df2110)
glimpse(df2111)
glimpse(df2112)
glimpse(df2201)
glimpse(df2202)

## Combining the 12-month data gives 5667986 observations of 13 variables
df<-rbind(df2103,df2104,df2105,df2106,df2107,df2108,df2109,df2110,df2111,df2112,df2201,df2202) 


## This gives 5663369 observations of 13 variables
df <- na.omit(df)
dim(df)

## Check the number of rows and if there are duplicated values under ride_id.
nrow(df)
sum(duplicated(df$ride_id))

## Create nine nore tables.
df$date <- as.Date(df$started_at) 
df$month <- format(as.Date(df$date), "%m")
df$month_name <- format(as.Date(df$date), "%B")
df$day <- format(as.Date(df$date), "%d")
df$year <- format(as.Date(df$date), "%Y")
df$weekday <- format(as.Date(df$date), "%A")
df$start_hour = format(as.POSIXct(df$started_at), "%H")
df$end_hour = format(as.POSIXct(df$ended_at), "%H")
df$season <- ifelse (df$month %in% c('06','07','08'), "Summer",
                     ifelse (df$month %in% c('09','10','11'), "Fall",
                             ifelse (df$month %in% c('12','01','02'), "Winter",
                                     ifelse (df$month %in% c('03','04','05'), "Spring", NA))))


## Create another column ride length in seconds. Then determine the range
## Range is from -3482 sec to 3356649 sec
df$ride_length <- difftime(df$ended_at,df$started_at)
range(df$ride_length)

## Remove less than 60 sec and more than 24 hours or 86400 sec.
## Less than 60 sec means the bike was not used. More than 24 hours means it was stolen.
## Remove blank start and end station names.
df <- df[!(df$start_station_name == ""| df$end_station_name == ""| 
             df$ride_length > 86400 | df$ride_length < 60),]

## Ride length is now between 60 and 86362 sec.
## New dimension of df is now 4569454 rows with 23 columns.
range(df$ride_length)
dim(df)

## Create columns for length of rides in hours and seconds.
df$ride_length_hr <- difftime(df$ended_at,df$started_at, units="hours")
df$ride_length_min <- difftime(df$ended_at,df$started_at, units="mins")

## Order months from March to February and days from Sunday to Saturday.
df$month_name <- ordered(df$month_name, levels=c("March", "April", "May", "June", "July","August","September","October","November","December","January", "February"))
df$day <- ordered(df$day, levels=c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

##Creae dtaframes with ride length's sum and average in minutes.
clean.data.sum.min<-aggregate(df$ride_length_min~df$member_casual+df$rideable_type+df$month_name+df$season+df$weekday+df$start_hour, FUN = sum)
clean.data.avg.min<-aggregate(df$ride_length_min~df$member_casual+df$rideable_type+df$month_name+df$season+df$weekday+df$start_hour, FUN = mean)


##Creae dtaframes with ride length's sum and average in hours.
clean.data.sum.hr<-aggregate(df$ride_length_hr~df$member_casual+df$rideable_type+df$month_name+df$season+df$weekday+df$start_hour, FUN = sum)
clean.data.avg.hr<-aggregate(df$ride_length_hr~df$member_casual+df$rideable_type+df$month_name+df$season+df$weekday+df$start_hour, FUN = mean)

##Save the four tables above as csv files to be used in Tableau.
write.csv(clean.data.sum.min,file="clean.data.sum.min.csv")
write.csv(clean.data.avg.min,file="clean.data.avg.min.csv")
write.csv(clean.data.sum.hr,file="clean.data.sum.hr.csv")
write.csv(clean.data.avg.hr,file="clean.data.avg.hr.csv")

## Create data frames with latitude, longitude, start or end station names, counting the number of rides then saving them as csv files to be use in Tableau.
start_lat_lng_stn<-df %>% count(start_lat,start_lng,member_casual,start_station_name,sort = T)
end_lat_lng_stn<-df %>% count(end_lat,end_lng,member_casual,end_station_name,sort = T)
write.csv(start_lat_lng_stn, file = "start_lat_lng_stn.csv")
write.csv(end_lat_lng_stn, file = "end_lat_lng_stn.csv")


