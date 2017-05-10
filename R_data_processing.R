## Heat Map Data ALL

yellow_april <- read.csv("yelloDataApril.csv", header=TRUE)


heatmap <- subset(yellow_april, select = c(day, Hours))


yellow_may <- read.csv("yelloDataMay.csv", header=TRUE)

heatmap1 <- subset(yellow_may, select = c(day, Hours))


yellow_june <- read.csv("yelloDataJune.csv", header=TRUE)

heatmap2 <- subset(yellow_june, select = c(day, Hours))

heatmap_1 <- bind_rows(heatmap, heatmap1, heatmap2)

write.csv(heatmap_1, "heatmappartial.csv")

yellow_july <- read.csv("yelloDatajuly.csv", header=TRUE)

heatmap3 <- subset(yellow_july, select = c(day, Hours))

gc()
yellow_aug <- read.csv("yelloDataaug.csv", header=TRUE)
heatmap4 <- subset(yellow_aug, select = c(day, Hours))

yellow_sept <- read.csv("yelloDataSep.csv", header=TRUE)

heatmap5 <- subset(yellow_sept, select = c(day, Hours))



install.packages("dplyr")
library(dplyr)
yellowdataall <- bind_rows(heatmap_1, heatmap3, heatmap4, heatmap5)

rm(heatmap_1,heatmap5, heatmap3, heatmap4, yellow_sept)
write.csv(yellowdataall, "heatmap_yellowall.csv")


green_april <- read.csv("greenDataApril.csv", header = TRUE)
green_May <- read.csv("greenDataMay.csv", header = TRUE)
green_June <- read.csv("greenDataJune.csv", header = TRUE)
green_july <- read.csv("greenDataJuly.csv", header = TRUE)
green_aug <- read.csv("greenDataAugust.csv", header = TRUE)
green_sept <- read.csv("greenDataSeptember.csv", header = TRUE)

greendataall <- bind_rows(green_april, green_May, green_June, green_july, green_aug, green_sept)
write.csv(greendataall, "heatmap_greenall.csv")

heatmap_green <- subset(greendataall, select = c(day, Hours))

heatmap_all <- bind_rows(yellowdataall,heatmap_green)

heatmapall <- as.data.frame(table(heatmap_all[ , c("day","Hours")]))


install.packages("jsonlite")
library(jsonlite)

x <- toJSON(unname(split(heatmapall, 1:nrow(heatmapall))))
cat(x)
write(x, "heatmap.json")

## Heat Map End

## Map 


greendataall <- read.csv("heatmap_greenall.csv", header=TRUE)
green_map <- subset(greendataall, select = c(Pickup_longitude, Pickup_latitude,day, Hours))

green_map$Cab <- rep("green",nrow(green_map))


library(jsonlite)

x <- toJSON(unname(split(green_map, 1:nrow(green_map))))
cat(x)
write(x, "greenmap.json")




write.csv(green_map,"green_map.csv")

yellow_april <- read.csv("yelloDataApril.csv", header=TRUE)

yellow_april <- subset(yellow_april, select = c(pickup_longitude, pickup_latitude,day, Hours))

yellow_may <- read.csv("yelloDataMay.csv", header=TRUE)

yellow_may <- subset(yellow_may, select = c(pickup_longitude, pickup_latitude,day, Hours))

yellow_june <- read.csv("yelloDataJune.csv", header=TRUE)

yellow_june <- subset(yellow_june, select = c(pickup_longitude, pickup_latitude,day, Hours))

yellow_july <- read.csv("yelloDatajuly.csv", header=TRUE)

yellow_july <- subset(yellow_july, select = c(pickup_longitude, pickup_latitude,day, Hours))

yellow_aug <- read.csv("yelloDataaug.csv", header=TRUE)
yellow_aug <- subset(yellow_aug, select = c(pickup_longitude, pickup_latitude,day, Hours))

yellow_sept <- read.csv("yelloDataSep.csv", header=TRUE)
yellow_sept <- subset(yellow_sept, select = c(pickup_longitude, pickup_latitude,day, Hours))

yellow_map <- bind_rows(yellow_april, yellow_may, yellow_june, yellow_july, yellow_aug, yellow_sept)

yellow_map$Cab <- rep("yellow",nrow(yellow_map))

write.csv(yellow_map, "yellow_map.csv")


## Last Chart

## Another Line chart
yellow_april <- read.csv("yelloDataApril.csv", header=TRUE)
yellow_april <- subset(yellow_april, select = c(passenger_count,day, Hours))


yellow_may <- read.csv("yelloDataMay.csv", header=TRUE)
yellow_may <- subset(yellow_may, select = c(passenger_count,day, Hours))


yellow_june <- read.csv("yelloDataJune.csv", header=TRUE)
yellow_june <- subset(yellow_june, select = c(passenger_count,day, Hours))

yellow_july <- read.csv("yelloDatajuly.csv", header=TRUE)
yellow_july <- subset(yellow_july, select = c(passenger_count,day, Hours))

yellow_aug <- read.csv("yelloDataaug.csv", header=TRUE)
yellow_aug <- subset(yellow_aug, select = c(passenger_count,day, Hours))

yellow_sept <- read.csv("yelloDataSep.csv", header=TRUE)
yellow_sept <- subset(yellow_sept, select = c(passenger_count,day, Hours))

library(dplyr)
yellow_linechart <- bind_rows(yellow_april, yellow_may, yellow_june, yellow_july, yellow_aug, yellow_sept)

rm(yellowdataall, yellow_april, yellow_aug, yellow_july, yellow_june, yellow_may, yellow_sept)

greenall <- read.csv("heatmap_greenall.csv", header=TRUE)
greenall <- subset(greenall, select = c(Passenger_count,day, Hours))

colnames(greenall)  <- c("passenger_count","day", "Hours")

linechartall <- bind_rows(greenall, yellow_linechart)

#heatmapall <- as.data.frame(table(heatmap_all[ , c("day","Hours")]))


greenall$passengercat <-ifelse(greenall$passenger_count>2,"Group", ifelse(greenall$passenger_count <=2, "Single" ))

dat <- transform(greenall, Rate = ifelse(passenger_count >2, "group", "single" ))

Linechartall <- as.data.frame(table(dat[ , c("Hours", "Rate")]))


#write.csv(yellow_linechart, "yellowlinechart.csv")


## Scatter Plot

yellow_april <- read.csv("yelloDataApril.csv", header=TRUE)
yellow_april <- subset(yellow_april, select = c(trip_distance,fare_amount))

yellow_april$trip_distance <- round(yellow_april$trip_distance)
final <- subset(yellow_april, !duplicated(yellow_april[,1]))

a<-final[order(final$trip_distance),]

a$Cab <- rep("yellow",nrow(a))


greendataall <- read.csv("greenDataApril.csv", header=TRUE)
greendataall <- subset(greendataall, select = c(Trip_distance,Fare_amount))
greendataall$Trip_distance <- round(greendataall$Trip_distance)
final1 <- subset(greendataall, !duplicated(greendataall[,1]))
b<-final1[order(final1$Trip_distance),]

b$Cab <- rep("green",nrow(b))

colnames(b)  <- c("trip_distance","fare_amount", "Cab")
library(dplyr)
scatterall <- bind_rows(a,b)

write.csv(scatterall, "scatterall.csv")

library(jsonlite)

x <- toJSON(unname(split(scatterall, 1:nrow(scatterall))))
cat(x)
write(x, "scatterall.json")

##map
yellow_april <- read.csv("yelloDataApril.csv", header=TRUE)
yellow_april <- subset(yellow_april, select = c(pickup_longitude, pickup_latitude,day, Hours))

x <- toJSON(unname(split(yellow_april, 1:nrow(yellow_april))))
cat(x)
write(x, "map_april.json")

c <- yellow_april[1:10,]


## reverge geo coordinate

c$res <- mapply(FUN = function(lon, lat) { 
  revgeocode(c(lon, lat), output = "more") 
}, 
c$pickup_longitude, c$pickup_latitude
)




result <- do.call(rbind,
                  lapply(1:nrow(yellow_april),
                         function(i)revgeocode(as.numeric(yellow_april[i,5:6]))))

c <- cbind(yellow_april,result)


install.packages("sp")
install.packages("rgdal")
library(sp)
library(rgdal)



## Extracting only one day data...


yellow_april <- read.csv("yellow_tripdata_2014-04.csv", header= TRUE)
install.packages("lubridate")
library(lubridate)
yellow_april$date <- (as.Date(yellow_april$pickup_datetime))



week_data <- yellow_april[yellow_april$date %in% as.Date(c('2014-04-12')),]

week_data$vendor_id<-NULL
week_data$dropoff_datetime <- NULL
week_data$store_and_fwd_flag <-NULL
week_data$rate_code<-NULL
week_data$dropoff_longitude <- NULL
week_data$dropoff_latitude<-NULL
week_data$surcharge<-NULL
week_data$mta_tax<-NULL
week_data$tolls_amount<-NULL
#yelloDataApril$Ehail_fee<-NULL
week_data$total_amount<-NULL
week_data$payment_type<-NULL
week_data$Trip_type<-NULL


week_data$starttime1 <- strptime(x = as.character(week_data$pickup_datetime), format = "%Y-%m-%d %H:%M:%S")

week_data$Hours <- format(as.POSIXct(strptime(week_data$starttime1,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H")


week_data$pickup_datetime <- NULL
week_data$fare_amount <- NULL
week_data$passenger_count <- NULL
week_data$trip_distance <- NULL
week_data$tip_amount <- NULL
week_data$starttime1 <- NULL
week_data$date <- NULL

rm(yellow_april)
gc()

week_data_sample <- week_data[sample(nrow(week_data), 135000), ]

write.csv(week_data_sample, "weekmapdatasample.csv")

library(jsonlite)

x <- toJSON(unname(split(week_data_sample, 1:nrow(week_data_sample))))
cat(x)
write(x, "week-data-sample.json")


## Green Map Data

greenaprildata <- read.csv("greenDataApril.csv", header=TRUE)


install.packages("lubridate")
library(lubridate)
greenaprildata$date <- (as.Date(greenaprildata$lpep_pickup_datetime))



week_greeen_data <- greenaprildata[greenaprildata$date %in% as.Date(c('2014-04-12')),]

week_greeen_data$X   <-NULL
#week_greeen_data$ldropoff_datetime <- NULL
#week_greeen_data$store_and_fwd_flag <-NULL
#week_data$rate_code<-NULL
#week_data$dropoff_longitude <- NULL
#week_data$dropoff_latitude<-NULL
week_greeen_data$Passenger_count<-NULL
week_greeen_data$Trip_distance<-NULL
week_greeen_data$Fare_amount<-NULL
#yelloDataApril$Ehail_fee<-NULL
#week_greeen_data$Tip_amount<-NULL
#week_data$payment_type<-NULL
#week_data$Trip_type<-NULL


week_greeen_data$starttime1 <- strptime(x = as.character(week_greeen_data$lpep_pickup_datetime), format = "%Y-%m-%d %H:%M:%S")

week_greeen_data$Hours <- format(as.POSIXct(strptime(week_greeen_data$starttime1,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H")


week_greeen_data$lpep_pickup_datetime <- NULL
#week_data$fare_amount <- NULL
#week_data$passenger_count <- NULL
#week_data$trip_distance <- NULL
#week_data$tip_amount <- NULL
week_greeen_data$starttime1 <- NULL
week_greeen_data$date <- NULL
week_greeen_data$day <- NULL


week_greeen_data_sample <- week_greeen_data[sample(nrow(week_greeen_data), 14330), ]

week_data_sample$Cab <- rep("yellow", ,nrow(week_data_sample))
week_greeen_data_sample$Cab <- rep("green",nrow(week_greeen_data_sample))


colnames(week_greeen_data_sample)  <- c("pickup_longitude","pickup_latitude", "Hours" , "Cab")

library(dplyr)
mapdata <- bind_rows(week_data_sample, week_greeen_data_sample)


write.csv(mapdata, "mapdata.csv")

x <- toJSON(unname(split(mapdata, 1:nrow(mapdata))))
cat(x)
write(x, "mapdata.json")


## tip data

yellowtipdata <- read.csv("yelloDataApril.csv", header= TRUE)
tipdata <- aggregate(yellowtipdata$tip_amount, by=list(Day=yellowtipdata$day, Hour = yellowtipdata$Hours), FUN=sum)
write.csv(tipdata, "tipdata.csv")
x <- toJSON(unname(split(tipdata, 1:nrow(tipdata))))
cat(x)
write(x, "tipdata.json")


## Line Chart


yellowlinechart <- subset(yellowtipdata, select = c(passenger_count,day, Hours))
dat <- transform(yellowlinechart, Rate = ifelse(passenger_count >2, "group", "single" ))

write.csv(dat , "categorygroupchart.csv")

Linechartday <- as.data.frame(table(dat[ , c("Hours","day", "Rate")]))
write.csv(Linechartday, "Daylinechart.csv")

x <- toJSON(unname(split(Linechartday, 1:nrow(Linechartday))))
cat(x)
write(x, "Linechartday.json")




Linechartday_hour <- as.data.frame(table(dat[ , c("day","Hours", "Rate")]))

write.csv(Linechartday_hour, "Hourdaylinechart.csv")

x <- toJSON(unname(split(Linechartday_hour, 1:nrow(Linechartday_hour))))
cat(x)
write(x, "Hourdaylinechart.json")



install.packages("tidyR")
library(tidyr)
separate(Linechartday, Linechartday$Rate)


my_data3 <- spread(Linechartday, 
                   key = "Rate",
                   value = "Freq"
)


write.csv(my_data3, "Linecharttransform.csv")

x <- toJSON(unname(split(my_data3, 1:nrow(my_data3))))
cat(x)
write(x, "Linecharttransform.json")



set.seed(1)
ages <- floor(runif(20, min = 20, max = 50))
ages

findInterval(ages, c(20, 30, 40))

