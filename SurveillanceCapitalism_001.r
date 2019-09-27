# These routines use rdata.table to pull lat,lon and activity state from the json export of Google Location
# from takeout.google.com. You must have a location store of data. Good luck. 9:00 AM 9/27/2019 - RMF
# Adjust setwd() as needed. Microsoft Open R 3.53 (Intel MKL) with data.table 1.12.2 
# setMKLthreads(4) and setDTthreads(0) 

library(data.table)
library(lubridate)
library(rjson)
library(plyr)
library(tidyr)
library(stringi)
fsum <- function(x) {sum(x,na.rm=TRUE)};

doc1 <- fromJSON(file="D:/Politics/Takeout/LocationHistory/LocationHistory.json", method="C")
object.size(doc1)

timestamp <- function(x) {as.list(x$timestampMs)}
timestamps <- as.list(plyr::llply(doc1$locations,timestamp))
timestamps <- rbindlist(timestamps)

latitude <- function(x) {as.list(x$latitudeE7)}
latitudes <- as.list(plyr::llply(doc1$locations,latitude))
latitudes <- rbindlist(latitudes)

longitude <- function(x) {as.list(x$longitudeE7)}
longitudes <- as.list(plyr::llply(doc1$locations,longitude))
longitudes <- rbindlist(longitudes)

# spruce up datageoms
datageoms <- setnames(cbind(timestamps,latitudes,longitudes),c("ts","lat","long"))[order(ts)]
datageoms[,latitude:=as.numeric(lat)/(10^7)]
datageoms[,longitude:=as.numeric(long)/(10^7)]
datageoms[,time:= ymd_hms(as_datetime(as.numeric(substr(ts,1,10))),tz="America/Los_Angeles")]
# datageoms[,time_ymd:= ymd(as.Date(time),tz="America/Los_Angeles")]
datageoms[,year:= year(as_datetime(as.numeric(substr(ts,1,10))))]
datageoms[,month:= month(as_datetime(as.numeric(substr(ts,1,10))))]
datageoms[,day:= day(as_datetime(as.numeric(substr(ts,1,10))))]
datageoms[,hour:= hour(as_datetime(as.numeric(substr(ts,1,10))))]
datageoms[,minute:= minute(as_datetime(as.numeric(substr(ts,1,10))))]
end <- nrow(datageoms)
datageoms[,ms_diff:=as.numeric(datageoms[-1]$ts) - as.numeric(datageoms$ts) ][-end]
datageoms[,sumMin:=round(as.numeric(ms_diff)/(60 * 1000),3)]
write.csv(datageoms,"datageoms.csv",row.names=FALSE)

# for time interval for Google Earth
datageoms[,slatitude:=round(latitude,5)]
datageoms[,slongitude:=round(longitude,5)]
# time_int <- datageoms[time > ymd("2018-04-03"),.N,.(slatitude,slongitude,time,hour,minute)][order(-time,-N)]
# write.csv(time_int, "time_int.csv",row.names= FALSE)

# The mess of pulling out 'activities' from nested list:
cat('

From json file we have:
{
  "locations" : [ {
    "timestampMs" : "1523000340475",
    "latitudeE7" : 487716171,
    "longitudeE7" : -1224894418,
    "accuracy" : 17,
    "altitude" : 8,
    "verticalAccuracy" : 2
  }, {
    "timestampMs" : "1522993931609",
    "latitudeE7" : 487716171,
    "longitudeE7" : -1224894418,
    "accuracy" : 17,
    "altitude" : 8,
    "verticalAccuracy" : 2,
    "activity" : [ {   # nested here problems:
      "timestampMs" : "1522993931486",
      "activity" : [ {
        "type" : "STILL",
        "confidence" : 100
      } ]
    } ]

#put into list
activities[[2]][1:2]
[[1]]
[[1]]$timestampMs
[1] "1522993931486"

[[1]]$activity
[[1]]$activity[[1]]
[[1]]$activity[[1]]$type
[1] "STILL"

[[1]]$activity[[1]]$confidence
[1] 100

# So using:
Reduce(cbind,Map(rbind,Reduce(rbind,Map(rbind,(pull(activities,2,1)))[[1]][1:2]))) :
                     type    confidence
[1,] "1522993931486" "STILL" 100       
')


activity <- function(x) {as.list(x$activity)}
activities <- as.list(plyr::llply(doc1$locations,activity))
# activities_list <- Map(rbind,activities))
# activities_list <- rbindlist(activities)	

rows <- c(1:length(activities))
pull <- function(lis, n, i) lis[[n]][i]
pull1 <- function(x) { Reduce(cbind,Map(rbind,Reduce(rbind,Map(rbind,(pull(activities,x,1)))[[1]][1:2])))}
pull2 <- function(x) {Map(ul,x)}  # not so useful plus forget what function ul is...?? function(x) {unlist(l1[x])}
pull3 <- function(x) {unlist(l1[x])} # This is messy output but fast output

l1 <- as.list(plyr::llply(rows,pull1))
l2 <- plyr::llply(1:length(l1),pull3)
l3 <- data.table(l2)
l4 <- l1[,.(ms=as.numeric(ms),activity=as.character(activity),confidence=as.character(confidence))]
# write.csv(l4,"l4.csv",row.names=FALSE)
l5 <- l3[l2 != "NULL"]

# k[,fsum(stri_count_words(V1))]

# The next routine can take hours to process. I could not dimension the activity block any other way.
# k[,fsum(stri_count_words(V1))] # stri_count_words needed all other word counts too slow
Sys.time()
setDTthreads(0)
k <- l5[,.(matrix(unlist(l2)))]
k1 <- k
end <- k1[,fsum(stri_count_words(V1))]
l <- {};i <- 0;for(i in seq(1,end,3)){l <- rbind(l,cbind(shift(k1[i:(2+i)],n=0,type="shift")))}
fwrite(l,file="j",col.names=FALSE)
j <- fread("j")
setnames(j, c("ms","activity","confidence"))
Sys.time()

# alternative but slower
# k <- l5[,.(matrix(unlist(l2)))]
# end <- k[,fsum(stri_count_words(V1))]
# l <- {};i <- 0;for(i in seq(1,end,3)){l <- rbind(l,as.data.table(t(k[i:(2+i)])))}
# setnames(l, c("ms","activity","confidence")))

fsum <- function(x) {sum(x,na.rm=TRUE)};
end <- nrow(l)
l1 <- j[,ms_diff:=as.numeric(j[-1]$ms) - as.numeric(j$ms) ][-end]
l1[,.(fsum.ms=round(fsum(ms_diff))),.(activity)]
l1[,.(fsum.sec=round(fsum(ms_diff)/(1000))),.(activity)]
l1[,.(fsum.min=round(fsum(ms_diff)/(60 * 1000))),.(activity)]
l1[,.(fsum.hour=round(fsum(ms_diff)/(60 * 60 * 1000))),.(activity)]
l1[,.(fsum.day=round(fsum(ms_diff)/(24 * 60 * 60 * 1000))),.(activity)]

datageoms$ts <- as.numeric(datageoms$ts)
l1$ms <- as.numeric(l1$ms)
l1[,merge:=paste0(round(ms/100000))]
datageoms[,merge:=paste0(round(ts/100000))]

merge(l1,datageoms,by="merge")
merge(l1,datageoms,by="merge")[order(merge,-confidence)][ms_diff != 0,]

## Grouping/Selection Examples:
# not 2014  
j1 <- merge(l1,datageoms,by="merge")[order(ms,-confidence)][ms_diff != 0,]
j1[,.(fsum.hour=round(fsum(as.numeric(ms_diff))/(60 * 60 * 1000))),
.(year,activity)][year != 2014,
dcast(.SD,activity ~ year,value.var="fsum.hour",fun.aggregate=fsum)]

# not 2014 and confidence interval > 50
j1 <- merge(l1,datageoms,by="merge")[order(ms,-confidence)][ms_diff != 0,]
j1[,.(fsum.hour=round(fsum(as.numeric(ms_diff))/(60 * 60 * 1000))),
.(year,confidence,activity)][year != 2014 & confidence > 50,
dcast(.SD,activity ~ year,value.var="fsum.hour",fun.aggregate=fsum)]

# Select from j1 merge with intervals from time periods and periods for activity
j2 <- j1[,.(merge,
	activity,
	confidence,
	minutes.x=round(ms_diff.x/(60 * 1000),3),
	minutes.y=round(ms_diff.y/(60 * 1000),3),
	lat,
	long,
	time,
	year,
	month,
	day,
	hour)]

# mergelist with position more than 25 times recorded;
j2[,.N,.(merge)][N > 25,][order(merge)]

# mergelist with position more than 25 times recorded;
# and remove duplicated lat/lon to
# produce suitably minimized (less than 2500 locations) CSV import for Google Earth Pro
mergelist <- j2[,.N,.(merge)][N > 25,][order(merge)]$merge
j3 <- j2[merge %in% mergelist & !duplicated(paste0(substr(lat,1,6),",",substr(long,1,6))),][order(merge)]
write.csv(j3,"j3.csv",row.names=FALSE)


