# These routines use rdata.table to pull lat and lon only from the kml export of Google Location
# from takeout.google.com. You must have a location store of data. Good luck. 9:00 AM 9/27/2019 - RMF
# Adjust setwd() as needed. Microsoft Open R 3.53 (Intel MKL) with data.table 1.12.2 
# setMKLthreads(4) and setDTthreads(0) 

library(data.table)
library(lubridate)
library(tidyr)
library(XML)

setwd("D:\\Politics\\Bernie\\Takeout-20190915T201442Z-001\\Takeout\\Location History")
p1 <- xmlToList(xmlParse("LocationHistory.kml"))
t1 <- as.data.table(Reduce(rbind,(Reduce(rbind,p1))))
t1 <- t1[-1][,.(Track=as.character(Track))]
t1 
t2 <- setnames(t1[endsWith(Track,"Z"),],"datetime")
t3 <- setnames(t1[!endsWith(Track,"Z"),],"position")
t2 <- t2[,tidyr::separate(t2,datetime,into=c("date","time"),sep="T")]
t2 <- t2[,.(date,time=gsub("Z","",time))]
t3 <- t3[,tidyr::separate(t3,position,into=c("long","lat","alt"),sep=" ")]
t4 <- cbind(t2,t3)
t5 <- t4[ ,.(datetime=ymd_hms(paste0(date," ",time),tz="UTC"),lat,long,alt)]
t5[,datetimePDT:=ymd_hms(datetime,tz="America/Los_Angeles") - hours(7)]
t5[]