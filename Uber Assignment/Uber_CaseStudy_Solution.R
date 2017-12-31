#Load Uber Request Data csv file
uberData <-read.csv("Uber Request Data.csv")
str(uberData)
#checking for NA values
which(is.na(uberData$Request.timestamp))

#loading libraries
library(dplyr)
library(tidyr)
library(ggplot2)


#converting Request.Timestamp into Date format
uberData$RequestTS <- gsub("/","-",uberData$Request.timestamp)
#ignoring secs data
uberData$RequestTS <-strptime(uberData$RequestTS,format="%d-%m-%Y %R")

uberData$RequestTS.date <- as.Date(uberData$RequestTS)
uberData$RequestTS.day <- format(uberData$RequestTS.date, "%d")
uberData$RequestTS.hour <- format(uberData$RequestTS, "%H")

#converting Drop.Timestamp into Date format
uberData$DropTS <- gsub("/","-",uberData$Drop.timestamp)
#ignoring secs data
uberData$DropTS <-strptime(uberData$DropTS,format="%d-%m-%Y %R")

uberData$DropTS.date <- as.Date(uberData$DropTS)
uberData$DropTS.day <- format(uberData$DropTS.date, "%d")
uberData$DropTS.hour <- format(uberData$DropTS, "%H")

#Finding the trip time for completed trips

uberData$triptime <- uberData$DropTS - uberData$RequestTS

write.csv(uberData,"uber_ouput.csv")

#Find the subset of requests which were cancelled or no cars available
uberData_cancelled <- subset(uberData,Status=="Cancelled")
uberData_nocars <- subset(uberData,Status=="No Cars Available")


#All Requests
#plotting graph of requests from pickup point (City, Airport) based on status
ggplot(uberData,aes(factor(Pickup.point),fill=Status))+geom_bar(position="dodge")+labs(x="Pickup Point")
#plotting graph of requests for the dates based on status
ggplot(uberData,aes(factor(RequestTS.date),fill=Status))+geom_bar(position="dodge") +labs(x="Request Date")

ggplot(uberData,aes(x=as.numeric(RequestTS.hour),color=Status)) + geom_line(stat="count") +labs(x="Request Time",title="Request count by time (All Requests)")
ggplot(uberData,aes(x=as.numeric(RequestTS.hour),color=Pickup.point)) + geom_line(stat="count")+labs(x="Request Time",color="Pickup Point",title="Request count by Pickup Point (All Requests)")
ggplot(uberData,aes(x=as.numeric(RequestTS.hour),color=RequestTS.day)) + geom_line(stat="count")+labs(x="Request Time",color="Request Date",title="Request count by time (All Requests)")

ggplot(uberData,aes(x=as.factor(RequestTS.date),fill=Pickup.point)) + geom_bar(position="dodge")

#Histogram of trip time for a binsize of 15 mins to identify which bin has max requests
ggplot(uberData,aes(x=as.numeric(triptime),fill=RequestTS.day))+geom_histogram(binwidth=15,na.rm=T,position="stack",show.legend=TRUE)+geom_freqpoly(binwidth=15,na.rm=T,position="stack")+labs(x="Trip Time(in mins) BinSize:15 mins",fill="Request Date")

#Plots for Drops made
ggplot(na.omit(uberData),aes(x=as.numeric(DropTS.hour),color=Pickup.point)) + geom_line(stat="count")+labs(x="Request Time",color="Pickup Point",title="Drop count by Pickup Point (All Requests)")
ggplot(na.omit(uberData),aes(x=as.numeric(DropTS.hour),color=DropTS.day)) + geom_line(stat="count")+labs(x="Request Time",color="Request Date",title="Request count by time (All Requests)")

#Plots for Cancelled Requests
ggplot(uberData_cancelled,aes(x=as.numeric(RequestTS.hour))) + geom_line(stat="count")+labs(x="Request Time")
ggplot(uberData_cancelled,aes(x=as.numeric(RequestTS.hour),color=Pickup.point)) + geom_line(stat="count")+labs(x="Request Time",color="Pickup Point")
ggplot(uberData_cancelled,aes(x=as.numeric(RequestTS.hour),color=RequestTS.day)) + geom_line(stat="count")+labs(x="Request Time",color="Request Date")

#Plots for No Cars Available Requests
ggplot(uberData_nocars,aes(x=as.numeric(RequestTS.hour))) + geom_line(stat="count")+labs(x="Request Time")
ggplot(uberData_nocars,aes(x=as.numeric(RequestTS.hour),color=Pickup.point)) + geom_line(stat="count")+labs(x="Request Time",color="Pickup Point")
ggplot(uberData_nocars,aes(x=as.numeric(RequestTS.hour),color=RequestTS.day)) + geom_line(stat="count")+labs(x="Request Time",color="Request Date")