#Soil Moisture for Ruth

library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
#set work directory
setwd("C:/Users/rutha/Desktop/TTB/RawOriginalData_Scripts")

#read in the data
T1<-read.csv('SoilH20_OUT_20200823_Ruth.csv', header=TRUE)
head(T1)
names(T1)

#Creating date variables
T1$date<-mdy(T1$date)
T1$floor_month<-floor_date(T1$date, "month")
T1$Year<-year(T1$date)
T1$yday<-yday(T1$date)
T1$month <- month(T1$date)

y7 <- subset(T1, month == 7)
y8 <- subset(T1, month == 8)

ggplot(T1, aes(yday, Scout, group=Site))+
  geom_line(aes(colour=Site))+
  facet_wrap(~Year, ncol=1, scales="fixed")+
  labs(x = "Julian day (Day of year)", y = "Volumetric water content (%)")

ggplot(T1, aes(Site, Scout))+
  geom_boxplot(aes(colour=Site), outlier.shape=NA)+
  facet_wrap(~Year, ncol=1, scales="fixed")+
  labs(x = "Site", y = "Volumetric water content (%)")

## this is for july only
ggplot(y7, aes(yday, Scout, group=Site))+
  geom_line(aes(colour=Site))+
  facet_wrap(~Year, ncol=1, scales="fixed")+
  labs(x = "Julian day (Day of year)", y = "Volumetric water content (%)")

ggplot(y7, aes(Site, Scout))+
  geom_boxplot(aes(colour=Site), outlier.shape=NA)+
  facet_wrap(~Year, ncol=1, scales="fixed")+
  labs(x = "Site", y = "Volumetric water content (%)")

## this is for august only
ggplot(y8, aes(yday, Scout, group=Site))+
  geom_line(aes(colour=Site))+
  facet_wrap(~Year, ncol=1, scales="fixed")+
  labs(x = "Julian day (Day of year)", y = "Volumetric water content (%)")

ggplot(y8, aes(Site, Scout))+
  geom_boxplot(aes(colour=Site), outlier.shape=NA)+
  facet_wrap(~Year, ncol=1, scales="fixed")+
  labs(x = "Site", y = "Volumetric water content (%)")

#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#ANOVA test to compare sites
A1<-aov(Scout~Site, data=T1)
summary(A1)
TukeyHSD(A1)

A2<-aov(Scout~Site+Year, data=T1)
summary(A2)

A3<-aov(Scout~Site*Year, data=T1)
summary(A3)


