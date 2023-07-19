### Light
rm(list = ls(all = TRUE))

library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
#set work directory
setwd("/Users/Nick/Documents/Projects/TreeTestBeds_TTB/Rwork")

#read in the data
T1<-read.csv('Light20200821.csv', header=TRUE)
head(T1)

T1$Site<-as.factor(T1$Site)
summary(T1$Site)

#Creating date variables
T1$dttm<-mdy_hm(T1$Date_Time)
T1$date<-date(T1$dttm)
T1$floor_month<-floor_date(T1$dttm, "month")
T1$round_date<-round_date(T1$dttm, "hour")


T2<- T1 %>%
  group_by(Site, date) %>%
  summarise(Lux = sum(Lux))
####################################################################


####################################################################
#just overlapping dates in 2018
T3 <- T1 %>%
  filter(dttm >= as.Date("2018-05-08 00:00:00") & 
           dttm <= as.Date("2018-07-02 23:00:00"))
summary(T3$Site)

T3<-arrange(T3, Site, dttm)

T3.1<- T3 %>%
  group_by(Site, date) %>%
  summarise(Lux = sum(Lux))

T3.2<- T3.1 %>%
  filter(date < as.Date("2018-07-02"))

#################################################
#Overlapping dates in 2017 growing season
T3.17a <- T1 %>%
  filter(dttm >= as.Date("2017-06-01 00:00:00") & 
           dttm <= as.Date("2017-09-01 23:00:00"))

T3.17a<-arrange(T3.17a, Site, dttm)

T3.17b<- T3.17a %>%
  group_by(Site, date) %>%
  summarise(Lux = sum(Lux))

T3.17c<- T3.17b %>%
  filter(date < as.Date("2017-09-01"))

ggplot(T3.17c, aes(date, Lux))+
  geom_line()+
  facet_wrap(~Site, ncol=1, scales="fixed")+
  labs(x = "Time", y = "Light (Lux)")

ggplot(T3.17c, aes(Site, Lux)) +
  geom_boxplot(outlier.shape=NA) 


##################################################################
#plotting
##################################################################

#plotting all data
ggplot(T2, aes(date, Lux))+
  geom_line()+
  facet_wrap(~Site, ncol=1, scales="fixed")+
  labs(x = "Time", y = "Light (Lux)")

#plotting all data
ggplot(T3.2, aes(date, Lux))+
  geom_line()+
  facet_wrap(~Site, ncol=1, scales="fixed")+
  labs(x = "Time", y = "Light (Lux)")

ggplot(T3.2, aes(Site, Lux)) +
  geom_boxplot(outlier.shape=NA) +
  coord_cartesian(ylim = c(0, 1800000))



##################################################################
#summary statistics
##################################################################
T.s<-rbind(T3.17c,T3.1)
T.s$Lux.H<-T.s$Lux/13
summary(T.s$Lux.H)
str(T.s)

T.s2<- T.s %>%
  group_by(Site) %>%
  summarise(Lux = mean(Lux.H),
            sd.Lux=sd(Lux.H),
            se.Lux=sd(Lux.H)/sqrt(length(Lux.H)))

A1<-aov(Lux.H~date+Site, data=T.s)
summary(A1)

TukeyHSD(A1,which="Site",conf.level=0.95)



