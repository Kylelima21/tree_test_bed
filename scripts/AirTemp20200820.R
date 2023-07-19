#Working with complete air temp dataset for TTB
#predicted data from AirTemp20200813.R

library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
#set work directory
setwd("/Users/Nick/Documents/Projects/TreeTestBeds_TTB/Rwork")

#read in the data
T1<-read.csv('AirTemp20200820final.csv', header=TRUE)
head(T1)

#Creating date variables
T1$dttm<-mdy_hm(T1$Date_Time)
T1$date<-date(T1$dttm)
T1$floor_month<-floor_date(T1$dttm, "month")
T1$floor_hour<-floor_date(T1$dttm, "hour")
  
T1$Site<-as.factor(T1$Site)

#remove days to begin June 1, 2017 and end Aug 31, 2019

T1.1 <- T1 %>%
  filter(T1$dttm >= as.Date("2017-06-01 00:00:00") & 
           T1$dttm<= as.Date("2019-08-31 23:59:59"))
  
head(T1.1)
summary(T1.1$Site)
#####################################################################
####################Creating summaries
#making sure there is one value per hour
T2.1<-T1.1 %>%
  group_by(Site, date, floor_month, floor_hour) %>%
  summarize(Temp = mean(Temp))
summary(T2.1$Site)

#summarizing to the day
T2<-T2.1 %>%
  group_by(Site, date, floor_month) %>%
  summarize(Tmean = mean(Temp),
            Tmin = min(Temp),
            Tmax = max(Temp))
#summary(T2$Site)

#summarizing to the month
T3<-T2 %>%
  group_by(Site, floor_month) %>%
  summarize(Tmean = mean(Tmean),
            Tmin = mean(Tmin),
            Tmax = mean(Tmax))
#summary(T3$Site)

##################################################################
#plotting
##################################################################

#plotting all data
ggplot(T1, aes(dttm, Temp))+
  geom_line()+
  facet_wrap(~Site, ncol=1, scales="fixed")+
  labs(x = "Time", y = "Temperature (°C)")

#monthly mean
ggplot(T3, aes(floor_month, Tmean, group=Site))+
  geom_line(aes(colour=Site))+
  labs(x = "Time", y = "Mean Temperature (°C)")

#monthly min
ggplot(T3, aes(floor_month, Tmin, group=Site))+
  geom_line(aes(colour=Site))+
  labs(x = "Time", y = "Min Temperature (°C)")

#monthly max
ggplot(T3, aes(floor_month, Tmean, group=Site))+
  geom_line(aes(colour=Site))+
  labs(x = "Time", y = "Max Temperature (°C)")


#average to the month
T3$month<-month(T3$floor_month)

T4<-T3 %>%
  group_by(Site, month) %>%
  summarize(Tmean = mean(Tmean),
            Tmin = mean(Tmin),
            Tmax = mean(Tmax))

write.csv(T4, "zz_tempOUT.csv", row.names=FALSE)


#want to count hours above 25 degrees
T5<-T1.1 %>%
  filter(Temp >= 25)  %>%
  group_by(Site) %>%
  summarise(Hours25 = length(Site))

rm(T5)


##Ignore Below
#############################
#plot for eNews
names(T1)
T1$TempF<-T1$Temp*9/5+32

T1.C<-T1[T1$Site == "1", ]
#plotting all data
ggplot(T1.C, aes(dttm, TempF))+
  geom_line()+
  geom_hline(yintercept=32, colour="gray")+
  labs(x = "Time", y = "Temperature (°F)")

T3.1<-T3[T3$Site != "2", ]
T3.1$TmeanF<-T3.1$Tmean*9/5+32

ggplot(T3.1, aes(floor_month, TmeanF, group=Site))+
  geom_line(aes(colour=Site),size=1.5)+
  labs(x = "Time", y = "Mean Temperature (°F)")+
  scale_colour_discrete(name = "Site", labels = c("Interior low elevation", 
                                                "Cadillac summit", "Schoodic campus"))



