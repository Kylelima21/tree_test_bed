### Soil Temperature
rm(list = ls(all = TRUE))

library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
#set work directory
setwd("/Users/Nick/Documents/Projects/TreeTestBeds_TTB/Rwork")

#read in the data
T1<-read.csv('SoilTemp20200820.csv', header=TRUE)
head(T1)

T1$Site<-as.factor(T1$Site)
summary(T1$Site)

#Creating date variables
T1$dttm<-mdy_hm(T1$Date_Time)
T1$date<-date(T1$dttm)
T1$floor_month<-floor_date(T1$dttm, "month")
T1$round_date<-round_date(T1$dttm, "hour")

#remove days to begin June 1, 2017 and end Aug 31, 2019

T1.1 <- T1 %>%
  filter(T1$dttm >= as.Date("2017-06-01 00:00:00") & 
           T1$dttm<= as.Date("2019-08-31 23:00:00"))
summary(T1.1$Site)

T2<-T1.1 %>%
  group_by(Site, floor_month, date, round_date) %>%
  summarise(Temp = mean(Temp))
summary(T2$Site)

#summarizing to the month
T3<-T2 %>%
  group_by(Site, floor_month, date) %>%
  summarize(Tmean = mean(Temp),
            Tmin = min(Temp),
            Tmax = max(Temp))
summary(T3$Site)

T4<-T3 %>%
  group_by(Site, floor_month) %>%
  summarise(Tmean = mean(Tmean),
            Tmin = mean(Tmin),
            Tmax = mean(Tmax))

T4$month<-month(T4$floor_month)

T5<- T4 %>%
  group_by(Site, month) %>%
  summarise(Tmean = mean(Tmean),
            Tmin = mean(Tmin),
            Tmax = mean(Tmax))

write.csv(T5, "Soil_Temp_OUT.csv", row.names=FALSE)

##################################################################
#plotting
##################################################################

#plotting all data
ggplot(T2, aes(round_date, Temp))+
  geom_line()+
  facet_wrap(~Site, ncol=1, scales="fixed")+
  labs(x = "Time", y = "Soil temperature (°C)")


##################################################################
#ANOVA
##################################################################
#just foolin' here with potential analyses
A1<-aov(Tmean ~ date + Site, data=T3)
summary(A1)

A2<-aov(Tmean ~ floor_month + Site, data=T4)
summary(A2)


