#Cleaning up and analyzing the sapling phenology data
rm(list = ls(all = TRUE))    #removes all objects

library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)

#set work directory
setwd("/Users/Nick/Documents/Projects/TreeTestBeds_TTB/Rwork")

#################################################################################
#################################################################################
# 
#read in the data
D1<-read.csv('Sap_PhenoDeathDate_OUT.csv', header=TRUE, check.names = FALSE)
names(D1)
str(D1)

D1$Live<-ifelse(D1$LDDate == "2019-08-19", 1, 0)

#Creating date variables so the below code treats the date like a date
D1$LDDate<-mdy(D1$LDDate)

#Determine the Season of Death
D1$Season<-ifelse(D1$LDDate == "2017-05-11", "Spring 2017",
                        ifelse(D1$LDDate <= "2017-10-31", "Summer 2017",
                        ifelse(D1$LDDate > "2017-10-31" & D1$LDDate < "2018-06-01", "Winter 2017/18",
                        ifelse(D1$LDDate >= "2018-06-01" & D1$LDDate <= "2018-10-31", "Summer 2018",
                        ifelse(D1$LDDate > "2018-10-31" & D1$LDDate < "2019-06-01", "Winter 2018/19", 
                        ifelse(D1$LDDate >="2019-06-06" & D1$LDDate < "2019-08-19", "Summer 2019", "Survived"))))))


D1$DeathSeas<-ifelse(D1$Season == "Summer 2017", "Transplant shock",
                        ifelse(D1$Season == "Winter 2017/18", "Winter",
                        ifelse(D1$Season == "Summer 2018", "Summer",
                        ifelse(D1$Season == "Winter 2018/19", "Winter",
                        ifelse(D1$Season == "Summer 2019", "Summer",
                               "Survived")))))

D1$DeathSeas<-factor(D1$DeathSeas, levels = c("Survived", 
                                          "Transplant shock", "Winter",
                                          "Summer"),
                   ordered=is.ordered(D1$DeathSeas))
#for plotting
ggplot(D1, aes(DeathSeas))+
  geom_bar()

#for plotting by species
ggplot(D1, aes(DeathSeas))+
  geom_bar()+
  facet_wrap(~Species, ncol=4)+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))


#####################################################################
#####################################################################
#####################################################################
#####################################################################
