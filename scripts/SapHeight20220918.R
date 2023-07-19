###Sapling Total length Data

#library(plyr)
library(ggplot2)
library(tidyr)
library(dplyr)

#set work directory
setwd("C:/Users/rutha/Desktop/TTB/RawOriginalData_Scripts")

#read in the data
T1<-read.csv('SaplingHT_20200728.csv', header=TRUE)
T1<-T1[1:384,1:17]
names(T1)
head(T1)
str(T1)

#rename columns for plotting
names(T1)[names(T1)=="Length1"] <- "Spring 2017"
names(T1)[names(T1)=="Length2"] <- "Fall 2017"
names(T1)[names(T1)=="Length3"] <- "Spring 2018"
names(T1)[names(T1)=="Length4"] <- "Fall 2018"
names(T1)[names(T1)=="Length5"] <- "Spring 2019"
names(T1)[names(T1)=="Length6"] <- "Fall 2019"


### Reshape the data to long
T2<-reshape(T1, varying=c("Spring 2017", "Fall 2017",
                          "Spring 2018", "Fall 2018",
                          "Spring 2019", "Fall 2019"),
            idvar="Sapling", times=c("Spring 2017", "Fall 2017",
                                     "Spring 2018", "Fall 2018",
                                     "Spring 2019", "Fall 2019"),
            timevar="Season", v.names="Length",
            direction='long')
head(T2)
names(T2)
T2<-T2[ ,c(1:3,12,13)]

###Order factor levels
T2$Season<-factor(T2$Season, levels=c("Spring 2017", "Fall 2017",
                                      "Spring 2018", "Fall 2018",
                                      "Spring 2019", "Fall 2019"),
                  ordered=is.ordered(T2$Season))
#Site as factor
T2$Site<-as.factor(T2$Site)

colnames(T2) <- c("site", "species", "sapling", "season", "length")
T2$species <- as.factor(T2$species)
T2$sapling <- as.factor(T2$sapling)

nums <- data.frame(season = c("Spring 2017", "Fall 2017", 
                       "Spring 2018", "Fall 2018", 
                       "Spring 2019", "Fall 2019"), 
                   num = c(1:6))

T2 <- merge(T2, nums, by = "season")

cols <- data.frame(site = c("1", "2", "3", "4"), 
                   col = c(1:4))

T2 <- merge(T2, cols, by = "site")


height.rel <- data.frame()
for(i in unique(levels(T2$species))){
  # i = "robps"
  temp <- subset(T2, species == i)
  # plot(temp$season, temp$length, lty = 0, col = "white", 
  #      xlab = "Season", ylab = "Height Growth (cm)", pch = NA, 
  #      main = i, ylim = c(min(temp$length, na.rm = T)-10, 
  #                         max(temp$length, na.rm = T)+10))
  # 
  # legend("topleft", 
  #        pch = 1, 
  #        col = c(1:4), 
  #        title = "Site", 
  #        legend = c("1", "2", "3", "4"), 
  #        bty = "n",
  #        cex = 1)
  
  for(j in unique(levels(temp$sapling))){
    # j = "1-F2 robps"
    temp2 <- subset(temp, sapling == j)
    temp2 <- temp2[order(temp2$num),]
    
    first <- temp2$length[1]
    
    # temp2$rel.h <- ((temp2$length-first)/temp2$length)*100
    temp2$h.growth <- temp2$length-first
    temp2 <- na.omit(temp2)
    
    # points(data = temp2, h.growth ~ num, col = temp2$col[1], 
    #        type = "b")
    
    height.rel <- rbind(height.rel, temp2)
  }
}

sum <- data.frame()
for(i in unique(levels(height.rel$site))){
  # i = "1"
  temp <- subset(height.rel, site == i)
  for(j in unique(levels(temp$species))){
    # j = "aceru"
    temp2 <- subset(temp, species == j)
    for(k in unique(levels(temp2$season))){
      # k = "Spring 2017"
      temp3 <- subset(temp2, season == k)
      
      mean <- mean(temp3$h.growth, na.rm = T)
      se <- sd(temp3$h.growth, na.rm = T)/sqrt(length(temp3$h.growth))
      n <- length(temp3$h.growth)
      
      num = temp3$num[1]
      col = temp3$col[1]
      
      site = i
      species = j
      season = k
      
      temp.out <- data.frame(site, species, season, mean, se, n, num, col)
      
      sum <- rbind(sum, temp.out)
    }
  }
}

sum$site <- as.factor(sum$site)
sum$species <- as.factor(sum$species)

# sum[order(names(sum)), ]

# need to do a matrix thing here.. 
setwd("C:\\Users\\rutha\\Desktop\\TTB\\Plots")
today = as.character(format(Sys.time(), "%Y-%m-%d"))
png(file = paste(paste("height", today, sep = "_"), ".png", sep = ""),
    width = 11, height = 8.5, units = "in", res = 1200)

layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 4, 4))
layout.show(n = 16)# creating matrix in which to insert plots
par(oma = c(4, 4.5, 2, 4.5), mar = c(1, 0.75, 0.75, 0.75), mgp = c(3, 1, 0))

for(i in unique(levels(sum$species))){
  # i = "robps"
  temp <- subset(sum, species == i)
  plot(data = temp, mean ~ num, lty = 0, col = "white", 
       xlab = "Season", ylab = "Height Growth (cm)", 
       pch = NA, ylim = c(-50, 50), yaxt = "n", xaxt = "n")
  abline(h = 0, lty = 2)
  if(i == "pinta"){
    legend("topleft", 
           title = expression("(G)" ~ italic("P. taeda")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(-50, 50, 10), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "pinvi"){
    legend("topleft", 
           title = expression("(K)" ~ italic("P. virginiana")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(-50, 50, 10), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "pinst"){
    legend("topleft", 
           title = expression("(C)" ~ italic("P. strobus")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(-50, 50, 10), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "pruse"){
    legend("topleft", 
           title = expression("(O)" ~ italic("Pr. serotina")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(-50, 50, 10), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "abiba"){
    legend("topleft", 
           title = expression("(A)" ~ italic("A. balsamea")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(-50, 50, 10), labels = T, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "pinri"){
    legend("topleft", 
           title = expression("(N)" ~ italic("Pi. rigida")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(-50, 50, 10), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "quepr"){
    legend("topleft", 
           title = expression("(H)" ~ italic("Q. prinus")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(-50, 50, 10), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "picgl"){
    legend("topleft", 
           title = expression("(B)" ~ italic("Pi. glauca")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(-50, 50, 10), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "junvi"){
    legend("topleft", 
           title = expression("(I)" ~ italic("J. virginiana")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(-50, 50, 10), labels = T, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "picru"){
    legend("topleft", 
           title = expression("(F)" ~ italic("Pi. rubens")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(-50, 50, 10), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "robps"){
    legend("topleft", 
           title = expression("(P)" ~ italic("R. pseudoacacia")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(-50, 50, 10), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
    legend("bottomright", 
           col = c(1:4),
           pch = 1,
           legend = c("1", "2", "3", "4"),
           title = "Site", 
           bty = "n",
           cex = 1)
  }
  if(i == "queal"){
    legend("topleft", 
           title = expression("(D)" ~ italic("Q. alba")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(-50, 50, 10), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "aceru"){
    legend("topleft", 
           title = expression("(E)" ~ italic("Ac. rubrum")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(-50, 50, 10), labels = T, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "queru"){
    legend("topleft", 
           title = expression("(L)" ~ italic("Q. rubra")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(-50, 50, 10), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "pinba"){
    legend("topleft", 
           title = expression("(J)" ~ italic("Pi. banksiana")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(-50, 50, 10), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "lirtu"){
    legend("topleft", 
           title = expression("(M)" ~ italic("L. tulipifera")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(-50, 50, 10), labels = T, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "pinri"){
    xtick<-seq(1, 6, by=1)
    axis(side=1, at=xtick, labels = FALSE)
    text(x=xtick,  par("usr")[3], 
         labels = c("Spring 2017", "Fall 2017", 
                    "Spring 2018", "Fall 2018", 
                    "Spring 2019", "Fall 2019"), 
         srt = 0, pos = 1, xpd = TRUE, cex = 0.5)
  }
  if(i == "pruse"){
    xtick<-seq(1, 6, by=1)
    axis(side=1, at=xtick, labels = FALSE)
    text(x=xtick,  par("usr")[3], 
         labels = c("Spring 2017", "Fall 2017", 
                    "Spring 2018", "Fall 2018", 
                    "Spring 2019", "Fall 2019"), 
         srt = 0, pos = 1, xpd = TRUE, cex = 0.5)
  }
  if(i == "robps"){
    xtick<-seq(1, 6, by=1)
    axis(side=1, at=xtick, labels = FALSE)
    text(x=xtick,  par("usr")[3], 
         labels = c("Spring 2017", "Fall 2017", 
                    "Spring 2018", "Fall 2018", 
                    "Spring 2019", "Fall 2019"), 
         srt = 0, pos = 1, xpd = TRUE, cex = 0.5)
  }
  if(i == "lirtu"){
    xtick<-seq(1, 6, by=1)
    axis(side=1, at=xtick, labels = FALSE)
    text(x=xtick,  par("usr")[3], 
         labels = c("Spring 2017", "Fall 2017", 
                    "Spring 2018", "Fall 2018", 
                    "Spring 2019", "Fall 2019"), 
         srt = 0, pos = 1, xpd = TRUE, cex = 0.5)
  }
  for(j in unique(levels(temp$site))){
    # j = "4"
    temp2 <- subset(temp, site == j)
    temp2 <- temp2[order(temp2$num),]
    
    points(data = temp2, mean ~ num, col = temp2$col[1], 
           type = "b")
    arrows(temp2$num, temp2$mean-temp2$se, 
           temp2$num, temp2$mean+temp2$se, 
           length=0.05, angle=90, code=3, 
           col = temp2$col[1], cex = 1.3)
  }
}
mtext("Season", side = 1, line = 2.5, outer = T)
mtext("Height Growth (cm)", side = 2, line = 2.5, outer = T)

dev.off()
######### THIS IS NICK'S PLOTTING - I AM DOING MY OWN!!! ########
#write.csv(T2, "SaplingHT_OUT.csv", row.names=FALSE)
############Plotting######################################################
#all species and all 6 time points
ggplot(T2, aes(Season, Length, group=Sapling))+
  geom_line(aes(colour=Site))+
  facet_wrap(~Species, ncol=4, scales="free")+
  labs(x = "Season", y = "Stem length (cm)")+
  theme(panel.background=element_blank(),
        axis.line = element_line(), panel.grid.minor = element_blank(),
        axis.ticks=element_line(colour="black"),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1)) 

##########################################################################
##########################################################################
##########################################################################
#all species and four time points
T3<-subset(T2, T2$Season %in% c("Spring 2017", "Fall 2017",
                                "Fall 2018",
                                "Fall 2019"))

ggplot(T3, aes(Season, Length, group=Sapling))+
  geom_line(aes(colour=Site))+
  facet_wrap(~Species, ncol=4, scales="free")+
  labs(x = "Season", y = "Stem length (cm)")+
  theme(panel.background=element_blank(),
        axis.line = element_line(), panel.grid.minor = element_blank(),
        axis.ticks=element_line(colour="black"),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1)) 



##########################################################################
##########################################################################
##########################################################################
##########################################################################
#all species and four time points mean +/- SE
names(T1)

T1$Gro.17<-T1$`Fall 2017` - T1$`Spring 2017`
T1$Gro.18<-T1$`Fall 2018` - T1$`Spring 2017`
T1$Gro.19<-T1$`Fall 2019` - T1$`Spring 2017`



T4<-T1 %>%
  group_by(Site, Species) %>% 
  summarise(Mean.2017=mean(na.omit(Gro.17)),
           sd.2017=sd(na.omit(Gro.17)),
           se.2017=sd(na.omit(Gro.17))/sqrt(length(na.omit(Gro.17))),
           Mean.2018=mean(na.omit(Gro.18)),
           sd.2018=sd(na.omit(Gro.18)),
           se.2018=sd(na.omit(Gro.18))/sqrt(length(na.omit(Gro.18))),
           Mean.2019=mean(na.omit(Gro.19)),
           sd.2019=sd(na.omit(Gro.19)),
           se.2019=sd(na.omit(Gro.19))/sqrt(length(na.omit(Gro.19))))
           
head(T4)

T4.1<-T4 %>%
  pivot_longer(
    c(-Site, -Species),
    names_to= "statistic",
    values_to= "value"
  )

head(T4.1)

T4.1<- T4.1 %>%
  separate(statistic, into = c("Variable", "Year"))

T5<-T4.1 %>%
  pivot_wider(
    names_from=Variable, 
    values_from=value
    )

head(T5)

#Site as factor
T5$Site<-as.factor(T5$Site)


ggplot(T5, aes(Year, Mean, group=Site))+
  geom_point(aes(colour=Site, shape=Site))+
  geom_errorbar(aes(ymax=Mean+se, ymin=Mean-se,
                    width=0.1, colour=Site))+
  geom_line(aes(colour=Site))+
  facet_wrap(~Species, ncol=4, scales="free")+
  labs(x = "Year", y = "Height growth (cm)")+
  theme(panel.background=element_blank(),
        axis.line = element_line(), panel.grid.minor = element_blank(),
        axis.ticks=element_line(colour="black"),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1)) 




########################################################################
########################################################################
###### Plotting to compare among sites and species for final total   ###
########################################################################
########################################################################
#need x to be Site
#need y to be 2019 growth

X1<-T4[,c(1,2,c(9:11))]

ggplot(X1, aes(Site, Mean.2019, group=Species))+
  geom_point(aes(colour=Species))+
  geom_errorbar(aes(ymax=Mean.2019+se.2019, ymin=Mean.2019-se.2019,
                    width=0.1, colour=Species))+
  geom_line(aes(colour=Species))+
  #facet_wrap(~Species, ncol=4, scales="free")+
  labs(x = "Site", y = "Height growth (cm)")+
  theme(panel.background=element_blank(),
        axis.line = element_line(), panel.grid.minor = element_blank(),
        axis.ticks=element_line(colour="black")) 

#without robps, which dominates y-axis
X2<-subset(X1[!(X1$Species %in% c("robps", "pruse")), ])
ggplot(X2, aes(Site, Mean.2019, group=Species))+
  geom_point(aes(colour=Species))+
  geom_errorbar(aes(ymax=Mean.2019+se.2019, ymin=Mean.2019-se.2019,
                    width=0.1, colour=Species))+
  geom_line(aes(colour=Species))+
  #facet_wrap(~Species, ncol=4, scales="free")+
  labs(x = "Site", y = "Height growth (cm)")+
  theme(panel.background=element_blank(),
        axis.line = element_line(), panel.grid.minor = element_blank(),
        axis.ticks=element_line(colour="black")) 


########################################################################
########################################################################



##########################################################################
##########################################################################
##########################################################################
#Survival
#number of stems at each time point

S1<-T1 %>%
  group_by(Site, Species) %>%
  summarise(Spring.2017c=length(na.omit(`Spring 2017`)),
           Fall.2017c=length(na.omit(`Fall 2017`)),
           Fall.2018c=length(na.omit(`Fall 2018`)),
           Fall.2019c=length(na.omit(`Fall 2019`)),
           Spring.2017p=100,
           Fall.2017p=100*(length(na.omit(`Fall 2017`))/
             length(na.omit(`Spring 2017`))),
           Fall.2018p=100*(length(na.omit(`Fall 2018`))/
                             length(na.omit(`Spring 2017`))),
           Fall.2019p=100*(length(na.omit(`Fall 2019`))/
                             length(na.omit(`Spring 2017`))))
   
           
S1$Site<-as.factor(S1$Site)
names(S1)
S1.1<-S1[ ,c(1,2,7:10)]

S2<-S1.1 %>%
  pivot_longer(
    c(-Site, -Species),
    names_to= "Time.Period",
    values_to= "Survivors"
  )

str(S2)
S2$Site<-as.factor(S2$Site)
S2$Survivors<-as.numeric(S2$Survivors)
S2$Time.Period<-factor(S2$Time.Period, levels=c("Spring.2017p", "Fall.2017p",
                                      "Fall.2018p","Fall.2019p"),
                  ordered=is.ordered(S2$Time.Period))

ggplot(S2, aes(Time.Period, Survivors, group=Site))+
  geom_line(aes(colour=Site))+
  geom_point(aes(colour=Site, shape=Site))+
  facet_wrap(~Species, ncol=4, scales="free")+
  labs(x = "Time period", y = "Survival (%)")+
  theme(panel.background=element_blank(),
        axis.line = element_line(), panel.grid.minor = element_blank(),
        axis.ticks=element_line(colour="black"),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1)) 




