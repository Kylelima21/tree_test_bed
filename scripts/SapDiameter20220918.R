###Sapling Diameter Data

#library(plyr) #should eventually shift to dplyr package
library(ggplot2)
library(tidyr)
library(dplyr)
#library(dp) #not sure why I need this?

#set work directory
setwd("C:/Users/rutha/Desktop/TTB/RawOriginalData_Scripts")

#read in the data
D1<-read.csv('SaplingDiameter_20200728.csv', header=TRUE)
#above created from SaplingDiameter_20200728MASTER.xlsx
D1<-D1[1:384,]

#need to average the diameter data and 
#have four measures: initial and end of each growing season
names(D1)
str(D1)
head(D1)
D1$Diam.Initial<-(D1$Diam1a+D1$Diam1b)/2
D1<-transform(D1, Diam.End2017 = rowMeans(D1[,c(7,9,10)], na.rm=TRUE))
D1<-transform(D1, Diam.End2018 = rowMeans(D1[,13:14], na.rm=TRUE))
D1<-transform(D1, Diam.End2019 = rowMeans(D1[,17:18], na.rm=TRUE))

####just taking the columns we need for plotting
D2<-D1[,c(1:3,21:24)]
head(D2)

#shifting from wide to long

D2.L<-D2 %>%
  pivot_longer(
    c(-Site, -Species, -Sapling),
    names_to= "Season",
    values_to= "Diameter"
  )

str(D2.L)
#order factor levels
D2.L$Season<-factor(D2.L$Season, levels=c(
  "Diam.Initial", "Diam.End2017", "Diam.End2018", "Diam.End2019"),
                  ordered=is.ordered(D2.L$Season))
#Site as factor
D2.L$Site<-as.factor(D2.L$Site)


match <- data.frame(Season = c("Diam.Initial", "Diam.End2017", "Diam.End2018", "Diam.End2019"), 
                    year = c("Spring 2017", "Fall 2017", "Fall 2018", "Fall 2019"), num = c(1:4))

dat <- merge(D2.L, match, by = "Season")

colnames(dat) <- c("season", "site", "species", "sapling", "diam", "year", "num")

dat$species <- as.factor(dat$species)
dat$sapling <- as.factor(dat$sapling)
dat$year <- as.factor(dat$year)
dat$num <- as.numeric(dat$num)

diam.rel <- data.frame()
for(i in unique(levels(dat$species))){
  # i = "robps"
  temp <- subset(dat, species == i)
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
    
    first <- temp2$diam[1]
    
    # temp2$rel.h <- ((temp2$length-first)/temp2$length)*100
    temp2$d.growth <- temp2$diam-first
    temp2 <- na.omit(temp2)
    
    # points(data = temp2, h.growth ~ num, col = temp2$col[1], 
    #        type = "b")
    
    diam.rel <- rbind(diam.rel, temp2)
  }
}

sum <- data.frame()
for(i in unique(levels(diam.rel$site))){
  # i = "1"
  temp <- subset(diam.rel, site == i)
  for(j in unique(levels(temp$species))){
    # j = "aceru"
    temp2 <- subset(temp, species == j)
    for(k in unique(levels(temp2$year))){
      # k = "Spring 2017"
      temp3 <- subset(temp2, year == k)
      
      mean <- mean(temp3$d.growth, na.rm = T)
      se <- sd(temp3$d.growth, na.rm = T)/sqrt(length(temp3$d.growth))
      n <- length(temp3$d.growth)
      
      num = temp3$num[1]
      # col = temp3$col[1]
      
      site = i
      species = j
      season = k
      
      temp.out <- data.frame(site, species, season, mean, se, n, num)
      
      sum <- rbind(sum, temp.out)
    }
  }
}

cols <- data.frame(site = c("1", "2", "3", "4"), 
                   col = c(1:4))

sum <- merge(sum, cols, by = "site")
sum$site <- as.factor(sum$site)
sum$species <- as.factor(sum$species)
sum$season <- as.factor(sum$season)
sum$col <- as.numeric(sum$col)

# now making a plot! 
setwd("C:\\Users\\rutha\\Desktop\\TTB\\Plots")
today = as.character(format(Sys.time(), "%Y-%m-%d"))
png(file = paste(paste("diam", today, sep = "_"), ".png", sep = ""),
    width = 11, height = 8.5, units = "in", res = 1200)

layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 4, 4))
layout.show(n = 16)# creating matrix in which to insert plots
par(oma = c(4, 4.5, 2, 4.5), mar = c(1, 0.75, 0.75, 0.75), mgp = c(3, 1, 0))

for(i in unique(levels(sum$species))){
  # i = "robps"
  temp <- subset(sum, species == i)
  plot(data = temp, mean ~ num, lty = 0, col = "white", 
       xlab = "Season", ylab = "Diameter Growth (cm)", 
       pch = NA, ylim = c(0, 35), yaxt = "n", xaxt = "n")
  abline(h = 0, lty = 2)
  if(i == "pinta"){
    legend("topleft", 
           title = expression("(G)" ~ italic("P. taeda")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 35, 5), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "pinvi"){
    legend("topleft", 
           title = expression("(K)" ~ italic("P. virginiana")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 35, 5), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "pinst"){
    legend("topleft", 
           title = expression("(C)" ~ italic("P. strobus")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 35, 5), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "pruse"){
    legend("topleft", 
           title = expression("(O)" ~ italic("Pr. serotina")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 35, 5), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "abiba"){
    legend("topleft", 
           title = expression("(A)" ~ italic("A. balsamea")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 35, 5), labels = T, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "pinri"){
    legend("topleft", 
           title = expression("(N)" ~ italic("Pi. rigida")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 35, 5), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "quepr"){
    legend("topleft", 
           title = expression("(H)" ~ italic("Q. prinus")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 35, 5), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "picgl"){
    legend("topleft", 
           title = expression("(B)" ~ italic("Pi. glauca")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 35, 5), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "junvi"){
    legend("topleft", 
           title = expression("(I)" ~ italic("J. virginiana")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 35, 5), labels = T, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "picru"){
    legend("topleft", 
           title = expression("(F)" ~ italic("Pi. rubens")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 35, 5), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "robps"){
    legend("topleft", 
           title = expression("(P)" ~ italic("R. pseudoacacia")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 35, 5), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "queal"){
    legend("topleft", 
           title = expression("(D)" ~ italic("Q. alba")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 35, 5), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "aceru"){
    legend("topleft", 
           title = expression("(E)" ~ italic("Ac. rubrum")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 35, 5), labels = T, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "queru"){
    legend("topleft", 
           title = expression("(L)" ~ italic("Q. rubra")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 35, 5), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "pinba"){
    legend("topleft", 
           title = expression("(J)" ~ italic("Pi. banksiana")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 35, 5), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
  }
  if(i == "lirtu"){
    legend("topleft", 
           title = expression("(M)" ~ italic("L. tulipifera")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 35, 5), labels = T, cex.axis = 1)
    axis(1, at = seq(0, 6, 1), labels = F, cex.axis = 1)
    legend("topright", 
           col = c(1:4),
           pch = 1,
           legend = c("1", "2", "3", "4"),
           title = "Site", 
           bty = "n",
           cex = 1)
  }
  if(i == "pinri"){
    xtick<-seq(1, 4, by=1)
    axis(side=1, at=xtick, labels = FALSE)
    text(x=xtick,  par("usr")[3], 
         labels = c("Spring 2017", "Fall 2017", 
                    "Fall 2018", "Fall 2019"), 
         srt = 0, pos = 1, xpd = TRUE, cex = 0.8)
  }
  if(i == "pruse"){
    xtick<-seq(1, 4, by=1)
    axis(side=1, at=xtick, labels = FALSE)
    text(x=xtick,  par("usr")[3], 
         labels = c("Spring 2017", "Fall 2017", 
                    "Fall 2018", "Fall 2019"), 
         srt = 0, pos = 1, xpd = TRUE, cex = 0.8)
  }
  if(i == "robps"){
    xtick<-seq(1, 4, by=1)
    axis(side=1, at=xtick, labels = FALSE)
    text(x=xtick,  par("usr")[3], 
         labels = c("Spring 2017", "Fall 2017", 
                    "Fall 2018", "Fall 2019"), 
         srt = 0, pos = 1, xpd = TRUE, cex = 0.8)
  }
  if(i == "lirtu"){
    xtick<-seq(1, 4, by=1)
    axis(side=1, at=xtick, labels = FALSE)
    text(x=xtick,  par("usr")[3], 
         labels = c("Spring 2017", "Fall 2017", 
                    "Fall 2018", "Fall 2019"), 
         srt = 0, pos = 1, xpd = TRUE, cex = 0.8)
  }
  for(j in unique(levels(temp$site))){
    # j = "2"
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
mtext("Diameter Growth (mm)", side = 2, line = 2.5, outer = T)

dev.off()






######## THIS IS NICK'S, I AM DOING SOMETHING ELSE!!! ########
###Plotting diamter for all four points in time for each sapling
ggplot(D2.L, aes(Season, Diameter, group=Sapling))+
  geom_line(aes(colour=Site))+
  facet_wrap(~Species, ncol=4, scales="free")+
  labs(x = "Time Period", y = "Diameter (mm)")+
  theme(panel.background=element_blank(),
        axis.line = element_line(), panel.grid.minor = element_blank(),
        axis.ticks=element_line(colour="black"),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1)) 

############################################################################
############################################################################
############################################################################
#mean and se at end of each year
#create new columns for growth
D2$Growth2017<-D2$Diam.End2017 - D2$Diam.Initial
D2$Growth2018<-D2$Diam.End2018 - D2$Diam.Initial
D2$Growth2019<-D2$Diam.End2019 - D2$Diam.Initial

#error perhaps propagated below about too many na's and empty cells
D3<-D2 %>%
  group_by(Site, Species) %>%
  summarize(Mean.2017 = mean(Growth2017, na.rm=T),
            sd.2017 = sd(Growth2017, na.rm=T),
            se.2017 = sd(Growth2017, na.rm=T)/sqrt(length(!is.na(Growth2017))),
            Mean.2018 = mean(Growth2018, na.rm=T),
            sd.2018 = sd(Growth2018, na.rm=T),
            se.2018 = sd(Growth2018, na.rm=T)/sqrt(length(!is.na(Growth2018))),
            Mean.2019 = mean(Growth2019, na.rm=T),
            sd.2019 = sd(Growth2019, na.rm=T),
            se.2019 = sd(Growth2019, na.rm=T)/sqrt(length(!is.na(Growth2019))))
########################################################################

########################################################################


D3.L<-D3 %>%
  pivot_longer(
    c(-Site, -Species),
    names_to= "Statistic",
    values_to= "Value"
  )

D3.L<- D3.L %>%
  separate(Statistic, into = c("Variable", "Year"))

D4<-D3.L %>%
  pivot_wider(
    names_from=Variable, 
    values_from=Value
  )

names(D4)

#Site as factor
D4$Site<-as.factor(D4$Site)



ggplot(D4, aes(Year, Mean, group=Site))+
  geom_point(aes(colour=Site, shape=Site))+
  geom_errorbar(aes(ymax=Mean+se, ymin=Mean-se,
                    width=0.1, colour=Site))+
  geom_line(aes(colour=Site))+
  facet_wrap(~Species, ncol=4, scales="free")+
  labs(x = "Year", y = "Diameter growth (mm)")+
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

X1<-D3[,c(1,2,c(9:11))]

ggplot(X1, aes(Site, Mean.2019, group=Species))+
  geom_point(aes(colour=Species))+
  geom_errorbar(aes(ymax=Mean.2019+se.2019, ymin=Mean.2019-se.2019,
                    width=0.1, colour=Species))+
  geom_line(aes(colour=Species))+
  #facet_wrap(~Species, ncol=4, scales="free")+
  labs(x = "Site", y = "Diameter growth (mm)")+
  theme(panel.background=element_blank(),
        axis.line = element_line(), panel.grid.minor = element_blank(),
        axis.ticks=element_line(colour="black")) 

#without robps, which dominates y-axis
X2<-subset(X1[X1$Species != "robps", ])
ggplot(X2, aes(Site, Mean.2019, group=Species))+
  geom_point(aes(colour=Species))+
  geom_errorbar(aes(ymax=Mean.2019+se.2019, ymin=Mean.2019-se.2019,
                    width=0.1, colour=Species))+
  geom_line(aes(colour=Species))+
  #facet_wrap(~Species, ncol=4, scales="free")+
  labs(x = "Site", y = "Diameter growth (mm)")+
  theme(panel.background=element_blank(),
        axis.line = element_line(), panel.grid.minor = element_blank(),
        axis.ticks=element_line(colour="black")) 


########################################################################
########################################################################


