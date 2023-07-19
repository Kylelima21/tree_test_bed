
setwd("C:/Users/rutha/Desktop/TTB/RawOriginalData_Scripts")

bio = read.csv(file = "TTB_BiomassTest (1).csv", header = T)

# str(bio)
# hist(bio$sample.mass..g.)
# unique(bio$site)
# unique(bio$sp.)
#yikes. lots of misspelled species
bio = within(bio, sp.[sp. == "picgla"] <- "picgl")
bio = within(bio, sp.[sp. == "pcgl"] <- "picgl")
bio = within(bio, sp.[sp. == "cirtu"] <- "lirtu")
bio = within(bio, sp.[sp. == "qepri"] <- "quepr")
bio = within(bio, sp.[sp. == "qupri"] <- "quepr")
bio = within(bio, sp.[sp. == "pinbar"] <- "pinba")
bio = within(bio, sp.[sp. == "rpbsp"] <- "robps")
bio = within(bio, sp.[sp. == "prusc"] <- "pruse")
bio = within(bio, sp.[sp. == "aciru"] <- "aceru")
bio = within(bio, sp.[sp. == "robsp"] <- "robps")

unique(bio$sp.)

unique(bio$ID)
table(bio$ID)

bio <- bio[, c(1, 3, 4, 5, 6, 9, 10)]

colnames(bio) <- c("date", "site", "id", "sp", "organ", "mass", "remarks")

bio$date <- as.POSIXct(bio$date, format = "%M/%d/%y")
bio$date <- substr(bio$date, 1, 10)
bio$site <- as.factor(bio$site)

levels(bio$site)
## ??? hmm 

bio$id <- as.factor(bio$id)
bio$sp <- as.factor(bio$sp)
levels(bio$sp)
## some funky stuff happening here

bio$organ <- as.factor(bio$organ)
levels(bio$organ)

bio$site.id <- paste(bio$site, bio$id, sep = "-")

bio[bio$site.id == "1-a3", "sp"] <- "pinta" 
bio[bio$site.id == "1-b2", "sp"] <- "pinvi" 
bio[bio$site.id == "2-n1", "sp"] <- "pinvi" 
bio[bio$site.id == "1-j4", "sp"] <- "picgl"
bio[bio$site.id == "1-k5", "sp"] <- "picru" 
bio[bio$site.id == "1-g5", "sp"] <- "pinri" 
bio[bio$site.id == "1-j3", "sp"] <- "pinvi" 
bio[bio$site.id == "1-m4", "sp"] <- "pinvi" 
bio[bio$site.id == "1-k4", "sp"] <- "pinri" 
 
bio[c(549, 550, 770), "site"] <- "1"
bio[c(549, 550, 770), "site.id"] <- "1-i2"

bio[c(811, 812, 817), "site"] <- "1"
bio[c(811, 812, 817), "site.id"] <- "1-j1"

bio$site.id <- as.factor(bio$site.id)

# junvi <- subset(bio, sp == "junvi")
# there is still one sapling whose site is undetermined... :( )

bio.new <- data.frame()
for(i in unique(levels(bio$site))){
  # i = "1"
  temp <- subset(bio, site == i)
  for(j in unique(levels(temp$id))){
    # j = "m5"
    temp2 <- subset(temp, id == j)
    for (k in unique(levels(temp2$organ))){
      # k = "s"
      temp3 <- subset(temp2, organ == k)
      
      site <- i
      id <- j
      organ <- k
      sp <- as.factor(as.character(temp3$sp[1]))
      mass <- sum(temp3$mass)
      site.id <- as.factor(as.character(temp3$site.id))[1]
      
      temp.out <- data.frame(site, id, site.id, sp, organ, mass)
      
      bio.new <- rbind(bio.new, temp.out)
    }
  }
}

bio.new <- na.omit(bio.new)

bio.new2 <- data.frame()
for(i in unique(levels(bio.new$site.id))){
  # i = "1-a3"
  temp <- subset(bio.new, site.id == i)
  
  site.id <- i
  leaf <- ifelse(is.null(subset(temp, organ == "l")$mass), NA, (subset(temp, organ == "l")$mass))
  shoot <- ifelse(is.null(subset(temp, organ == "s")$mass), NA, (subset(temp, organ == "s")$mass))
  root <- ifelse(is.null(subset(temp, organ == "r")$mass), NA, (subset(temp, organ == "r")$mass))
  r.to.s <- root/shoot
  total <- leaf+shoot+root
  sp <- as.factor(as.character(temp$sp))[1]
  site <- as.factor(as.character(temp$site))[1]
  id <- as.factor(as.character(temp$id))[1]
  
  temp.out <- data.frame(site.id, site, id, sp, leaf, shoot, root, r.to.s, total)
  
  bio.new2 <- rbind(bio.new2, temp.out)
}

unique(levels(bio.new2$sp))

type <- read.csv(file = "species_types.csv")

bio.new3 <- merge(bio.new2, type, by = "sp")

bio.new3[order(names(bio.new3))]

bio.new3$type.comb <- as.factor(paste(bio.new3$native, bio.new3$type, sep = "-"))

# plotting root to shoot ratio by species and by site
setwd("C:\\Users\\rutha\\Desktop\\TTB\\Plots")
today = as.character(format(Sys.time(), "%Y-%m-%d"))
png(file = paste(paste("R.to.S", today, sep = "_"), ".png", sep = ""),
    width = 11, height = 8.5, units = "in", res = 1200)
layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 8,
                9, 10, 11, 12, 13, 14, 15, 16), 4, 4))
par(oma = c(4.5, 4.5, 0.5, 4.5), mar = c(0.5, 0.5, 0.5, 0.5), mgp = c(3, 1, 0))
for(i in unique(levels(bio.new3$sp))){
  # i = "aceru"
  temp <- subset(bio.new3, sp == i)
  plot(data = temp, r.to.s ~ site, ylim = c(0, 5), xaxt = "n", 
       yaxt = "n")
  if(temp$type.comb == "y-a"){
    points(4, 4, pch = 0, cex = 1)
  }
  if(temp$type.comb == "y-g"){
    points(4, 4, pch = 16, cex = 1)
  }
  if(temp$type.comb == "n-a"){
    points(4, 4, pch = 2, cex = 1)
  }
  if(temp$type.comb == "n-g"){
    points(4, 4, pch = 3, cex = 1)
  }
  if(i == "pinta"){
    legend("topleft", 
           title = expression("(A)" ~ italic("P. taeda")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 5, 1), labels = T, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "pinvi"){
    legend("topleft", 
           title = expression("(B)" ~ italic("P. virginiana")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "pinst"){
    legend("topleft", 
           title = expression("(C)" ~ italic("P. strobus")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "pruse"){
    legend("topleft", 
           title = expression("(D)" ~ italic("Pr. serotina")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "abiba"){
    legend("topleft", 
           title = expression("(E)" ~ italic("A. balsamea")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 5, 1), labels = T, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "pinri"){
    legend("topleft", 
           title = expression("(F)" ~ italic("Pi. rigida")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "quepr"){
    legend("topleft", 
           title = expression("(G)" ~ italic("Q. prinus")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "picgl"){
    legend("topleft", 
           title = expression("(H)" ~ italic("Pi. glauca")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "junvi"){
    legend("topleft", 
           title = expression("(I)" ~ italic("J. virginiana")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 5, 1), labels = T, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "picru"){
    legend("topleft", 
           title = expression("(J)" ~ italic("Pi. rubens")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "robps"){
    legend("topleft", 
           title = expression("(K)" ~ italic("R. pseudoacacia")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "queal"){
    legend("topleft", 
           title = expression("(L)" ~ italic("Q. alba")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "aceru"){
    legend("topleft", 
           title = expression("(M)" ~ italic("Ac. rubrum")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 5, 1), labels = T, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = T, cex.axis = 1)
  }
  if(i == "queru"){
    legend("topleft", 
           title = expression("(N)" ~ italic("Q. rubru")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = T, cex.axis = 1)
  }
  if(i == "pinba"){
    legend("topleft", 
           title = expression("(O)" ~ italic("Pi. banksiana")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = T, cex.axis = 1)
  }
  if(i == "lirtu"){
    legend("topleft", 
           title = expression("(P)" ~ italic("L. tulipifera")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = T, cex.axis = 1)
    legend("bottomleft", 
           pch = c(0, 16, 2, 3), 
           legend = c("N-A", "N-G", "NN-A", "NN-G"), 
           bty = "n",
           cex = 1)
  }
}
mtext("Site", side = 1, line = 2.5, outer = T)
mtext("R:S", side = 2, line = 2.5, outer = T)
dev.off()


# # plotting total biomass by species and by site
setwd("C:\\Users\\rutha\\Desktop\\TTB\\Plots")
today = as.character(format(Sys.time(), "%Y-%m-%d"))
png(file = paste(paste("TotalBiomass", today, sep = "_"), ".png", sep = ""),
    width = 11, height = 8.5, units = "in", res = 1200)
layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 8,
                9, 10, 11, 12, 13, 14, 15, 16), 4, 4))
par(oma = c(4.5, 4.5, 0.5, 4.5), mar = c(0.5, 0.5, 0.5, 0.5), mgp = c(3, 1, 0))
for(i in unique(levels(bio.new3$sp))){
  # i = "aceru"
  temp <- subset(bio.new3, sp == i)
  plot(data = temp, total ~ site, ylim = c(0, 500), xaxt = "n", 
       yaxt = "n")
  if(temp$type.comb == "y-a"){
    points(4, 450, pch = 0, cex = 1)
  }
  if(temp$type.comb == "y-g"){
    points(4, 450, pch = 16, cex = 1)
  }
  if(temp$type.comb == "n-a"){
    points(4, 450, pch = 2, cex = 1)
  }
  if(temp$type.comb == "n-g"){
    points(4, 450, pch = 3, cex = 1)
  }
  if(i == "pinta"){
    legend("topleft", 
           title = expression("(A)" ~ italic("P. taeda")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 500, 100), labels = T, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "pinvi"){
    legend("topleft", 
           title = expression("(B)" ~ italic("P. virginiana")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "pinst"){
    legend("topleft", 
           title = expression("(C)" ~ italic("P. strobus")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "pruse"){
    legend("topleft", 
           title = expression("(D)" ~ italic("Pr. serotina")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "abiba"){
    legend("topleft", 
           title = expression("(E)" ~ italic("A. balsamea")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 500, 100), labels = T, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "pinri"){
    legend("topleft", 
           title = expression("(F)" ~ italic("Pi. rigida")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "quepr"){
    legend("topleft", 
           title = expression("(G)" ~ italic("Q. prinus")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "picgl"){
    legend("topleft", 
           title = expression("(H)" ~ italic("Pi. glauca")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "junvi"){
    legend("topleft", 
           title = expression("(I)" ~ italic("J. virginiana")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 500, 100), labels = T, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "picru"){
    legend("topleft", 
           title = expression("(J)" ~ italic("Pi. rubens")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "robps"){
    legend("topleft", 
           title = expression("(K)" ~ italic("R. pseudoacacia")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "queal"){
    legend("topleft", 
           title = expression("(L)" ~ italic("Q. alba")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
  }
  if(i == "aceru"){
    legend("topleft", 
           title = expression("(M)" ~ italic("Ac. rubrum")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 500, 100), labels = T, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = T, cex.axis = 1)
  }
  if(i == "queru"){
    legend("topleft", 
           title = expression("(N)" ~ italic("Q. rubra")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = T, cex.axis = 1)
  }
  if(i == "pinba"){
    legend("topleft", 
           title = expression("(O)" ~ italic("P. banksiana")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = T, cex.axis = 1)
  }
  if(i == "lirtu"){
    legend("topleft", 
           title = expression("(P)" ~ italic("L. tulipifera")),
           bty = "n", 
           legend = "", 
           cex = 1)
    axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
    axis(1, at = seq(0, 4, 1), labels = T, cex.axis = 1)
    legend("bottomleft", 
           pch = c(0, 16, 2, 3), 
           legend = c("N-A", "N-G", "NN-A", "NN-G"), 
           bty = "n",
           cex = 0.8)
  }
}
mtext("Site", side = 1, line = 2.5, outer = T)
mtext("Total Biomass (g)", side = 2, line = 2.5, outer = T)
dev.off()

setwd("C:/Users/rutha/Desktop/TTB/ProcessedData_Scripts")
write.csv(bio.new3, file = paste(paste("ProcessedBiomass", today, sep = "_"), ".csv", sep = ""))

# now to summarise the biomass data... summarize by species and site, need mean, se, and n
library(dplyr)
dat.sum <- group_by(bio.new3, sp, site) %>%
  summarise(rs.mean = mean(r.to.s), rs.se = sd(r.to.s)/sqrt(length(r.to.s)), 
            total.mean = mean(total), total.se = sd(total)/sqrt(length(total)), 
            rs.n = length(r.to.s), total.n = length(total))

write.csv(dat.sum, file = paste(paste("ProcessedBiomassSummary", today, sep = "_"), ".csv", sep = ""))

dat.sum$site <- as.numeric(as.character(dat.sum$site))
dat.sum <- merge(dat.sum, type, by = "sp")
dat.sum$type.comb <- as.factor(paste(dat.sum$native, dat.sum$type, sep = "-"))
# # now I am plotting the means and se for these data! 
# plotting root to shoot ratio by species and by site
# setwd("C:\\Users\\rutha\\Desktop\\TTB\\Plots")
# today = as.character(format(Sys.time(), "%Y-%m-%d"))
# png(file = paste(paste("R.to.S_sum", today, sep = "_"), ".png", sep = ""),
#     width = 11, height = 8.5, units = "in", res = 1200)
# layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 8,
#                 9, 10, 11, 12, 13, 14, 15, 16), 4, 4))
# par(oma = c(4.5, 4.5, 0.5, 4.5), mar = c(0.5, 0.5, 0.5, 0.5), mgp = c(3, 1, 0))
# for(i in unique(levels(dat.sum$sp))){
#   # i = "aceru"
#   temp <- subset(dat.sum, sp == i)
#   plot(data = temp, rs.mean ~ site, ylim = c(0, 5), xaxt = "n", 
#        yaxt = "n")
#   arrows(temp$site, temp$rs.mean-temp$rs.se, 
#          temp$site, temp$rs.mean+temp$rs.se, 
#          length=0.05, angle=90, code=3, cex = 1.3)
#   if(temp$type.comb[1] == "y-a"){
#     points("topright", pch = 0, cex = 1)
#   }
#   if(temp$type.comb[1] == "y-g"){
#     points("topright", pch = 16, cex = 1)
#   }
#   if(temp$type.comb[1] == "n-a"){
#     points("topright", pch = 2, cex = 1)
#   }
#   if(temp$type.comb[1] == "n-g"){
#     points("topright", pch = 3, cex = 1)
#   }
#   if(i == "pinta"){
#     legend("topleft", 
#            title = expression("(A)" ~ italic("P. taeda")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 5, 1), labels = T, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "pinvi"){
#     legend("topleft", 
#            title = expression("(B)" ~ italic("P. virginiana")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "pinst"){
#     legend("topleft", 
#            title = expression("(C)" ~ italic("P. strobus")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "pruse"){
#     legend("topleft", 
#            title = expression("(D)" ~ italic("Pr. serotina")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "abiba"){
#     legend("topleft", 
#            title = expression("(E)" ~ italic("A. balsamea")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 5, 1), labels = T, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "pinri"){
#     legend("topleft", 
#            title = expression("(F)" ~ italic("Pi. rigida")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "quepr"){
#     legend("topleft", 
#            title = expression("(G)" ~ italic("Q. prinus")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "picgl"){
#     legend("topleft", 
#            title = expression("(H)" ~ italic("Pi. glauca")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "junvi"){
#     legend("topleft", 
#            title = expression("(I)" ~ italic("J. virginiana")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 5, 1), labels = T, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "picru"){
#     legend("topleft", 
#            title = expression("(J)" ~ italic("Pi. rubens")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "robps"){
#     legend("topleft", 
#            title = expression("(K)" ~ italic("R. pseudoacacia")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "queal"){
#     legend("topleft", 
#            title = expression("(L)" ~ italic("Q. alba")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "aceru"){
#     legend("topleft", 
#            title = expression("(M)" ~ italic("Ac. rubrum")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 5, 1), labels = T, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = T, cex.axis = 1)
#   }
#   if(i == "queru"){
#     legend("topleft", 
#            title = expression("(N)" ~ italic("Q. rubru")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = T, cex.axis = 1)
#   }
#   if(i == "pinba"){
#     legend("topleft", 
#            title = expression("(O)" ~ italic("Pi. banksiana")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = T, cex.axis = 1)
#   }
#   if(i == "lirtu"){
#     legend("topleft", 
#            title = expression("(P)" ~ italic("L. tulipifera")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 5, 1), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = T, cex.axis = 1)
#     legend("bottomleft", 
#            pch = c(0, 16, 2, 3), 
#            legend = c("N-A", "N-G", "NN-A", "NN-G"), 
#            bty = "n",
#            cex = 1)
#   }
# }
# mtext("Site", side = 1, line = 2.5, outer = T)
# mtext("R:S", side = 2, line = 2.5, outer = T)
# dev.off()


# # # plotting total biomass by species and by site
# setwd("C:\\Users\\rutha\\Desktop\\TTB\\Plots")
# today = as.character(format(Sys.time(), "%Y-%m-%d"))
# png(file = paste(paste("TotalBiomass", today, sep = "_"), ".png", sep = ""),
#     width = 11, height = 8.5, units = "in", res = 1200)
# layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 8,
#                 9, 10, 11, 12, 13, 14, 15, 16), 4, 4))
# par(oma = c(4.5, 4.5, 0.5, 4.5), mar = c(0.5, 0.5, 0.5, 0.5), mgp = c(3, 1, 0))
# for(i in unique(levels(bio.new3$sp))){
#   # i = "aceru"
#   temp <- subset(bio.new3, sp == i)
#   plot(data = temp, total ~ site, ylim = c(0, 500), xaxt = "n", 
#        yaxt = "n")
#   if(temp$type.comb == "y-a"){
#     points(4, 450, pch = 0, cex = 1)
#   }
#   if(temp$type.comb == "y-g"){
#     points(4, 450, pch = 16, cex = 1)
#   }
#   if(temp$type.comb == "n-a"){
#     points(4, 450, pch = 2, cex = 1)
#   }
#   if(temp$type.comb == "n-g"){
#     points(4, 450, pch = 3, cex = 1)
#   }
#   if(i == "pinta"){
#     legend("topleft", 
#            title = expression("(A)" ~ italic("P. taeda")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 500, 100), labels = T, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "pinvi"){
#     legend("topleft", 
#            title = expression("(B)" ~ italic("P. virginiana")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "pinst"){
#     legend("topleft", 
#            title = expression("(C)" ~ italic("P. strobus")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "pruse"){
#     legend("topleft", 
#            title = expression("(D)" ~ italic("Pr. serotina")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "abiba"){
#     legend("topleft", 
#            title = expression("(E)" ~ italic("A. balsamea")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 500, 100), labels = T, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "pinri"){
#     legend("topleft", 
#            title = expression("(F)" ~ italic("Pi. rigida")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "quepr"){
#     legend("topleft", 
#            title = expression("(G)" ~ italic("Q. prinus")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "picgl"){
#     legend("topleft", 
#            title = expression("(H)" ~ italic("Pi. glauca")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "junvi"){
#     legend("topleft", 
#            title = expression("(I)" ~ italic("J. virginiana")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 500, 100), labels = T, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "picru"){
#     legend("topleft", 
#            title = expression("(J)" ~ italic("Pi. rubens")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "robps"){
#     legend("topleft", 
#            title = expression("(K)" ~ italic("R. pseudoacacia")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "queal"){
#     legend("topleft", 
#            title = expression("(L)" ~ italic("Q. alba")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = F, cex.axis = 1)
#   }
#   if(i == "aceru"){
#     legend("topleft", 
#            title = expression("(M)" ~ italic("Ac. rubrum")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 500, 100), labels = T, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = T, cex.axis = 1)
#   }
#   if(i == "queru"){
#     legend("topleft", 
#            title = expression("(N)" ~ italic("Q. rubra")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = T, cex.axis = 1)
#   }
#   if(i == "pinba"){
#     legend("topleft", 
#            title = expression("(O)" ~ italic("P. banksiana")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = T, cex.axis = 1)
#   }
#   if(i == "lirtu"){
#     legend("topleft", 
#            title = expression("(P)" ~ italic("L. tulipifera")),
#            bty = "n", 
#            legend = "", 
#            cex = 1)
#     axis(2, at = seq(0, 500, 100), labels = F, cex.axis = 1)
#     axis(1, at = seq(0, 4, 1), labels = T, cex.axis = 1)
#     legend("bottomleft", 
#            pch = c(0, 16, 2, 3), 
#            legend = c("N-A", "N-G", "NN-A", "NN-G"), 
#            bty = "n",
#            cex = 0.8)
#   }
# }
# mtext("Site", side = 1, line = 2.5, outer = T)
# mtext("Total Biomass (g)", side = 2, line = 2.5, outer = T)
# dev.off()