library(doBy)


sumfun <- function(x, ...){
  c(m=mean(x, ...), sd=sd(x, ...), l=length(x), 
    sd.up = mean(x, ...) + sd(x, ...),
    sd.down = mean(x, ...) - sd(x, ...),
    se.up = mean(x, ...) + 2*(sd(x, ...) / sqrt(length(x))),
    se.down = mean(x, ...) - 2*(sd(x, ...) / sqrt(length(x))))
}



mydir = "C:\\Users\\Jay Wason\\OneDrive - University of Maine System\\UMaine\\Research\\2020_Tree_test_bed\\biomass\\"
setwd(mydir)


bio = read.csv(file = "TTB_BiomassTest (1).csv", header = T)

str(bio)
hist(bio$sample.mass..g.)
unique(bio$site)
unique(bio$sp.)
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
#good enough for now. still need to settle some but will just exclude those for now.

bio2 = summaryBy(sample.mass..g. ~ site + ID + sp. + organ..r.s.l., data = bio,
                 FUN = sum)

summary(bio2)
#Looks like some missing data that needs to be fixed. Species names missing for one sample ID, for example.

#doesn't matter for this prelim analysis I am doing.

bio3 = subset(bio2, sp. == "picru" | sp. == "abiba" | sp. == "picgl" | sp. == "pinst"| sp. == "queru" | sp. == "aceru")

bio3.r = subset(bio3, organ..r.s.l. == "r")
bio3.r$r.mass = bio3.r$sample.mass..g..sum
bio3.r = subset(bio3.r, select = -c(sample.mass..g..sum, organ..r.s.l.))

bio3.s = subset(bio3, organ..r.s.l. == "s")
bio3.s$s.mass = bio3.s$sample.mass..g..sum
bio3.s = subset(bio3.s, select = -c(sample.mass..g..sum, organ..r.s.l.))

bio3.l = subset(bio3, organ..r.s.l. == "l")
bio3.l$l.mass = bio3.l$sample.mass..g..sum
bio3.l = subset(bio3.l, select = -c(sample.mass..g..sum, organ..r.s.l.))

bio.merge = merge(bio3.r, bio3.s, by = c("site", "ID", "sp."), all = T)
bio.merge2 = merge(bio.merge, bio3.l, by = c("site", "ID", "sp."), all = T)

#some missing data points. remove those
bio.merge3 = bio.merge2[complete.cases(bio.merge2),]
summary(bio.merge3) 

bio.merge3$rs.ratio = bio.merge3$r.mass / (bio.merge3$s.mass + bio.merge3$l.mass)
bio.merge3$total = bio.merge3$r.mass + bio.merge3$s.mass + bio.merge3$l.mass
bio.merge3$sp. = as.factor(bio.merge3$sp.)
bio.merge3$site = as.factor(bio.merge3$site)
bio.merge3$ID = as.factor(bio.merge3$ID)

plot(rs.ratio ~ sp., data = bio.merge3)


#simplify to just two sites
#site 1 is summit, hot in summer, colder in winter (less moderation)
#site 4 is schoodic, cool summer, moderate winter (more moderation)

dat = subset(bio.merge3, site == "1"| site == "4")
dat$site = as.factor(as.character(dat$site))
str(dat)

par(oma=c(3,3,3,1), mar=c(0.5,0.5,0.5,0.5), mgp = c(3, 1, 0)  )
layout(matrix(c(1,2, 3, 4, 5, 6), 2, 3, byrow  = T))

for(i in c("abiba", "picru", "picgl", "pinst", "aceru", "queru")){
  temp = subset(dat, sp. == i)
  my.ymax = 2.6
  if(i == "abiba" | i == "picru" | i == "picgl"){my.ymax = 0.75}
  
  
  plot(rs.ratio ~ site, data = temp, ylim = c(0.1, my.ymax), 
       xaxt = "n", yaxt = "n", col = c("firebrick3", "dodgerblue3"))
  if(i == "abiba"){axis(side = 2)}
  if(i == "abiba" | i == "picru" | i == "picgl"){axis(side = 1, labels = NA, at = c(1,2))}
  if(i == "picru" | i == "picgl"){axis(side = 2, labels = NA)}
  if(i == "pinst" | i == "aceru" | i == "queru"){axis(side = 1, labels = c("Inland", "Coastal"), at = c(1,2))}
  if(i == "pinst" ){axis(side = 2)}
  if(i == "aceru" | i == "queru"){axis(side = 2, labels = NA)}
  
  mtext("Site", outer = T, side = 1, line = 1.75)
  mtext("Root:shoot biomass", outer = T, side = 2, line = 1.75)
  if(i == "abiba"){my.sp = "A) balsam fir"}
  if(i == "picru"){my.sp = "B) red spruce"}
  if(i == "picgl"){my.sp = "C) white spruce"}
  if(i == "pinst"){my.sp = "D) white pine"}
  if(i == "aceru"){my.sp = "E) red maple"}
  if(i == "queru"){my.sp = "F) red oak"}
  legend("topright", my.sp, bty = "n")
}



for(i in c("abiba", "picru", "picgl", "pinst", "aceru", "queru")){
  temp = subset(dat, sp. == i)
  plot(total ~ site, data = temp, main = i, ylim = c(0, 60))
  
}

dat.m = summaryBy(total ~ sp. + site, data = dat, FUN = mean)
dat.m1 = subset(dat.m, site == "1")
dat.m4 = subset(dat.m, site == "4")
dat.mc = merge(dat.m1, dat.m4, by = c("sp."))
dat.mc$per.diff = ((dat.mc$total.mean.y - dat.mc$total.mean.x)/dat.mc$total.mean.y)*100
