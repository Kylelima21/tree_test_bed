mydir = "C:\\Users\\Jay Wason\\OneDrive - University of Maine System\\UMaine\\Research\\2020_Tree_test_bed\\2022_models_w_nick\\"
setwd(mydir)
library(lme4)
library(car)# for variance inflation factors
library(doBy)
library(AICcmodavg)#for aic tables


dat = read.csv(file = "TTB-All data compiled-RT-01 Dec2022.csv", header = T)
str(dat)
dat$Species = as.factor(dat$Species)
dat$ANP.Native = as.factor(dat$ANP.Native)
dat$Range.Gymno.Angio = as.factor(dat$Range.Gymno.Angio)
dat$Tree.ID = as.factor(dat$Tree.ID)
dat$Site = as.factor(dat$Site)
dat$Site_Tree.ID = as.factor(paste(dat$Site, dat$Tree.ID, sep = "_"))

#testing how this works
mymod = lmer(Relative.Height.... ~ Species + (1|Site), data = dat)
summary(mymod)
anova(mymod)

summary(dat$Relative.Height....)  
hist(dat$Relative.Height....)  

#some candidates
mymod1 = lmer(Relative.Height.... ~ 1 + (1|Site), data = dat)
mymod2 = lmer(Relative.Height.... ~ Species + (1|Site), data = dat)
mymod3 = lmer(Relative.Height.... ~ Range.Gymno.Angio + (1|Site), data = dat)
mymod4 = lmer(Relative.Height.... ~ ANP.Native + (1|Site), data = dat)

AIC(mymod1, mymod2, mymod3, mymod4)

#diameter models
mymod1 = lmer(Relative.Dia.... ~ 1 + (1|Site), data = dat)
mymod2 = lmer(Relative.Dia.... ~ Species + (1|Site), data = dat)
mymod3 = lmer(Relative.Dia.... ~ Range.Gymno.Angio + (1|Site), data = dat)
mymod4 = lmer(Relative.Dia.... ~ ANP.Native + (1|Site), data = dat)

AIC(mymod1, mymod2, mymod3, mymod4)

#total biomass
str(dat)
hist(log(dat$Total.Biomass..gm.))
mymod1 = lmer(log(Total.Biomass..gm.) ~ 1 + (1|Site), data = dat)
mymod2 = lmer(log(Total.Biomass..gm.) ~ Species + (1|Site), data = dat)
mymod3 = lmer(log(Total.Biomass..gm.) ~ Range.Gymno.Angio + (1|Site), data = dat)
mymod4 = lmer(log(Total.Biomass..gm.) ~ ANP.Native + (1|Site), data = dat)

AIC(mymod1, mymod2, mymod3, mymod4)

#Root to shoot ratio
hist(dat$Root.shoot.ratio)
temp= subset(dat, Root.shoot.ratio <=10)
hist(temp$Root.shoot.ratio)
hist(log(dat$Root.shoot.ratio))

mymod1 = lmer(log(Root.shoot.ratio) ~ 1 + (1|Site), data = dat)
mymod2 = lmer(log(Root.shoot.ratio) ~ Species + (1|Site), data = dat)
mymod3 = lmer(log(Root.shoot.ratio) ~ Range.Gymno.Angio + (1|Site), data = dat)
mymod4 = lmer(log(Root.shoot.ratio) ~ ANP.Native + (1|Site), data = dat)

AIC(mymod1, mymod2, mymod3, mymod4)

#Now for survival
summary(dat$Alive.or.Dead)
str(dat$Alive.or.Dead)
dat$ad.n = as.numeric(dat$Alive.or.Dead)
summary(dat$ad.n)

mymod1 = glmer(ad.n ~ 1 + (1|Site), family = "binomial", data = dat)
mymod2 = glmer(ad.n ~ Species + (1|Site), family = "binomial", data = dat)
mymod3 = glmer(ad.n ~ Range.Gymno.Angio + (1|Site), family = "binomial", data = dat)
mymod4 = glmer(ad.n ~ ANP.Native + (1|Site), family = "binomial", data = dat)

AIC(mymod1, mymod2, mymod3, mymod4)

#brainstorming about climate modeling
#leaning towards running a model selection process for each species
#and report the best/final model for each species
#Random effect structure stays the same
#so we just need to decide on starting climate and model reduction approach
#need to select final climate variables and remove based on VIF?
#MeanH2O, summerTmean, JanTmin, Lux
#^this are our starting point for modeling next time
#five response variables: relative height, relative diameter, total biomass,
#survival, and root to shoot ratio

#Time to bring in the climate data from Nicks summaries
#decided to do site averages
#then will check VIF and run model selection
climate = read.csv(file = "EnvironmentalVariables_3years.csv", header = T)
str(climate)
summary(climate)
clim.s = summaryBy(SummerTmean + JanTmin + Drought10 + Lux ~ Site,
                   data = climate, FUN = mean, na.rm = T)

dat2 = merge(dat, clim.s, by = "Site", all = T)
str(dat2)


plot(Relative.Height.... ~ SummerTmean.mean, data = dat2,
     pch = NA)
counter = 0
for (i in unique(levels(dat2$Species))){
  temp  = subset(dat2, Species == i)
  counter = counter + 1
  points(Relative.Height.... ~ jitter(SummerTmean.mean, 1), data = temp,
         pch = counter, col = counter)
  if(i != "lirtu"){
    mylm = lm(Relative.Height.... ~ SummerTmean.mean, data = temp)
    abline(mylm, col = counter)
  }
  
}

#check VIF?
temp = subset(dat2, Species == "robps")

mymod1 = lm(log(Relative.Height....) ~  SummerTmean.mean + JanTmin.mean + Drought10.mean, data = temp)
mymod2 = lm(log(Relative.Height....) ~  SummerTmean.mean + JanTmin.mean + Lux.mean, data = temp)
mymod3 = lm(log(Relative.Height....) ~  SummerTmean.mean + Lux.mean + Drought10.mean, data = temp)
mymod4 = lm(log(Relative.Height....) ~  Lux.mean + JanTmin.mean + Drought10.mean, data = temp)
mymod5 = lm(log(Relative.Height....) ~  JanTmin.mean + Drought10.mean, data = temp)
mymod6 = lm(log(Relative.Height....) ~  Lux.mean  + Drought10.mean, data = temp)
mymod7 = lm(log(Relative.Height....) ~  SummerTmean.mean + Drought10.mean, data = temp)
mymod8 = lm(log(Relative.Height....) ~  JanTmin.mean , data = temp)
AIC(mymod1, mymod2, mymod3, mymod4, mymod5, mymod6, mymod7, mymod8, mymod9)
plot(Relative.Height.... ~ Lux.mean, data = temp)

mymod2 = lm(log(Relative.Height....) ~  SummerTmean.mean + Drought10.mean, data = temp)
mymod3 = lm(log(Relative.Height....) ~ Drought10.mean, data = temp)
mymod4 = lm(log(Relative.Height....) ~  SummerTmean.mean, data = temp)
mymod5 = lm(log(Relative.Height....) ~  1, data = temp)
AIC(mymod1, mymod2, mymod3, mymod4, mymod5)

plot(Drought10.mean ~ SummerTmean.mean, data = temp)
plot(log(Relative.Height....) ~ SummerTmean.mean, 
     data = temp, cex = temp$Drought10.mean)


str(temp)
summary(mymod1)
anova(mymod1)
plot(Relative.Height.... ~ Drought10.mean, data = temp)

mymod1 = lm(Relative.Height.... ~ Site , data = temp)
mymod2 = lm(Relative.Height.... ~ SummerTmean.mean, data = temp)
AIC(mymod1, mymod2)

anova(mymod1)
vif(mymod1)
pairs(~ SummerTmean.mean + JanTmin.mean + meanH2O.mean + Lux.mean, data = temp)

######
#we are thinking of doing multiple linear models (no mixed effects)
#at the species level for all additive combinations of our 4 climate vars
#but only up to 2 predictors in the model max (no interactions)
#AIC selection on those ~10 options per species
#select most parsimonious model within 2 AIC of the lowest.

#okay, let's get this working for one species then loop through for the others and build an output table
dat.out = data.frame()
for(i in c(unique(levels(dat2$Species)))){
  #i = "aceru"
  if(i != "lirtu" & i != "pinta" & i != "pinvi"){
    temp = subset(dat2, Species == i)
    layout(matrix(c(1,2,3,4), 2, 2))
    plot(Relative.Height.... ~ SummerTmean.mean, data = temp, main = i)
    abline(lm(Relative.Height.... ~ SummerTmean.mean, data = temp))
    plot(Relative.Height.... ~ JanTmin.mean, data = temp, main = i)
    abline(lm(Relative.Height.... ~ JanTmin.mean, data = temp))
    plot(Relative.Height.... ~ Drought10.mean, data = temp, main = i)
    abline(lm(Relative.Height.... ~ Drought10.mean, data = temp))
    plot(Relative.Height.... ~ Lux.mean, data = temp, main = i)
    abline(lm(Relative.Height.... ~ Lux.mean, data = temp))
    
    mymod1 = lm((Relative.Height....) ~  SummerTmean.mean + JanTmin.mean, data = temp)
    mymod2 = lm((Relative.Height....) ~  SummerTmean.mean + Drought10.mean, data = temp)
    mymod3 = lm((Relative.Height....) ~  SummerTmean.mean + Lux.mean, data = temp)
    mymod4 = lm((Relative.Height....) ~  JanTmin.mean + Drought10.mean, data = temp)
    mymod5 = lm((Relative.Height....) ~  JanTmin.mean + Lux.mean, data = temp)
    mymod6 = lm((Relative.Height....) ~  Drought10.mean + Lux.mean, data = temp)
    mymod7 = lm((Relative.Height....) ~  SummerTmean.mean, data = temp)
    mymod8 = lm((Relative.Height....) ~  JanTmin.mean , data = temp)
    mymod9 = lm((Relative.Height....) ~  Drought10.mean, data = temp)
    mymod10 = lm((Relative.Height....) ~  Lux.mean , data = temp)
    mymod11 = lm((Relative.Height....) ~  1 , data = temp)
    
    myaic = data.frame(aictab(cand.set  = list(mymod1, mymod2, mymod3, mymod4, mymod5, mymod6, mymod7, mymod8, mymod9, mymod10, mymod11),
           second.ord = T, modnames = c("mymod1", "mymod2", "mymod3", "mymod4", "mymod5", "mymod6", "mymod7", "mymod8", "mymod9", "mymod10", "mymod11")))
    myaic.top = subset(myaic, Delta_AICc <= 2)
    myaic.top$species = i
    myaic.top$response = "log(rel.h)"
    dat.out = rbind(dat.out, myaic.top)
    print(i)
    temp2 = subset(temp, (!is.na(temp$Relative.Height....)))
    print("sample size:")
    print(length(temp2[,1]))
    print(myaic)
  }#loop that excludes lirtu (only one survivor), pinta (only at two sites), 
    #pinvi (at two sites, then only one individual at a third)
}

mymods.tab = data.frame(Modnames = c("mymod1", "mymod2", "mymod3", "mymod4", "mymod5", "mymod6", "mymod7", "mymod8", "mymod9", "mymod10", "mymod11"),
                        model = c("SumJan", "SumDro", "SumLux", "JanDro", "JanLux", "DroLux", "Sum", "Jan", "Dro", "Lux", "none"))

dat.out2 = merge(dat.out, mymods.tab, by = "Modnames")
write.csv(dat.out2, file = "aicmods_out.csv")
