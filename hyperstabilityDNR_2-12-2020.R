# looking at hyperstability of angling CPUE as a function of electrofishing CPUE
# 2-12-2020
# CLM, CJD, SEJ

setwd("~/Documents/Research/Fishscapes/hyperstability/hsSurvey/")
rm(list=ls())

# load function to load data from google drive
source("gdriveURL.R")
library(dplyr)


### Data Wrangle ####

######## angling CPUE
# load creel data from google drive
creel1=gdriveURL("https://drive.google.com/open?id=1lxUd742QZMXDQunyFBnENKMYZ1XNM_Pc")
creel2=gdriveURL("https://drive.google.com/open?id=1UYhbGH28WXjmi-4BzhfwO4KYwrBCNO2Q")
creel=rbind(creel1,creel2)

# reduce to columns we care about
creel=creel[,c(1,3,6,12,18,25:26,30,36,38)]

# calculate effort
# add zeroes to times with only 2 or 3 digits
creel$timeStart[nchar(creel$timeStart)==3]=paste("0",creel$timeStart[nchar(creel$timeStart)==3],sep="")
creel$timeStart[nchar(creel$timeStart)==2]=paste("00",creel$timeStart[nchar(creel$timeStart)==2],sep="")
creel$timeStart[creel$timeStart=="0"]="0000"
creel=creel[creel$timeStart!="1",]  # 4 entries with "1", so we don't know start time

creel$timeEnd[nchar(creel$timeEnd)==3]=paste("0",creel$timeEnd[nchar(creel$timeEnd)==3],sep="")
creel$timeEnd[nchar(creel$timeStart)==2]=paste("00",creel$timeEnd[nchar(creel$timeEnd)==2],sep="")
creel$timeEnd[creel$timeEnd=="0"]="0000"

creel$boatHrs=0
# remove rows when end time is less than start time (assumes the boat was out over midnight)
creel=creel[strptime(creel$timeEnd,format="%H%M")>=strptime(creel$timeStart,format="%H%M"),]
# calculate difference of time in hours for rows where end time is greater than start time (fishing occurred in one day only)
creel$boatHrs[strptime(creel$timeEnd,format="%H%M")>=strptime(creel$timeStart,format="%H%M")]=as.numeric(difftime(strptime(creel$timeEnd[strptime(creel$timeEnd,format="%H%M")>=strptime(creel$timeStart,format="%H%M")],format="%H%M"),strptime(creel$timeStart[strptime(creel$timeEnd,format="%H%M")>=strptime(creel$timeStart,format="%H%M")],format="%H%M"),units="hours"))

# removing rows with a non-zero notFishingAmt because we don't know what it means to be non-zero...
creel=creel[creel$notFishingAmt==0,]

# remove rows with non-integer anglersAmt 
creel=creel[!grepl(".",creel$anglersAmt,fixed=TRUE),]

# remove rows with anglersAmt above 10? (arbitrary choice for now)
creel=creel[creel$anglersAmt<=10,]

# get angler hours of effort from party size and boat hours
creel$anglerHrs=creel$boatHrs*creel$anglersAmt

# remove rows with no species code
creel=creel[!is.na(creel$fishSpeciesCode),]

# remove rows with NA for caughtAmt
creel=creel[!is.na(creel$caughtAmt),]

# remove no effort (anglerHrs==0) rows
creel=creel[creel$anglerHrs>0,]

# calculate angling CPUE
creel$anglingCPUE=creel$caughtAmt/creel$anglerHrs

# removing instances of CPUE >=30 (arbitrary...)
creel=creel[creel$anglingCPUE<30,]

# calculate average angling CPUE and sample size for each lake-year-species combination
lake_yearCPUE=creel %>%
              group_by(WBIC,fishSpeciesCode,surveyYear,anglerHrs,county) %>%
              summarize(meanCPUE=mean(anglingCPUE),
                        N=n())
lake_yearCPUE=as.data.frame(lake_yearCPUE)



####### electrofishing abundance
bassEF=gdriveURL("https://drive.google.com/open?id=11v8FbT2wnKx_CqUfxu_V9r_8fyCfcdD2")
bassEF=bassEF[,c(1,3,5,13,19,27:29)]
bassEF$CPEkm=bassEF$CPEmile/1.60934   # convert fish per mile to fish per km
bassEF$distanceShockedKm=bassEF$distanceShockedMiles*0.621371 # convert miles to km
lake_yearBASSef= bassEF %>%
  group_by(WBIC,species,surveyYear,county) %>%
  summarize(meanEF_CPEkm=mean(CPEkm),
            totalFishCaught=sum(totalNumberCaughtFish),
            totalDistShockedKm=sum(distanceShockedKm),
            totalHoursSampled=sum(numberHoursSampled),
            std=sd(CPEkm),
            N=n())
lake_yearBASSef=as.data.frame(lake_yearBASSef)


panEF=gdriveURL("https://drive.google.com/open?id=1QIqCBQ9gbOgRFUJQbnokwwTZJi5VZZIR")
panEF=panEF[,c(1,3,5,13,19,25:27)]
panEF$CPEkm=panEF$CPEmile/1.60934   # convert fish per mile to fish per km
panEF$distanceShockedKm=panEF$distanceShockedMiles*0.621371 # convert miles to km
lake_yearPANef= panEF %>%
  group_by(WBIC,species,surveyYear,county) %>%
  summarize(meanEF_CPEkm=mean(CPEkm),
            totalFishCaught=sum(totalNumberCaughtFish),
            totalDistShockedKm=sum(distanceShockedKm),
            totalHoursSampled=sum(numberHoursSampled),
            std=sd(CPEkm),
            N=n())
lake_yearPANef=as.data.frame(lake_yearPANef)

walleyeEF=gdriveURL("https://drive.google.com/open?id=1DPRROWv6Cf_fP6Z-kE9ZgUfdf_F_jSNT")
walleyeEF=walleyeEF[,c(1,3,5,13,19,23:24,27)]
walleyeEF$CPEkm=walleyeEF$CPEmile/1.60934   # convert fish per mile to fish per km
walleyeEF$distanceShockedKm=walleyeEF$distanceShockedMiles*0.621371 # convert miles to km
#remove commas from total fish caught
walleyeEF$totalNumberCaughtFish=as.numeric(gsub(",","",walleyeEF$totalNumberCaughtFish))
lake_yearWALLef= walleyeEF %>%
  group_by(WBIC,species,surveyYear,county) %>%
  summarize(meanEF_CPEkm=mean(CPEkm),
            totalFishCaught=sum(totalNumberCaughtFish),
            totalDistShockedKm=sum(distanceShockedKm),
            totalHoursSampled=sum(numberHoursSampled),
            std=sd(CPEkm),
            N=n())
lake_yearWALLef=as.data.frame(lake_yearWALLef)


##### merge data sets from angling CPUE and electrofishing CPUE to get exact lake-year matches
# convert fishSpeciesCode in lake_yearCPUE to species (name from ef stuff)
lake_yearCPUE$species=""
lake_yearCPUE$species[lake_yearCPUE$fishSpeciesCode=="X22"]="WALLEYE"
lake_yearCPUE$species[lake_yearCPUE$fishSpeciesCode=="W11"]="SMALLMOUTH BASS"
lake_yearCPUE$species[lake_yearCPUE$fishSpeciesCode=="W12"]="LARGEMOUTH BASS"
lake_yearCPUE$species[lake_yearCPUE$fishSpeciesCode=="X15"]="YELLOW PERCH"
lake_yearCPUE$species[lake_yearCPUE$fishSpeciesCode=="W14"]="BLACK CRAPPIE"
lake_yearCPUE$species[lake_yearCPUE$fishSpeciesCode=="W09"]="BLUEGILL"

# trim species without EF data (can we get other species EF data?)
lake_yearCPUE=lake_yearCPUE[lake_yearCPUE$species!="",]

bassJoin=left_join(lake_yearBASSef,lake_yearCPUE,by=c("WBIC"="WBIC","species"="species","surveyYear"="surveyYear","county"="county"))
bassJoin=bassJoin[!is.na(bassJoin$meanCPUE),]

panJoin=left_join(lake_yearPANef,lake_yearCPUE,by=c("WBIC"="WBIC","species"="species","surveyYear"="surveyYear","county"="county"))
panJoin=panJoin[!is.na(panJoin$meanCPUE),]


wallJoin=left_join(lake_yearWALLef,lake_yearCPUE,by=c("WBIC"="WBIC","species"="species","surveyYear"="surveyYear","county"="county"))
wallJoin=wallJoin[!is.na(wallJoin$meanCPUE),]

table(lake_yearCPUE$species)
nrow(lake_yearBASSef)
nrow(bassJoin)
nrow(lake_yearPANef)
nrow(panJoin)
nrow(lake_yearWALLef)
nrow(wallJoin)

table(lake_yearCPUE$surveyYear)
table(lake_yearBASSef$surveyYear)

library(ggplot2)

####### Hyperstability (abund and CPUE plots) #####

#ploting and colors based on wbic, for bass 1-4 plots
ggplot(data=bassJoin,aes(x=bassJoin$meanEF_CPEkm,y=bassJoin$meanCPUE))+
         geom_point(aes(color=WBIC))

#smooth line making trendline of observations
ggplot(data=bassJoin,aes(x=bassJoin$meanEF_CPEkm,y=bassJoin$meanCPUE))+
                           geom_smooth(model=lm)
#connecting with line 
ggplot(data=bassJoin,aes(x=bassJoin$meanEF_CPEkm,y=bassJoin$meanCPUE))+
  geom_line()

#color coded by LMB or SMB
ggplot(data=bassJoin,aes(x=bassJoin$meanEF_CPEkm,y=bassJoin$meanCPUE))+
  geom_point(aes(color=species))+theme(legend.position = "right")

#ploting for walleye population
ggplot(data=wallJoin,aes(x=wallJoin$meanEF_CPEkm,y=wallJoin$meanCPUE))+
  geom_smooth(model=lm)

ggplot(data=wallJoin,aes(x=wallJoin$meanEF_CPEkm,y=wallJoin$meanCPUE))+
  geom_line()

ggplot(data=wallJoin,aes(x=wallJoin$meanEF_CPEkm,y=wallJoin$meanCPUE))+
  geom_point()


#plotting for panfish species

ggplot(data=panJoin,aes(x=panJoin$meanEF_CPEkm,y=panJoin$meanCPUE))+
  geom_point(aes(color=species))+theme(legend.position = "right")

######## log values and model fits #######


#creating log CPUE and log N columns, removing na's and infinite values so glm can run and we can make fit
bassJoin$logCPUE=log(bassJoin$meanCPUE)
bassJoin$logAbun=log(bassJoin$meanEF_CPEkm)
bassJoin<- bassJoin[is.na(bassJoin$logCPUE)==F,]
bassJoin<- bassJoin[bassJoin$logCPUE!=-Inf,]
bassJoin<- bassJoin[is.na(bassJoin$logAbun)==F,]
bassJoin<- bassJoin[bassJoin$logAbun!=-Inf,]

wallJoin$logCPUE=log(wallJoin$meanCPUE)
wallJoin$logAbun=log(wallJoin$meanEF_CPEkm)
wallJoin<- wallJoin[wallJoin$logCPUE!=-Inf,]


panJoin$logCPUE=log(panJoin$meanCPUE)
panJoin$logAbun=log(panJoin$meanEF_CPEkm)
panJoin<- panJoin[panJoin$logCPUE!=-Inf,]


#making upper and lower confidence intervals using std and the mean to help with measuring betas
bassJoin$PE.ucl=bassJoin$std+bassJoin$meanCPUE
bassJoin$PE.lcl=bassJoin$std-bassJoin$meanCPUE

wallJoin$PE.ucl=wallJoin$std+wallJoin$meanCPUE
wallJoin$PE.lcl=wallJoin$std-wallJoin$meanCPUE

panJoin$PE.ucl=panJoin$std+panJoin$meanCPUE
panJoin$PE.lcl=panJoin$std-panJoin$meanCPUE

#join tables to compare species with lm model
LMBvsSMB<-lm(bassJoin$logCPUE~bassJoin$logAbun*bassJoin$species)
summary(LMBvsSMB)
#both are hyperstable but largemouth bass are more hyperstable 

LMBplusWall=rbind(bassJoin[bassJoin$species=="LARGEMOUTH BASS",],wallJoin)
#generate linear model to compare hyperstability of 
LMBvsWall<-lm(LMBplusWall$logCPUE~LMBplusWall$logAbun*LMBplusWall$species)
summary(LMBvsWall)
#Walleye statistically significant different from LMB hyperstability similar lines on fit

LMBplusPan=rbind(bassJoin[bassJoin$species=="LARGEMOUTH BASS",],panJoin)
LMBvsPan<-lm(LMBplusPan$logCPUE~LMBplusPan$logAbun*LMBplusPan$species)
summary(LMBvsPan)
#difference in hyperstability among panfish species and lmb, maybe check combinations

#bass vs blugegill hyperstability model fit
LMBplusBLG=rbind(bassJoin[bassJoin$species=="LARGEMOUTH BASS",],panJoin[panJoin$species=="BLUEGILL",])
LMBvsBLG<-lm(LMBplusBLG$logCPUE~LMBplusBLG$logAbun*LMBplusBLG$species)
summary(LMBvsBLG)#no

#plotting bass fit and LMBvsSMB fits
plot(x=bassJoin$logAbun,y=bassJoin$logCPUE)
abline(LMBvsSMB, col="red")
abline(fit1, col="blue")

#general linear model for bass, using glm function 
fit1<-glm(bassJoin$logCPUE~bassJoin$logAbun)
summary(fit1)#p value

#looking at relationship with county a few sig codes, need to check equation
BassCountyfit<-glm(bassJoin$logCPUE~bassJoin$logAbun+bassJoin$logAbun:bassJoin$county)
summary(BassCountyfit)

#ploting model with fit line bass log transformed abund. and CPUE
plot(x=bassJoin$logAbun,y=bassJoin$logCPUE)
abline(fit1)

#normal spcae plot of model fit to the data, exponential(intercept)*x^slope this is qN^B
#coefficients 2 is beta
plot(x=bassJoin$meanEF_CPEkm,y=bassJoin$meanCPUE)
plot(1:65,exp(fit1$coefficients[1])*(1:65)^fit1$coefficients[2])
#can use lines function as well

ggplot(bassJoin,aes(bassJoin$meanEF_CPEkm,bassJoin$meanCPUE))+
  geom_point(aes(colour = surveyYear))
ggplot(fit1,aes(bassJoin$meanEF_CPEkm,bassJoin$meanCPUE))+geom_smooth(model=lm)

#relationship with anglerHrs and bass
#bassEffortfit<-glm(bassJoin$logCPUE~bassJoin$logAbun+bassJoin$logAbun:bassJoin$anglerHrs)
#summary(bassEffortfit)#effort not independent

#relationship with anglerHrs and panfish
#panEffortfit<-glm(panJoin$logCPUE~panJoin$logAbun+panJoin$logAbun:panJoin$anglerHrs)
#summary(panEffortfit)#effort not independent

#relationship with anglerHrs and Walleye
#wallEffortfit<-glm(wallJoin$logCPUE~wallJoin$logAbun+wallJoin$logAbun:wallJoin$anglerHrs)
#summary(wallEffortfit)#effort not independent

#model for panfish, fit summary estimate 0.23189 
fit2<-glm(panJoin$logCPUE~panJoin$logAbun)
summary(fit2)#p value

#determining model fit, to check hyperstability for bluegill
BLGJoin=panJoin[panJoin$species=="BLUEGILL",]
BLGfit<-glm(BLGJoin$logCPUE~BLGJoin$logAbun)
summary(BLGfit)

#ploting model with fit line log trans. for panfish
plot(x=panJoin$logAbun,y=panJoin$logCPUE)
abline(fit2)

#normal spcae plot of model fit to the data, qN^B
#coefficients 2 is beta
plot(x=panJoin$meanEF_CPEkm,y=panJoin$meanCPUE)
plot(0:160,exp(fit2$coefficients[1])*(0:160)^fit2$coefficients[2])


# glmodel for walleye, fit summary estimate 0.63073
fit3<-glm(wallJoin$logCPUE~wallJoin$logAbun)
summary(fit3)#p value

#ploting model with fit line bass log transformed abund. and CPUE
plot(x=wallJoin$logAbun,y=wallJoin$logCPUE)
abline(fit3)

WallCountyFit<-glm(wallJoin$logCPUE~wallJoin$logAbun+wallJoin$logAbun:wallJoin$county)
summary(WallCountyFit)#nothing significant about county

#normal spcae plot of model fit to the data, exponential(intercept)*x^slope this is qN^B
#coefficients 2 is beta
plot(x=wallJoin$meanEF_CPEkm,y=wallJoin$meanCPUE)
plot(1:165,exp(fit3$coefficients[1])*(1:165)^fit3$coefficients[2],
     main="Hyperstability of Walleye",ylab = "angling CPUE",xlab = "ef CPUE")

### Ploting hyperstability ###
plot(x=1:165,y=exp(fit3$coefficients[1])*(1:165)^fit3$coefficients[2], col='darkgreen', type = "l",ylim = c(0,5),
     main = "model fits for hyperstability", xlab="effort", ylab = "catch")
lines(1:165,exp(fit1$coefficients[1])*(1:165)^fit1$coefficients[2],col="blue")
lines(1:165,exp(fit2$coefficients[1])*(1:165)^fit2$coefficients[2],col="red")
legend("topright",paste("Fit = ",c("LMB","Panfish","Walleye")), lty = 1:5, col = 1:5)
#levels of CPUE index

#using betaBootstrapping R script to calucate betas from model fit to simulated data

### Bootstrapping ####

#make d the dataframe you want, using bass as example
d=bassJoin
z=d[!duplicated(d$meanEF_CPEkm),]
agg_logCPUE=log(z$meanCPUE)
#wallJoin and panJoin agg_logCPUE has Inf value, needs to be removed for glm fit 
agg_logN=log(z$meanEF_CPEkm)
aggFit_wLK=glm(agg_logCPUE~agg_logN)
#estimate agg fit glm summary 0.42386
summary(aggFit_wLK)

betas=numeric(1000) #betas from model fit to simulated data
ps=numeric(1000) #difference in AIC values between the simulated data model fit and the experimental data model fit


for(i in 1:1000){
  pe=rlnorm(n=length(z$meanEF_CPEkm), meanlog = log(z$meanEF_CPEkm))
  #rlnorm from chpt 14-45 of RMark book
  fit=glm(agg_logCPUE ~ log(pe)+z$meanCPUE)
  betas[i]=fit$coefficients[2]
  comp=abs(fit$aic - aggFit_wLK$aic)
  ps[i]=comp
}
plot(betas, ps)
hist(betas, main = "bass betas")
hist(ps)

#pulling in CWH info/linfo join with WBIC, keep all rows and fill in NA where its missing, dpplyr left join (bass,wood)
#literature review for building density vilas co., anna marburg
#try model with dummy variable to compare species to each other fit

#lm(loganCPUE~logefCPUE*species), efCPUE + species + efCPUE:species
#B0 + b1efCPUE + B2*species +B3efCPUE scpeies

####  models with building densities ####

#bringing in buildling density data
buildDensity2018=gdriveURL("https://drive.google.com/open?id=11lPPduqiXIxz00fm6xxFzUA8u9nCOBnN")

#joining building density to bass catch + abund info
bassbuildJoin=left_join(bassJoin,buildDensity2018,by="WBIC")
bassbuildJoin=bassbuildJoin[!is.na(bassbuildJoin$buildingCount50m),]
#only 28 unique ones, 94 entries with wbics that have lake density estimates (about 25%)

#building density numbers for walleye lake yr observations
wallbuildJoin=left_join(wallJoin,buildDensity2018,by="WBIC")
wallbuildJoin=wallbuildJoin[!is.na(wallbuildJoin$buildingCount50m),]
#104 observations with walleye info + building density

#builing density numbers for panfish lake yr observations
panbuildJoin=left_join(panJoin,buildDensity2018,by="WBIC")
panbuildJoin=panbuildJoin[!is.na(panbuildJoin$buildingCount50m),]
#44 observations

#bringing in ntl building density data from 2001-2004
NTLBuild<- read.csv("NTLBuildDensData(2001-2004).csv")

#fixing column names to join tables 
NTLBuild$WBIC=NTLBuild$wbic
NTLBuild$surveyYear=NTLBuild$survey_year
NTLBuild<-NTLBuild[,c(1:4,8:34)]

#joining building density to bass catch + abund info
bassbuildJoin=left_join(bassJoin,NTLBuild, by="WBIC","surveyYear")
bassbuildJoin=bassbuildJoin[!is.na(bassbuildJoin$buildings_per_km),]
#only 24 observations left of 62

#building density numbers for walleye lake yr observations
wallbuildJoin=left_join(wallJoin,NTLBuild,by="WBIC","surveyYear")
wallbuildJoin=wallbuildJoin[!is.na(wallbuildJoin$buildings_per_km),]
#28 observations left of 62

#builing density numbers for panfish lake yr observations
panbuildJoin=left_join(panJoin,NTLBuild,by="WBIC","surveyYear")
panbuildJoin=panbuildJoin[!is.na(panbuildJoin$buildings_per_km),]
#only 12 observations left of 62

#model fits building density from ntl

fit4<-glm(bassbuildJoin$logCPUE~bassbuildJoin$logAbun+bassbuildJoin$logAbun:bassbuildJoin$buildings_per_km)
summary(fit4)#nothing significant 

fit4.1<-glm(bassbuildJoin$logCPUE~bassbuildJoin$logAbun+bassbuildJoin$logAbun:bassbuildJoin$buildings_per_km_quintile)
summary(fit4.1)#small signif.

#looking at residuals as a function of building density,look at relationship between beta and density
VilasBassFit<-glm(bassbuildJoin$logCPUE~bassbuildJoin$logAbun+bassbuildJoin$logAbun:bassbuildJoin$buildingCount200m)
plot(bassbuildJoin$buildingDensity200m,residuals(VilasBassFit))
VilasBassFit<-glm(bassbuildJoin$logCPUE~bassbuildJoin$logAbun+bassbuildJoin$logAbun:bassbuildJoin$buildings_per_km_quintile)
plot(bassbuildJoin$buildings_per_km_quintile,residuals(VilasBassFit))
residuals(VilasBassFit)


fit5<-glm(panbuildJoin$logCPUE~panbuildJoin$logAbun+panbuildJoin$logAbun:panbuildJoin$buildingDensity200m)
summary(fit5)#look at p value
VilasPanFit<-glm(panbuildJoin$logCPUE~panbuildJoin$logAbun)
plot(panbuildJoin$buildingDensity200m,residuals(VilasPanFit))
fit5<-glm(panbuildJoin$logCPUE~panbuildJoin$logAbun+panbuildJoin$logAbun:panbuildJoin$buildings_per_km)
summary(fit5)#not significant

fit6<-glm(wallbuildJoin$logCPUE~wallbuildJoin$logAbun+wallbuildJoin$logAbun:wallbuildJoin$buildingDensity200m)
summary(fit6)#look at p value
VilasWallFit<-glm(wallbuildJoin$logCPUE~wallbuildJoin$logAbun)
plot(wallbuildJoin$buildingDensity200m,residuals(VilasWallFit), 
     main="relationship between walleye betas and building density", ylab="Residuals")
fit5.1<-glm(panbuildJoin$logCPUE~panbuildJoin$logAbun+panbuildJoin$logAbun:panbuildJoin$buildings_per_km)
summary(fit5.1)#not sig.

fit6<-glm(wallbuildJoin$logCPUE~wallbuildJoin$logAbun+wallbuildJoin$logAbun:wallbuildJoin$buildings_per_km)
summary(fit6)#not sig. netiher building quintile

#VilasWallFit<-glm(wallbuildJoin$logCPUE~wallbuildJoin$logAbun)
#plot(wallbuildJoin$buildingDensity200m,residuals(VilasWallFit), 
#    main="relationship between walleye betas and building density", ylab="Residuals")

#### Models with CWH density ####

#bringing in CWH data from ntl data 2001-2004
ntlCWH=read.csv("ntl125_2_v1_0.csv")

#bringing in coarse woody habitat estimates from Jake Ziegler data from YOY mort. study
CWHdensity=gdriveURL("https://drive.google.com/open?id=1x1_JdeamiU2auqrlPQ3G_wA6Spuf0vwf")
#only 61 observations*

#seeing how many lake yr for a species have supplemental CWH denisty info

bassCWHJoin=left_join(bassJoin,CWHdensity,by="WBIC")
bassCWHJoin=bassCWHJoin[!is.na(bassCWHJoin$Total.CWH.per.km.shoreline),]
#not all shoreline data points have wood data, still only 23 observations

wallCWHJoin=left_join(wallJoin,CWHdensity,by="WBIC")
wallCWHJoin=wallCWHJoin[!is.na(wallCWHJoin$Total.CWH.per.km.shoreline),]
#only 27 observations for wall lakes with CWH density info

panCWHJoin=left_join(panJoin,CWHdensity,by="WBIC")
panCWHJoin=panCWHJoin[!is.na(panCWHJoin$Total.CWH.per.km.shoreline),]
#only 12 big yikes

#checking which lakes have CWH and buidling density info for a given species

bassbuildCWHJoin=left_join(bassbuildJoin,CWHdensity,by="WBIC")
bassbuildCWHJoin=bassbuildCWHJoin[!is.na(bassbuildCWHJoin$CWH.greater.than.10cm.per.km.shoreline),]
#trimming table for values with measurements for CWH info and building density, only 23 obs
panbuildCWHJoin=left_join(panbuildJoin,CWHdensity,by="WBIC")
panbuildCWHJoin=panbuildCWHJoin[!is.na(panbuildCWHJoin$CWH.greater.than.10cm.per.km.shoreline),]

wallbuildCWHJoin=left_join(wallbuildJoin,CWHdensity,by="WBIC")
wallbuildCWHJoin=wallbuildCWHJoin[!is.na(wallbuildCWHJoin$CWH.greater.than.10cm.per.km.shoreline),]

#model fits, fix to plots with residuals and CWH interactions

fit7<-glm(bassbuildCWHJoin$logCPUE~bassbuildCWHJoin$logAbun+bassbuildCWHJoin$logAbun:bassbuildCWHJoin$Total.CWH.per.km.shoreline)
summary(fit7)#not significant
CWHBassFit<-glm(bassbuildCWHJoin$logCPUE~bassbuildCWHJoin$logAbun)
plot(bassbuildCWHJoin$Total.CWH.per.km.shoreline,residuals(CWHBassFit))

fit8<-glm(panCWHJoin$logCPUE~panCWHJoin$logAbun+panCWHJoin$logAbun:panCWHJoin$Total.CWH.per.km.shoreline)
summary(fit8)#not significant
CWHPanFit<-glm(panCWHJoin$logCPUE~panCWHJoin$logAbun)
plot(panCWHJoin$Total.CWH.per.km.shoreline,residuals(CWHPanFit))


fit9<-glm(wallCWHJoin$logCPUE~wallCWHJoin$logAbun+wallCWHJoin$logAbun:wallCWHJoin$Total.CWH.per.km.shoreline)
summary(fit9)#not significant 
CWHWallFit<-glm(wallbuildCWHJoin$logCPUE~wallbuildCWHJoin$logAbun)
plot(wallbuildCWHJoin$Total.CWH.per.km.shoreline,residuals(CWHWallFit))



#not significant, small obs. number for all, building density may be better metric

#some cwh data avaliable online for yrs 2001-2004, unable to see yr/date of obs,
#https://lter.limnology.wisc.edu/dataset/biocomplexity-north-temperate-lakes-lter-coordinated-field-studies-riparian-plots-2001-2004
#probably will stick with fishscapes buildin density + CWH data

CwhNTL<-read.csv("CwhNTL2001-2004.csv")
#no wbics, will come back to it 
#will need to focus on lakeshore development data over CWH

#bringing in CWH data from ntl data 2001-2004
ntlCWH=read.csv("ntl125_2_v1_0.csv")

#reducing columns to lake name, lake id, log present
ntlCWH<-ntlCWH[,c(1,2,9)]

#assigning numeric values to logs present
ntlCWH=ntlCWH %>% 
  mutate(numLog=recode(ntlCWH$type,"LOG"=1))

#replacing NA's with 0 in the numLog column
ntlCWH=ntlCWH[is.na(ntlCWH$numLog)==FALSE,]

#summing observations by lake name
lakeCWH=aggregate(ntlCWH$numLog,by=list(lakename=ntlCWH$lakename),FUN=sum)

##using linfo to add WBICS to CWH counts
lakeName<-linfo[,c(1,2,14)]
lakeNameVilas<-lakeName[lakeName$county=="Vilas",]
lakeNameVilas<-lakeNameVilas[,c(1,2)]
colnames(lakeNameVilas)<-c("WBIC","lakename")

lakeCWHVilas<-left_join(lakeCWH,lakeNameVilas,by="lakename")

#calculating Log/KMshoreline as CWH density
for(i in (1:nrow(lakeCWHVilas))){
  lakeCWHVilas$CWHkm[i]<-lakeCWHVilas$x[i]/0.4
}

VilasCWHperKM<-lakeCWHVilas[,c(3,4)]

#joining to tables 
bassCWHJoin=left_join(bassJoin,VilasCWHperKM,by="WBIC")
bassCWHJoin=bassCWHJoin[!is.na(bassCWHJoin$CWHkm),]
bassCWHJoin=bassCWHJoin[bassCWHJoin$surveyYear==c(2001:2004),] #ONLY 8 Obs.
#647 Observations

wallCWHJoin=left_join(wallJoin,VilasCWHperKM,by="WBIC")
wallCWHJoin=wallCWHJoin[!is.na(wallCWHJoin$CWHkm),]
wallCWHJoin=wallCWHJoin[wallCWHJoin$surveyYear==c(2001:2004),] #ONLY 4 Obs.
#1811 observations

panCWHJoin=left_join(panJoin,VilasCWHperKM,by="WBIC")
panCWHJoin=panCWHJoin[!is.na(panCWHJoin$CWHkm),]
#287 observations

#model fits
#Bass
CWHkmBassFit<-glm(bassCWHJoin$logCPUE~bassCWHJoin$logAbun+bassCWHJoin$logAbun:bassCWHJoin$CWHkm)
summary(CWHkmBassFit)
#Walleye
CWHkmWallFit<-glm(wallCWHJoin$logCPUE~wallCWHJoin$logAbun+wallCWHJoin$logAbun:wallCWHJoin$CWHkm)
summary(CWHkmWallFit)
#Pan
CWHkmPanFit<-glm(panCWHJoin$logCPUE~panCWHJoin$logAbun+panCWHJoin$logAbun:panCWHJoin$CWHkm)
summary(CWHkmPanFit)

#check year number for observations

#look at the relationship with county
library(ggplot2)
table(bassJoin$county)
plot(table(bassJoin$county),ylab= "# of observations (surveyyr)",xlab="county", type = "h",
     lwd=4,ylim = c(0,60), cex.axis=0.6)
#relationship between hypersta. and county
CountyBassFit<-glm(bassJoin$logCPUE~bassJoin$logAbun+bassJoin$logAbun:bassJoin$county)
summary(CountyBassFit)

#relationship between county and effort???
COeffortBass<-glm(bassJoin$meanEF_CPEkm~bassJoin$county)
summary(COeffortBass)

CountyWallFit<-glm(wallJoin$logCPUE~wallJoin$logAbun+wallJoin$logAbun:wallJoin$county)
summary(CountyWallFit)

#nothing stands out

#### Linfo ####

#Relationship between hyperstability (catch vs abundance) and lake characterisitcs
#bring in lake fishscapes data
linfo<-gdriveURL("https://drive.google.com/open?id=1ot9rEYnCG07p7aUxbeqN2mJ3cNrzYA0Y")
linfo=linfo[,1:13]
#join fish info with lake info using the waterbody codes
bassLinfo<-left_join(bassJoin,linfo,by="WBIC")
wallLinfo<-left_join(wallJoin,linfo,by="WBIC")
panLinfo<-left_join(panJoin,linfo,by="WBIC")
BLGJoinLinfo<-left_join(BLGJoin,linfo,by="WBIC")

#model fits, testing out bass first
#lake depth
BassLDepth<-glm(bassLinfo$logCPUE~bassLinfo$logAbun+bassLinfo$logAbun:bassLinfo$maxDepth)
summary(BassLDepth)#no
#lake size
BassLsize<-glm(bassLinfo$logCPUE~bassLinfo$logAbun+bassLinfo$logAbun:bassLinfo$sizeAcres)
summary(BassLsize)#no
#lake type
BassLtype<-glm(bassLinfo$logCPUE~bassLinfo$logAbun+bassLinfo$logAbun:bassLinfo$lakeType)
summary(BassLtype)#no
#water clarity
BassLclar<-glm(bassLinfo$logCPUE~bassLinfo$logAbun+bassLinfo$logAbun:bassLinfo$waterClarity)
summary(BassLclar)#no

#will check for walleye
#lake depth
WallLDepth<-glm(wallLinfo$logCPUE~wallLinfo$logAbun+wallLinfo$logAbun:wallLinfo$maxDepth)
summary(WallLDepth)#no
#lake size
WallLsize<-glm(wallLinfo$logCPUE~wallLinfo$logAbun+wallLinfo$logAbun:wallLinfo$sizeAcres)
summary(WallLsize)#no
#lake type
WallLtype<-glm(wallLinfo$logCPUE~wallLinfo$logAbun+wallLinfo$logAbun:wallLinfo$lakeType)
summary(WallLtype)#no
#water clarity
WallLclar<-glm(bassLinfo$logCPUE~bassLinfo$logAbun+bassLinfo$logAbun:bassLinfo$waterClarity)
summary(WallLclar)#no

length(unique(bassbuildJoin$WBIC))
#add in fishscapes lake data and run the same models

library(readxl)
LMB2019 <- read_excel("pe2019-CM-3-5-20.xlsx")
View(LMB2019)
#plotting catch vs abundance
#note: totalCPETime = totalNum/timeEffort, totalCPEDistMI=totalNum/distEffortmi, 
#totalCPEDistKM=totalNum/distEffortkm
library(ggplot2)
ggplot(LMB2019, aes(x=nHat,y=totalCPEDistKM))+geom_point()
ggplot(LMB2019, aes(x=nHat,y=totalCPETime))+geom_point()

Fishfit<-glm(LMB2019$totalCPETime~LMB2019$nHat)
summary(Fishfit)

LMB2018<-gdriveURL("https://drive.google.com/open?id=11YqL34QNdqwg59TdKAXL0t2sYd2yN7GY")

#make survey year column for 2018-19 fish data then join tables
bassJoin$PE.ucl=bassJoin$std+bassJoin$meanCPUE
LMB2018$surveyYear=2018
LMB2019$surveyYear=2019

#making effort columns with the same names to join columns and datasets
LMB2019$fishPerKmShoreline=LMB2019$distEffortkm
LMB=full_join(LMB2018[,2:14],LMB2019[,2:16])

LMBfit<-glm(LMB$distEffortkm~LMB$nHat)
summary(LMBfit)


