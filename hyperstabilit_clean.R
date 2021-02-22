#Cleaned up hyperstability survey code 
#looking at hyperstability of angling CPUE as a function of electrofishing CPUE
# 2-12-2020
# CLM, CJD, SEJ

setwd("~/Documents/Research/Fishscapes/hyperstability/hsSurvey/")

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

Bcreel=creel[creel$fishSpeciesCode==c("W11","W12"),]
Wcreel=creel[creel$fishSpeciesCode=="X22",]
Pcreel=creel[creel$fishSpeciesCode==c("X15","W14","W09"),]

# calculate average angling CPUE and sample size for each lake-year-species combination
lake_yearCPUE=creel %>%
  group_by(WBIC,fishSpeciesCode,surveyYear,county) %>%
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

bassJoin=left_join(lake_yearBASSef,lake_yearCPUE,by=c("WBIC"="WBIC","species"="species","surveyYear"="surveyYear", "county"="county"))
bassJoin=bassJoin[!is.na(bassJoin$meanCPUE),]

panJoin=left_join(lake_yearPANef,lake_yearCPUE,by=c("WBIC"="WBIC","species"="species","surveyYear"="surveyYear", "county"="county"))
panJoin=panJoin[!is.na(panJoin$meanCPUE),]


wallJoin=left_join(lake_yearWALLef,lake_yearCPUE,by=c("WBIC"="WBIC","species"="species","surveyYear"="surveyYear", "county"="county"))
wallJoin=wallJoin[!is.na(wallJoin$meanCPUE),]

table(lake_yearCPUE$species)
nrow(lake_yearBASSef)
nrow(bassJoin)
nrow(lake_yearPANef)
nrow(panJoin)
nrow(lake_yearWALLef)
nrow(wallJoin)


### Quantifying hyperstability ####

#creating log CPUE and log N columns, removing na's and infinite values so glm can run and we can make fit
bassJoin$logCPUE=log(bassJoin$meanCPUE)
bassJoin$logAbun=log(bassJoin$meanEF_CPEkm)
bassJoin<- bassJoin[is.na(bassJoin$logCPUE)==F,]
bassJoin<- bassJoin[is.na(bassJoin$logAbun)==F,]
bassJoin<- bassJoin[bassJoin$logCPUE!=-Inf,]

wallJoin$logCPUE=log(wallJoin$meanCPUE)
wallJoin$logAbun=log(wallJoin$meanEF_CPEkm)
wallJoin<- wallJoin[wallJoin$logCPUE!=-Inf,]

panJoin$logCPUE=log(panJoin$meanCPUE)
panJoin$logAbun=log(panJoin$meanEF_CPEkm)
panJoin<- panJoin[panJoin$logCPUE!=-Inf,]


#combining all fish species to one lm

BWJoin=full_join(bassJoin,wallJoin)
BWPJoin=full_join(BWJoin,panJoin)

BWPFit<-glm(BWPJoin$logCPUE~BWPJoin$logAbun)
summary(BWPFit)#beta less than 1 hyperstable, signficant

#Species relationship with hyperstability 

BWPspeciesFit<-lm(BWPJoin$logCPUE~BWPJoin$logAbun+BWPJoin$logAbun:BWPJoin$species)
summary(BWPspeciesFit)#signficantly different slopes/betas, species relationship with hyperstability present

#general linear model for bass, using lm function 
fit1<-lm(bassJoin$logCPUE~bassJoin$logAbun)
summary(fit1)


#model for panfish
fit2<-lm(panJoin$logCPUE~panJoin$logAbun)
summary(fit2)

# model for walleye
fit3<-lm(wallJoin$logCPUE~wallJoin$logAbun)
summary(fit3)


### Build Density relationship with hyperstability ####


#add in Building density csv from GIS files from Hsbuild github repo on joneslabND
buildDens<-read.csv("HSbuildingsSummary_yrs.csv")
#formatting to join columns 
buildDens$WBIC=buildDens$wbic
buildDens=buildDens[,2:12]


#join with fish-lake yr data for all species
BWPbuild=left_join(BWPJoin, buildDens, by="WBIC","county")
BWPbuild=BWPbuild[!is.na(BWPbuild$buildingCount100m),]
BWPbuild=BWPbuild[!is.na(BWPbuild$meanEF_CPEkm),]


#looking at the relationship between catch abundance and building density 

BWPbuildFit<-glm(BWPbuild$logCPUE~BWPbuild$logAbun+BWPbuild$logAbun:BWPbuild$buildingDensity200m)
summary(BWPbuildFit)#overall no

#relationship between catch abun building density and species 
BWPbuildspecies<-glm(BWPbuild$logCPUE~BWPbuild$logAbun+BWPbuild$logAbun:BWPbuild$species+BWPbuild$logAbun:BWPbuild$buildingDensity200m+
                       BWPbuild$logAbun:BWPbuild$species:BWPbuild$buildingDensity200m)
summary(BWPbuildspecies)#no signficance

#join with fish-lake yr data for bass
Bassbuild=left_join(bassJoin, buildDens, by="WBIC","county")
Bassbuild=Bassbuild[!is.na(Bassbuild$buildingCount100m),]
Bassbuild=Bassbuild[!is.na(Bassbuild$meanEF_CPEkm),]
BassbuildFit200<-glm(Bassbuild$logCPUE~Bassbuild$logAbun+Bassbuild$logAbun:Bassbuild$buildingCount200m)
summary(BassbuildFit200)#no relationship

#Walleye subset
Wallbuild=left_join(wallJoin, buildDens, by="WBIC","county")
Wallbuild=Wallbuild[!is.na(Wallbuild$buildingCount100m),]
Wallbuild=Wallbuild[!is.na(Wallbuild$meanEF_CPEkm),]
WallbuildFit200<-glm(Wallbuild$logCPUE~Wallbuild$logAbun+Wallbuild$logAbun:Wallbuild$buildingDensity200m)
summary(WallbuildFit200)#sig for all building denisties

#Panfish subset
Panbuild=left_join(panJoin, buildDens, by="WBIC","county")
Panbuild=Panbuild[!is.na(Panbuild$buildingCount100m),]
Panbuild=Panbuild[!is.na(Panbuild$meanEF_CPEkm),]
PanbuildFit200<-glm(Panbuild$logCPUE~Panbuild$logAbun+Panbuild$logAbun:Panbuild$buildingCount200m)
summary(PanbuildFit200)#no relationship


### Lake characteristics and hyperstability ####


#add in lake characterisitcs
linfo<-gdriveURL("https://drive.google.com/open?id=1ot9rEYnCG07p7aUxbeqN2mJ3cNrzYA0Y")
linfo=linfo[,1:13]

#joing linfo with all observations
BWPlinfo<-left_join(BWPJoin, linfo,by="WBIC")

# max depth
BWPDepth<-glm(BWPlinfo$logCPUE~BWPlinfo$logAbun+BWPlinfo$logAbun:BWPlinfo$maxDepth)
summary(BWPDepth)#no 
#lake size
BWPLsize<-glm(BWPlinfo$logCPUE~BWPlinfo$logAbun+BWPlinfo$logAbun:BWPlinfo$sizeAcres)
summary(BWPLsize)#no
#lake type
BWPLtype<-glm(BWPlinfo$logCPUE~BWPlinfo$logAbun+BWPlinfo$logAbun:BWPlinfo$lakeType)
summary(BWPLtype)#no
#water clarity
BWPLclar<-glm(BWPlinfo$logCPUE~BWPlinfo$logAbun+BWPlinfo$logAbun:BWPlinfo$waterClarity)
summary(BWPLclar)#no
#mean depth
BWPLmDepth<-glm(BWPlinfo$logCPUE~BWPlinfo$logAbun+BWPlinfo$logAbun:BWPlinfo$meanDepth)
summary(BWPLmDepth)#no
#survey year
BWPLyear<-glm(BWPlinfo$logCPUE~BWPlinfo$logAbun+BWPlinfo$logAbun:BWPlinfo$surveyYear)
summary(BWPLyear)#significant difference in beta, pos effect less hyperstability due to survey year
#county
BWPcounty<-glm(BWPlinfo$logCPUE~BWPlinfo$logAbun+BWPlinfo$logAbun:BWPlinfo$county)
summary(BWPcounty)#some counties 

######## BETA W MODEL #######
library("png")
library("grid")
library("ggplot2")
bass_background<- png::readPNG("bass_background.png")
#ggplot(bassJoin,aes(bassJoin$ef_CPEkm,bassJoin$meanCPUE))+
  #ggtitle("Hyperstability of Bass (Micropterus salmoides)")+
  #annotation_custom(rasterGrob(bass_background, 
                               #width = unit(1,"npc"), 
                               #height = unit(1,"npc")), 
                   # -Inf, Inf, -Inf, Inf)+ 
  #geom_point(color="gray")


par(mfrow=c(1,3))#make 3 columns to place plots 
plot(x=bassJoin$meanEF_CPEkm,y=bassJoin$meanCPUE, col="gray", main = "Bass (Micropterus salmoides)",
     xlab="Fish Abundance (ef_CPEkm)", ylab="Catch rate (mean CPUE)")
#intercept from glm is q (cathability), slope from fit is Beta
nonlogfit1=exp(-1.36302)*bassJoin$meanEF_CPEkm^( 0.34553)
#coefficient 1 is the intercept, 
lines(0:327,exp(fit1$coefficients[1]+0.03770)*(0:327)^fit1$coefficients[2]+0.02108,lwd=1, col="blue")
lines(0:327,exp(fit1$coefficients[1]-0.03770)*(0:327)^fit1$coefficients[2]-0.02108,lwd=1, col="blue")
lines(0:327,exp(fit1$coefficients[1])*(0:327)^fit1$coefficients[2],lwd=3)
#legend("topright",paste("Beta = ",("0.35")), col="black")

#same plot but in ggplot
ggplot(bassJoin, aes(x=meanEF_CPEkm, y=meanCPUE))+geom_point()+theme_classic()+geom_smooth(method = 'glm')



#geom_abline(aes(x=1:327,y=exp(fit1$coefficients[1]+0.03770)*(0:327)^fit1$coefficients[2]+0.02108))

plot(x=panJoin$meanEF_CPEkm,y=panJoin$meanCPUE, col="gray", main = "Panfish (Lepomis,Pomoxis,and Perca flavescens)",xlab="Fish Abundance (ef_CPEkm)", ylab="Catch rate (mean CPUE)")
#intercept from glm is q (cathability), slope from fit is Beta
#coefficient 1 is the intercept, 
lines(0:327,exp(fit2$coefficients[1]+0.0547)*(0:327)^fit2$coefficients[2]+0.02704,lwd=1, col="blue")
lines(0:327,exp(fit2$coefficients[1]-0.0547)*(0:327)^fit2$coefficients[2]-0.02704,lwd=1, col="blue")
lines(0:327,exp(fit2$coefficients[1])*(0:327)^fit2$coefficients[2],lwd=3)

plot(x=wallJoin$meanEF_CPEkm,y=wallJoin$meanCPUE, col="gray", main = "Walleye (Sander vitreus)",xlab="Fish Abundance (ef_CPEkm)", ylab="Catch rate (mean CPUE)")
#intercept from glm is q (cathability), slope from fit is Beta
#coefficient 1 is the intercept, 
lines(0:327,exp(fit3$coefficients[1]+0.11398)*(0:327)^fit3$coefficients[2]+0.04235,lwd=1, col="blue")
lines(0:327,exp(fit3$coefficients[1]-0.11398)*(0:327)^fit3$coefficients[2]-0.04235,lwd=1, col="blue")
lines(0:327,exp(fit3$coefficients[1])*(0:327)^fit3$coefficients[2],lwd=3)

### Figures ####

#looking at different number of obs per species
ggplot(BWPJoin, aes(x=meanEF_CPEkm, y=meanCPUE))+geom_point(aes(col=species))+facet_wrap(vars(species))+labs(x="Fish density (efCPUE)", y="Angling CPUE", title = "Catch rate vs Abundance observations by species")+theme_classic()

#looking at different number of observations over the years
ggplot(BWPJoin, aes(x=meanEF_CPEkm, y=meanCPUE))+geom_point()+facet_wrap(vars(surveyYear))+labs(x="Fish density (efCPUE)", y="Angling CPUE",
                                                                                                title = "Wisconsin DNR fish data CPUE calculations from 1995-2016")+theme_classic()
#plotting fits of all the different species together
plot(x=1:165,y=exp(fit1$coefficients[1])*(1:165)^fit1$coefficients[2], col='blue', type = "l",ylim = c(0,5),
     main = "Hyperstability of fish Species in WI", xlab="Fish Abundance (efCPUE)", ylab = "Angling CPUE")
lines(1:165,exp(fit2$coefficients[1])*(1:165)^fit2$coefficients[2],col="red")
lines(1:165,exp(fit3$coefficients[1])*(1:165)^fit3$coefficients[2],col="darkgreen")
legend("topright",paste("Fit = ",c("LMB","Panfish","Walleye")), lty = 1:5, col = 1:5)

#Walleye PE vs efCPUE

#bringing in walleye PEs from 2016 DNR survey to check efCPUE proxy for fish density 
WalleyePE_2016=gdriveURL("https://drive.google.com/open?id=18e5zP5e5PuuCbeN0ShlWFMlcPgAbB4TV")
WalleyePE_2016<-rename(WalleyePE_2016, WBICdate=ï.., WBIC=X , county=X.1, Lake=X.2, acres=X.3, surveyYear=X.4, adultwalleyePop=X.6, WalleyeDensity=X.7)
WalleyePE_2016<-WalleyePE_2016[2:152,c(2:6,8:9)]

WalleyePE_2016$surveyYear<-as.numeric(WalleyePE_2016$surveyYear)
WalleyePE_2016$WBIC<-as.numeric(WalleyePE_2016$WBIC)
WalleyePE_2016$WalleyeDensity<-as.numeric(WalleyePE_2016$WalleyeDensity)
WalleyePE_2016$adultwalleyePop<-as.numeric(WalleyePE_2016$adultwalleyePop)

#join walleye PE data with efCPUE walleye data, values in PE_2016 are integers
WallPE<-full_join(lake_yearWALLef,WalleyePE_2016,by=c("WBIC","surveyYear"))
WallPE<-WallPE[!is.na(WallPE$WalleyeDensity),]

woTurtles<-WallPE[WallPE$adultwalleyePop<30000,]


#log Transform 
woTurtles$logCPUE=log(woTurtles$meanEF_CPEkm)
woTurtles$logAbun=log(woTurtles$adultwalleyePop)
woTurtles$logDens=log(woTurtles$WalleyeDensity)

fit1<-glm(WallPE$logCPUE~WallPE$logAbun)
summary(fit1)

fit2<-glm(woTurtles$logCPUE~woTurtles$logAbun)
summary(fit2)

fit3<-glm(woTurtles$logCPUE~woTurtles$logDens)
summary(fit3)

plot(fit1)

plot(x=WallPE$logAbun,y=WallPE$logCPUE)
abline(fit1)

plot(x=woTurtles$adultwalleyePop,y=woTurtles$meanEF_CPEkm)

library(ggplot2)
ggplot(data = woTurtles, aes(x=WalleyeDensity, y=meanEF_CPEkm))+geom_point()+theme_classic()+ggtitle("efCPUE vs Population Estimates for Walleye")
ggplot(data = woTurtles, aes(x=logDens, y=logCPUE))+geom_smooth(method="lm")+
  theme_classic()+xlab("Walleye Abundance (PE)")+ylab("Electrofishing CPUE")+
  ggtitle("Walleye efCPUE relationship with Population Estimates")

