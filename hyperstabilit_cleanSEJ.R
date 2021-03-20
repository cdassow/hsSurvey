#Cleaned up hyperstability survey code 
#looking at hyperstability of angling CPUE as a function of electrofishing CPUE
# 2-12-2020
# CLM, CJD, SEJ

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

Bcreel=creel[creel$fishSpeciesCode%in%c("W11","W12"),]
Wcreel=creel[creel$fishSpeciesCode=="X22",]
Pcreel=creel[creel$fishSpeciesCode%in%c("X15","W14","W09"),]

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
bassJoin<- bassJoin[!is.na(bassJoin$logCPUE),]
bassJoin<- bassJoin[!is.na(bassJoin$logAbun),]
bassJoin<- bassJoin[is.finite(bassJoin$logCPUE),]

wallJoin$logCPUE=log(wallJoin$meanCPUE)
wallJoin$logAbun=log(wallJoin$meanEF_CPEkm)
wallJoin<- wallJoin[is.finite(wallJoin$logCPUE),]

panJoin$logCPUE=log(panJoin$meanCPUE)
panJoin$logAbun=log(panJoin$meanEF_CPEkm)
panJoin<- panJoin[is.finite(panJoin$logCPUE),]


#combining all fish species

BWJoin=full_join(bassJoin,wallJoin)
BWPJoin=full_join(BWJoin,panJoin)



##*******Species relationship with hyperstability 
#  fit without random effects is useful for visualization, but running random effect lake intercept
#  to make sure this doesn't matter for results...
BWPspeciesFit<-lm(BWPJoin$logCPUE~BWPJoin$logAbun*BWPJoin$species)
summary(BWPspeciesFit)
anova(BWPspeciesFit)

BWPspecies_q_fit<-lm(BWPJoin$logCPUE~BWPJoin$logAbun+BWPJoin$species)
BWPfit<-lm(BWPJoin$logCPUE~BWPJoin$logAbun)
BWPspecies_int_fit<-lm(BWPJoin$logCPUE~BWPJoin$logAbun+BWPJoin$logAbun:BWPJoin$species)

anova(BWPfit,BWPspecies_q_fit)
anova(BWPfit,BWPspecies_int_fit)

anova(BWPspecies_int_fit,BWPspeciesFit)
anova(BWPspecies_q_fit,BWPspeciesFit)

library(lme4)
BWPJoin$WBICfactor=as.factor(BWPJoin$WBIC)
BWPspeciesFit_random<-lmer(logCPUE~logAbun*species+(1|WBICfactor),data=BWPJoin)
BWPqFit_random<-lmer(logCPUE~logAbun+species+(1|WBICfactor),data=BWPJoin)
BWPintFit_random<-lmer(logCPUE~logAbun+logAbun:species+(1|WBICfactor),data=BWPJoin)
BWPfit_random<-lmer(logCPUE~logAbun+(1|WBICfactor),data=BWPJoin)

anova(BWPqFit_random,BWPfit_random)
anova(BWPintFit_random,BWPfit_random)

anova(BWPspeciesFit_random,BWPfit_random)  # species matter
anova(BWPspeciesFit_random,BWPqFit_random)
anova(BWPspeciesFit_random,BWPintFit_random)


# individual fits to get SE and do posthoc comparison
cur=BWPJoin[BWPJoin$species=="BLACK CRAPPIE",]
summary(lmer(logCPUE~logAbun+(1|WBICfactor),data=cur))   # 0.189 +/- 0.063
cur=BWPJoin[BWPJoin$species=="BLUEGILL",]
summary(lmer(logCPUE~logAbun+(1|WBICfactor),data=cur))   # 0.184 +/- 0.040
cur=BWPJoin[BWPJoin$species=="YELLOW PERCH",]
summary(lmer(logCPUE~logAbun+(1|WBICfactor),data=cur))   # 0.051 +/- 0.046
cur=BWPJoin[BWPJoin$species=="LARGEMOUTH BASS",]
summary(lmer(logCPUE~logAbun+(1|WBICfactor),data=cur))   # 0.406 +/- 0.027
cur=BWPJoin[BWPJoin$species=="SMALLMOUTH BASS",]
summary(lmer(logCPUE~logAbun+(1|WBICfactor),data=cur))   # 0.211 +/- 0.032
cur=BWPJoin[BWPJoin$species=="WALLEYE",]
summary(lmer(logCPUE~logAbun+(1|WBICfactor),data=cur))   # 0.605 +/- 0.045

abundPred=0:150
par(mfrow=c(1,1))
plot(abundPred,exp(-0.26758)*abundPred^0.18390,col='purple',xlim=c(0,150),ylim=c(0,5),type='l',lwd=2,xlab="abundance",ylab="Angler CPUE")
lines(abundPred,exp(-0.26758)*exp(0.50806)*abundPred^(0.18390+0.02864),col='blue',lwd=2)
lines(abundPred,exp(-0.26758)*exp(-1.21)*abundPred^(0.18390+0.22231),col='darkgreen',lwd=2)
lines(abundPred,exp(-0.26758)*exp(-0.97972)*abundPred^(0.18390+0.05139),col='brown',lwd=2)
lines(abundPred,exp(-0.26758)*exp(-2.96276)*abundPred^(0.18390+0.44682),col='red',lwd=2)
lines(abundPred,exp(-0.26758)*exp(-0.12560)*abundPred^(0.18390-0.13197),col='yellow',lwd=2)
legend('topleft',c('Walleye','Largemouth Bass','Smallmouth Bass','Black Crappie','Bluegill','Yellow Perch'),col=c('red','darkgreen','brown','purple','blue','yellow'),lty=1,box.lty=0)

par(mfrow=c(3,2))
plot(BWPJoin$meanEF_CPEkm[BWPJoin$species=="BLACK CRAPPIE"],BWPJoin$meanCPUE[BWPJoin$species=="BLACK CRAPPIE"],xlab="abundance",ylab="Angler CPUE",main="Black Crappie")
lines(abundPred,exp(-0.26758)*abundPred^0.18390,col='purple',lwd=2)
text(35,2.3,expression(beta=="0.18"))
plot(BWPJoin$meanEF_CPEkm[BWPJoin$species=="LARGEMOUTH BASS"],BWPJoin$meanCPUE[BWPJoin$species=="LARGEMOUTH BASS"],xlab="abundance",ylab="Angler CPUE",main="Largemouth Bass")
lines(abundPred,exp(-0.26758)*exp(-1.21)*abundPred^(0.18390+0.22231),col='darkgreen',lwd=2)
text(45,2,expression(beta=="0.41"))
plot(BWPJoin$meanEF_CPEkm[BWPJoin$species=="BLUEGILL"],BWPJoin$meanCPUE[BWPJoin$species=="BLUEGILL"],xlab="abundance",ylab="Angler CPUE",main="Bluegill")
lines(abundPred,exp(-0.26758)*exp(0.50806)*abundPred^(0.18390+0.02864),col='blue',lwd=2)
text(115,5,expression(beta=="0.21"))
plot(BWPJoin$meanEF_CPEkm[BWPJoin$species=="SMALLMOUTH BASS"],BWPJoin$meanCPUE[BWPJoin$species=="SMALLMOUTH BASS"],xlab="abundance",ylab="Angler CPUE",main="Smallmouth Bass")
lines(abundPred,exp(-0.26758)*exp(-0.97972)*abundPred^(0.18390+0.05139),col='brown',lwd=2)
text(15,0.2,expression(beta=="0.24"))
plot(BWPJoin$meanEF_CPEkm[BWPJoin$species=="YELLOW PERCH"],BWPJoin$meanCPUE[BWPJoin$species=="YELLOW PERCH"],xlab="abundance",ylab="Angler CPUE",main="Yellow Perch")
lines(abundPred,exp(-0.26758)*exp(-0.12560)*abundPred^(0.18390-0.13197),col='yellow',lwd=2)
text(28,2.6,expression(beta=="0.05"))
plot(BWPJoin$meanEF_CPEkm[BWPJoin$species=="WALLEYE"],BWPJoin$meanCPUE[BWPJoin$species=="WALLEYE"],xlab="abundance",ylab="Angler CPUE",main="Walleye")
lines(abundPred,exp(-0.26758)*exp(-2.96276)*abundPred^(0.18390+0.44682),col='red',lwd=2)
text(130,1.8,expression(beta=="0.63"))

#***** I wonder if we could pull some "natural history" data (averaged adult body size,
#      generation time, prey choices, habitat use, etc) from fishbase and see if 
#      there are correlates with the betas we estimated...
#      this approach would be a bit more data-driven than just making up stories about why betas differ
#      Might not be anything there, but worth a shot...
betas=c('BLC'=0.18,"BLG"=0.21,"YWP"=0.05,"LMB"=0.41,"SMB"=0.24,"WYE"=0.63)
betasWrand=c('BLC'=0.189,"BLG"=0.184,"YWP"=0.051,"LMB"=0.406,"SMB"=0.211,"WYE"=0.605)



##********* lake characteristics using Hansen et al. dataset
# compile lake information from Gretchen Hansen
hansenFiles=list.files("HansenData/")   # the 1st and last files have year-specific data; rest have a single observtion for 14,364 lakes
i=4
cur=read.csv(paste("HansenData/",hansenFiles[i],sep=""),header=TRUE)
name=gsub("Hansen_WI_","",hansenFiles[i])
name=gsub(".csv","",name)
colnames(cur)[2:ncol(cur)]=paste(name,colnames(cur)[2:ncol(cur)],sep="_")
hansen=cur
hansen=hansen[order(hansen$wbic),]

for(i in c(2:3,5,7:9)){
  cur=read.csv(paste("HansenData/",hansenFiles[i],sep=""),header=TRUE)
  name=gsub("Hansen_WI_","",hansenFiles[i])
  name=gsub(".csv","",name)
  cur=cur[order(cur$wbic),]
  print(sum(cur$wbic==hansen$wbic))
  colnames(cur)=paste(name,colnames(cur),sep="_")
  hansen=cbind(hansen,cur[,2:ncol(cur)])
}

dim(hansen)
colnames(hansen)[22]="lakeOrder"
colnames(hansen)[1]="WBIC"
colnames(hansen)

# join lake data with hyperstability data
BWPlc=left_join(BWPJoin, hansen, by="WBIC")

# calculate a couple other lake shape metrics
BWPlc$depthRatio=BWPlc$lakeChar_MaxDepth/BWPlc$lakeChar_MeanDepth
BWPlc$shorelineComplexity=BWPlc$lakeChar_Length/(2*sqrt(pi*BWPlc$lakeChar_Area))

# with mixed effects model
cur=BWPlc[BWPlc$species=="LARGEMOUTH BASS",]   # N = 172 
# for some reason LMB data has troube (generates a singular fit) with mixed effects model...
HSfit<-lmer(logCPUE~logAbun+(1|WBICfactor),data=cur)

summary(lm(logCPUE~logAbun+logAbun:riparian_Developed,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_Area,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_MaxDepth,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_MeanDepth,data=cur))
summary(lm(logCPUE~logAbun+logAbun:depthRatio,data=cur))
summary(lm(logCPUE~logAbun+logAbun:shorelineComplexity,data=cur))
summary(lm(logCPUE~logAbun+logAbun:fishingPressure_publicaccess,data=cur))
summary(lm(logCPUE~logAbun+logAbun:fishingPressure_accessscore,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeOrder,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Secchi,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Conductance,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_pH,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_totalP,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Color,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_MEI,data=cur))
summary(lm(logCPUE~logAbun+logAbun:watershed_Wetlands,data=cur))
summary(lm(logCPUE~logAbun+logAbun:watershed_Cultivated,data=cur))



cur=BWPlc[BWPlc$species=="SMALLMOUTH BASS",]   # N = 155
summary(lm(logCPUE~logAbun+logAbun:riparian_Developed,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_Area,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_MaxDepth,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_MeanDepth,data=cur))
summary(lm(logCPUE~logAbun+logAbun:depthRatio,data=cur))
summary(lm(logCPUE~logAbun+logAbun:shorelineComplexity,data=cur))
summary(lm(logCPUE~logAbun+logAbun:fishingPressure_publicaccess,data=cur))
summary(lm(logCPUE~logAbun+logAbun:fishingPressure_accessscore,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeOrder,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Secchi,data=cur))   # close
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Conductance,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_pH,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_totalP,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Color,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_MEI,data=cur))
summary(lm(logCPUE~logAbun+logAbun:watershed_Wetlands,data=cur))
summary(lm(logCPUE~logAbun+logAbun:watershed_Cultivated,data=cur))  # very significant...

fitHS=lmer(logCPUE~logAbun+(1|WBICfactor),data=cur)

lcFit=lmer(logCPUE~logAbun+logAbun:riparian_Developed+(1|WBICfactor),data=cur)
anova(lcFit,fitHS)
lcFit=lmer(logCPUE~logAbun+logAbun:lakeChar_Area+(1|WBICfactor),data=cur)
anova(lcFit,fitHS)
lcFit=lmer(logCPUE~logAbun+logAbun:riparian_Developed+(1|WBICfactor),data=cur)
anova(lcFit,fitHS)

cur=BWPlc[BWPlc$species=="WALLEYE",]   # N = 302
summary(lm(logCPUE~logAbun+logAbun:riparian_Developed,data=cur))   # almost
summary(lm(logCPUE~logAbun+logAbun:lakeChar_Area,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_MaxDepth,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_MeanDepth,data=cur))
summary(lm(logCPUE~logAbun+logAbun:depthRatio,data=cur))
summary(lm(logCPUE~logAbun+logAbun:shorelineComplexity,data=cur))
summary(lm(logCPUE~logAbun+logAbun:fishingPressure_publicaccess,data=cur))
summary(lm(logCPUE~logAbun+logAbun:fishingPressure_accessscore,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeOrder,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Secchi,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Conductance,data=cur))   #  very significant
summary(lm(logCPUE~logAbun+logAbun:waterQuality_pH,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_totalP,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Color,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_MEI,data=cur))     #  very  significant
summary(lm(logCPUE~logAbun+logAbun:watershed_Wetlands,data=cur))   # very significant
summary(lm(logCPUE~logAbun+logAbun:watershed_Cultivated,data=cur))




cur=BWPlc[BWPlc$species=="BLACK CRAPPIE",]   # N = 66
summary(lm(logCPUE~logAbun+logAbun:riparian_Developed,data=cur))   #  maybe
summary(lm(logCPUE~logAbun+logAbun:lakeChar_Area,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_MaxDepth,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_MeanDepth,data=cur))
summary(lm(logCPUE~logAbun+logAbun:depthRatio,data=cur))
summary(lm(logCPUE~logAbun+logAbun:shorelineComplexity,data=cur))
summary(lm(logCPUE~logAbun+logAbun:fishingPressure_publicaccess,data=cur))
summary(lm(logCPUE~logAbun+logAbun:fishingPressure_accessscore,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeOrder,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Secchi,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Conductance,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_pH,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_totalP,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Color,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_MEI,data=cur))
summary(lm(logCPUE~logAbun+logAbun:watershed_Wetlands,data=cur))
summary(lm(logCPUE~logAbun+logAbun:watershed_Cultivated,data=cur))



cur=BWPlc[BWPlc$species=="BLUEGILL",]   # N = 89
summary(lm(logCPUE~logAbun+logAbun:riparian_Developed,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_Area,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_MaxDepth,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_MeanDepth,data=cur))
summary(lm(logCPUE~logAbun+logAbun:depthRatio,data=cur))
summary(lm(logCPUE~logAbun+logAbun:shorelineComplexity,data=cur))
summary(lm(logCPUE~logAbun+logAbun:fishingPressure_publicaccess,data=cur))
summary(lm(logCPUE~logAbun+logAbun:fishingPressure_accessscore,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeOrder,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Secchi,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Conductance,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_pH,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_totalP,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Color,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_MEI,data=cur))
summary(lm(logCPUE~logAbun+logAbun:watershed_Wetlands,data=cur))   #  almost
summary(lm(logCPUE~logAbun+logAbun:watershed_Cultivated,data=cur))




cur=BWPlc[BWPlc$species=="YELLOW PERCH",]   # N = 88
summary(lm(logCPUE~logAbun+logAbun:riparian_Developed,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_Area,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_MaxDepth,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_MeanDepth,data=cur))
summary(lm(logCPUE~logAbun+logAbun:depthRatio,data=cur))
summary(lm(logCPUE~logAbun+logAbun:shorelineComplexity,data=cur))
summary(lm(logCPUE~logAbun+logAbun:fishingPressure_publicaccess,data=cur))
summary(lm(logCPUE~logAbun+logAbun:fishingPressure_accessscore,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeOrder,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Secchi,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Conductance,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_pH,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_totalP,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Color,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_MEI,data=cur))
summary(lm(logCPUE~logAbun+logAbun:watershed_Wetlands,data=cur))
summary(lm(logCPUE~logAbun+logAbun:watershed_Cultivated,data=cur))  # almost



cur=BWPlc[BWPlc$species%in%c("BLACK CRAPPIE","BLUEGILL","YELLOW PERCH"),]  #  N = 243
summary(lm(logCPUE~logAbun+logAbun:riparian_Developed,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_Area,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_MaxDepth,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeChar_MeanDepth,data=cur))
summary(lm(logCPUE~logAbun+logAbun:depthRatio,data=cur))
summary(lm(logCPUE~logAbun+logAbun:shorelineComplexity,data=cur))
summary(lm(logCPUE~logAbun+logAbun:fishingPressure_publicaccess,data=cur))
summary(lm(logCPUE~logAbun+logAbun:fishingPressure_accessscore,data=cur))
summary(lm(logCPUE~logAbun+logAbun:lakeOrder,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Secchi,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Conductance,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_pH,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_totalP,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_Color,data=cur))
summary(lm(logCPUE~logAbun+logAbun:waterQuality_MEI,data=cur))
summary(lm(logCPUE~logAbun+logAbun:watershed_Wetlands,data=cur))
summary(lm(logCPUE~logAbun+logAbun:watershed_Cultivated,data=cur))




anova(BWPspeciesFit_random,BWPfit_random)  # species matter


# look at Hansen Walleye density data vs. our EFcpue
walleyeDensity=read.csv("HansenData/Hansen_WI_adultWLYdens.csv",header=TRUE)
colnames(walleyeDensity)[1:2]=c("WBIC","surveyYear")

cpueCheck=left_join(lake_yearWALLef,walleyeDensity,by=c("WBIC","surveyYear"))

cpueCheck=cpueCheck[!is.na(cpueCheck$adults.acre),]
cpueCheck$wbicFactor=as.factor(cpueCheck$WBIC)

par(mfrow=c(1,1))
plot(cpueCheck$adults.acre,cpueCheck$meanEF_CPEkm)
lm_fit=lm(meanEF_CPEkm~adults.acre,data=cpueCheck)
abline(lm_fit,lwd=2,col='red')

fit=lmer(meanEF_CPEkm~adults.acre+(1|wbicFactor),data=cpueCheck)
nullFit=lmer(meanEF_CPEkm~(1|wbicFactor),data=cpueCheck)

anova(fit,nullFit)

#### looking at WDNR walleye PE vs efCPUE ####

#join by WBIC, surveyYear
library(stringr)
WallPE=read.csv("WalleyeData_Age0toAge1MortalitywithPE_Zebro_9_18_20.csv")
WallPE=WallPE[2:645,]
#WallPE$surveyYear<-str_sub(WallPE$ï..Age.0.to.age.1.mortality.with.PE.for.cultivation.depensation.ms,start=-4)
#WallPE$WBIC=str_sub(WallPE$ï..Age.0.to.age.1.mortality.with.PE.for.cultivation.depensation.ms,end=-5)
WallPE=rename(WallPE, WBIC = X, surveyYear = X.4 , PE = X.8 , Density = X.9)
WallPE=WallPE[2:644,c(2,6,10:11)]
WallPE_2018=read.csv("WalleyeWDNR_18-20_PEs.csv") #REFDATE = Survey year, NumberAcre is the density of adult walleye
WallPE_2018=rename(WallPE_2018, WBIC = ï..MWBCODE, surveyYear = REFDATE, PE = NUMBER, Density = NUMACRE)
WallPE_2018=WallPE_2018[,c(1,6,8,15)]
WallPE$WBIC <-as.integer(as.character(WallPE$WBIC))
WallPE$surveyYear<-as.integer(as.character(WallPE$surveyYear))
WallPE$PE<-as.integer(as.character(WallPE$PE))
WallPE$Density<-as.numeric((as.character(WallPE$Density)))

WallPE=full_join(WallPE,WallPE_2018)

rownames(WallPE) = seq(length=nrow(WallPE)) #final df 767 observations of Walleye PE's from WDNR

cpueCheck_WDNR=full_join(lake_yearWALLef,WallPE,by=c("WBIC","surveyYear"))

cpueCheck_WDNR=cpueCheck_WDNR[!is.na(cpueCheck_WDNR$Density),]
cpueCheck_WDNR$wbicFactor=as.factor(cpueCheck_WDNR$WBIC)

par(mfrow=c(1,1))
plot(cpueCheck_WDNR$Density,cpueCheck_WDNR$meanEF_CPEkm)
lm_fit=lm(meanEF_CPEkm~Density,data=cpueCheck_WDNR)
abline(lm_fit,lwd=2,col='red')