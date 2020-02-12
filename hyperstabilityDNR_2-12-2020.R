# looking at hyperstability of angling CPUE as a function of electrofishing CPUE
# 2-12-2020
# CLM, CJD, SEJ

setwd("~/Documents/Research/Fishscapes/hyperstability/hsSurvey/")

# load function to load data from google drive
source("gdriveURL.R")
library(dplyr)

######## angling CPUE
# load creel data from google drive
creel1=gdriveURL("https://drive.google.com/open?id=1lxUd742QZMXDQunyFBnENKMYZ1XNM_Pc")
creel2=gdriveURL("https://drive.google.com/open?id=1UYhbGH28WXjmi-4BzhfwO4KYwrBCNO2Q")
creel=rbind(creel1,creel2)

# reduce to columns we care about
creel=creel[,c(3,6,12,18,25:26,30,36,38)]

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
              group_by(WBIC,fishSpeciesCode,surveyYear) %>%
              summarize(meanCPUE=mean(anglingCPUE),
                        N=n())
lake_yearCPUE=as.data.frame(lake_yearCPUE)



####### electrofishing abundance
bassEF=gdriveURL("https://drive.google.com/open?id=11v8FbT2wnKx_CqUfxu_V9r_8fyCfcdD2")
bassEF=bassEF[,c(3,5,13,19,27:29)]
bassEF$CPEkm=bassEF$CPEmile/1.60934   # convert fish per mile to fish per km
bassEF$distanceShockedKm=bassEF$distanceShockedMiles*0.621371 # convert miles to km
lake_yearBASSef= bassEF %>%
  group_by(WBIC,species,surveyYear) %>%
  summarize(meanEF_CPEkm=mean(CPEkm),
            totalFishCaught=sum(totalNumberCaughtFish),
            totalDistShockedKm=sum(distanceShockedKm),
            totalHoursSampled=sum(numberHoursSampled),
            N=n())
lake_yearBASSef=as.data.frame(lake_yearBASSef)


panEF=gdriveURL("https://drive.google.com/open?id=1QIqCBQ9gbOgRFUJQbnokwwTZJi5VZZIR")
panEF=panEF[,c(3,5,13,19,25:27)]
panEF$CPEkm=panEF$CPEmile/1.60934   # convert fish per mile to fish per km
panEF$distanceShockedKm=panEF$distanceShockedMiles*0.621371 # convert miles to km
lake_yearPANef= panEF %>%
  group_by(WBIC,species,surveyYear) %>%
  summarize(meanEF_CPEkm=mean(CPEkm),
            totalFishCaught=sum(totalNumberCaughtFish),
            totalDistShockedKm=sum(distanceShockedKm),
            totalHoursSampled=sum(numberHoursSampled),
            N=n())
lake_yearPANef=as.data.frame(lake_yearPANef)

walleyeEF=gdriveURL("https://drive.google.com/open?id=1DPRROWv6Cf_fP6Z-kE9ZgUfdf_F_jSNT")
walleyeEF=walleyeEF[,c(3,5,13,19,23:24,27)]
walleyeEF$CPEkm=walleyeEF$CPEmile/1.60934   # convert fish per mile to fish per km
walleyeEF$distanceShockedKm=walleyeEF$distanceShockedMiles*0.621371 # convert miles to km
#remove commas from total fish caught
walleyeEF$totalNumberCaughtFish=as.numeric(gsub(",","",walleyeEF$totalNumberCaughtFish))
lake_yearWALLef= walleyeEF %>%
  group_by(WBIC,species,surveyYear) %>%
  summarize(meanEF_CPEkm=mean(CPEkm),
            totalFishCaught=sum(totalNumberCaughtFish),
            totalDistShockedKm=sum(distanceShockedKm),
            totalHoursSampled=sum(numberHoursSampled),
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

bassJoin=left_join(lake_yearBASSef,lake_yearCPUE,by=c("WBIC"="WBIC","species"="species","surveyYear"="surveyYear"))
bassJoin=bassJoin[!is.na(bassJoin$meanCPUE),]

panJoin=left_join(lake_yearPANef,lake_yearCPUE,by=c("WBIC"="WBIC","species"="species","surveyYear"="surveyYear"))
panJoin=panJoin[!is.na(panJoin$meanCPUE),]


wallJoin=left_join(lake_yearWALLef,lake_yearCPUE,by=c("WBIC"="WBIC","species"="species","surveyYear"="surveyYear"))
wallJoin=wallJoin[!is.na(wallJoin$meanCPUE),]

table(lake_yearCPUE$species)
nrow(lake_yearBASSef)
nrow(bassJoin)
nrow(lake_yearPANef)
nrow(panJoin)
nrow(lake_yearWALLef)
nrow(wallJoin)

library(ggplot2)

#plotting angling CPUE vs ef catch/mile for species

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

