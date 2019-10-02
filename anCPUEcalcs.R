#script to calculate angling CPUEs
# CD 2019.09.26

#clear global environment
rm(list=ls())

library(dplyr)
##read in data from MFE database (in the future all data will come from db, for now 2019 data hasn't been proofed and entered into db yet)
#MFEdb
setwd("C:/Users/jones/Box Sync/NDstuff/ND_R") #setting my working directory to location of db and db utilities script
source("dbUtil.r")
fishI=dbTable("FISH_INFO")
fishS=dbTable("FISH_SAMPLES")
#combining sample and fish info together
fish=fishS%>%
  inner_join(fishI, by="sampleID")%>%
  filter(gear=="AN" & projectID==37)

#bring in the 2019 fishing data that's currently housed in the "in-season" database, basically it's in a form we can work with to set up scripts but still needs some cleaning and proofing. When it's all ready we can just rerun this script all at once and get updated CPUEs

setwd("C:/Users/jones/OneDrive/temp_fishscapesCSVentries") #setting directory to in-season db location
fishI.is=read.csv("fishInfoIS.csv", header = T, stringsAsFactors = F)
fishS.is=read.csv("fishsamplesIS.csv", header = T, stringsAsFactors = F)


fish.is=fishS.is%>%
  inner_join(fishI.is, by="sampleID")%>%
  filter(gear=="AN" & caughtBy!="" & useCPUE=="yes")

#reformatting date columns
fish.is$dateSet=as.POSIXct(fish.is$dateSet)
fish.is$dateSample=as.POSIXct(fish.is$dateSample)
fish.is$dateTimeSet=as.POSIXct(fish.is$dateTimeSet, format= "%m/%d/%Y %H:%M:%S")
fish.is$dateTimeSample=as.POSIXct(fish.is$dateTimeSample, format= "%m/%d/%Y %H:%M:%S")


#now let's combine all the fishscapes data from the database with the in-season db data so we can work with it all at once. 

#are there any columns that are not common between the two databases?
colnames(fish)
colnames(fish.is)

#trim data frames to just the columns we want then join them
fish=fish[,c(1:17,19:23,26,56)]
fish.is=fish.is[,c(1:17,19:23,26,58)]

any(colnames(fish)%in%colnames(fish.is)==F) #checking to make sure we have all the same columns


#bind the two dataframes together into one
fish=rbind(fish, fish.is)
fish=fish[fish$species=="largemouth_bass",]

#now let's make a lakeID and year column so we can easily subset that data to each lake's data
for(i in 1:nrow(fish)){
  fish$lakeID[i]=strsplit(as.character(fish$siteID[i]), split = '_')[[1]][1]
  fish$year[i]=strsplit(as.character(fish$dateSample[i]), split = "-")[[1]][1]
}
unique(fish$lakeID)#check to see we ended up with a bunch of lakeIDs

#now we need to group data together by sample ID in order to calculate the total effort

#we can use the 'caughtBy' column to get data grouped by angler so we can calculate individual CPUEs then combine them to get averages for that lake on that day
#first let's look at the caught by column to see if there's anything weird we need to deal with
unique(fish$caughtBy)

#fishing effort
effort <- fish %>%
  group_by(sampleID, lakeID, year, dateSample, caughtBy) %>%
  distinct(effort)

#number caught
nDat <- fish %>%
  group_by(sampleID, lakeID, year, dateSample, caughtBy) %>%
  summarise(totalNum = n())

#CPUE for each person at each lake on each day
indvCPE <- nDat %>%
  left_join(effort, by=c("sampleID", "lakeID", "year", "dateSample", "caughtBy"))%>%
  mutate(totalCPE = totalNum/effort)

#CPE for each lake on each day pooled across anglers
dayCPE <- indvCPE%>%
  group_by(lakeID, year, dateSample)%>%
  summarize(meanCPE=mean(totalCPE), sdCPE=sd(totalCPE))

#CPE for each lake pooled across day and year
lkCPE <- indvCPE%>%
  group_by(lakeID)%>%
  summarize(lkmeanCPE=mean(totalCPE), lksdCPE=sd(totalCPE))

#write mean lake CPUEs to a .csv file for use in model
#write.csv(lkCPE, "C:/Users/jones/Box Sync/NDstuff/CNH/hsSurvey/meanLakeCPUE.csv", row.names = F)
