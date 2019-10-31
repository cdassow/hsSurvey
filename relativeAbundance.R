<<<<<<< HEAD
#script to build relationship between bass population estimates and electrofishing CPUEs and then convert them to relative abundances.
#10.1.2019 CD

#clear global evnironment
rm(list=ls())

#load any packages we'll need
library(dplyr)
library(ggplot2)

#### PULLING IN DATA ####
#load PE data, fish info data, lake info data

#PE data from PEs calculated on a separate script
#setwd("~/../Box Sync/NDstuff/CNH/hsSurvey") #Colin's working directory
setwd("C:/Users/jcaff/Documents/Jones Lab/hsSurvey")
pes1=read.csv("2019PEs.csv", header = T, stringsAsFactors = F) #2019 lake PEs
pes2=read.csv("fishscapes2018_peSum_20180914.csv", header = T, stringsAsFactors = F) #2018 lakePEs

#bring in lake info
linfo=read.csv("biocom_Lakes.csv", header = T, stringsAsFactors = F) #lake characteristic information

#drivers log for 2019 shocking temp, pH, conductivity
dlog19=read.csv("driversLog2019.csv", header = T, stringsAsFactors = F)
dlog18=read.csv("driversLog_condTemp.csv", header = T, stringsAsFactors = F)

#bringin lakeID info from mfe db
setwd("~/../Box Sync/NDstuff/ND_R")
source("dbUtil.r")
dbTableList()
dbLinfo=dbTable("LAKES")
dbfishI=dbTable('FISH_INFO')
dbfishS=dbTable('FISH_SAMPLES')
fishDB=dbfishS%>%
  inner_join(dbfishI, by="sampleID")
fishDB$dayOfYear=as.numeric(fishDB$dayOfYear)
fishDB$projectID=as.numeric(fishDB$projectID)
fishDB$fishNum=as.numeric(fishDB$fishNum)
#bring in inseason database
setwd("C:/Users/jones/OneDrive/temp_fishscapesCSVentries")
fishI.is=read.csv("fishInfoIS.csv")
fishS.is=read.csv("fishSamplesIS.csv")
#combining sample and fish info together
fishIS=fishI.is%>%
  inner_join(fishS.is, by="sampleID")%>%
  filter(species%in% c("largemouth_bass", "smallmouth_bass"))
#reformatting date columns
fishIS$dateSet=as.Date(as.POSIXct(fishIS$dateSet))
fishIS$dateSample=as.Date(as.POSIXct(fishIS$dateSample))
fishIS$dateTimeSet=as.POSIXct(fishIS$dateTimeSet, format= "%m/%d/%Y %H:%M:%S")
fishIS$dateTimeSample=as.POSIXct(fishIS$dateTimeSample, format= "%m/%d/%Y %H:%M:%S")

#combining db and inseason db data together
fish=fishIS%>%
  bind_rows(fishDB)%>%
  filter(projectID==37, species=="largemouth_bass")
#makeing lakeID column and year column
for(i in 1:nrow(fish)){
  fish$lakeID[i]=strsplit(as.character(fish$siteID[i]), split = '_')[[1]][1]
  fish$year[i]=strsplit(as.character(fish$dateSample[i]), split = "-")[[1]][1]
}
  
#### ORGANIZE DATA ####

#combine and standardize conductivity, temp, ph data from 2018 in db and 2019 (drivers log)
d19=dlog19%>%
  group_by(lakeID)%>%
  summarise(meanCond=mean(conductivity))
d18=dlog18%>%
  group_by(lakeID)%>%
  summarise(meanCond=mean(SpC))

logCond=bind_rows(d19,d18)

#combining and standardizing the PE data 

#see what info each table has
colnames(pes1)
colnames(pes2)

#trim 2018 pe info to just the columns that the 2019 pe info has (lakeID, nEvents/nSamples, nHat, nHatLow, nHatHigh)
pes1=pes1[pes1$nEvents!=0, c(1,3,4,5,6)]
pes2=pes2[,c(2,3,4,5,6)]

#we're pretty close, we have all the right columsn but not with uniform names or in the same order between dataframes
colnames(pes1)
colnames(pes2)

#we'll rename the columns in pes2 to match pes1 then use the functionality of dplyr to combine them and have the function bind_rows match the correct columns between the two dataframes
colnames(pes2)=c("lakeID", "nEvents", "nHatLow", "nHat", "nHatHigh")
colnames(pes2)

#cominbing all our pes into one data frame
pes=bind_rows(pes1,pes2)

###  now need to pull in info on lake size and fish info to help with out regression
#lets look at what info is in linfo
str(linfo)

#we have to deal with the problem that there are some duplicate lake names so we need to make sure we're linking the correct lake characteristics to each lake. Every lake in WI has a unique numerical code to identify it regardless of whether or not there is a duplicate named lake somewhere else in the state so we can use WBICs to make sure we pulling the info for the 'right' partridge lake and so on...

#use db table to match our mfe lakeIDs to statewide WBICs
wbics=dbLinfo[dbLinfo$lakeID%in%pes$lakeID,]
wbics$WBIC=as.numeric(wbics$WBIC)
dbLinfo$WBIC=as.numeric(dbLinfo$WBIC)
linfo=linfo[linfo$WBIC%in%wbics$WBIC,]

#this gets us most of the way there
lperms=left_join(x=wbics, y=linfo, by="WBIC")%>%
  select("lakeName.x","lakeID.x","WBIC","surfaceArea", "maxDepth", "perimeter", "conductance", "conductanceBiocom", "conductivityQuintile")%>%
  rename("lakeID"="lakeID.x", "lakeName"="lakeName.x")
#we have one issue to deal with. Some lakes we don't have perimeters and conductances for so I'm pulling those manually from the DNR website and drivers log

#assigning conductances to the appropriate lakes
lakes=unique(lperms$lakeID)
for(i in 1:length(lakes)){
  if(any(logCond$lakeID==lakes[i])){
  lperms$conductance[lperms$lakeID==lakes[i]]=logCond$meanCond[logCond$lakeID==lakes[i]]
  }
}
#Bay lake row 2 of lperms is the row we need to change.
lperms$perimeter[2]=7.69 #km
lperms$conductance[2]=22.34 #mean cond for surfacee water in bay pulled form mfe db

#found lake row 5
lperms$surfaceArea[5]=132 #ha
lperms$maxDepth[5]=6.4 #m

#hunter lake row6
lperms$perimeter[6]= 5.13 #km

#lake of the hills row9
lperms$WBIC[9]=1620500 #from widnr
lperms$perimeter[9]=2.35#km

#street lake row 11
lperms$perimeter[11]=3.05 #km

#silver lake row 12
lperms$perimeter[12]=2.2 #km

#upper gresham row 14
lperms$perimeter[14]=9.32 #km

#white birch lake row 15
lperms$perimeter[15]=3.7 #km

#wildcat lake  row 16
#permeter needs to be traced on GIS to include big kitten
lperms$conductance[16]=169.6

#wabasso lake row 17
lperms$perimeter[17]=2.12 #km



#subset lperms (which also contains all the other lake info we might be interested in) to just the lakes we have PEs for

all=full_join(pes, lperms)

#creating electrofishing CPUEs (distance) and pulling largest fish caught to build model

fish$distanceShocked[fish$sampleID=='LV_wholeShoreline_20190708_2225_BE_FishscapesSurvey0.5mile.20180606']=0.5 #need to manually fix one distance shocked that didn't get corrected in fish EntryTool
#fishing effort
effort <- fish %>%
  filter(gear=="BE")%>%
  group_by(sampleID, lakeID, year) %>%
  distinct(distanceShocked)%>%
  mutate(distanceShockedKM=(as.numeric(distanceShocked)*1.609))%>%
  filter(is.na(distanceShocked)==F)%>%
  filter(distanceShocked!="NA")%>%
  filter(distanceShocked!=0)

#number caught
nDat <- fish %>%
  filter(gear=="BE")%>%
  group_by(sampleID, lakeID, year) %>%
  summarise(totalNum = n())

#CPUE for each sample at each lake
indvCPE <- nDat %>%
  left_join(effort, by=c("sampleID", "lakeID", "year"))%>%
  mutate(sampleCPE = totalNum/distanceShockedKM)

#CPE for each lake pooled across day and year
lkCPE <- indvCPE%>%
  group_by(lakeID)%>%
  summarize(lkmeanCPE=mean(sampleCPE, na.rm=T), lksdCPE=sd(sampleCPE, na.rm = T))

#combining all PE & lake characteristic data with CPUE and fishkm data
full=all%>%
  left_join(lkCPE, by='lakeID')%>%
  mutate(fishPerKM=nHat/perimeter)

#adding column with max fish size
full$maxSize=numeric(nrow(full))

for(i in 1:nrow(full)){
  full$maxSize[i]=max(fish$fishLength[fish$lakeID==full$lakeID[i]], na.rm = T)
}

write.csv(full, "pe_cpue_ModelBuild.csv", row.names = F)

#### MODEL DATA ####

full=read.csv("pe_cpue_ModelBuild.csv", header = T, stringsAsFactors = F)

ggplot(full,aes(x=fishPerKM, y=lkmeanCPE))+
  geom_point()+geom_text(aes(label=lakeID), hjust=-0.5, vjust=-1)
  
#glm fits
fit0=lm(full$lkmeanCPE~full$fishPerKM)
summary(fit0)

fitCond=lm(full$lkmeanCPE~full$fishPerKM+full$conductance)
<<<<<<< HEAD
summary(fitCond)  



summary(fitCond)  

fitCDepthSize=lm(full$lkmeanCPE~full$fishPerKM+full$conductance+full$maxDepth+full$maxSize)
summary (fitCDepthSize)
#testing models
summary(lm(full$lkmeanCPE~full$conductance+full$surfaceArea+full$maxDepth))#R2 -0.19, p 0.8279

summary(lm(full$lkmeanCPE~full$maxSize+full$perimeter))#R2 -0.05905, p 0.547

summary(lm(full$lkmeanCPE~full$conductivityQuintile+full$perimeter))#R2 0.05712, p 0.3722

summary(lm(full$lkmeanCPE~full$maxSize+full$maxDepth+full$surfaceArea))#R2 -0.1043, p 0.6528

summary(lm(full$lkmeanCPE~full$fishPerKM+full$conductance))#R2 0.01587, p 0.371

summary(lm(full$lkmeanCPE~full$fishPerKM+full$conductance+full$surfaceArea))#R2 -0.0811, p 0.574

summary(lm(full$lkmeanCPE~full$fishPerKM+full$conductance+full$maxSize))#R2 -0.08326, p 0.5793

summary(lm(full$lkmeanCPE~full$conductance+full$maxDepth))#R2 -0.09988, p 0.6736

summary(lm(full$lkmeanCPE~full$conductance+full$maxDepth+full$fishPerKM))#R2 -0.08335, p 0.5795

summary(lm(full$lkmeanCPE~full$surfaceArea+full$fishPerKM))#R2 -0.1161, p 0.7301

summary(lm(full$lkmeanCPE~full$surfaceArea+full$maxDepth))#R2 -0.1266, p 0.8109

summary(lm(full$lkmeanCPE~full$perimeter+full$conductance))#R2 -0.1324, p 0.7485

summary(lm(full$lkmeanCPE~full$fishPerKM+full$conductance))#R2 0.01587, p 0.371

summary(lm(full$lkmeanCPE~full$fishPerKM+full$conductance+full$maxDepth))#R2 -0.08335, p 0.5795

summary(lm(full$lkmeanCPE~full$lksdCPE+full$maxDepth))#can i use lksdCPE probably not bc CPE andCPE
#R2 0.856, p 9.372e-06

summary(lm(full$lkmeanCPE~full$maxDepth))#R2 -0.04969, p 0.5714

#script to build relationship between bass population estimates and electrofishing CPUEs and then convert them to relative abundances.
#10.1.2019 CD

#clear global evnironment
rm(list=ls())

#load any packages we'll need
library(dplyr)
library(ggplot2)

#### PULLING IN DATA ####
#load PE data, fish info data, lake info data

#PE data from PEs calculated on a separate script
#setwd("~/../Box Sync/NDstuff/CNH/hsSurvey") #Colin's working directory

pes1=read.csv("2019PEs.csv", header = T, stringsAsFactors = F) #2019 lake PEs
pes2=read.csv("fishscapes2018_peSum_20180914.csv", header = T, stringsAsFactors = F) #2018 lakePEs

#bring in lake info
linfo=read.csv("biocom_Lakes.csv", header = T, stringsAsFactors = F) #lake characteristic information

#drivers log for 2019 shocking temp, pH, conductivity
dlog19=read.csv("driversLog2019.csv", header = T, stringsAsFactors = F)
dlog18=read.csv("driversLog_condTemp.csv", header = T, stringsAsFactors = F)

#bringin lakeID info from mfe db
setwd("~/../Box Sync/NDstuff/ND_R")
source("dbUtil.r")
dbTableList()
dbLinfo=dbTable("LAKES")
dbfishI=dbTable('FISH_INFO')
dbfishS=dbTable('FISH_SAMPLES')
fishDB=dbfishS%>%
  inner_join(dbfishI, by="sampleID")
fishDB$dayOfYear=as.numeric(fishDB$dayOfYear)
fishDB$projectID=as.numeric(fishDB$projectID)
fishDB$fishNum=as.numeric(fishDB$fishNum)
#bring in inseason database
setwd("C:/Users/jones/OneDrive/temp_fishscapesCSVentries")
fishI.is=read.csv("fishInfoIS.csv")
fishS.is=read.csv("fishSamplesIS.csv")
#combining sample and fish info together
fishIS=fishI.is%>%
  inner_join(fishS.is, by="sampleID")%>%
  filter(species%in% c("largemouth_bass", "smallmouth_bass"))
#reformatting date columns
fishIS$dateSet=as.Date(as.POSIXct(fishIS$dateSet))
fishIS$dateSample=as.Date(as.POSIXct(fishIS$dateSample))
fishIS$dateTimeSet=as.POSIXct(fishIS$dateTimeSet, format= "%m/%d/%Y %H:%M:%S")
fishIS$dateTimeSample=as.POSIXct(fishIS$dateTimeSample, format= "%m/%d/%Y %H:%M:%S")

#combining db and inseason db data together
fish=fishIS%>%
  bind_rows(fishDB)%>%
  filter(projectID==37, species=="largemouth_bass")
#makeing lakeID column and year column
for(i in 1:nrow(fish)){
  fish$lakeID[i]=strsplit(as.character(fish$siteID[i]), split = '_')[[1]][1]
  fish$year[i]=strsplit(as.character(fish$dateSample[i]), split = "-")[[1]][1]
}
  
#### ORGANIZE DATA ####

#combine and standardize conductivity, temp, ph data from 2018 in db and 2019 (drivers log)
d19=dlog19%>%
  group_by(lakeID)%>%
  summarise(meanCond=mean(conductivity))
d18=dlog18%>%
  group_by(lakeID)%>%
  summarise(meanCond=mean(SpC))

logCond=bind_rows(d19,d18)

#combining and standardizing the PE data 

#see what info each table has
colnames(pes1)
colnames(pes2)

#trim 2018 pe info to just the columns that the 2019 pe info has (lakeID, nEvents/nSamples, nHat, nHatLow, nHatHigh)
pes1=pes1[pes1$nEvents!=0, c(1,3,4,5,6)]
pes2=pes2[,c(2,3,4,5,6)]

#we're pretty close, we have all the right columsn but not with uniform names or in the same order between dataframes
colnames(pes1)
colnames(pes2)

#we'll rename the columns in pes2 to match pes1 then use the functionality of dplyr to combine them and have the function bind_rows match the correct columns between the two dataframes
colnames(pes2)=c("lakeID", "nEvents", "nHatLow", "nHat", "nHatHigh")
colnames(pes2)

#cominbing all our pes into one data frame
pes=bind_rows(pes1,pes2)

###  now need to pull in info on lake size and fish info to help with out regression
#lets look at what info is in linfo
str(linfo)

#we have to deal with the problem that there are some duplicate lake names so we need to make sure we're linking the correct lake characteristics to each lake. Every lake in WI has a unique numerical code to identify it regardless of whether or not there is a duplicate named lake somewhere else in the state so we can use WBICs to make sure we pulling the info for the 'right' partridge lake and so on...

#use db table to match our mfe lakeIDs to statewide WBICs
wbics=dbLinfo[dbLinfo$lakeID%in%pes$lakeID,]
wbics$WBIC=as.numeric(wbics$WBIC)
dbLinfo$WBIC=as.numeric(dbLinfo$WBIC)
linfo=linfo[linfo$WBIC%in%wbics$WBIC,]

#this gets us most of the way there
lperms=left_join(x=wbics, y=linfo, by="WBIC")%>%
  select("lakeName.x","lakeID.x","WBIC","surfaceArea", "maxDepth", "perimeter", "conductance", "conductanceBiocom", "conductivityQuintile")%>%
  rename("lakeID"="lakeID.x", "lakeName"="lakeName.x")
#we have one issue to deal with. Some lakes we don't have perimeters and conductances for so I'm pulling those manually from the DNR website and drivers log

#assigning conductances to the appropriate lakes
lakes=unique(lperms$lakeID)
for(i in 1:length(lakes)){
  if(any(logCond$lakeID==lakes[i])){
  lperms$conductance[lperms$lakeID==lakes[i]]=logCond$meanCond[logCond$lakeID==lakes[i]]
  }
}
#Bay lake row 2 of lperms is the row we need to change.
lperms$perimeter[2]=7.69 #km
lperms$conductance[2]=22.34 #mean cond for surfacee water in bay pulled form mfe db

#found lake row 5
lperms$surfaceArea[5]=132 #ha
lperms$maxDepth[5]=6.4 #m

#hunter lake row6
lperms$perimeter[6]= 5.13 #km

#lake of the hills row9
lperms$WBIC[9]=1620500 #from widnr
lperms$perimeter[9]=2.35#km

#street lake row 11
lperms$perimeter[11]=3.05 #km

#silver lake row 12
lperms$perimeter[12]=2.2 #km

#upper gresham row 14
lperms$perimeter[14]=9.32 #km

#white birch lake row 15
lperms$perimeter[15]=3.7 #km

#wildcat lake  row 16
#permeter needs to be traced on GIS to include big kitten
lperms$conductance[16]=169.6

#wabasso lake row 17
lperms$perimeter[17]=2.12 #km



#subset lperms (which also contains all the other lake info we might be interested in) to just the lakes we have PEs for

all=full_join(pes, lperms)

#creating electrofishing CPUEs (distance) and pulling largest fish caught to build model

fish$distanceShocked[fish$sampleID=='LV_wholeShoreline_20190708_2225_BE_FishscapesSurvey0.5mile.20180606']=0.5 #need to manually fix one distance shocked that didn't get corrected in fish EntryTool
#fishing effort
effort <- fish %>%
  filter(gear=="BE")%>%
  group_by(sampleID, lakeID, year) %>%
  distinct(distanceShocked)%>%
  mutate(distanceShockedKM=(as.numeric(distanceShocked)*1.609))%>%
  filter(is.na(distanceShocked)==F)%>%
  filter(distanceShocked!="NA")%>%
  filter(distanceShocked!=0)

#number caught
nDat <- fish %>%
  filter(gear=="BE")%>%
  group_by(sampleID, lakeID, year) %>%
  summarise(totalNum = n())

#CPUE for each sample at each lake
indvCPE <- nDat %>%
  left_join(effort, by=c("sampleID", "lakeID", "year"))%>%
  mutate(sampleCPE = totalNum/distanceShockedKM)

#CPE for each lake pooled across day and year
lkCPE <- indvCPE%>%
  group_by(lakeID)%>%
  summarize(lkmeanCPE=mean(sampleCPE, na.rm=T), lksdCPE=sd(sampleCPE, na.rm = T))

#combining all PE & lake characteristic data with CPUE and fishkm data
full=all%>%
  left_join(lkCPE, by='lakeID')%>%
  mutate(fishPerKM=nHat/perimeter)

#adding column with max fish size
full$maxSize=numeric(nrow(full))

for(i in 1:nrow(full)){
  full$maxSize[i]=max(fish$fishLength[fish$lakeID==full$lakeID[i]], na.rm = T)
}

write.csv(full, "pe_cpue_ModelBuild.csv", row.names = F)

#### MODEL DATA ####
setwd("C:/Users/Camille/Desktop/Fishscapes/hsSurvey")
full=read.csv("pe_cpue_ModelBuild.csv", header = T, stringsAsFactors = F)

ggplot(full,aes(x=fishPerKM, y=lkmeanCPE))+
  geom_point()+geom_text(aes(label=lakeID), hjust=-0.5, vjust=-1)
  
#glm fits
fit0=glm(full$lkmeanCPE~full$fishPerKM)
summary(fit0)

fitCond=glm(full$lkmeanCPE~full$fishPerKM+full$conductance)
summary(fitCond)  

fitConSurMax=lm(full$lkmeanCPE~full$fishPerKM+full$conductance+full$surfaceArea+full$maxSize)
summary(fitConSurMax)
