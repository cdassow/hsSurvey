## code to generate PE from fishScapes shocking survey data
## modeled after AR_surveyPEs_20180914.r script
## 2019-06-18 CD

rm(list=ls())
library(dplyr)
library(ggplot2)
#pe data from 2018
pe2018=read.csv('C:/Users/jones/Box Sync/NDstuff/CNH/fishscapes2018_peSum_20180914.csv', header=T, stringsAsFactors=F)

#read in data, either MFE db for fully processed data or in-season database for PE's during the field season

#MFEdb
setwd("C:/Users/jones/Box Sync/NDstuff/ND_R")
source("dbUtil.r")
source("schnabel.R")
fishI=dbTable("FISH_INFO")
fishS=dbTable("FISH_SAMPLES")
#combining sample and fish info together
fish=fishS%>%
  inner_join(fishI, by="sampleID")


#in-season database
setwd("C:/Users/jones/OneDrive/temp_fishscapesCSVentries")
source("schnabel.R")
fishI.is=read.csv("fishInfoIS.csv")
fishS.is=read.csv("fishSamplesIS.csv")
#combining sample and fish info together
fishIS=fishI.is%>%
  inner_join(fishS.is, by="sampleID")%>%
  filter(species%in% c("largemouth_bass", "smallmouth_bass"))
#reformatting date columns
fishIS$dateSet=as.POSIXct(fishIS$dateSet)
fishIS$dateSample=as.POSIXct(fishIS$dateSample)
fishIS$dateTimeSet=as.POSIXct(fishIS$dateTimeSet, format= "%m/%d/%Y %H:%M:%S")
fishIS$dateTimeSample=as.POSIXct(fishIS$dateTimeSample, format= "%m/%d/%Y %H:%M:%S")
####  IN SEASON PEs ####
#Generating Schnabel PEs for each lake (just doing in-season database now but this can be modified)

#adding lakeID column to make subsetting easier
for(i in 1:nrow(fishIS)){
  fishIS$lakeID[i]=strsplit(as.character(fishIS$siteID[i]), split = '_')[[1]][1]
}
peLakes=unique(fishIS$lakeID)
#build data frame to store each PE
PEs=data.frame(lakeID=character(length(peLakes)*2), species=character(length(peLakes)*2), nEvents=numeric(length(peLakes)*2), nHat=numeric(length(peLakes)*2), nHatLow=numeric(length(peLakes)*2), nHatHigh=numeric(length(peLakes)*2))
PEs$lakeID=as.character(PEs$lakeID)
PEs$species=as.character(PEs$species)

#### HUNTER_HT ####

#getting data for hunter
lmb=fishIS[fishIS$lakeID=="HT" & fishIS$species=="largemouth_bass",]

#putting samples into batches for one sampling night of work
#adding a column for batches
lmb$batch=numeric(nrow(lmb))

samp=unique(lmb$sampleID)
samp
lmb$batch[lmb$sampleID%in%samp[1:4]]=1
lmb$batch[lmb$sampleID%in%samp[5:7]]=2
lmb$batch[lmb$sampleID%in%samp[8:11]]=3
lmb$batch[lmb$sampleID%in%samp[12]]=4
lmb$batch[lmb$sampleID%in%samp[13:15]]=5

#collected now
collectedNow=count(lmb, batch)

#marked now
markedNow=lmb%>%
  group_by(batch)%>%
  filter(clipApply=="AF")%>%
  summarize(markedNow=n())

#recaptured now
recapturedNow=lmb%>%
  group_by(batch)%>%
  filter(clipRecapture=="AF")%>%
  summarize(recapturedNow=n())

#combinbing collected, mark, recap data
recapStats=merge(collectedNow, markedNow, by="batch", all=T)
recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
recapStats$recapturedNow[c(1:2,4)]=0 # no recaps on the first and second samples

#calculate markedPrior for each sample
recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
for(i in 2:nrow(recapStats)){
  recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
}
recapStats

# PE
HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
HTpe
#assign to summary dataframe
PEs[1,]=c("HT","largemouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])

#SMB PE

smb=fishIS[fishIS$lakeID=="HT" & fishIS$species=="smallmouth_bass",]

if(nrow(smb[smb$clipRecapture=="AF",])>5){
#putting samples into batches for one sampling night of work
#adding a column for batches
smb$batch=numeric(nrow(smb))

samp=unique(smb$sampleID)
samp
smb$batch[smb$sampleID%in%samp[1:3]]=1
smb$batch[smb$sampleID%in%samp[4:6]]=2
smb$batch[smb$sampleID%in%samp[7:10]]=3
smb$batch[smb$sampleID%in%samp[11]]=4
smb$batch[smb$sampleID%in%samp[12:14]]=5

#collected now
collectedNow=count(smb, batch)

#marked now
markedNow=smb%>%
  group_by(batch)%>%
  filter(clipApply=="AF")%>%
  summarize(markedNow=n())

#recaptured now
recapturedNow=smb%>%
  group_by(batch)%>%
  filter(clipRecapture=="AF")%>%
  summarize(recapturedNow=n())

#combinbing collected, mark, recap data
recapStats=merge(collectedNow, markedNow, by="batch", all=T)
recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples

#calculate markedPrior for each sample
recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
for(i in 2:nrow(recapStats)){
  recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
}
recapStats

# PE
HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
HTpe
#assign to summary dataframe
PEs[2,]=c("HT","smallmouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])
}else{
  PEs[2,]=c("HT","smallmouth_bass", rep(NA,4))
  print("not enough SMB captured for PE at HT")
}



# #### BOOT_BOT ####

#getting data for hunter
lmb=fishIS[fishIS$lakeID=="BOT" & fishIS$species=="largemouth_bass",]

#putting samples into batches for one sampling night of work
#adding a column for batches
lmb$batch=numeric(nrow(lmb))

samp=unique(lmb$sampleID)
samp
lmb$batch[lmb$sampleID%in%samp[1:2]]=1
lmb$batch[lmb$sampleID%in%samp[3]]=2
lmb$batch[lmb$sampleID%in%samp[4:5]]=3

#collected now
collectedNow=count(lmb, batch)

#marked now
markedNow=lmb%>%
  group_by(batch)%>%
  filter(clipApply=="AF")%>%
  summarize(markedNow=n())

#recaptured now
recapturedNow=lmb%>%
  group_by(batch)%>%
  filter(clipRecapture=="AF")%>%
  summarize(recapturedNow=n())

#combinbing collected, mark, recap data
recapStats=merge(collectedNow, markedNow, by="batch", all=T)
recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples

#calculate markedPrior for each sample
recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
for(i in 2:nrow(recapStats)){
  recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
}
recapStats

# PE
HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
HTpe
#assign to summary dataframe
PEs[3,]=c("BOT","largemouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])

# #SMB PE
# 
# smb=fishIS[fishIS$lakeID=="BOT" & fishIS$species=="smallmouth_bass",]
# 
# if(nrow(smb[smb$clipRecapture=="AF",])>5){
#   #putting samples into batches for one sampling night of work
#   #adding a column for batches
#   smb$batch=numeric(nrow(smb))
#   
#   samp=unique(smb$sampleID)
#   samp
#   smb$batch[smb$sampleID%in%samp[1:3]]=1
#   smb$batch[smb$sampleID%in%samp[4:6]]=2
#   smb$batch[smb$sampleID%in%samp[7:10]]=3
#   smb$batch[smb$sampleID%in%samp[11]]=4
#   smb$batch[smb$sampleID%in%samp[12:14]]=5
#   
#   #collected now
#   collectedNow=count(smb, batch)
#   
#   #marked now
#   markedNow=smb%>%
#     group_by(batch)%>%
#     filter(clipApply=="AF")%>%
#     summarize(markedNow=n())
#   
#   #recaptured now
#   recapturedNow=smb%>%
#     group_by(batch)%>%
#     filter(clipRecapture=="AF")%>%
#     summarize(recapturedNow=n())
#   
#   #combinbing collected, mark, recap data
#   recapStats=merge(collectedNow, markedNow, by="batch", all=T)
#   recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
#   recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples
#   
#   #calculate markedPrior for each sample
#   recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
#   for(i in 2:nrow(recapStats)){
#     recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
#   }
#   recapStats
#   
#   # PE
#   HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
#   HTpe
#   #assign to summary dataframe
#   PEs[4,]=c("BOT","smallmouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])
# }else{
#   PEs[4,]=c("BOT","smallmouth_bass", rep(NA,4))
#   print("not enough SMB captured for PE at BOT")
# }
# 

#### BRANDY_BY ####

#getting data for hunter
lmb=fishIS[fishIS$lakeID=="BY" & fishIS$species=="largemouth_bass",]

#putting samples into batches for one sampling night of work
#adding a column for batches
lmb$batch=numeric(nrow(lmb))

samp=unique(lmb$sampleID)
samp
lmb$batch[lmb$sampleID%in%samp[1:2]]=1
lmb$batch[lmb$sampleID%in%samp[3:6]]=2
lmb$batch[lmb$sampleID%in%samp[7:9]]=3
lmb$batch[lmb$sampleID%in%samp[10:14]]=4
lmb$batch[lmb$sampleID%in%samp[15:17]]=5
lmb$batch[lmb$sampleID%in%samp[18:20]]=6
lmb$batch[lmb$sampleID%in%samp[21:22]]=7
lmb$batch[lmb$sampleID%in%samp[23]]=8
lmb$batch[lmb$sampleID%in%samp[24]]=9
lmb$batch[lmb$sampleID%in%samp[25]]=10




#collected now
collectedNow=count(lmb, batch)

#marked now
markedNow=lmb%>%
  group_by(batch)%>%
  filter(clipApply=="AF")%>%
  summarize(markedNow=n())

#recaptured now
recapturedNow=lmb%>%
  group_by(batch)%>%
  filter(clipRecapture=="AF")%>%
  summarize(recapturedNow=n())

#combinbing collected, mark, recap data
recapStats=merge(collectedNow, markedNow, by="batch", all=T)
recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
recapStats$recapturedNow[c(1,10)]=0 # no recaps on the first sample and the 10th


#calculate markedPrior for each sample
recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
for(i in 2:nrow(recapStats)){
  recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
}
recapStats

# PE
BYpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
BYpe
#assign to summary dataframe
PEs[5,]=c("BY","largemouth_bass", max(BYpe$event), BYpe[max(nrow(BYpe)),3], BYpe[max(nrow(BYpe)),2], BYpe[max(nrow(BYpe)),4])

#SMB PE

smb=fishIS[fishIS$lakeID=="BY" & fishIS$species=="smallmouth_bass",]

if(nrow(smb[smb$clipRecapture=="AF",])>5){
  #putting samples into batches for one sampling night of work
  #adding a column for batches
  smb$batch=numeric(nrow(smb))
  
  samp=unique(smb$sampleID)
  samp
  smb$batch[smb$sampleID%in%samp[1:3]]=1
  smb$batch[smb$sampleID%in%samp[4:6]]=2
  smb$batch[smb$sampleID%in%samp[7:10]]=3
  smb$batch[smb$sampleID%in%samp[11]]=4
  smb$batch[smb$sampleID%in%samp[12:14]]=5
  
  #collected now
  collectedNow=count(smb, batch)
  
  #marked now
  markedNow=smb%>%
    group_by(batch)%>%
    filter(clipApply=="AF")%>%
    summarize(markedNow=n())
  
  #recaptured now
  recapturedNow=smb%>%
    group_by(batch)%>%
    filter(clipRecapture=="AF")%>%
    summarize(recapturedNow=n())
  
  #combinbing collected, mark, recap data
  recapStats=merge(collectedNow, markedNow, by="batch", all=T)
  recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
  recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples
  
  #calculate markedPrior for each sample
  recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
  for(i in 2:nrow(recapStats)){
    recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
  }
  recapStats
  
  # PE
  HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
  HTpe
  #assign to summary dataframe
  PEs[6,]=c("BY","smallmouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])
}else{
  PEs[6,]=c("BY","smallmouth_bass", rep(NA,4))
  print("not enough SMB captured for PE at BY")
}

#### DAY_DY ####

#getting data for hunter
lmb=fishIS[fishIS$lakeID=="DY" & fishIS$species=="largemouth_bass",]

#putting samples into batches for one sampling night of work
#adding a column for batches
lmb$batch=numeric(nrow(lmb))

samp=unique(lmb$sampleID)
samp
lmb$batch[lmb$sampleID%in%samp[1:3]]=1
lmb$batch[lmb$sampleID%in%samp[4:8]]=2
lmb$batch[lmb$sampleID%in%samp[9:10]]=3
lmb$batch[lmb$sampleID%in%samp[11:14]]=4
lmb$batch[lmb$sampleID%in%samp[12:14]]=5

#collected now
collectedNow=count(lmb, batch)

#marked now
markedNow=lmb%>%
  group_by(batch)%>%
  filter(clipApply=="AF")%>%
  summarize(markedNow=n())

#recaptured now
recapturedNow=lmb%>%
  group_by(batch)%>%
  filter(clipRecapture=="AF")%>%
  summarize(recapturedNow=n())

#combinbing collected, mark, recap data
recapStats=merge(collectedNow, markedNow, by="batch", all=T)
recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
recapStats$recapturedNow[c(1:2, 4:5)]=0 # no recaps on the first and second samples

#calculate markedPrior for each sample
recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
for(i in 2:nrow(recapStats)){
  recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
}
recapStats

# PE
DYpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
DYpe
#assign to summary dataframe
PEs[7,]=c("DY","largemouth_bass", max(DYpe$event), DYpe[max(nrow(DYpe)),3], DYpe[max(nrow(DYpe)),2], DYpe[max(nrow(DYpe)),4])

#SMB PE

smb=fishIS[fishIS$lakeID=="DY" & fishIS$species=="smallmouth_bass",]

if(nrow(smb[smb$clipRecapture=="AF",])>5){
  #putting samples into batches for one sampling night of work
  #adding a column for batches
  smb$batch=numeric(nrow(smb))
  
  samp=unique(smb$sampleID)
  samp
  smb$batch[smb$sampleID%in%samp[1:3]]=1
  smb$batch[smb$sampleID%in%samp[4:6]]=2
  smb$batch[smb$sampleID%in%samp[7:10]]=3
  smb$batch[smb$sampleID%in%samp[11]]=4
  smb$batch[smb$sampleID%in%samp[12:14]]=5
  
  #collected now
  collectedNow=count(smb, batch)
  
  #marked now
  markedNow=smb%>%
    group_by(batch)%>%
    filter(clipApply=="AF")%>%
    summarize(markedNow=n())
  
  #recaptured now
  recapturedNow=smb%>%
    group_by(batch)%>%
    filter(clipRecapture=="AF")%>%
    summarize(recapturedNow=n())
  
  #combinbing collected, mark, recap data
  recapStats=merge(collectedNow, markedNow, by="batch", all=T)
  recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
  recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples
  
  #calculate markedPrior for each sample
  recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
  for(i in 2:nrow(recapStats)){
    recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
  }
  recapStats
  
  # PE
  HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
  HTpe
  #assign to summary dataframe
  PEs[8,]=c("DY","smallmouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])
}else{
  PEs[8,]=c("DY","smallmouth_bass", rep(NA,4))
  print("not enough SMB captured for PE at DY")
}

#### LONE TREE_LE ####

#getting data for hunter
lmb=fishIS[fishIS$lakeID=="LE" & fishIS$species=="largemouth_bass",]

#putting samples into batches for one sampling night of work
#adding a column for batches
lmb$batch=numeric(nrow(lmb))

samp=unique(lmb$sampleID)
samp
lmb$batch[lmb$sampleID%in%samp[1:3]]=1
lmb$batch[lmb$sampleID%in%samp[4:6]]=2
lmb$batch[lmb$sampleID%in%samp[7:10]]=3
lmb$batch[lmb$sampleID%in%samp[11]]=4
lmb$batch[lmb$sampleID%in%samp[12:14]]=5

#collected now
collectedNow=count(lmb, batch)

#marked now
markedNow=lmb%>%
  group_by(batch)%>%
  filter(clipApply=="AF")%>%
  summarize(markedNow=n())

#recaptured now
recapturedNow=lmb%>%
  group_by(batch)%>%
  filter(clipRecapture=="AF")%>%
  summarize(recapturedNow=n())

#combinbing collected, mark, recap data
recapStats=merge(collectedNow, markedNow, by="batch", all=T)
recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples

#calculate markedPrior for each sample
recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
for(i in 2:nrow(recapStats)){
  recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
}
recapStats

# PE
HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
HTpe
#assign to summary dataframe
PEs[9,]=c("LE","largemouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])

#SMB PE

smb=fishIS[fishIS$lakeID=="LE" & fishIS$species=="smallmouth_bass",]

if(nrow(smb[smb$clipRecapture=="AF",])>5){
  #putting samples into batches for one sampling night of work
  #adding a column for batches
  smb$batch=numeric(nrow(smb))
  
  samp=unique(smb$sampleID)
  samp
  smb$batch[smb$sampleID%in%samp[1:3]]=1
  smb$batch[smb$sampleID%in%samp[4:6]]=2
  smb$batch[smb$sampleID%in%samp[7:10]]=3
  smb$batch[smb$sampleID%in%samp[11]]=4
  smb$batch[smb$sampleID%in%samp[12:14]]=5
  
  #collected now
  collectedNow=count(smb, batch)
  
  #marked now
  markedNow=smb%>%
    group_by(batch)%>%
    filter(clipApply=="AF")%>%
    summarize(markedNow=n())
  
  #recaptured now
  recapturedNow=smb%>%
    group_by(batch)%>%
    filter(clipRecapture=="AF")%>%
    summarize(recapturedNow=n())
  
  #combinbing collected, mark, recap data
  recapStats=merge(collectedNow, markedNow, by="batch", all=T)
  recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
  recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples
  
  #calculate markedPrior for each sample
  recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
  for(i in 2:nrow(recapStats)){
    recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
  }
  recapStats
  
  # PE
  HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
  HTpe
  #assign to summary dataframe
  PEs[10,]=c("LE","smallmouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])
}else{
  PEs[10,]=c("LE","smallmouth_bass", rep(NA,4))
  print("not enough SMB captured for PE at LE")
}

#### NICHOLS_NH ####

#getting data for hunter
lmb=fishIS[fishIS$lakeID=="NH" & fishIS$species=="largemouth_bass",]

#putting samples into batches for one sampling night of work
#adding a column for batches
lmb$batch=numeric(nrow(lmb))

samp=unique(lmb$sampleID)
samp
lmb$batch[lmb$sampleID%in%samp[1:3]]=1
lmb$batch[lmb$sampleID%in%samp[4:6]]=2
lmb$batch[lmb$sampleID%in%samp[7:10]]=3
lmb$batch[lmb$sampleID%in%samp[11]]=4
lmb$batch[lmb$sampleID%in%samp[12:14]]=5

#collected now
collectedNow=count(lmb, batch)

#marked now
markedNow=lmb%>%
  group_by(batch)%>%
  filter(clipApply=="AF")%>%
  summarize(markedNow=n())

#recaptured now
recapturedNow=lmb%>%
  group_by(batch)%>%
  filter(clipRecapture=="AF")%>%
  summarize(recapturedNow=n())

#combinbing collected, mark, recap data
recapStats=merge(collectedNow, markedNow, by="batch", all=T)
recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples

#calculate markedPrior for each sample
recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
for(i in 2:nrow(recapStats)){
  recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
}
recapStats

# PE
HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
HTpe
#assign to summary dataframe
PEs[11,]=c("NH","largemouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])

#SMB PE

smb=fishIS[fishIS$lakeID=="NH" & fishIS$species=="smallmouth_bass",]

if(nrow(smb[smb$clipRecapture=="AF",])>5){
  #putting samples into batches for one sampling night of work
  #adding a column for batches
  smb$batch=numeric(nrow(smb))
  
  samp=unique(smb$sampleID)
  samp
  smb$batch[smb$sampleID%in%samp[1:3]]=1
  smb$batch[smb$sampleID%in%samp[4:6]]=2
  smb$batch[smb$sampleID%in%samp[7:10]]=3
  smb$batch[smb$sampleID%in%samp[11]]=4
  smb$batch[smb$sampleID%in%samp[12:14]]=5
  
  #collected now
  collectedNow=count(smb, batch)
  
  #marked now
  markedNow=smb%>%
    group_by(batch)%>%
    filter(clipApply=="AF")%>%
    summarize(markedNow=n())
  
  #recaptured now
  recapturedNow=smb%>%
    group_by(batch)%>%
    filter(clipRecapture=="AF")%>%
    summarize(recapturedNow=n())
  
  #combinbing collected, mark, recap data
  recapStats=merge(collectedNow, markedNow, by="batch", all=T)
  recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
  recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples
  
  #calculate markedPrior for each sample
  recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
  for(i in 2:nrow(recapStats)){
    recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
  }
  recapStats
  
  # PE
  HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
  HTpe
  #assign to summary dataframe
  PEs[12,]=c("NH","smallmouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])
}else{
  PEs[12,]=c("NH","smallmouth_bass", rep(NA,4))
  print("not enough SMB captured for PE at NH")
}

#### PICKEREL_PK ####

#getting data for hunter
lmb=fishIS[fishIS$lakeID=="PK" & fishIS$species=="largemouth_bass",]

#putting samples into batches for one sampling night of work
#adding a column for batches
lmb$batch=numeric(nrow(lmb))

samp=unique(lmb$sampleID)
samp
lmb$batch[lmb$sampleID%in%samp[1:3]]=1
lmb$batch[lmb$sampleID%in%samp[4:6]]=2
lmb$batch[lmb$sampleID%in%samp[7:10]]=3
lmb$batch[lmb$sampleID%in%samp[11]]=4
lmb$batch[lmb$sampleID%in%samp[12:14]]=5

#collected now
collectedNow=count(lmb, batch)

#marked now
markedNow=lmb%>%
  group_by(batch)%>%
  filter(clipApply=="AF")%>%
  summarize(markedNow=n())

#recaptured now
recapturedNow=lmb%>%
  group_by(batch)%>%
  filter(clipRecapture=="AF")%>%
  summarize(recapturedNow=n())

#combinbing collected, mark, recap data
recapStats=merge(collectedNow, markedNow, by="batch", all=T)
recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples

#calculate markedPrior for each sample
recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
for(i in 2:nrow(recapStats)){
  recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
}
recapStats

# PE
HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
HTpe
#assign to summary dataframe
PEs[13,]=c("PK","largemouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])

#SMB PE

smb=fishIS[fishIS$lakeID=="PK" & fishIS$species=="smallmouth_bass",]

if(nrow(smb[smb$clipRecapture=="AF",])>5){
  #putting samples into batches for one sampling night of work
  #adding a column for batches
  smb$batch=numeric(nrow(smb))
  
  samp=unique(smb$sampleID)
  samp
  smb$batch[smb$sampleID%in%samp[1:3]]=1
  smb$batch[smb$sampleID%in%samp[4:6]]=2
  smb$batch[smb$sampleID%in%samp[7:10]]=3
  smb$batch[smb$sampleID%in%samp[11]]=4
  smb$batch[smb$sampleID%in%samp[12:14]]=5
  
  #collected now
  collectedNow=count(smb, batch)
  
  #marked now
  markedNow=smb%>%
    group_by(batch)%>%
    filter(clipApply=="AF")%>%
    summarize(markedNow=n())
  
  #recaptured now
  recapturedNow=smb%>%
    group_by(batch)%>%
    filter(clipRecapture=="AF")%>%
    summarize(recapturedNow=n())
  
  #combinbing collected, mark, recap data
  recapStats=merge(collectedNow, markedNow, by="batch", all=T)
  recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
  recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples
  
  #calculate markedPrior for each sample
  recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
  for(i in 2:nrow(recapStats)){
    recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
  }
  recapStats
  
  # PE
  HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
  HTpe
  #assign to summary dataframe
  PEs[14,]=c("PK","smallmouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])
}else{
  PEs[14,]=c("PK","smallmouth_bass", rep(NA,4))
  print("not enough SMB captured for PE at PK")
}

#### STREET_SE ####

#getting data for hunter
lmb=fishIS[fishIS$lakeID=="SE" & fishIS$species=="largemouth_bass",]

#putting samples into batches for one sampling night of work
#adding a column for batches
lmb$batch=numeric(nrow(lmb))

samp=unique(lmb$sampleID)
samp
lmb$batch[lmb$sampleID%in%samp[1:3]]=1
lmb$batch[lmb$sampleID%in%samp[4:6]]=2
lmb$batch[lmb$sampleID%in%samp[7:10]]=3
lmb$batch[lmb$sampleID%in%samp[11]]=4
lmb$batch[lmb$sampleID%in%samp[12:14]]=5

#collected now
collectedNow=count(lmb, batch)

#marked now
markedNow=lmb%>%
  group_by(batch)%>%
  filter(clipApply=="AF")%>%
  summarize(markedNow=n())

#recaptured now
recapturedNow=lmb%>%
  group_by(batch)%>%
  filter(clipRecapture=="AF")%>%
  summarize(recapturedNow=n())

#combinbing collected, mark, recap data
recapStats=merge(collectedNow, markedNow, by="batch", all=T)
recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples

#calculate markedPrior for each sample
recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
for(i in 2:nrow(recapStats)){
  recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
}
recapStats

# PE
HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
HTpe
#assign to summary dataframe
PEs[15,]=c("SE","largemouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])

#SMB PE

smb=fishIS[fishIS$lakeID=="SE" & fishIS$species=="smallmouth_bass",]

if(nrow(smb[smb$clipRecapture=="AF",])>5){
  #putting samples into batches for one sampling night of work
  #adding a column for batches
  smb$batch=numeric(nrow(smb))
  
  samp=unique(smb$sampleID)
  samp
  smb$batch[smb$sampleID%in%samp[1:3]]=1
  smb$batch[smb$sampleID%in%samp[4:6]]=2
  smb$batch[smb$sampleID%in%samp[7:10]]=3
  smb$batch[smb$sampleID%in%samp[11]]=4
  smb$batch[smb$sampleID%in%samp[12:14]]=5
  
  #collected now
  collectedNow=count(smb, batch)
  
  #marked now
  markedNow=smb%>%
    group_by(batch)%>%
    filter(clipApply=="AF")%>%
    summarize(markedNow=n())
  
  #recaptured now
  recapturedNow=smb%>%
    group_by(batch)%>%
    filter(clipRecapture=="AF")%>%
    summarize(recapturedNow=n())
  
  #combinbing collected, mark, recap data
  recapStats=merge(collectedNow, markedNow, by="batch", all=T)
  recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
  recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples
  
  #calculate markedPrior for each sample
  recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
  for(i in 2:nrow(recapStats)){
    recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
  }
  recapStats
  
  # PE
  HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
  HTpe
  #assign to summary dataframe
  PEs[16,]=c("SE","smallmouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])
}else{
  PEs[16,]=c("SE","smallmouth_bass", rep(NA,4))
  print("not enough SMB captured for PE at SE")
}


#### STORMY_SM ####

#getting data for hunter
lmb=fishIS[fishIS$lakeID=="SM" & fishIS$species=="largemouth_bass",]

#putting samples into batches for one sampling night of work
#adding a column for batches
lmb$batch=numeric(nrow(lmb))

samp=unique(lmb$sampleID)
samp
lmb$batch[lmb$sampleID%in%samp[1:3]]=1
lmb$batch[lmb$sampleID%in%samp[4:6]]=2
lmb$batch[lmb$sampleID%in%samp[7:10]]=3
lmb$batch[lmb$sampleID%in%samp[11]]=4
lmb$batch[lmb$sampleID%in%samp[12:14]]=5

#collected now
collectedNow=count(lmb, batch)

#marked now
markedNow=lmb%>%
  group_by(batch)%>%
  filter(clipApply=="AF")%>%
  summarize(markedNow=n())

#recaptured now
recapturedNow=lmb%>%
  group_by(batch)%>%
  filter(clipRecapture=="AF")%>%
  summarize(recapturedNow=n())

#combinbing collected, mark, recap data
recapStats=merge(collectedNow, markedNow, by="batch", all=T)
recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples

#calculate markedPrior for each sample
recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
for(i in 2:nrow(recapStats)){
  recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
}
recapStats

# PE
HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
HTpe
#assign to summary dataframe
PEs[17,]=c("SM","largemouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])

#SMB PE

smb=fishIS[fishIS$lakeID=="SM" & fishIS$species=="smallmouth_bass",]

if(nrow(smb[smb$clipRecapture=="AF",])>5){
  #putting samples into batches for one sampling night of work
  #adding a column for batches
  smb$batch=numeric(nrow(smb))
  
  samp=unique(smb$sampleID)
  samp
  smb$batch[smb$sampleID%in%samp[1:3]]=1
  smb$batch[smb$sampleID%in%samp[4:6]]=2
  smb$batch[smb$sampleID%in%samp[7:10]]=3
  smb$batch[smb$sampleID%in%samp[11]]=4
  smb$batch[smb$sampleID%in%samp[12:14]]=5
  
  #collected now
  collectedNow=count(smb, batch)
  
  #marked now
  markedNow=smb%>%
    group_by(batch)%>%
    filter(clipApply=="AF")%>%
    summarize(markedNow=n())
  
  #recaptured now
  recapturedNow=smb%>%
    group_by(batch)%>%
    filter(clipRecapture=="AF")%>%
    summarize(recapturedNow=n())
  
  #combinbing collected, mark, recap data
  recapStats=merge(collectedNow, markedNow, by="batch", all=T)
  recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
  recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples
  
  #calculate markedPrior for each sample
  recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
  for(i in 2:nrow(recapStats)){
    recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
  }
  recapStats
  
  # PE
  HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
  HTpe
  #assign to summary dataframe
  PEs[18,]=c("SM","smallmouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])
}else{
  PEs[18,]=c("SM","smallmouth_bass", rep(NA,4))
  print("not enough SMB captured for PE at SM")
}

#### SILVER_SV ####

#getting data for hunter
lmb=fishIS[fishIS$lakeID=="SV" & fishIS$species=="largemouth_bass" ,]

#putting samples into batches for one sampling night of work
#adding a column for batches
lmb$batch=numeric(nrow(lmb))

samp=unique(lmb$sampleID)
samp
lmb$batch[lmb$sampleID%in%samp[1:3]]=1
lmb$batch[lmb$sampleID%in%samp[4:7]]=2
lmb$batch[lmb$sampleID%in%samp[8]]=3
lmb$batch[lmb$sampleID%in%samp[9:10]]=4
lmb$batch[lmb$sampleID%in%samp[11:12]]=5
lmb$batch[lmb$sampleID%in%samp[13:16]]=6

#collected now
collectedNow=count(lmb, batch)

#marked now
markedNow=lmb%>%
  group_by(batch)%>%
  filter(clipApply=="AF")%>%
  summarize(markedNow=n())

#recaptured now
recapturedNow=lmb%>%
  group_by(batch)%>%
  filter(clipRecapture=="AF")%>%
  summarize(recapturedNow=n())

#combinbing collected, mark, recap data
recapStats=merge(collectedNow, markedNow, by="batch", all=T)
recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
recapStats$recapturedNow[1]=0 # no recaps on the first sample

#calculate markedPrior for each sample
recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
for(i in 2:nrow(recapStats)){
  recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
}
recapStats

# PE
SVpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
SVpe
#assign to summary dataframe
PEs[19,]=c("SV","largemouth_bass", max(SVpe$event), SVpe[max(nrow(SVpe)),3], SVpe[max(nrow(SVpe)),2], SVpe[max(nrow(SVpe)),4])

#SMB PE

smb=fishIS[fishIS$lakeID=="SV" & fishIS$species=="smallmouth_bass",]

if(nrow(smb[smb$clipRecapture=="AF",])>5){
  #putting samples into batches for one sampling night of work
  #adding a column for batches
  smb$batch=numeric(nrow(smb))
  
  samp=unique(smb$sampleID)
  samp
  smb$batch[smb$sampleID%in%samp[1:3]]=1
  smb$batch[smb$sampleID%in%samp[4:6]]=2
  smb$batch[smb$sampleID%in%samp[7:10]]=3
  smb$batch[smb$sampleID%in%samp[11]]=4
  smb$batch[smb$sampleID%in%samp[12:14]]=5
  
  #collected now
  collectedNow=count(smb, batch)
  
  #marked now
  markedNow=smb%>%
    group_by(batch)%>%
    filter(clipApply=="AF")%>%
    summarize(markedNow=n())
  
  #recaptured now
  recapturedNow=smb%>%
    group_by(batch)%>%
    filter(clipRecapture=="AF")%>%
    summarize(recapturedNow=n())
  
  #combinbing collected, mark, recap data
  recapStats=merge(collectedNow, markedNow, by="batch", all=T)
  recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
  recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples
  
  #calculate markedPrior for each sample
  recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
  for(i in 2:nrow(recapStats)){
    recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
  }
  recapStats
  
  # PE
  HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
  HTpe
  #assign to summary dataframe
  PEs[20,]=c("SV","smallmouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])
}else{
  PEs[20,]=c("SV","smallmouth_bass", rep(NA,4))
  print("not enough SMB captured for PE at SV")
}

#### UPPER GRESHAM_UG ####

#getting data for hunter
lmb=fishIS[fishIS$lakeID=="UG" & fishIS$species=="largemouth_bass",]

#putting samples into batches for one sampling night of work
#adding a column for batches
lmb$batch=numeric(nrow(lmb))

samp=unique(lmb$sampleID)
samp
lmb$batch[lmb$sampleID%in%samp[1:3]]=1
lmb$batch[lmb$sampleID%in%samp[4:7]]=2
lmb$batch[lmb$sampleID%in%samp[8]]=3
lmb$batch[lmb$sampleID%in%samp[9:11]]=4
lmb$batch[lmb$sampleID%in%samp[12:16]]=5

#collected now
collectedNow=count(lmb, batch)

#marked now
markedNow=lmb%>%
  group_by(batch)%>%
  filter(clipApply=="AF")%>%
  summarize(markedNow=n())

#recaptured now
recapturedNow=lmb%>%
  group_by(batch)%>%
  filter(clipRecapture=="AF")%>%
  summarize(recapturedNow=n())

#combinbing collected, mark, recap data
recapStats=merge(collectedNow, markedNow, by="batch", all=T)
recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
recapStats$recapturedNow[1:4]=0 # no recaps on the first though fourth

#calculate markedPrior for each sample
recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
for(i in 2:nrow(recapStats)){
  recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
}
recapStats

# PE
UGpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
UGpe
#assign to summary dataframe
PEs[21,]=c("UG","largemouth_bass", max(UGpe$event), UGpe[max(nrow(UGpe)),3], UGpe[max(nrow(UGpe)),2], UGpe[max(nrow(UGpe)),4])

#SMB PE

smb=fishIS[fishIS$lakeID=="UG" & fishIS$species=="smallmouth_bass",]

if(nrow(smb[smb$clipRecapture=="AF",])>5){
  #putting samples into batches for one sampling night of work
  #adding a column for batches
  smb$batch=numeric(nrow(smb))
  
  samp=unique(smb$sampleID)
  samp
  smb$batch[smb$sampleID%in%samp[1:3]]=1
  smb$batch[smb$sampleID%in%samp[4:6]]=2
  smb$batch[smb$sampleID%in%samp[7:10]]=3
  smb$batch[smb$sampleID%in%samp[11]]=4
  smb$batch[smb$sampleID%in%samp[12:14]]=5
  
  #collected now
  collectedNow=count(smb, batch)
  
  #marked now
  markedNow=smb%>%
    group_by(batch)%>%
    filter(clipApply=="AF")%>%
    summarize(markedNow=n())
  
  #recaptured now
  recapturedNow=smb%>%
    group_by(batch)%>%
    filter(clipRecapture=="AF")%>%
    summarize(recapturedNow=n())
  
  #combinbing collected, mark, recap data
  recapStats=merge(collectedNow, markedNow, by="batch", all=T)
  recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
  recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples
  
  #calculate markedPrior for each sample
  recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
  for(i in 2:nrow(recapStats)){
    recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
  }
  recapStats
  
  # PE
  HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
  HTpe
  #assign to summary dataframe
  PEs[22,]=c("UG","smallmouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])
}else{
  PEs[22,]=c("UG","smallmouth_bass", rep(NA,4))
  print("not enough SMB captured for PE at UG")
}

#### PIONEER_PN ####

#getting data for hunter
lmb=fishIS[fishIS$lakeID=="PN" & fishIS$species=="largemouth_bass",]

#putting samples into batches for one sampling night of work
#adding a column for batches
lmb$batch=numeric(nrow(lmb))

samp=unique(lmb$sampleID)
samp
lmb$batch[lmb$sampleID%in%samp[1:3]]=1
lmb$batch[lmb$sampleID%in%samp[4:6]]=2
lmb$batch[lmb$sampleID%in%samp[7:10]]=3
lmb$batch[lmb$sampleID%in%samp[11]]=4
lmb$batch[lmb$sampleID%in%samp[12:14]]=5

#collected now
collectedNow=count(lmb, batch)

#marked now
markedNow=lmb%>%
  group_by(batch)%>%
  filter(clipApply=="AF")%>%
  summarize(markedNow=n())

#recaptured now
recapturedNow=lmb%>%
  group_by(batch)%>%
  filter(clipRecapture=="AF")%>%
  summarize(recapturedNow=n())

#combinbing collected, mark, recap data
recapStats=merge(collectedNow, markedNow, by="batch", all=T)
recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples

#calculate markedPrior for each sample
recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
for(i in 2:nrow(recapStats)){
  recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
}
recapStats

# PE
HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
HTpe
#assign to summary dataframe
PEs[23,]=c("PN","largemouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])

#SMB PE

smb=fishIS[fishIS$lakeID=="PN" & fishIS$species=="smallmouth_bass",]

if(nrow(smb[smb$clipRecapture=="AF",])>5){
  #putting samples into batches for one sampling night of work
  #adding a column for batches
  smb$batch=numeric(nrow(smb))
  
  samp=unique(smb$sampleID)
  samp
  smb$batch[smb$sampleID%in%samp[1:3]]=1
  smb$batch[smb$sampleID%in%samp[4:6]]=2
  smb$batch[smb$sampleID%in%samp[7:10]]=3
  smb$batch[smb$sampleID%in%samp[11]]=4
  smb$batch[smb$sampleID%in%samp[12:14]]=5
  
  #collected now
  collectedNow=count(smb, batch)
  
  #marked now
  markedNow=smb%>%
    group_by(batch)%>%
    filter(clipApply=="AF")%>%
    summarize(markedNow=n())
  
  #recaptured now
  recapturedNow=smb%>%
    group_by(batch)%>%
    filter(clipRecapture=="AF")%>%
    summarize(recapturedNow=n())
  
  #combinbing collected, mark, recap data
  recapStats=merge(collectedNow, markedNow, by="batch", all=T)
  recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
  recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples
  
  #calculate markedPrior for each sample
  recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
  for(i in 2:nrow(recapStats)){
    recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
  }
  recapStats
  
  # PE
  HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
  HTpe
  #assign to summary dataframe
  PEs[24,]=c("PN","smallmouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])
}else{
  PEs[24,]=c("PN","smallmouth_bass", rep(NA,4))
  print("not enough SMB captured for PE at PN")
}

#### WHITNEY_WN ####

#getting data for hunter
lmb=fishIS[fishIS$lakeID=="WN" & fishIS$species=="largemouth_bass",]

#putting samples into batches for one sampling night of work
#adding a column for batches
lmb$batch=numeric(nrow(lmb))

samp=unique(lmb$sampleID)
samp
lmb$batch[lmb$sampleID%in%samp[1:3]]=1
lmb$batch[lmb$sampleID%in%samp[4:6]]=2
lmb$batch[lmb$sampleID%in%samp[7:10]]=3
lmb$batch[lmb$sampleID%in%samp[11]]=4
lmb$batch[lmb$sampleID%in%samp[12:14]]=5

#collected now
collectedNow=count(lmb, batch)

#marked now
markedNow=lmb%>%
  group_by(batch)%>%
  filter(clipApply=="AF")%>%
  summarize(markedNow=n())

#recaptured now
recapturedNow=lmb%>%
  group_by(batch)%>%
  filter(clipRecapture=="AF")%>%
  summarize(recapturedNow=n())

#combinbing collected, mark, recap data
recapStats=merge(collectedNow, markedNow, by="batch", all=T)
recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples

#calculate markedPrior for each sample
recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
for(i in 2:nrow(recapStats)){
  recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
}
recapStats

# PE
HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
HTpe
#assign to summary dataframe
PEs[25,]=c("WN","largemouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])

#SMB PE

smb=fishIS[fishIS$lakeID=="WN" & fishIS$species=="smallmouth_bass",]

if(nrow(smb[smb$clipRecapture=="AF",])>5){
  #putting samples into batches for one sampling night of work
  #adding a column for batches
  smb$batch=numeric(nrow(smb))
  
  samp=unique(smb$sampleID)
  samp
  smb$batch[smb$sampleID%in%samp[1:3]]=1
  smb$batch[smb$sampleID%in%samp[4:6]]=2
  smb$batch[smb$sampleID%in%samp[7:10]]=3
  smb$batch[smb$sampleID%in%samp[11]]=4
  smb$batch[smb$sampleID%in%samp[12:14]]=5
  
  #collected now
  collectedNow=count(smb, batch)
  
  #marked now
  markedNow=smb%>%
    group_by(batch)%>%
    filter(clipApply=="AF")%>%
    summarize(markedNow=n())
  
  #recaptured now
  recapturedNow=smb%>%
    group_by(batch)%>%
    filter(clipRecapture=="AF")%>%
    summarize(recapturedNow=n())
  
  #combinbing collected, mark, recap data
  recapStats=merge(collectedNow, markedNow, by="batch", all=T)
  recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
  recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples
  
  #calculate markedPrior for each sample
  recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
  for(i in 2:nrow(recapStats)){
    recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
  }
  recapStats
  
  # PE
  HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
  HTpe
  #assign to summary dataframe
  PEs[26,]=c("WN","smallmouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])
}else{
  PEs[26,]=c("WN","smallmouth_bass", rep(NA,4))
  print("not enough SMB captured for PE at WN")
}

#### WHITE BIRCH_WB ####

#getting data for hunter
lmb=fishIS[fishIS$lakeID=="WB" & fishIS$species=="largemouth_bass",]

#putting samples into batches for one sampling night of work
#adding a column for batches
lmb$batch=numeric(nrow(lmb))

samp=unique(lmb$sampleID)
samp
lmb$batch[lmb$sampleID%in%samp[1:3]]=1
lmb$batch[lmb$sampleID%in%samp[4:6]]=2
lmb$batch[lmb$sampleID%in%samp[7:10]]=3
lmb$batch[lmb$sampleID%in%samp[11]]=4
lmb$batch[lmb$sampleID%in%samp[12:14]]=5

#collected now
collectedNow=count(lmb, batch)

#marked now
markedNow=lmb%>%
  group_by(batch)%>%
  filter(clipApply=="AF")%>%
  summarize(markedNow=n())

#recaptured now
recapturedNow=lmb%>%
  group_by(batch)%>%
  filter(clipRecapture=="AF")%>%
  summarize(recapturedNow=n())

#combinbing collected, mark, recap data
recapStats=merge(collectedNow, markedNow, by="batch", all=T)
recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples

#calculate markedPrior for each sample
recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
for(i in 2:nrow(recapStats)){
  recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
}
recapStats

# PE
HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
HTpe
#assign to summary dataframe
PEs[27,]=c("WB","largemouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])

#SMB PE

smb=fishIS[fishIS$lakeID=="WB" & fishIS$species=="smallmouth_bass",]

if(nrow(smb[smb$clipRecapture=="AF",])>5){
  #putting samples into batches for one sampling night of work
  #adding a column for batches
  smb$batch=numeric(nrow(smb))
  
  samp=unique(smb$sampleID)
  samp
  smb$batch[smb$sampleID%in%samp[1:3]]=1
  smb$batch[smb$sampleID%in%samp[4:6]]=2
  smb$batch[smb$sampleID%in%samp[7:10]]=3
  smb$batch[smb$sampleID%in%samp[11]]=4
  smb$batch[smb$sampleID%in%samp[12:14]]=5
  
  #collected now
  collectedNow=count(smb, batch)
  
  #marked now
  markedNow=smb%>%
    group_by(batch)%>%
    filter(clipApply=="AF")%>%
    summarize(markedNow=n())
  
  #recaptured now
  recapturedNow=smb%>%
    group_by(batch)%>%
    filter(clipRecapture=="AF")%>%
    summarize(recapturedNow=n())
  
  #combinbing collected, mark, recap data
  recapStats=merge(collectedNow, markedNow, by="batch", all=T)
  recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
  recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples
  
  #calculate markedPrior for each sample
  recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
  for(i in 2:nrow(recapStats)){
    recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
  }
  recapStats
  
  # PE
  HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
  HTpe
  #assign to summary dataframe
  PEs[28,]=c("WB","smallmouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])
}else{
  PEs[28,]=c("WB","smallmouth_bass", rep(NA,4))
  print("not enough SMB captured for PE at WB")
}

#### PARTRIDGE_PT ####

#getting data for hunter
lmb=fishIS[fishIS$lakeID=="PT" & fishIS$species=="largemouth_bass",]

#putting samples into batches for one sampling night of work
#adding a column for batches
lmb$batch=numeric(nrow(lmb))

samp=unique(lmb$sampleID)
samp
lmb$batch[lmb$sampleID%in%samp[1:3]]=1
lmb$batch[lmb$sampleID%in%samp[4:6]]=2
lmb$batch[lmb$sampleID%in%samp[7:10]]=3
lmb$batch[lmb$sampleID%in%samp[11]]=4
lmb$batch[lmb$sampleID%in%samp[12:14]]=5

#collected now
collectedNow=count(lmb, batch)

#marked now
markedNow=lmb%>%
  group_by(batch)%>%
  filter(clipApply=="AF")%>%
  summarize(markedNow=n())

#recaptured now
recapturedNow=lmb%>%
  group_by(batch)%>%
  filter(clipRecapture=="AF")%>%
  summarize(recapturedNow=n())

#combinbing collected, mark, recap data
recapStats=merge(collectedNow, markedNow, by="batch", all=T)
recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples

#calculate markedPrior for each sample
recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
for(i in 2:nrow(recapStats)){
  recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
}
recapStats

# PE
HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
HTpe
#assign to summary dataframe
PEs[29,]=c("PT","largemouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])

#SMB PE

smb=fishIS[fishIS$lakeID=="PT" & fishIS$species=="smallmouth_bass",]

if(nrow(smb[smb$clipRecapture=="AF",])>5){
  #putting samples into batches for one sampling night of work
  #adding a column for batches
  smb$batch=numeric(nrow(smb))
  
  samp=unique(smb$sampleID)
  samp
  smb$batch[smb$sampleID%in%samp[1:3]]=1
  smb$batch[smb$sampleID%in%samp[4:6]]=2
  smb$batch[smb$sampleID%in%samp[7:10]]=3
  smb$batch[smb$sampleID%in%samp[11]]=4
  smb$batch[smb$sampleID%in%samp[12:14]]=5
  
  #collected now
  collectedNow=count(smb, batch)
  
  #marked now
  markedNow=smb%>%
    group_by(batch)%>%
    filter(clipApply=="AF")%>%
    summarize(markedNow=n())
  
  #recaptured now
  recapturedNow=smb%>%
    group_by(batch)%>%
    filter(clipRecapture=="AF")%>%
    summarize(recapturedNow=n())
  
  #combinbing collected, mark, recap data
  recapStats=merge(collectedNow, markedNow, by="batch", all=T)
  recapStats=merge(recapStats, recapturedNow, by="batch", all=T)
  recapStats$recapturedNow[1:2]=0 # no recaps on the first and second samples
  
  #calculate markedPrior for each sample
  recapStats$markedPrior=numeric(nrow(recapStats)) #fills in all 0s, which is what we want for the first sample anyway
  for(i in 2:nrow(recapStats)){
    recapStats$markedPrior[i]=recapStats$markedNow[i-1]+recapStats$markedPrior[i-1]
  }
  recapStats
  
  # PE
  HTpe=schnabel(markedPrior = recapStats$markedPrior, collectedNow = recapStats$n, recapturedNow = recapStats$recapturedNow )
  HTpe
  #assign to summary dataframe
  PEs[30,]=c("PT","smallmouth_bass", max(HTpe$event), HTpe[max(nrow(HTpe)),3], HTpe[max(nrow(HTpe)),2], HTpe[max(nrow(HTpe)),4])
}else{
  PEs[30,]=c("PT","smallmouth_bass", rep(NA,4))
  print("not enough SMB captured for PE at PT")
}


##### CPUE vs PE PLOTTING ####
#lake perimeter info
linfo=read.csv("C:/Users/jones/Box Sync/NDstuff/CNH/proposedFishScapeLakeList_20180627.csv", header = T, stringsAsFactors = F)
linfo=linfo[linfo$lakeName%in%c("Hunter Lake", "Brandy Lake", "Day Lake", "Silver Lake", "Upper Gresham Lake"),]
perm=c(linfo$lakePerimeter[1:2], 5310, linfo$lakePerimeter[3:4])

#calculating shocking catch per mile
pe2019=PEs[PEs$nHat!=0,]
fish=fishIS[fishIS$lakeID%in%c(pe2019$lakeID),]
fish=fish[fish$gear=="BE",]
fish=fish[fish$species=="largemouth_bass",]

#totalNum
nDat <- fish %>%
  group_by(lakeID) %>%
  summarise(totalNum = n())
#totalCPE
totalCPE <- fish %>%
  group_by(lakeID, sampleID) %>%
  distinct(effort,distanceShocked)
effort <- totalCPE %>%
  group_by(lakeID) %>%
  summarize(timeEffort = sum(as.numeric(effort)), distEffortmi=(sum(as.numeric(distanceShocked))), distEffortkm=(sum(as.numeric(distanceShocked)))*1.60934) #converting mi to km
totalCPE <- nDat %>%
  left_join(effort, by="lakeID") %>%
  mutate(totalCPETime = totalNum/timeEffort, totalCPEDistMI=totalNum/distEffortmi ,totalCPEDistKM=totalNum/distEffortkm)

#combining pe and cpue data for 2019
pe2019=pe2019%>%
  inner_join(totalCPE, by="lakeID")
pe2019$permKM=perm/1000
pe2019$nHat=as.numeric(pe2019$nHat)
pe2019$fishPerKM=pe2019$nHat/pe2019$permKM

pe2018=pe2018[pe2018$lakeID!="BA",]

peCPEmi=ggplot()+theme_classic()+
  geom_point(aes(x=pe2018$nHat, y=pe2018$CPEmi_shock))+
  geom_point(aes(x=pe2019$nHat, y=pe2019$totalCPEDistMI), color="red")+
  geom_text(aes(x=pe2018$nHat, y=pe2018$CPEmi_shock, label=pe2018$lakeID), hjust=-1, vjust=1)+
  geom_text(aes(x=pe2019$nHat, y=pe2019$totalCPEDistMI, label=pe2019$lakeID), hjust=-1)+
  scale_x_continuous(name= "population estimate", limits = c(0,4000))+
  scale_y_continuous(name= "cpe (mile)", limits = c(0,50))
peCPEmi

peFishkm=ggplot()+theme_classic()+
  geom_point(aes(y=pe2018$fishPerKmShoreline, x=pe2018$CPEmi_shock))+
  geom_point(aes(y=pe2019$fishPerKM, x=pe2019$totalCPEDistMI), color="red")+
  #geom_text(aes(y=pe2018$fishPerKmShoreline, x=pe2018$CPEmi_shock, label=pe2018$lakeID), hjust=-1, vjust=1)+
  #geom_text(aes(y=pe2019$fishPerKM, x=pe2019$totalCPEDistMI, label=pe2019$lakeID), hjust=-1)+
  scale_x_continuous(name= "cpe (mile)", limits=c(0,50))+
  scale_y_continuous(name= "fish per km shoreline", limits=c(0,800))#+
  #geom_smooth(method=lm, aes(x=c(pe2018$CPEmi_shock,pe2019$totalCPEDistMI), y=c(pe2018$fishPerKmShoreline, pe2019$fishPerKM)))
peFishkm

RossaPlot=ggplot()+theme_classic()+
  geom_point(x=pe2018$CPEmi_shock, y=pe2018$fishPerKmShoreline)+
  geom_text(aes(x=pe2018$CPEmi_shock, y=pe2018$fishPerKmShoreline,label=pe2018$lakeID), hjust=-1)+
  scale_x_continuous(name= "cpe (mile)", limits=c(0,50))+
  scale_y_continuous(name= "fish per km shoreline", limits=c(0,800))+
  geom_smooth(method=lm, aes(x=pe2018$CPEmi_shock, y=pe2018$fishPerKmShoreline))
RossaPlot

peFishkm2=ggplot()+theme_classic()+
  geom_point(aes(y=pe2018$fishPerKmShoreline, x=pe2018$nHat))+
  geom_point(aes(y=pe2019$fishPerKM, x=pe2019$nHat), color="red")+
  geom_text(aes(y=pe2018$fishPerKmShoreline, x=pe2018$nHat, label=pe2018$lakeID), hjust=-1, vjust=1)+
  geom_text(aes(y=pe2019$fishPerKM, x=pe2019$nHat, label=pe2019$lakeID), hjust=-1)+
  scale_x_continuous(name= "Population Estimate")+
  scale_y_continuous(name= "fish per km shoreline", limits=c(0,800))
  #geom_smooth(method=lm, aes(x=c(pe2018$nHat,pe2019$nHat), y=c(pe2018$fishPerKmShoreline, pe2019$fishPerKM)))
peFishkm2
