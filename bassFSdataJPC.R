#Looking for existing data for LMB and SMB
#JC 19.11.19 (like racecar)

#clear global environment
rm(list = ls())

#setting working directory
setwd("C:/Users/jcaff/Documents/Jones Lab/hsSurvey")

#PACKAGES
library(dplyr)

##Loading Data##

#gDrive Upload script:
gdriveURL <- function(x){
  x =  
    upURL = sub("^[^=]*", "", x)
  y1 =  "https://docs.google.com/uc?id"
  y2 = "&export=download"
  downURL = paste0(y1,upURL,y2)
  read.csv(downURL, header = TRUE, stringsAsFactors = FALSE)
}

###Loading BASS data from Google Drive###
dnrBass<-gdriveURL("https://drive.google.com/open?id=11v8FbT2wnKx_CqUfxu_V9r_8fyCfcdD2")
#shareable link for WI-DNR Bass Abundance data

#Loading Creel (Vilas and Oneida) data from Google Drive
creelFishLengVO<-gdriveURL("https://drive.google.com/open?id=1zDM2mn2A0xsaRvsf0r6Gh1Orco-kculc")
#shareable link for WI-DNR length data Vilas-Oneida

creelFishint1VO<-gdriveURL("https://drive.google.com/open?id=1T_QeJTms9QmG65iT1ul_FC1H1XbhBhd5")
#sharebale link for WI-DNR fish interview data Vilas-Oneida

lakeLMB<-gdriveURL("https://drive.google.com/open?id=1FArMiTuOsoC4E08nssZUpCekSkhkMTow")
#lake info from WI LMB lakes




##Organizing data##

## ALL COUNTIES ##

#removing extraneous columns from dnrBass
dnrBass<-dnrBass[,c(1,2,3,5,12,13,27,28,29)]

#removing extraneous columns from lakeLMB
lakeLMB<-lakeLMB[,c(1,2,3,4,14)]

#Joing lake info to dnrBass
BassLinfo<-semi_join(lakeLMB,dnrBass,by="WBIC")
BassFish=dnrBass%>%
  inner_join(BassLinfo,by="WBIC")


## VILAS ##

#subsetting DNR Bass information to Vilas county ### ONLY IF NEEDED
dnrBassVilas<-dnrBass[dnrBass$county=="VILAS",]

#joining lake info to dnrBassVilas
vilasBassLinfo<-semi_join(lakeLMB,dnrBassVilas,by="WBIC")
vilasBassFish=dnrBassVilas%>%
  inner_join(vilasBassLinfo,by="WBIC")

#sort to lake and year and unique values
wbicsYear<-vilasBassFish[,c(1,2,3,5)]
wbicsBassFishUniq<-unique(wbicsYear)#generate data frame with list of lakes which lake data is present and bass BE surveys have been conducted for.

wbicsBassFishUniq<-wbicsBassFishUniq%>%
  rename(lakeName=waterbody,county=county.x)


#####  CREEL DATA   #####
#subsetting LMB and SMB data from creel data
creelLMB<-creelFishint1VO[creelFishint1VO$fishSpeciesCode=="W12",]
creelSMB<-creelFishint1VO[creelFishint1VO$fishSpeciesCode=="W11",]

#pulling out vilas county creel data
vilasCreelLMB<-creelLMB[creelLMB$county=="VILAS",]
vilasCreelSMB<-creelSMB[creelSMB$county=="VILAS",]

#selecting columns of creel data needed
vilasCreelLMB<-vilasCreelLMB[,c(1,2,3,6,12,18,25,26,30,36,37,38)]
vilasCreelSMB<-vilasCreelSMB[,c(1,2,3,6,12,18,25,26,30,36,37,38)]

#remove NA's
vilasCreelLMB<-vilasCreelLMB[is.na(vilasCreelLMB$WBIC)==FALSE,]
vilasCreelSMB<-vilasCreelSMB[is.na(vilasCreelSMB$WBIC)==FALSE,]

#selecting for rows with by fishedPerct
vilasCreelLMB<-vilasCreelLMB[vilasCreelLMB$fishedPerc!=0,]
vilasCreelSMB<-vilasCreelSMB[vilasCreelSMB$fishedPerc!=0,]

#reformating Date Columns


#adding 0 character to 3-digit times
for (i in 1:length(vilasCreelLMB$timeStart)) {
  if (nchar(vilasCreelLMB$timeStart[i])<4){
    vilasCreelLMB$timeStart[i]<-paste("0",vilasCreelLMB$timeStart[i],sep = "")
  }
}

for (i in 1:length(vilasCreelLMB$timeEnd)) {
  if (nchar(vilasCreelLMB$timeEnd[i])<4){
    vilasCreelLMB$timeEnd[i]<-paste("0",vilasCreelLMB$timeEnd[i],sep = "")
  }
}

#create dateSet Column
vilasCreelLMB$dateSet<-paste(vilasCreelLMB$dateSample,vilasCreelLMB$timeStart)
vilasCreelLMB$dateSet<-as.POSIXct(vilasCreelLMB$dateSet,format="%m/%d/%y %H%M")

#create dateSample Column
vilasCreelLMB$dateSample<-paste(vilasCreelLMB$dateSample,vilasCreelLMB$timeEnd)
vilasCreelLMB$dateSample<-as.POSIXct(vilasCreelLMB$dateSample,format="%m/%d/%y %H%M")

#modifying notFishAmt to time
for (i in 1:length(vilasCreelLMB$notFishingAmt)) {
  vilasCreelLMB$notFishingAmt[i]<-(vilasCreelLMB$notFishingAmt[i]/100)
}

vilasCreelLMB$notFishingAmt<-as.character(vilasCreelLMB$notFishingAmt)

for (i in 1:length(vilasCreelLMB$notFishingAmt)){
  vilasCreelLMB$notFishingAmt[i]<-strsplit(vilasCreelLMB$notFishingAmt[i],"/.")
}

?strsplit

#calculating effort
vilasCreelLMB$effort<-difftime(vilasCreelLMB$dateSample,vilasCreelLMB$dateSet,units = "hours","minutes")


#repeating the wbics sort for Creel data
#LMB
wbicsYearCreelLMB<-vilasCreelLMBsort[,c(1,2,3,4)]
wbicsCreelLMBUniq<-unique(wbicsYearCreelLMB) #generate data frame with list of lakes that have had creel data for LMB and the year the surveys were conducted

#SMB
wbicsYearCreelSMB<-vilasCreelSMBsort[,c(1,2,3,4)]
wbicsCreelSMBUniq<-unique(wbicsYearCreelSMB)

#renaming the surveyYear column to surveyYearCreel and surveyYearBE
wbicsCreelLMBUniq<-wbicsCreelLMBUniq%>%
  rename(surveyYearCreel=surveyYear)
wbicsCreelSMBUniq<-wbicsCreelSMBUniq%>%
  rename(surveyYearCreel=surveyYear)




#joining creel surveys to list of years where both SMB and LMB surveys occured
vilasCreelWBICS=inner_join(wbicsCreelLMBUniq,wbicsCreelSMBUniq,by=c("WBIC","county","lakeName","surveyYearCreel"))

#joining bass abundance to creel surveys

vilasWBICSLMB=inner_join(wbicsBassFishUniq,vilasCreelWBICS,by=c("WBIC","county","lakeName"))
             
