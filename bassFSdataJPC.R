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

#Loading BASS data from Google Drive
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

#subsetting DNR Bass information to Vilas county
dnrBassVilas<-dnrBass[dnrBass$county=="VILAS",]


#joining lake info to dnrBassVilas
vilasBassLinfo<-semi_join(lakeLMB,dnrBassVilas,by="WBIC")
vilasBassFish=dnrBassVilas%>%
  inner_join(vilasBassLinfo,by="WBIC")

#subsetting LMB and SMB data from creel data
creelLMB<-creelFishint1VO[creelFishint1VO$fishSpeciesCode=="W12",]
creelSMB<-creelFishint1VO[creelFishint1VO$fishSpeciesCode=="W11",]

#pulling out vilas county creel data
vilasCreelLMB<-creelLMB[creelLMB$county=="VILAS",]
vilasCreelSMB<-creelSMB[creelSMB$county=="VILAS",]

#selecting rows of creel data needed
vilasCreelLMBsort<-vilasCreelLMB[,c(1,2,3,6,25,26,30,36,37,38)]
vilasCreelSMBsort<-vilasCreelSMB[,c(1,2,3,6,25,26,30,36,37,38)]

#sort to lake and year and unique values
wbicsYear<-vilasBassFish[,c(1,2,3,5)]
wbicsBassFishUniq<-unique(wbicsYear)#generate data frame with list of lakes which lake data is present and bass BE surveys have been conducted for.

#repeating the wbics sort for Creel data
#LMB
wbicsYearCreelLMB<-vilasCreelLMBsort[,c(1,2,3,4)]
wbicsCreelLMBUniq<-unique(wbicsYearCreelLMB) #generate data frame with list of lakes that have had creel data for LMB and the year the surveys were conducted

#SMB
wbicsYearCreelSMB<-vilasCreelSMBsort[,c(1,2,3,4)]
wbicsCreelSMBUniq<-unique(wbicsYearCreelSMB)

#renaming the surveyYear column to surveyYearCreel and surveyYearBE
wbicsBassFishUniq<-wbicsBassFishUniq%>%
  rename(lakeName=waterbody,county=county.x)
wbicsCreelLMBUniq<-wbicsCreelLMBUniq%>%
  rename(surveyYearCreel=surveyYear)
wbicsCreelSMBUniq<-wbicsCreelSMBUniq%>%
  rename(surveyYearCreel=surveyYear)

#joining creel surveys to list of years where both SMB and LMB surveys occured
vilasCreelWBICS=inner_join(wbicsCreelLMBUniq,wbicsCreelSMBUniq,by=c("WBIC","county","lakeName","surveyYearCreel"))

#joining bass abundance to creel surveys

vilasWBICSLMB=inner_join(wbicsBassFishUniq,vilasCreelWBICS,by=c("WBIC","county","lakeName"))
             