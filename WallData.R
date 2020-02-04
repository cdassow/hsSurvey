#Walleye data wrangling 

#clear global evnironment
rm(list=ls())

#load any packages we'll need
library(dplyr)
library(ggplot2)

#go to hsSurvey folder 
setwd("C:/Users/Camille/Desktop/Fishscapes/hsSurvey/")

gdriveURL <- function(x){
  x =  
    upURL = sub("^[^=]*", "", x)
  y1 =  "https://docs.google.com/uc?id"
  y2 = "&export=download"
  downURL = paste0(y1,upURL,y2)
  read.csv(downURL, header = TRUE)
}

#get walleye pe data from dnr and load data
walldnr<-gdriveURL("https://drive.google.com/open?id=1DPRROWv6Cf_fP6Z-kE9ZgUfdf_F_jSNT")

#get creel interview data and load
creelindata<-gdriveURL("https://drive.google.com/open?id=1pyCKCcAQZiNZz-tX5U2QnZUc79OQEWX2")

#get lake info data
walylinfo<-gdriveURL("https://drive.google.com/open?id=1RiiiT4nmAkWyYIr7VmPk3iPCtTKlGHli")

#subset data for vilas and walleye species 
walldnrvilas<-walldnr[walldnr$county=="VILAS",]
vilaswallPE<-walldnrvilas[walldnrvilas$species=="WALLEYE",]
vilaswallPE<- vilaswallPE[,c(1,2,3,5,19:27)]
#vilaswallPE now has dnr walleye, PE data for only vilas county walleye species observations

creeldata<-creelindata[creelindata$county=="VILAS",]
creeldatawall<-creeldata[creeldata$speciesCode=="X22",]
creeldatawall<-creeldatawall[,c(1,2,3,6,15,16,17)]
#sorting to get columns for wbics lake yr fish counts and fish lengths 
#sorting to get important columns
#creeldatawall now has dnr creel interview data for only vilas county walleye species observations

wallinfoV<-walylinfo[walylinfo$county=="Vilas",]
wallinfo=wallinfoV[grep('Walleye', wallinfoV$fishPresent),]
#all the lakes from this data have walleye, did grep to make sure
#wallinfo=wallinfoV[grep('Walleye', wallinfoV$fishPresent),] View(wallinfoV)
#musky=wallinfoV[grep('Musky', wallinfoV$fishPresent),]

#combining PE data and lake info data by WBICs

vilasWallLinfo<-left_join(walldnrvilas,walylinfo)
#join by columns with the same name,by = c("county", "WBIC")


wbicsYear<-vilasWallLinfo[,c(1,2,3,5,16,18,19,23,24,27)]

wbicsYearCreelwall<-creeldatawall[,c(1,2,3,4)]
#joining creel and lake characterisitcs and fish data together
wbicsCreelLinfo<-left_join(vilasWallLinfo,wbicsYearCreelwall)

wbicsunique<-unique(wbicsCreelLinfo$WBIC)
wbicsunique

#need to check years for electrofishing data

#bringing in Walleye PE from DNR/fishscapes data

walltreaty<-gdriveURL("https://drive.google.com/open?id=1EYaQpLr_Hp9YbARWFS3tgE6L_tEtWV0G")

#removing unimportant info
wallPE<-walltreaty[,c(1:4,8,11)]
#see how many years of data there is for walleye population estimates
unique(walltreaty$surveyYear)
#1995-2015, 18 yrs

#find CPUE catch/effort
#sorting for walleye species by code,
creeldatawall<-creelindata[creelindata$speciesCode=="X22",]
creelwall<-creeldatawall[,c(1:3,6,12,18,26,25,30,35)]

#adjusting date columns for creel data, to reflect time and not regular integers
for (i in 1:length(creelwall$timeStart)) {
  if (nchar(creelwall$timeStart[i])<4){
    creelwall$timeStart[i]<-paste("0",creelwall$timeStart[i],sep = "")
  }
}

for (i in 1:length(creelwall$timeEnd)) {
  if (nchar(creelwall$timeEnd[i])<4){
    creelwall$timeEnd[i]<-paste("0",creelwall$timeEnd[i],sep = "")
  }
}

#create dateSet Column
creelwall$dateSet<-paste(creelwall$dateSample,creelwall$timeStart)
#John's format 1/1/2020 time, my format 2004-01-24 time
creelwall$dateSet<-as.POSIXct(creelwall$dateSet,format="%Y-%m-%d %H%M")

#create dateSample Column
creelwall$dateSample<-paste(creelwall$dateSample,creelwall$timeEnd)
creelwall$dateSample<-as.POSIXct(creelwall$dateSample,format="%Y-%m-%d %H%M")
#modifying for non fish amt time
creelwall$output=rep(NA, nrow(creelwall))

# use for loop to make sure every entry have 4 digits
# this also changes data type to character
for(i in 1:nrow(creelwall)){
  if(nchar(creelwall$notFishingAmt[i])==3){
    creelwall$output[i]=paste0(0, creelwall$notFishingAmt[i])
  }else if(nchar(creelwall$notFishingAmt[i])==2){
    creelwall$output[i]=paste0(0,0, creelwall$notFishingAmt[i])
  }else{
    creelwall$output[i]=creelwall$notFishingAmt[i]
  }
}

# use tidyverse seperate to seperate hours from minutes and store in new dataframe 
library(tidyverse)
creelwall2= creelwall%>% separate(output,c("hour", "min"),sep=c(2,4))

# change both columns to numeric 
creelwall2$hour=as.numeric(creelwall2$hour)
creelwall2$min=as.numeric(creelwall2$min)

# convert to total hours by multiplying hours by 60 and adding minutes 
creelwall2$adjNotFishAMT=creelwall2$hour+creelwall2$min/60

#entering adjusted notFishAmt back to original column
creelwall2$notFishingAmt<-creelwall2$adjNotFishAMT
creelwall2<-creelwall2[,c(1:13)]

#replacing NA's with 0
creelwall2$notFishingAmt[is.na(creelwall2$notFishingAmt)]<-0

#calculating effort
creelwall2$effort<-((difftime(creelwall2$dateSample,creelwall2$dateSet,units = "hours","minutes"))-creelwall2$notFishingAmt)

creelwall2$effort<-as.numeric(creelwall2$effort)
#calculating anglerCPUE

creelwall2$anCPUE<-(creelwall2$fishCount/(creelwall2$effort*creelwall2$anglersAmt))

WallAnCPUE<-creelwall2[,c(1,2,3,4,10,13,14,15)]




#equation for whole boat CPUE [[time(end-start)-notfish]x number of anglers]/catch
#creating loop to calculate CPUEs for each row 
  for(i in 1:nrow(creelwall)){
  #end time - start time
  time <- creelwall[,7]-creelwall[,8]
  #time - non fishing time
  num <- time - creelwall[,9]
  #total fish time multiplied by the # of anglers divded by catch
  CPUE <- num*creelwall[,6]/creelwall[10]
  #create CPUE column
  creelwall$CPUE[i]=CPUE
  }









#have some CPUE and treaty PEs need to see which WBICS both tables have to do comparison
#check wbics for electrofishing data
