#clear global environment
rm(list = ls())

#setting working directory
setwd("C:/Users/jcaff/Documents/Jones Lab/hsSurvey")

#PACKAGES
library(dplyr)
library(tidyr)

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

creelFishint1VO<-gdriveURL("https://drive.google.com/open?id=1T_QeJTms9QmG65iT1ul_FC1H1XbhBhd5")
#sharebale link for WI-DNR fish interview data Vilas-Oneida

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

##modifying notFishAmt to time

# create a new column to store values as they are manipulated
vilasCreelLMB$output=rep(NA, nrow(vilasCreelLMB))

# use for loop to make sure every entry have 4 digits
# this also changes data type to character
for(i in 1:nrow(vilasCreelLMB)){
  if(nchar(vilasCreelLMB$notFishingAmt[i])==3){
    vilasCreelLMB$output[i]=paste0(0, vilasCreelLMB$notFishingAmt[i])
  }else if(nchar(vilasCreelLMB$notFishingAmt[i])==2){
    vilasCreelLMB$output[i]=paste0(0,0, vilasCreelLMB$notFishingAmt[i])
  }else{
    vilasCreelLMB$output[i]=vilasCreelLMB$notFishingAmt[i]
  }
}

# use tidyverse seperate to seperate hours from minutes and store in new dataframe 
library(tidyverse)
vilasCreelLMB2= vilasCreelLMB%>% separate(output,c("hour", "min"),sep=c(2,4))

# change both columns to numeric 
vilasCreelLMB2$hour=as.numeric(vilasCreelLMB2$hour)
vilasCreelLMB2$min=as.numeric(vilasCreelLMB2$min)

# convert to total hours by multiplying hours by 60 and adding minutes 
vilasCreelLMB2$adjNotFishAMT=vilasCreelLMB2$hour+vilasCreelLMB2$min/60

#entering adjusted notFishAmt back to original column
vilasCreelLMB2$notFishingAmt<-vilasCreelLMB2$adjNotFishAMT
vilasCreelLMB2<-vilasCreelLMB2[,c(1:13)]

#replacing NA's with 0
vilasCreelLMB2$notFishingAmt[is.na(vilasCreelLMB2$notFishingAmt)]<-0

#calculating effort
vilasCreelLMB2$fishedPerc<-vilasCreelLMB2$fishedPerc/100
vilasCreelLMB2$effort<-((difftime(vilasCreelLMB2$dateSample,vilasCreelLMB2$dateSet,units = "hours","minutes"))-vilasCreelLMB2$notFishingAmt)*vilasCreelLMB2$fishedPerc

vilasCreelLMB2$effort<-as.numeric(vilasCreelLMB2$effort)
#calculating anglerCPUE

vilasCreelLMB2$anCPUE<-(vilasCreelLMB2$caughtAmt/(vilasCreelLMB2$effort*vilasCreelLMB2$anglersAmt))

vilasAnCPUE<-vilasCreelLMB2[,c(1,2,3,4,10,13,14,15)]

