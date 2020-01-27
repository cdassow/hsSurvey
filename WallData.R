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

#need to check years for electrofishing data


