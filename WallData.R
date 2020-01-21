#Walleye data wrangling 

#clear global evnironment
rm(list=ls())

#load any packages we'll need
library(dplyr)
library(ggplot2)

#go to hsSurvey folder with creel data
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

#get creel length data and load
creeldata<-gdriveURL("https://drive.google.com/open?id=1-d90GHr4iq_xdycFnkis1VKoYqaps00c")

#get creel interview data and load
creelindata<-gdriveURL("https://drive.google.com/open?id=1pyCKCcAQZiNZz-tX5U2QnZUc79OQEWX2")

#get lake info data
walylinfo<-gdriveURL("https://drive.google.com/open?id=1RiiiT4nmAkWyYIr7VmPk3iPCtTKlGHli")

#subset data for vilas and walleye species 
walldnrvilas<-walldnr[walldnr$county=="VILAS",]
vilaswallPE<-walldnrvilas[walldnrvilas$species=="WALLEYE",]
#vilaswallPE now has dnr walleye PE data for only vilas county walleye species observations

creeldataV<-creeldata[creeldata$County=="VILAS",]
creeldatawall<-creeldataV[creeldataV$Species.Code=="X22",]
#creeldatawall now has dnr creel length data for only vilas county walleye species observations

creeldataVC<-creelindata[creelindata$county=="VILAS",]
creeldatawall2<-creeldataVC[creeldataVC$speciesCode=="X22",]
#creeldatawall2 now has dnr creel interview data for only vilas county walleye species observations

wallinfoV<-walylinfo[walylinfo$county=="Vilas",]
wallinfo=wallinfoV[grep('Walleye', wallinfoV$fishPresent),]
#all the lakes from this data have walleye, did grep to make sure
#wallinfo=wallinfoV[grep('Walleye', wallinfoV$fishPresent),] View(wallinfoV)
#musky=wallinfoV[grep('Musky', wallinfoV$fishPresent),]

#combining PE data and lake info data by WBICs
vilasWallinfo<-inner_join(wallinfo,vilaswallPE,by="WBIC")

creeldatawallsort<-creeldatawall[,c(1,2,3,6,15,16,17)]
#sorting to get columns for wbics lake yr fish counts and fish lengths 

creeldatasort2<-creeldatawall2[,c(1,2,3,6,25,26,30,36,37,38)]
#sorting to get important columns

vilasWallinfosort<-vilasWallinfo[,c(1:5,13,18,31:40)]

#joining data by wbic, removing columns without important info
WallData<-full_join(vilasWallinfosort,creeldata,by="WBIC")
WallData<-WallData[,c(1:33,35)]
WallData<-WallData[WallData$County=="VILAS",]
WallData<-WallData[WallData$Species.Code=="X22",]
#had to resort info from the creel and linfo maybe because of full join instead of inner?

#combine last bit of data

#start on bluegill data for abundance + lake data 
