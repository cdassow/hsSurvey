#walleye data wrangling from DNR creel data for VIlas county 

#clear global evnironment
rm(list=ls())

#load any packages we'll need
library(dplyr)
library(ggplot2)

#go to hsSurvey folder with creel data
setwd("C:/Users/Camille/Desktop/Fishscapes/hsSurvey/")

#load data from ecreel survey about fishing pressure
library(readxl)
creelSurvey <- read_excel("creelSurvey_FishPressure.xlsx")
View(creelSurvey_FishPressure)

#loading data from creel interview data from vilas
creel_fishdata <- read_excel("creel_raw_interview_fish_data_VO.xlsx")
View(creel_raw_interview_fish_data_VO)

#loading data from walleye creel survey sheet
WALLEYE_CPE <- read_excel("WALLEYE_FALLYOY_CPE.xlsx")
View(WALLEYE_FALLYOY_CPE)

#ORGANIZE DATA

#take out unique lakes using county then WBICs
VilasWal <- WALLEYE_CPE[WALLEYE_CPE$county=="VILAS",]

#doesn't have WBIC, join by something else first 
VilasWal2 <- creelSurvey[creelSurvey$county=="Villas",]

#has species code colums in vilas wal 3, X22=walleye
vilaswal3 <- creel_fishdata[creel_fishdata$county=="VILAS",]
#sorting by county then by walleye species code 
vilaswal3.0 <- vilaswal3[vilaswal3$fishSpeciesCode=="X22",]


#joining vilaswal and vilaswal3.0(all walleye obs) by WBIC
Walfish=VilasWal%>%
  inner_join(vilaswal3.0, by="WBIC")

#try to combine by species instead since vilaswal2 has no WBICs
#have to change column species in vilaswal2 bc of capiatlization
names(VilasWal2)[names(VilasWal2) == "Species"] <- "species"

AllWAll= Walfish%>%
  left_join(VilasWal2, by="species")

#below code did not work but part of thought process 
#sort out observations of walleye in vilaswal2 to help join
#VilasWal2.0 <- VilasWal2[VilasWal2$Species=="walleye",]

# get column names
#colnames(VilasWal2.0)

# Rename column where names is "lake" to waterbody
#names(VilasWal2.0)[names(VilasWal2.0) == "lake"] <- "waterbody"

#join walfish and vilaswal2.0 to put all data together, does not work
#ALLWalfish=Walfish%>%
  #left_join(VilasWal2.0, by = "waterbody")




