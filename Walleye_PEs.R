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

#take out unique lakes using WBICs
VilasWal <- WALLEYE_CPE[WALLEYE_CPE$county=="VILAS",]

VilasWal2 <- creelSurvey[creelSurvey$county=="Villas",]

vilaswal3 <- creel_fishdata[creel_fishdata$county=="VILAS",]





