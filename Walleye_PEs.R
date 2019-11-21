#walleye data wrangling from DNR creel data for VIlas county 

#clear global evnironment
rm(list=ls())

#load any packages we'll need
library(dplyr)
library(ggplot2)

#go to hsSurvey folder with creel data
setwd("C:/Users/Camille/Desktop/Fishscapes/hsSurvey/")

#load data from excel spreadsheet
library(readxl)
creelSurvey_FishPressure <- read_excel("creelSurvey_FishPressure.xlsx")
View(creelSurvey_FishPressure)

#sort data for walleye, use species column 
  if(creelSurvey_FishPressure$Species=="walleye"){
   wallobs <- (creelSurvey_FishPressure$Species=="walleye")
  }else{
    othrfish <- creelSurvey_FishPressure$Species 
  }
