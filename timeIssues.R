rm(list=ls())

# fake data
fake=data.frame(time=c(130, 30, 230, 1015, 30, 100))
# create a new column to store values as they are manipulated
fake$output=rep(NA, length(fake))

# use for loop to make sure every entry have 4 digits
# this also changes data type to character
for(i in 1:nrow(fake)){
  if(nchar(fake$time[i])==3){
    fake$output[i]=paste0(0, fake$time[i])
  }else if(nchar(fake$time[i])==2){
    fake$output[i]=paste0(0,0, fake$time[i])
  }else{
    fake$output[i]=fake$time[i]
  }
}

# use tidyverse seperate to seperate hours from minutes and store in new dataframe 
library(tidyverse)
fake2=fake %>% separate(output,c("hour", "min"),sep=c(2,4))

# change both columns to numeric 
fake2$hour=as.numeric(fake2$hour)
fake2$min=as.numeric(fake2$min)

# convert to total minutes by multiplying hours by 60 and adding minutes 
fake2$totalMinutes=fake2$hour*60+fake2$min
