#10.31.19 Transforming be cpues to fish/km using relationship built in relativeAbundance.R script

rm(list=ls())
#PACKAGES
library(dplyr)

#### DATA ####
setwd("C:/Users/jones/BoxSync/NDstuff/CNH/hsSurvey")
full=read.csv("pe_cpue_ModelBuild.csv", header = T, stringsAsFactors = F)
noWS=full[1:16,]

#fishscapes angling cpues averaged for each lake
anCPUE=read.csv("meanLakeCPUE.csv", stringsAsFactors = F)

#bringin lakeID info from mfe db
setwd("~/../BoxSync/NDstuff/ND_R")
source("dbUtil.r")
dbTableList()
dbLinfo=dbTable("LAKES")
dbfishI=dbTable('FISH_INFO')
dbfishS=dbTable('FISH_SAMPLES')
fishDB=dbfishS%>%
  inner_join(dbfishI, by="sampleID")
fishDB$dayOfYear=as.numeric(fishDB$dayOfYear)
fishDB$projectID=as.numeric(fishDB$projectID)
fishDB$fishNum=as.numeric(fishDB$fishNum)
#bring in inseason database
setwd("C:/Users/jones/OneDrive/temp_fishscapesCSVentries")
fishI.is=read.csv("fishInfoIS.csv")
fishS.is=read.csv("fishSamplesIS.csv")
#combining sample and fish info together
fishIS=fishI.is%>%
  inner_join(fishS.is, by="sampleID")%>%
  filter(species%in% c("largemouth_bass", "smallmouth_bass"))
#reformatting date columns
fishIS$dateSet=as.Date(as.POSIXct(fishIS$dateSet))
fishIS$dateSample=as.Date(as.POSIXct(fishIS$dateSample))
fishIS$dateTimeSet=as.POSIXct(fishIS$dateTimeSet, format= "%m/%d/%Y %H:%M:%S")
fishIS$dateTimeSample=as.POSIXct(fishIS$dateTimeSample, format= "%m/%d/%Y %H:%M:%S")

#combining db and inseason db data together
fish=fishIS%>%
  bind_rows(fishDB)%>%
  filter(projectID==37, species=="largemouth_bass")
#makeing lakeID column and year column
for(i in 1:nrow(fish)){
  fish$lakeID[i]=strsplit(as.character(fish$siteID[i]), split = '_')[[1]][1]
  fish$year[i]=strsplit(as.character(fish$dateSample[i]), split = "-")[[1]][1]
}

gdriveURL <- function(x){
  x =  
    upURL = sub("^[^=]*", "", x)
  y1 =  "https://docs.google.com/uc?id"
  y2 = "&export=download"
  downURL = paste0(y1,upURL,y2)
  read.csv(downURL, header = TRUE)
}

#combine wood and lake development data 

#lake info, specifically lake development, for NHLD region
lakes=gdriveURL("https://drive.google.com/open?id=1KcpLmmqi702xK_qvKn72VBvhAH_IfYlt") 

# wood data, for NHLD region
cwh=gdriveURL("https://drive.google.com/open?id=1x1_JdeamiU2auqrlPQ3G_wA6Spuf0vwf") 

linfo=cwh%>%
  inner_join(lakes, by="WBIC")%>%
  select(lakeID,WBIC,surveyYear,perimeter,area,secchi,conductance,conductivityQuintile,lakeAccess,build2011,build2012,build2013,numberBuildings,buildingsPerKm,buildingsPerKmQuantile,CWH.greater.than.10cm.per.km.shoreline,CWH.greater.than.5cm.but.less.than.10cm.per.km.shoreline,Total.CWH.per.km.shoreline)
linfo$WBIC=as.character(linfo$WBIC)
##### DATA WRANGLING ####


fish$distanceShocked[fish$sampleID=='LV_wholeShoreline_20190708_2225_BE_FishscapesSurvey0.5mile.20180606']=0.5 #need to manually fix one distance shocked that didn't get corrected in fish EntryTool
#fishing effort
effort <- fish %>%
  filter(gear=="BE")%>%
  group_by(sampleID, lakeID, year) %>%
  distinct(distanceShocked)%>%
  mutate(distanceShockedKM=(as.numeric(distanceShocked)*1.609))%>%
  filter(is.na(distanceShocked)==F)%>%
  filter(distanceShocked!="NA")%>%
  filter(distanceShocked!=0)

#number caught
nDat <- fish %>%
  filter(gear=="BE")%>%
  group_by(sampleID, lakeID, year) %>%
  summarise(totalNum = n())

#CPUE for each sample at each lake
indvCPE <- nDat %>%
  left_join(effort, by=c("sampleID", "lakeID", "year"))%>%
  mutate(sampleCPE = totalNum/distanceShockedKM)

#CPE for each lake pooled across day and year
lkCPE <- indvCPE%>%
  group_by(lakeID)%>%
  summarize(lkmeanCPE=mean(sampleCPE, na.rm=T), lksdCPE=sd(sampleCPE, na.rm = T))


#MODEL FIT AGAIN BUT FLIPPING THE X AND Y VARIABLES
fitWS1=lm(noWS$fishPerKM~noWS$lkmeanCPE) 
summary(fitWS1)

#GENERATE NEW FISH PER KM

ra=fitWS1$coefficients[1]+(lkCPE$lkmeanCPE[lkCPE$lakeID%in%noWS$lakeID==F]*fitWS1$coefficients[2])

ra=cbind(lkCPE[lkCPE$lakeID%in%noWS$lakeID==F,],ra)

#creating estimated abundances using our lm for the lakes where we have real abundances
Fra=fitWS1$coefficients[1]+(noWS$lkmeanCPE*fitWS1$coefficients[2])


#PLOT BACK CALCULATED DATA ONTO OBSERVED DATA
plot(noWS$lkmeanCPE, noWS$fishPerKM, pch=16, xlab = "Electrofishing CPUE", ylab = "Fish per Km Shoreline")
abline(fitWS1)
points(ra$lkmeanCPE, ra$ra)

#plots of residuals
plot(fitWS1$residuals, noWS$conductance[-c(4,9,16)])
plot(fitWS1$residuals, noWS$fishPerKM[-c(4,9,16)])
plot(fitWS1$residuals, noWS$lkmeanCPE[-c(4,9,16)])
plot(fitWS1$residuals, Fra[-c(4,9,16)])

#COMBINE OBSERVED AND BACK CALCULATED DATA TOGETHER

noWS=noWS[,c(1,14:16)]
colnames(ra)=colnames(noWS)

abund=rbind(noWS,ra)

#lining up with angling data
colnames(anCPUE)=c("lakeID", "anMeanCPUE", "anSdCPUE")

allCPE=abund%>%
  full_join(anCPUE,by="lakeID")
allCPE=allCPE[is.na(allCPE$fishPerKM)==F,]

allCPE=left_join(allCPE, dbLinfo[,c(1,2,11)], by="lakeID")

allCPE=left_join(allCPE, linfo[,c(2,4:8,10:18)], by="WBIC")
allCPE=allCPE[allCPE$lakeID!="AV",]

#write.csv(allCPE, "C:/Users/jones/Box Sync/NDstuff/CNH/hsSurvey/survey_AN_BE_CPUE.csv", row.names=F)


##### ESTIMATING BETA AND PLOTTING ####
allCPE=read.csv("C:/Users/jones/Box Sync/NDstuff/CNH/hsSurvey/survey_AN_BE_CPUE.csv", stringsAsFactors = F)

plot(allCPE$fishPerKM, allCPE$anMeanCPUE, pch=16, ylab = "Mean Angler CPUE", xlab = "Fish per km Shoreline")
plot(allCPE$lkmeanCPE, allCPE$anMeanCPUE, pch=16, ylab = "Mean Angler CPUE", xlab = "Electrofishing CPUE")

plot(allCPE$fishPerKM[is.na(allCPE$Total.CWH.per.km.shoreline)==F], allCPE$anMeanCPUE[is.na(allCPE$Total.CWH.per.km.shoreline)==F], pch=16, ylab = "Mean Angler CPUE", xlab = "Fish per km Shoreline")

plot(allCPE$lkmeanCPE[is.na(allCPE$Total.CWH.per.km.shoreline)==F], allCPE$anMeanCPUE[is.na(allCPE$Total.CWH.per.km.shoreline)==F], pch=16, ylab = "Mean Angler CPUE", xlab = "Electrofishing CPUE")

betaNLL.cwh=function(parms, CWH, N, CPE){
  a0=parms[1]
  aCWH=parms[2]
  shapeCPE=parms[3]
  q=(parms[4])
  
  b=a0+aCWH*CWH
  CPEhat=q*N^b
  
  nll=-sum(dgamma(x=CPE, shape = shapeCPE, scale = (CPEhat/shapeCPE), log = T))
  return(nll)
}

guess=c(a0=.3, aCWH=0.005, shapeCPE=4, q=.003)
fit.cwh1=optim(guess, fn=betaNLL.cwh, 
          CWH=allCPE$Total.CWH.per.km.shoreline[is.na(allCPE$Total.CWH.per.km.shoreline)==F],
          N=allCPE$fishPerKM[is.na(allCPE$Total.CWH.per.km.shoreline)==F],
          CPE=allCPE$anMeanCPUE[is.na(allCPE$Total.CWH.per.km.shoreline)==F], 
          control=list(maxit=1000))
fit.cwh1
#model fits estimate aCWH to be close to 0 meaning the slope of the relationship between beta and CWH is near 0 suggesting that CWH has no effect on beta

fit.cwh2=optim(guess, fn=betaNLL.cwh, 
               CWH=allCPE$Total.CWH.per.km.shoreline[is.na(allCPE$Total.CWH.per.km.shoreline)==F],
               N=allCPE$lkmeanCPE[is.na(allCPE$Total.CWH.per.km.shoreline)==F],
               CPE=allCPE$anMeanCPUE[is.na(allCPE$Total.CWH.per.km.shoreline)==F], 
               control=list(maxit=1000))
fit.cwh2
#same result, aCWH is 0 suggesting no effect of CWH on beta
betaNLL=function(parms, N, CPE){
  b=parms[1]
  shapeCPE=parms[2]
  q=(parms[3])
  
  CPEhat=q*N^b
  
  nll=-sum(dgamma(x=CPE, shape = shapeCPE, scale = (CPEhat/shapeCPE), log = T))
  return(nll)
}

guess=c(b= .3,shapeCPE=.5, q=.003)
fit=optim(guess, fn=betaNLL,
          N=allCPE$fishPerKM[is.na(allCPE$Total.CWH.per.km.shoreline)==F],
          CPE=allCPE$anMeanCPUE[is.na(allCPE$Total.CWH.per.km.shoreline)==F],
          control=list(maxit=1000))
fit
