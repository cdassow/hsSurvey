##### Script for generating shoreline building density for lakes in hyperstability survey DNR dataset
##### SEJ
##### 4-13-2020
rm(list=list())

# load spatial packages
library(rgdal)
library(rgeos)
library(raster)
library(geosphere)
library(sf)

####################
# Load WDNR HYDRO 24K
####################
setwd("~/Documents/Research/People/Students/current/Mosley_Camille/codeHelp/hyperstabilitySurveyBuildingDensities/WDNR_HYDRO_24K/")
gdb='WDNR_HYDRO_24K.gdb'
ogrListLayers(gdb)

# load a layer from a geodatabase
WDNRwbic=readOGR(gdb,layer="WD_HYDRO_WATERBODY_WBIC_AR_24K")
wbic=as.data.frame(WDNRwbic,stringsAsFactors=FALSE)

# project WDNR 24K in NAD83 (epsg:4269)
# epsg:4326 is WGS84
WDNRwbic_NAD83=spTransform(WDNRwbic,CRS("+init=epsg:4269"))


# get list of county geospatial files
setwd("~/Documents/Research/People/Students/current/Mosley_Camille/codeHelp/hyperstabilitySurveyBuildingDensities/HsSurvey_WI_buildings/")
countyFiles=list.files()
countyFiles=countyFiles[!grepl("x$",countyFiles)]

# need list of wbics for each county...
HSwbics=read.csv()

# data frame to store output
HSbuildingsSummary=data.frame(matrix(NA,nrow(HSwbics),10))
colnames(buildingsSummary)=c('wbic','county','lakePerimeter_m','lakeArea_m2','buildingCount50m','buildingCount100m','buildingCount200m','buildingDensity50m','buildingDensity100m','buildingDensity200m')

# loop through each county
for(i in 1:length(countyFiles)){
  setwd("~/Documents/Research/People/Students/current/Mosley_Camille/codeHelp/hyperstabilitySurveyBuildingDensities/HsSurvey_WI_buildings/") 
  # if data is in a geodatabase
  if(grepl("gdb",countyFiles[i])){
    gdb=countyFiles[i]
    curCounty=readOGR(gdb,layer=gsub(".gdb","",countyFiles[i],fixed=TRUE))
    
  # other counties use a shapefile  
  }else{
    setwd(countyFiles[i])
    curCounty=st_read(paste(countyFiles[i],".shp",sep=""))
  }

# subset focal county's lakes from WDNRwbic_NAD83 based on wbics
  curLakes=WDNRwbic_NAD83[rownames(centWDNRwbic_NAD83@coords)%in%HSwbics$wbic,]

# count building densities for each lake and store in table
  for(j in 1:length(curlakes)){
    
  }
  
}



# project WDNR 24K in NAD83 (epsg:4269)
# epsg:4326 is WGS84
WDNRwbic_NAD83=spTransform(WDNRwbic,CRS("+init=epsg:4269"))

centWDNRwbic_NAD83=gCentroid(WDNRwbic_NAD83,byid=TRUE,id=wbic$WATERBODY_WBIC)
coordsWDNRwbic_NAD83=centWDNRwbic_NAD83@coords
