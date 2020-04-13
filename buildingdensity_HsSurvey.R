#Integrating WI state building data for Vilas and other HsSurvey Ctys with WDNR 24k geodatabase
# 04-13-20
# SEJ,CLM
source("gdriveURL.R")
library(rgdal)
library(rgeos)
library(raster)
library(geosphere)

##############
# get Cty building centroids
##############
# data from http://maps.sco.wisc.edu/opengeoportal/
setwd("C:/Users/Camille/Desktop/Fishscapes/hsSurvey")
#ex.ASHLAND Cty
gdb='Ashland_Buildings_2018.gdb'
ogrListLayers(gdb)

# load a layer from geodatabase
VilasBuild=readOGR(gdb,layer="Ashland_Buildings_2018")
vb=as.data.frame(VilasBuild,stringsAsFactors=FALSE)

# define centroids of polygons
centBuild=gCentroid(VilasBuild,byid=TRUE,id=vb$OBJECTID)

# project building centroids in NAD83 (epsg:4269) instead of lcc?
# epsg:4326 is WGS84
centBuild_NAD83=spTransform(centBuild,CRS("+init=epsg:4269"))

# extract coordinates for building centroids
centBuild_NAD83_coords=centBuild_NAD83@coords

plot(centBuild_NAD83_coords[,1],centBuild_NAD83_coords[,2],cex=0.5)

####################
# Load WDNR HYDRO 24K
####################
#WDNR_HYDRO_24K.gdb<-gdriveURL("https://drive.google.com/open?id=1agF9kFYgCMHUYJckDVJ2mxwg42tDUVZF")
setwd("C:/Users/Camille/Desktop/Fishscapes/hsSurvey")
gdb='WDNR_HYDRO_24K.gdb'
ogrListLayers(gdb)

# load a layer from a geodatabase
WDNRwbic=readOGR(gdb,layer="WD_HYDRO_WATERBODY_WBIC_AR_24K")
wbic=as.data.frame(WDNRwbic,stringsAsFactors=FALSE)
