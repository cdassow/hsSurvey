# Integrating WI state building data for Vilas Cty with WDNR 24k geodatabase
# 03-14-2018
# SEJ

library(rgdal)
library(rgeos)
library(raster)
library(geosphere)

##############
# get Vilas Cty building centroids
##############
# data from http://maps.sco.wisc.edu/opengeoportal/
setwd("~/Documents/Research/Fishscapes/wiGIS/")
gdb='Vilas_Buildings_YE2016.gdb'
ogrListLayers(gdb)

# load a layer from geodatabase
VilasBuild=readOGR(gdb,layer="Vilas_Buildings_YE2016")
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
setwd("~/Documents/Research/Fishscapes/wiGIS/WDNR_HYDRO_24K")
gdb='WDNR_HYDRO_24K.gdb'
ogrListLayers(gdb)

# load a layer from a geodatabase
WDNRwbic=readOGR(gdb,layer="WD_HYDRO_WATERBODY_WBIC_AR_24K")
wbic=as.data.frame(WDNRwbic,stringsAsFactors=FALSE)

# project WDNR 24K in NAD83 (epsg:4269)
# epsg:4326 is WGS84
WDNRwbic_NAD83=spTransform(WDNRwbic,CRS("+init=epsg:4269"))

centWDNRwbic_NAD83=gCentroid(WDNRwbic_NAD83,byid=TRUE,id=wbic$WATERBODY_WBIC)
coordsWDNRwbic_NAD83=centWDNRwbic_NAD83@coords

#subsetting to Vilas Cty lakes
# use bounding box coords and <, > to get wbics in vilas cty
vilasBounds=bbox(centBuild_NAD83)
vilasLakes=rownames(coordsWDNRwbic_NAD83[(((coordsWDNRwbic_NAD83[,1]>vilasBounds[1,1]) & (coordsWDNRwbic_NAD83[,1]<vilasBounds[1,2])) & ((coordsWDNRwbic_NAD83[,2]>vilasBounds[2,1]) & (coordsWDNRwbic_NAD83[,2]<vilasBounds[2,2]))),])

#subset based on wbics
vilas24k=WDNRwbic_NAD83[rownames(centWDNRwbic_NAD83@coords)%in%vilasLakes,]

##################
# counting buildings around lakes
##################
# test case with wildcat lake
WCwbic=2336800

buildingsSummary=data.frame(matrix(NA,length(vilas24k),9))
colnames(buildingsSummary)=c('wbic','lakePerimeter_m','lakeArea_m2','buildingCount50m','buildingCount100m','buildingCount200m','buildingDensity50m','buildingDensity100m','buildingDensity200m')

for(i in 1:length(vilas24k)){
  print(i)
  buildingsSummary[i,1:3]=unlist(vilas24k[i,]@data)
  
  p=vilas24k[i,]
  p=spTransform(p,CRS=CRS("+proj=utm"))
  b50=gBuffer(p,width=50,quadsegs=1000)
  b50=spTransform(b50,CRS="+init=epsg:4269")
  b100=gBuffer(p,width=100,quadsegs=1000)
  b100=spTransform(b100,CRS="+init=epsg:4269")
  b200=gBuffer(p,width=200,quadsegs=1000)
  b200=spTransform(b200,CRS="+init=epsg:4269")
  
  buildingsSummary$buildingCount50m[i]=sum(!is.na(over(centBuild_NAD83,b50)))
  buildingsSummary$buildingCount100m[i]=sum(!is.na(over(centBuild_NAD83,b100)))
  buildingsSummary$buildingCount200m[i]=sum(!is.na(over(centBuild_NAD83,b200)))
}

buildingsSummary$buildingDensity50m=buildingsSummary$buildingCount50m/(buildingsSummary$lakePerimeter_m/1000)
buildingsSummary$buildingDensity100m=buildingsSummary$buildingCount100m/(buildingsSummary$lakePerimeter_m/1000)
buildingsSummary$buildingDensity200m=buildingsSummary$buildingCount200m/(buildingsSummary$lakePerimeter_m/1000)

plot(buildingsSummary$buildingDensity50m,buildingsSummary$buildingDensity100m)
plot(buildingsSummary$buildingDensity50m,buildingsSummary$buildingDensity200m)
plot(buildingsSummary$buildingDensity100m,buildingsSummary$buildingDensity200m)

hist(buildingsSummary$buildingDensity50m,breaks=seq(0,50,1))
hist(buildingsSummary$buildingDensity100m,breaks=seq(0,150,1))
hist(buildingsSummary$buildingDensity200m,breaks=seq(0,500,5))

setwd("/Users/stuartjones/Documents/Research/Fishscapes/wiGIS")
write.csv(buildingsSummary,"ShorelineDevelopment_Vilas_03-15-2018.csv",row.names=FALSE)
