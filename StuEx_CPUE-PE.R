# looking at CPUE-PE relationships from 2018 Fishscapes electrofishing data
# 2/7/2019
# SEJ
rm(list=ls())
setwd("~/Documents/Research/Fishscapes/summer2019/preseason_planning/PEfromCPUE/")

all=read.csv("pe_CPUE_allBatchs.csv",header=TRUE,stringsAsFactors=FALSE)
summ=read.csv("fishD_sum.csv",header=TRUE,stringsAsFactors=FALSE)
summ$medianCPUE[2]=9.55
cond=read.csv("fishscapesConductivity.csv",header=TRUE,stringsAsFactors=FALSE,row.names=1)
cond=rbind(cond,c("BA",14))
cond$SpC=as.numeric(cond$SpC)

# back out perimeter from linear density
summ$lakePerim=summ$nHat/summ$fishPerKmShoreline
# can calculate shoreline development index
summ$SDI=summ$lakePerim/(2*sqrt((pi*summ$surfaceArea/100)))

summ=merge(summ,cond,all.x=TRUE)

#### work with summary data first and look at lake size, shoreline, cond. etc.
plot(summ$CPEkm_shock,summ$nHat)
summary(lm(summ$nHat~summ$CPEkm_shock)) # R2=0.32, p=0.086

summary(lm(summ$nHat~summ$CPEkm_shock+summ$surfaceArea)) # R2=0.399, p=0.169
summary(lm(summ$nHat~summ$CPEkm_shock+summ$minSize)) # R2=0.538, p=0.0671
summary(lm(summ$nHat~summ$CPEkm_shock+summ$maxSize)) # R2=0.893, p=0.000404
summary(lm(summ$nHat~summ$maxSize)) # R2=0.8417, p=0.0002
summary(lm(summ$nHat~summ$CPEkm_shock+summ$lakePerim)) # R2=0.431, p=0.1394
summary(lm(summ$nHat~summ$CPEkm_shock+summ$SDI)) # R2=0.384, p=0.184
summary(lm(summ$nHat~summ$CPEkm_shock+summ$SpC)) # R2=0.469, p=0.109

# interaction helps a bit with SpC...
summary(lm(summ$nHat~summ$CPEkm_shock*summ$surfaceArea)) # R2=0.401, p=0.347
summary(lm(summ$nHat~summ$CPEkm_shock*summ$minSize)) # R2=0.541, p=0.171
summary(lm(summ$nHat~summ$CPEkm_shock*summ$maxSize)) # R2=0.893, p=0.00259
summary(lm(summ$nHat~summ$CPEkm_shock*summ$lakePerim)) # R2=0.431, p=0.304
summary(lm(summ$nHat~summ$CPEkm_shock*summ$SDI)) # R2=0.39, p=0.364
summary(lm(summ$nHat~summ$CPEkm_shock*summ$SpC)) # R2=0.597, p=0.119 ****** a bit better...

##### median does worse in general
summary(lm(summ$nHat~summ$medianCPUE)) # R2=0.23, p=0.162
summary(lm(summ$nHat~summ$medianCPUE+summ$surfaceArea)) # R2=0.278, p=0.319
summary(lm(summ$nHat~summ$medianCPUE+summ$minSize)) # R2=0.463, p=0.113
summary(lm(summ$nHat~summ$medianCPUE+summ$maxSize)) # R2=0.889, p=0.0005
summary(lm(summ$nHat~summ$medianCPUE+summ$lakePerim)) # R2=0.337, p=0.238
summary(lm(summ$nHat~summ$medianCPUE+summ$SDI)) # R2=0.312, p=0.27
summary(lm(summ$nHat~summ$medianCPUE+summ$SpC)) # R2=0.4, p=0.171


#******* look at fish linear density instead
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock)) # R2=0.219, p=0.173
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$surfaceArea)) # R2=0.323, p=0.256
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$minSize)) # R2=0.51, p=0.0825
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$maxSize)) # R2=0.594, p=0.043
summary(lm(summ$fishPerKmShoreline~summ$maxSize)) # R2=0.559, p=0.013
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$lakePerim)) # R2=0.404, p=0.163
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$SDI)) # R2=0.393, p=0.175
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$SpC)) # R2=0.392, p=0.176

# interaction helps a bit and a lot with some
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$surfaceArea)) # R2=0.324, p=0.471
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$minSize)) # R2=0.85, p=0.007
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$maxSize)) # R2=0.594, p=0.122
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$lakePerim)) # R2=0.404, p=0.342
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$SDI)) # R2=0.404, p=0.343
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$SpC)) # R2=0.506, p=0.209

##### median is a mixed bag
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE)) # R2=0.182, p=0.219
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$surfaceArea)) # R2=0.305, p=0.28
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$minSize)) # R2=0.503, p=0.087
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$maxSize)) # R2=0.605, p=0.039
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$lakePerim)) # R2=0.364, p=0.2
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$SDI)) # R2=0.327, p=0.25
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$SpC)) # R2=0.384, p=0.184

##### median and interaction
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE)) # R2=0.182, p=0.219
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$surfaceArea)) # R2=0.31, p=0.5
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$minSize)) # R2=0.82, p=0.012
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$maxSize)) # R2=0.605, p=0.113
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$lakePerim)) # R2=0.37, p=0.398
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$SDI)) # R2=0.327, p=0.465
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$SpC)) # R2=0.431, p=0.304



#******* look at fish areal density instead
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock)) # R2=0.219, p=0.173
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$surfaceArea)) # R2=0.323, p=0.256
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$minSize)) # R2=0.51, p=0.0825
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$maxSize)) # R2=0.594, p=0.043
summary(lm(summ$fishPerKmShoreline~summ$maxSize)) # R2=0.559, p=0.013
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$lakePerim)) # R2=0.404, p=0.163
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$SDI)) # R2=0.393, p=0.175
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$SpC)) # R2=0.392, p=0.176

# interaction helps a bit and a lot with some
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$surfaceArea)) # R2=0.324, p=0.471
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$minSize)) # R2=0.85, p=0.007
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$maxSize)) # R2=0.594, p=0.122
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$lakePerim)) # R2=0.404, p=0.342
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$SDI)) # R2=0.404, p=0.343
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$SpC)) # R2=0.506, p=0.209

##### median is a mixed bag
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE)) # R2=0.182, p=0.219
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$surfaceArea)) # R2=0.305, p=0.28
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$minSize)) # R2=0.503, p=0.087
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$maxSize)) # R2=0.604, p=0.039
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$lakePerim)) # R2=0.365, p=0.2
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$SDI)) # R2=0.327, p=0.25
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$SpC)) # R2=0.384, p=0.184

##### median and interaction
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE)) # R2=0.182, p=0.219
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$surfaceArea)) # R2=0.31, p=0.501
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$minSize)) # R2=0.82, p=0.012
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$maxSize)) # R2=0.605, p=0.113
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$lakePerim)) # R2=0.37, p=0.398
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$SDI)) # R2=0.327, p=0.465
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$SpC)) # R2=0.431, p=0.304



# remove WS
summ=summ[-10,]

summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock)) # R2=0.764, p=0.002
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$surfaceArea)) 
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$minSize)) 
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$maxSize)) # R2=0.881, p=0.0017
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$lakePerim)) 
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$SDI))
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock+summ$SpC)) 

# interaction helps a bit and a lot with some
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$surfaceArea)) # R2=0.81, p=0.0293
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$minSize)) 
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$maxSize)) # R2=0.94, p=0.0015
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$lakePerim)) # R2=0.822, p=0.025
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$SDI)) # R2=0.838, p=0.02
summary(lm(summ$fishPerKmShoreline~summ$CPEkm_shock*summ$SpC)) # R2=0.837, p=0.02

##### median is a mixed bag
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE)) # R2=0.68, p=0.006
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$surfaceArea)) 
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$minSize)) 
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$maxSize)) # R2=0.88, p=0.0017
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$lakePerim)) 
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$SDI)) 
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE+summ$SpC)) 

##### median and interaction
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE)) # R2=0.68, p=0.0059
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$surfaceArea)) 
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$minSize)) 
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$maxSize)) # R2=0.956, p=0.0008
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$lakePerim)) 
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$SDI)) # R2=0.836, p=0.021
summary(lm(summ$fishPerKmShoreline~summ$medianCPUE*summ$SpC)) # R2=0.912, p=0.0046

# look at these models
fit=lm(summ$fishPerKmShoreline~summ$medianCPUE)
plot(summ$maxSize,residuals(fit))
plot(summ$SDI,residuals(fit))
plot(summ$SpC,residuals(fit))
