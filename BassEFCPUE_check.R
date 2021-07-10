#Bass efCPUE check with Fishcapes survey (2018-2019)
#SEJ, CM 7-9-2021
#source the dbUtil.R
setwd("C:/Users/mosle/Documents/JonesLakeExperiment_Datasheets_2020")
source("dbUtil.r")
#change this path to where you stored the current database on your own computer
dbdir="C:/Users/mosle/Documents/JonesLakeExperiment_Datasheets_2020"
db="MFEdb_20210528.db"


# load fish samples and fish info
fs=dbTable("FISH_SAMPLES")
fi=dbTable("FISH_INFO")

# lakes we have PEs for
lakes=c("HT","BY","DY","SE","SV","UG","WB","AR","BA","FD","JS","LC","LH","LR","TO","WC","WS")

# subset fish samples to fishscapes 1/2 mile electrofishing
fsfs=fs[fs$projectID==37,]
fsfsdnr=fsfs[fsfs$metadataID%in%c("FishscapesSurvey.0.5mile.20180606","FishscapesSurvey.1.5mile.20180607"),]
fsfsdnrpe=fsfsdnr[fsfsdnr$lakeID%in%lakes,]

# create vector to store average efCPE for each lake
efCPE=numeric(length(lakes))
for(i in 1:length(lakes)){
  cur=fsfsdnrpe[fsfsdnrpe$lakeID==lakes[i],] # pull only samples from the ith lake
  cur=cur[cur$useCPUE=="yes",]
  
  cur_efCPE=numeric(nrow(cur))     # create vector to store individual trip (sample) efCPE for the  ith lake
  for(j in 1:nrow(cur)){
    curFI=fi[fi$sampleID==cur$sampleID[j],]
    cur_efCPE[j]=sum(curFI$otu=="largemouth_bass")/as.numeric(cur$distanceShocked[j])
  }
  
  efCPE[i]=mean(cur_efCPE)
}

names(efCPE)=lakes
# remove SE and BA - no DNR-style  electrofishing
efCPE=efCPE[!is.na(efCPE)]

# compare PEs to efCPE
setwd("C:/Users/mosle/Documents/HsSurvey/HsSurvey/hsSurvey")

pes1=read.csv("2019PEs.csv", header = T, stringsAsFactors = F) #2019 lake PEs
pes2=read.csv("fishscapes2018_peSum_20180914.csv", header = T, stringsAsFactors = F) #2018 lakePEs
#checking shock vs anlging PE for fishscapes 2018 survey: CPE mi shock, take out SE and BA

par(mfrow=c(1,1))
plot(pes2$nHat,pes2$fishPerKmShoreline)
lm_fit=lm(fishPerKmShoreline~nHat,data=pes2)
abline(lm_fit,lwd=2,col='blue')

nullfit=lm(fishPerKmShoreline~1,data=pes2)
anova(lm_fit,nullfit)

#remove uneeded columns 
pes2=pes2[,c(2,5)]
#reomve BA
pes2=pes2[c(1,3:10),]
pes2$lakeID <-as.character(pes2$lakeID)
#remove SE
pes1=pes1[c(1:3,5:7),]
pes1=pes1[,c(1,4)]


#cominbing all our pes into one data frame
PEs=bind_rows(pes1,pes2)
PEs=c(583.0000,  867.7333, 2415.0000,  352.7000, 2408.8095,  815.2222, 2334.7273, 1872.0000,  640.0000,
        934.0000,  233.3333 , 918.6000, 1066.7500,  674.0000 ,1893.0000
       )
names(PEs)=names(efCPE)

plot(PEs,efCPE)
model=lm(efCPE~PEs)
abline(model,lwd=2,col='blue')

null=lm(efCPE~1)
anova(model,null)
