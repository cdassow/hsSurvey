#hyperstability data exploration. Using Biocomp data and DNR data

rm(list=ls())

library(dplyr)
library(lattice)
library(ggplot2)
gdriveURL <- function(x){
  x =  
    upURL = sub("^[^=]*", "", x)
  y1 =  "https://docs.google.com/uc?id"
  y2 = "&export=download"
  downURL = paste0(y1,upURL,y2)
  read.csv(downURL, header = TRUE)
}

########## combine wood and lake development data #########

#lake info, specifically lake development, for NHLD region
lakes=gdriveURL("https://drive.google.com/open?id=1KcpLmmqi702xK_qvKn72VBvhAH_IfYlt") 

# wood data, for NHLD region
cwh=gdriveURL("https://drive.google.com/open?id=1x1_JdeamiU2auqrlPQ3G_wA6Spuf0vwf") 

linfo=cwh%>%
  inner_join(lakes, by="WBIC")%>%
  select(lakeID,WBIC,surveyYear,perimeter,area,secchi,conductance,conductivityQuintile,lakeAccess,build2011,build2012,build2013,numberBuildings,buildingsPerKm,buildingsPerKmQuantile,CWH.greater.than.10cm.per.km.shoreline,CWH.greater.than.5cm.but.less.than.10cm.per.km.shoreline,Total.CWH.per.km.shoreline)

########## adding fish data from DNR shocking estimates ############

lmb=gdriveURL("https://drive.google.com/open?id=11v8FbT2wnKx_CqUfxu_V9r_8fyCfcdD2")

pan=gdriveURL("https://drive.google.com/open?id=1QIqCBQ9gbOgRFUJQbnokwwTZJi5VZZIR")

wly=gdriveURL("https://drive.google.com/open?id=1EYaQpLr_Hp9YbARWFS3tgE6L_tEtWV0G")

wly2=gdriveURL("https://drive.google.com/open?id=1mp6oFNd8C7Q4wOWcdZxTrWwCHZRZjvc3")

wly3=gdriveURL("https://drive.google.com/open?id=1DPRROWv6Cf_fP6Z-kE9ZgUfdf_F_jSNT")
  #combining treaty walleye and dnr walleye data and lake characteristic data
winfo=wly%>%
  full_join(wly3,by="WBIC")%>%
  inner_join(linfo, by="WBIC")

# plot(winfo$Total.CWH.per.km.shoreline, (winfo$totalNumberCaughtFish/winfo$numberHoursSampled), xlab = "Total CWH per km shorline", ylab = "Shocking CPUE", main = "WLY")
# 
# fit=lm((winfo$totalNumberCaughtFish/winfo$numberHoursSampled)~winfo$Total.CWH.per.km.shoreline)
# summary(fit) #CWH doesn't seem to be a good predictor of electrofishing CPUE, but I rerun this test later on in this script and it does seem to matter


lmbinfo=lmb%>%
  filter(totalNumberCaughtFish>0 & numberHoursSampled>0)%>%
  group_by(WBIC,waterbody)%>%
  summarize(sum(distanceShockedMiles),sum(totalNumberCaughtFish), sum(numberHoursSampled),n())%>%
  inner_join(linfo, by=c("WBIC"))
lmbinfo$CPmile=lmbinfo$`sum(totalNumberCaughtFish)`/lmbinfo$`sum(distanceShockedMiles)`
colnames(lmbinfo)[6]="n.shocks"
lmbinfo=lmbinfo[,-8]
lmbinfo$shockCPUE=lmbinfo$`sum(totalNumberCaughtFish)`/lmbinfo$`sum(numberHoursSampled)`

# plot(lmbinfo$Total.CWH.per.km.shoreline, lmbinfo$CPmile,xlab = "Total CWH per km shorline", ylab = "Shocking CPEmile", main = "LMB")
# 
# fit=lm(lmbinfo$CPmile~lmbinfo$Total.CWH.per.km.shoreline)
# summary(fit) #CWH doesn't seem to be a good predictor of electrofishing CPUE



  #combining PANFISH data and lake characteristic data
pinfo=pan%>%
  inner_join(linfo, by="WBIC")
# plot(pinfo$Total.CWH.per.km.shoreline, (pinfo$totalNumberCaughtFish/pinfo$numberHoursSampled),xlab = "Total CWH per km shorline", ylab = "Shocking CPUE", main = "Panfish")
# 
# fit=lm((pinfo$totalNumberCaughtFish/pinfo$numberHoursSampled)~pinfo$Total.CWH.per.km.shoreline)
# summary(fit) #CWH not a good predictor of electrofishing CPUE



########## adding angler effort data #######

#creel_raw_interview_fish_data_VO.csv
c.int = gdriveURL("https://drive.google.com/open?id=1T_QeJTms9QmG65iT1ul_FC1H1XbhBhd5")

#class manipulations needed to calculate effort
c.int$timeEnd=strptime(c.int$timeEnd, format = "%H%M")
c.int$timeStart=strptime(c.int$timeStart, format = "%H%M")
c.int$effort=difftime(c.int$timeEnd, c.int$timeStart, units = "hours")
c.int$effort=as.numeric(c.int$effort)
c.int$cpue=c.int$caughtAmt/c.int$effort 
c.int$timeStart=as.numeric(c.int$timeStart)
c.int$timeEnd=as.numeric(c.int$timeEnd)
#NOTE the above CPUE calculation works for trips within the same day but if they fished on both sides of midnight the calculation will give you a wrong answer...doesn't look like any of the data from vilas has that issue. 3 anglers started and stopped at 715 so their effort is 0. (rows 6959:6961)

#summarizing data by lake, year, and species
fcint=c.int%>%
  group_by(WBIC, lakeName, fishSpeciesCode)%>%
  summarize(mean(cpue, na.rm=T), sd(cpue, na.rm=T), n())
colnames(fcint)[4:6]=c("anglerCPUE","sd.anglerCPUE", "nAnglers")

#Summary statistics looking at the mean and sd of angler CPUE per lake, range in variances is larger than the range in means....data is pretty variable
fcint2=c.int%>%
  filter(fishSpeciesCode=="W12")%>%
  group_by(WBIC, fishSpeciesCode)%>%
  summarise(mean(cpue), sd(cpue))
fcint2=fcint2[is.finite(fcint2$`mean(cpue)`),] #getting rid of one inf value
barplot(fcint2$`mean(cpue)`)
range(fcint2$`mean(cpue)`, na.rm = T)
range(fcint2$`sd(cpue)`, na.rm = T)
hist(fcint2$`mean(cpue)`)
hist(fcint2$`sd(cpue)`)


#combining creel data with shocking and lake data
fwinfo=winfo%>%
  inner_join(fcint, by="WBIC")%>%
  filter(fishSpeciesCode=="X22")

flmbinfo=lmbinfo%>%
  inner_join(fcint, by="WBIC")%>%
  filter(fishSpeciesCode=="W12")

flmbinfo2=linfo%>% #making this just for the following plot, avg angler cpe for all lakes across all lakes with wood estimates regardless of whether or not they were shocked
  inner_join(fcint, by="WBIC")%>%
  filter(fishSpeciesCode=="W12")
fit=lm(flmbinfo2$anglerCPUE~flmbinfo2$Total.CWH.per.km.shoreline)
plot(flmbinfo2$Total.CWH.per.km.shoreline, flmbinfo2$anglerCPUE, ylab = "Average Angler CPUE", xlab = "CWH Density (total per km shoreline)", pch=16)
abline(fit)

fpinfo=pinfo%>%
  inner_join(fcint, by="WBIC")%>%
  filter(fishSpeciesCode=="W09" |fishSpeciesCode=="X15"|fishSpeciesCode=="W14"|fishSpeciesCode=="W06"|fishSpeciesCode=="W04"|fishSpeciesCode=="W05")


########## plotting the walleye shocking and angler data ######

plot(fwinfo$populationEstimate, fwinfo$`mean(cpue)`, xlab = "Pop. Estimate", ylab = "Avg. Angler CPUE", main = "WLY")#very noisy, no clear pattern between Population estimate and cpue

plot(fwinfo$surveyYear.x, fwinfo$populationEstimate, xlab = "Survey Year", ylab = "Pop. Estimate", main = "WLY") #electroshocking survey year vs. population estimate, also super noisy. not filtered by lake.

xyplot(populationEstimate ~ surveyYear.x|waterbody.x, data = fwinfo, main = "WLY") #filtered by lake. doesn't seem to be much of a trend in shocking abundance over time

fit=lm(fwinfo$populationEstimate~fwinfo$Total.CWH.per.km.shoreline)
summary(fit)
plot(fwinfo$Total.CWH.per.km.shoreline, fwinfo$populationEstimate, xlab = "Total CWH per km shorline", ylab = "Pop. Estimate", main = "WLY")# wood is a really good predictor of pop estimate
abline(fit)

fit=lm(fwinfo$`mean(cpue)`~fwinfo$Total.CWH.per.km.shoreline)
summary(fit)
plot(fwinfo$Total.CWH.per.km.shoreline, fwinfo$`mean(cpue)`, xlab = "Total CWH per km shorline", ylab = "Average angler CPUE", main = "WLY")# wood is maybe a predictor of average angler CPUE, maybe a hump shape
abline(fit)


########## plotting the LMB shocking and angler data #####

# A few plots looking at how angler CPUE, shocking CPE, and CWH are related
plot(flmbinfo$shockCPUE, flmbinfo$anglerCPUE, xlab="Shocking CPUE", ylab = "angling CPUE", main = "LMB")
#highest angling CPUEs occur near the lowest shocking CPUE(proxy for lowest abundance?)

plot(flmbinfo$CPmile, flmbinfo$anglerCPUE, xlab="Shocking CPEmile", ylab = "angling CPUE", main = "LMB")

fit=lm(flmbinfo$shockCPUE~flmbinfo$Total.CWH.per.km.shoreline)
summary(fit) #wood doesn't predic shocking CPUE
plot(flmbinfo$Total.CWH.per.km.shoreline, flmbinfo$shockCPUE, xlab = "Total CWH per km shorline", ylab = "Shocking CPUE", main = "LMB")
abline(fit)

fit=lm(flmbinfo$anglerCPUE~flmbinfo$Total.CWH.per.km.shoreline)
summary(fit) #wood doesn't predict angler CPUE
plot(flmbinfo$Total.CWH.per.km.shoreline, flmbinfo$anglerCPUE, xlab = "Total CWH per km shoreline", ylab = "Average angler CPUE", main = "LMB")
abline(fit)

########## plotting the panfish shocking and angler data #####

plot((fpinfo$totalNumberCaughtFish/fpinfo$numberHoursSampled), fpinfo$`mean(cpue)`, xlab="Shocking CPUE", ylab = "angling CPUE", main = "Panfish")#no real pattern

plot(fpinfo$surveyYear.x, (fpinfo$totalNumberCaughtFish/fpinfo$numberHoursSampled), xlab = "Survey Year", ylab = "Shocking CPUE", main = "Panfish") #electroshocking survey year vs. shocking CPUE...maybe a slight increase in CPUE over time, maybe just an increase in sampling over time

xyplot((totalNumberCaughtFish/numberHoursSampled) ~ surveyYear.x|waterbody, data = fpinfo, main = "Panfish") #filtered by lake, not enough data to see much

fit=lm((fpinfo$totalNumberCaughtFish/fpinfo$numberHoursSampled)~fpinfo$Total.CWH.per.km.shoreline)
summary(fit)
plot(fpinfo$Total.CWH.per.km.shoreline, (fpinfo$totalNumberCaughtFish/fpinfo$numberHoursSampled), xlab = "Total CWH per km shorline", ylab = "Shocking CPUE", main = "Panfish") # Wood is a strong predictor of shocking CPUE, more wood=lower CPUE...maybe not suprising since DNR uses AC which isn't great around structure
abline(fit)

fit=lm(fpinfo$`mean(cpue)`~fpinfo$Total.CWH.per.km.shoreline)
summary(fit)
plot(fpinfo$Total.CWH.per.km.shoreline, fpinfo$`mean(cpue)`, xlab = "Total CWH per km shoreline", ylab = "Average angler CPUE", main = "Panfish")# wood is strong predictor of angler CPUE...lower angler CPUE at higher wood
abline(fit)


########## binning wood data to calculate beta across wood densities ########

hist(linfo$Total.CWH.per.km.shoreline, breaks = 50)

olmb=flmbinfo[order(flmbinfo$Total.CWH.per.km.shoreline),]

olmb$bin=as.factor(rep(1:4, each=12))#making 4 bins with 12 members each bin 1 is lowest CWH, bin 4 is highest
#olmb$bin=as.factor(rep(1:8, each=6))

xyplot(log(1+`mean(cpue)`)~log(1+(totalNumberCaughtFish/numberHoursSampled))|bin ,data = olmb, type=c("p", "r"), ylab = "log angler CPUE", xlab = "log shocking CPUE") #type argement adds points "p" and regression "r" to the plot

#just change the bin number to extract the slope for any panel on the xyplot
fit1=lm(log(1+`mean(cpue)`)~log(1+(totalNumberCaughtFish/numberHoursSampled)), data = olmb, subset = olmb$bin=="1")
summary(fit1)

#Plotting by catch per mile instead of catch per time doesn't seem to make much of a difference
xyplot(log(1+`mean(cpue)`)~log(1+CPEmile)|bin ,data = olmb, type=c("p", "r"), ylab = "log angler CPUE", xlab = "log shocking CPEmile") 

#just change the bin number to extract the slope for any panel on the xyplot
fit2=lm(log(1+`mean(cpue)`)~log(1+CPEmile), data = olmb, subset = olmb$bin=="4")
summary(fit2)

#log log plot of angler cpue vs. shocking cpemile, color coded by CWH availability
colfunc=colorRampPalette(c("red","yellow","springgreen","blue"))
plot(log(1+olmb$CPEmile),log(1+olmb$`mean(cpue)`), xlab="log CPEmile", ylab = " log Lake angler meanCPUE", pch=16, col=(colfunc(48)), cex=1.5) #red->blue is low to high wood. Doesn't seem to be much of a pattern. 


########## Heirarchical model of Beta #######
#rough estimation of abundance by scaling up from fish/mi shocked to whole lake perimeter
flmbinfo$abund=flmbinfo$CPmile*(flmbinfo$perimeter*.62) #.62 converts lake info perimeter from km to mi to match DNR data

betaNLL=function(parms, CWH, N, CPE){
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
fit=optim(guess, fn=betaNLL, CWH=flmbinfo$Total.CWH.per.km.shoreline, N=flmbinfo$abund, CPE=flmbinfo$anglerCPUE)
fit

null_wN<-function(parms,N,CPUE){
  beta=parms[1]
  q=parms[2]
  shape=parms[3]
  expected=q*N^beta
  nll=-sum(dgamma(x=CPUE,shape=shape,scale=expected/shape,log=T))
  return(nll)
}
guess=c(beta=1, q=.01, shape=2)
fit2=optim(guess, fn=null_wN, N=flmbinfo$abund, CPUE=flmbinfo$anglerCPUE)
fit2

null_woN<-function(parms,CPUE){
  expected=parms[1]
  shape=parms[2]
  nll=-sum(dgamma(x=CPUE,shape=shape,scale=expected/shape,log=T))
  return(nll)
}
guess=c(expected=1, shape=2)
fit3=optim(guess, fn=null_woN, CPUE=flmbinfo$anglerCPUE)
fit3


##1:1 plot of expected vs observed betas using CWH relationship from betaNLL model
ebeta=fit$par[1]+(fit$par[2]*flmbinfo$Total.CWH.per.km.shoreline)
ecpe=fit$par[4]*flmbinfo$abund^ebeta
ocpe=flmbinfo$anglerCPUE

cpefit=lm(ocpe~ecpe)
plot(ecpe,ocpe, pch=16, ylab="Observed CPUE", xlab="Expected CPUE", xlim = c(0, 1.7), ylim = c(0, 1.7))
abline(a=0, b=1)
abline(cpefit)

#calculating beta assuming we know CPUE and abundance
#assuming constant catchability = .01

flmbinfo$beta=log((flmbinfo$`mean(cpue)`/.01), base = (flmbinfo$abund))
mean(flmbinfo$beta, na.rm=T);sd(flmbinfo$beta, na.rm=T); range(flmbinfo$beta, na.rm = T)
# mean= -0.4     sd= 1.74   range=-5.8 to 5.1

fit3=lm(flmbinfo$beta~flmbinfo$Total.CWH.per.km.shoreline)
summary(fit3)
fit4=lm(flmbinfo$beta~poly(flmbinfo$Total.CWH.per.km.shoreline,2))
summary(fit4)
plot(flmbinfo$Total.CWH.per.km.shoreline, flmbinfo$beta, pch=16, xlab = "CWH density", ylab = "Beta")
abline(fit3)
abline(h=1, col="red")

########## fake exp data analysis ####

#looking to see if we can recover the beta I used to generate the fake data 
#analysis of hyperstability experiment data in a scenario where catch is proportional to abundance...no hyperstability

fden1=c(500, 400, 300, 250, 200, 600, 700, 750, 800, 850) #changing abundace
fden2=c(500, 600, 700, 750, 800, 400, 300, 250, 200, 150) #changing abundance

#CPUE data generated from proportional relationship to abundance
fcp1=.01*(fden1)^1 
fcp2=.01*(fden2)^1
time=1:10 #sampling events
#adding noise to CPUE data
fcp1=fcp1*(rnorm(10, mean = 1, sd = .1))
fcp2=fcp2*(rnorm(10, mean = 1, sd = .1))
#combining all into a dataframe
fdata=cbind(fcp1,fcp2)
fdata=cbind(fdata,fden1)
fdata=cbind(fdata,fden2)
fdata=as.data.frame(cbind(fdata,time))

plot(fdata$time, fdata$fcp1, type = "l")
lines(fdata$time, fdata$fcp2, lty=2)

fit1=lm(log(fdata$fcp1)~log(fdata$fden1))
summary(fit1)
fit2=lm(log(fdata$fcp2)~log(fdata$fden2))
summary(fit2)

plot(log(fdata$fden1), log(fdata$fcp1), pch=16, col="red")
points(log(fdata$fden2), log(fdata$fcp2), pch=16)
abline(fit1, col="red")
abline(fit2)

o1=order(fdata$fden1)
o2=order(fdata$fden2)
plot(fdata$fden1[o1], fdata$fcp1[o1], type = "l", ylab = "CPUE", xlab = "Abundance", ylim = c(0, 10))
lines(fdata$fden1[o2], fdata$fcp2[o2], lty=2)
legend("top", legend = c("Basin 1", "Basin 2"), lty = 1:2)

#beta calc for fake data
beta1=mean(log((fdata$fcp1/.01), base=fdata$fden1)) #beta near 1
beta2=mean(log((fdata$fcp2/.01), base=fdata$fden2)) #beta near 1




#analysis of hyperstability experiment data in a scenario where catch isn't proportional to abundance... hyperstability

fden1=c(500, 400, 300, 250, 200, 600, 700, 750, 800, 850) #changing abundace
fden2=c(500, 600, 700, 750, 800, 400, 300, 250, 200, 150) #changing abundance

#CPUE data generated from proportional relationship to abundance 
fcp1=.01*(fden1)^.2
fcp2=.01*(fden2)^.2
time=1:10 #sampling events
#adding noise to CPUE data
fcp1=fcp1*(rnorm(10, mean = 1, sd = .1))
fcp2=fcp2*(rnorm(10, mean = 1, sd = .1))
#combining all into a dataframe
fdata=cbind(fcp1,fcp2)
fdata=cbind(fdata,fden1)
fdata=cbind(fdata,fden2)
fdata=as.data.frame(cbind(fdata,time))

plot(fdata$time, fdata$fcp1, type = "l")
lines(fdata$time, fdata$fcp2, lty=2)

fit1=lm(log(fdata$fcp1)~log(fdata$fden1))
summary(fit1)
fit2=lm(log(fdata$fcp2)~log(fdata$fden2))
summary(fit2)

plot(log(fdata$fden1), log(fdata$fcp1), pch=16, col="red")
points(log(fdata$fden2), log(fdata$fcp2), pch=16)
abline(fit1, col="red")
abline(fit2)

o1=order(fdata$fden1)
o2=order(fdata$fden2)
plot(fdata$fden1[o1], fdata$fcp1[o1], type = "l", ylab = "CPUE", xlab = "Abundance")
lines(fdata$fden1[o2], fdata$fcp2[o2], lty=2)
legend("top", legend = c("Basin 1", "Basin 2"), lty = 1:2)

#beta calc for fake data
beta1=mean(log((fdata$fcp1/.01), base=fdata$fden1)) #beta near .2
beta2=mean(log((fdata$fcp2/.01), base=fdata$fden2)) #beta near .2

#hierarchical model analysis of fake data

fcwh=runif(40, 10, 600)
fb=.3+(.7/600)*fcwh
fN=runif(40, 0, 500)
fq=1e-5
fCPE=fq*N^fb+rnorm(40, 0, .001)

betaNLL=function(parms, CWH, N, CPE){
  a0=parms[1]
  aCWH=parms[2]
  shapeCPE=parms[3]
  q=10^(parms[4])
  
  b=a0+aCWH*CWH
  CPEhat=q*N^b
  
  nll=-sum(dgamma(x=CPE, shape = shapeCPE, scale = (CPEhat/shapeCPE), log = T))
  return(nll)
}

guess=c(a0=.3, aCWH=-0.001, shapeCPE=1, q=.01)
f.fit=optim(guess, fn=betaNLL, CWH=fcwh, N=fN, CPE=fCPE)

#figure for DNR proposal

par(mfcol=c(2,1), mar=c(4,4,1,.5), oma=c(1,0,0,0), lwd=2, box(lwd=2), font=2)
plot(fdata$time, fdata$fden1, type = "l", xlab = "Manipulation Week", ylab = "Abundance")
lines(fdata$time, fdata$fden2, lty=2)
text(x=1, y=800, labels = "(a)")

plot(seq(150, 850, by=(850-150)/9), (.01*(seq(150, 850, by=(850-150)/9))^1), xlab = "Abundance", ylab = "CPUE", type = "l")
abline(h=5, lty=2)
text(x=150, y=8, labels = "(b)")
#legend("bottomright", legend = c("Basin 1", "Basin 2"), lty = 1:2)


########## random stuff #######

c.int$effort=c.int$timeEnd-c.int$timeStart
c.int$cpue=c.int$caughtAmt/c.int$effort

creel=c.int%>%
  group_by(WBIC,surveyYear, dateSample,fishSpeciesCode)%>%
  summarise(mean(cpue), sd(cpue))%>%
  filter(`mean(cpue)`>0, fishSpeciesCode=="W12")
creel=creel[is.finite(creel$`mean(cpue)`),] #getting rid of one inf value

xyplot(creel$`mean(cpue)`~as.Date(creel$dateSample, "%d/%m/%y")|WBIC, data=creel)
plot(as.Date(creel$dateSample, "%d/%m/%y"), creel$`mean(cpue)`)


creel$month=as.numeric(sub("/.{,2}/.{,2}", "", creel$dateSample))

creel1=creel%>%
  group_by(month)%>%
  summarize(mean(`mean(cpue)`), sd(`mean(cpue)`))
colnames(creel1)=c('month', 'meanCPUE', 'sdCPUE')

plot(creel1$month, creel1$meanCPUE, type = "l", ylim = c(-.01,.035))
lines(creel1$month, (creel1$meanCPUE+creel1$sdCPUE), lty=2)
lines(creel1$month, (creel1$meanCPUE-creel1$sdCPUE), lty=2)

##plot of angler CPUE vs shocking CPUE with point size proportional to number of anglers in a creel sample

plot(flmbinfo$shockCPUE, flmbinfo$anglerCPUE, cex=log(flmbinfo$nAnglers), pch=16)
p1=ggplot(flmbinfo, aes(x=shockCPUE, y=anglerCPUE))+geom_point(aes(color=lakeName, size=log(nAnglers)))+
  theme_bw()+
  theme(panel.grid = element_blank())
p1
#did the same thing for point size proportional to number of shocking events

plot(flmbinfo$shockCPUE, flmbinfo$anglerCPUE, cex=log(flmbinfo$n.shocks), pch=16)

#Plotting CV in angler and shocking CPUE

hist((flmbinfo$sd.anglerCPUE/flmbinfo$anglerCPUE), xlab="CV", main = "Angling CPUE CV")

#figuring out the time between a shocking and an angling sample

lmbdate=lmb%>%
  filter(totalNumberCaughtFish>0 & numberHoursSampled>0)%>%
  group_by(WBIC,waterbody, surveyYear)%>%
  summarize(sum(distanceShockedMiles),sum(totalNumberCaughtFish), sum(numberHoursSampled), mean(totalNumberCaughtFish/numberHoursSampled), sd((totalNumberCaughtFish/numberHoursSampled)),n())%>%
  inner_join(linfo, by=c("WBIC"), suffix=c(".lmb", ".linfo"))
lmbdate$CPmile=lmbdate$`sum(totalNumberCaughtFish)`/lmbdate$`sum(distanceShockedMiles)`
colnames(lmbdate)[7:9]=c("meanShockCPUE", "sdShockCPUE","n.shocks")
lmbdate=lmbdate[,-11]
#lmbdate$shockCPUE=lmbdate$`sum(totalNumberCaughtFish)`/lmbdate$`sum(numberHoursSampled)`

plot(lmbdate$surveyYear.lmb, lmbdate$meanShockCPUE, xlab="year", ylab = "Shock CPUE", pch=16)
plot(lmbdate$surveyYear.lmb, (lmbdate$sdShockCPUE/lmbdate$meanShockCPUE), xlab = "year", ylab = "CV", main = "Shock", pch=16)
hist((lmbdate$sdShockCPUE/lmbdate$meanShockCPUE), xlab = "CV", main = "Shocking CPUE CV")

cintdat=c.int%>%
  group_by(WBIC, lakeName, fishSpeciesCode, surveyYear)%>%
  summarize(mean(cpue, na.rm=T), sd(cpue, na.rm=T), n())%>%
  filter(fishSpeciesCode=="W12")
colnames(cintdat)[5:7]=c("anglerCPUE","sd.anglerCPUE", "nAnglers")

plot(cintdat$surveyYear, cintdat$anglerCPUE, xlab="year", ylab = "Angler CPUE")
plot(cintdat$surveyYear, (cintdat$sd.anglerCPUE/cintdat$anglerCPUE), xlab = "year", ylab = "CV", main = "Angling")
hist((cintdat$sd.anglerCPUE/cintdat$anglerCPUE), xlab = "CV", main = "Angling CPUE CV")

datdiff=lmbdate%>%
  inner_join(cintdat, by="WBIC", suffix=c(".shock", ".angle"))%>%
  filter(fishSpeciesCode=="W12")
datdiff$diff=abs(datdiff$surveyYear.lmb-datdiff$surveyYear)

plot(datdiff$surveyYear.lmb, (datdiff$sdShockCPUE/datdiff$meanShockCPUE), xlab = "year", ylab = "CV", main = "shocking")
plot(datdiff$surveyYear, (datdiff$sd.anglerCPUE/datdiff$anglerCPUE), xlab = "year", ylab = "CV", main = "angler")
hist((datdiff$sdShockCPUE/datdiff$meanShockCPUE), xlab = "CV", main = "Shocking CV")
hist((datdiff$sd.anglerCPUE/datdiff$anglerCPUE), xlab = "CV", main = "angling cv")
fit=lm(anglerCPUE~as.numeric(meanShockCPUE), data = datdiff[datdiff$diff==0,])
summary(fit)


datdiff$abund=datdiff$CPmile*(datdiff$perimeter*.62) #.62 converts lake info perimeter from km to mi to match DNR data

#fitting heirarchical model to samples where diff=0
#samples where both creel and shock happened in same year
datdif0=datdiff[datdiff$diff==0,]
datdif0=datdif0[datdif0$anglerCPUE>0,]

guess=c(a0=.3, aCWH=0.001, shapeCPE=1, q=.001)
fit2=optim(guess, fn=betaNLL, CWH=datdif0$Total.CWH.per.km.shoreline, N=datdif0$abund, CPE=datdif0$anglerCPUE)
fit2




datdiff$shockCPUE=as.factor(datdiff$meanShockCPUE)

p1=ggplot(datdiff[datdiff$diff==0,], aes(x=as.numeric(meanShockCPUE), y=anglerCPUE))+geom_point(aes(color=lakeName, size=diff))+
  labs(x="meanShockCPUE")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text())+
  guides(size=F)
p1

plot(lmbdate$surveyYear.lmb, lmbdate$meanShockCPUE, xlab="year", ylab = "Shock CPUE")
p2=ggplot(lmbdate, aes(x=surveyYear.lmb, y=meanShockCPUE)) +geom_point(aes(color=lakeID, size=2))+
            theme_bw()+
            theme(panel.grid=element_blank(),
                  axis.text = element_text())
p2
xyplot(lmbdate$meanShockCPUE~lmbdate$surveyYear.lmb|lakeID, data = lmbdate, xlab = "Year", ylab = "shockCPUE", pch=16)

plot(cintdat$surveyYear, cintdat$anglerCPUE, xlab="year", ylab = "Angler CPUE")
p3=ggplot(cintdat, aes(x=surveyYear, y=anglerCPUE))+
  geom_point(aes(color=lakeName))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.text = element_text())+
  guides(color=F)
p3

xyplot(cintdat$anglerCPUE[cintdat$fishSpeciesCode=="W12"]~cintdat$surveyYear[cintdat$fishSpeciesCode=="W12"]|as.character(WBIC), data=cintdat, xlab="year", ylab = "Angler CPUE", pch=16, subset =cintdat$anglerCPUE<=4)

xyplot(datdiff$anglerCPUE~datdiff$surveyYear|lakeID, data=datdiff, xlab="year", ylab = "Angler CPUE", pch=16)


#### looking for shocking data on minoqua chain wly####
wlyAll=c(wly, wly2, wly3) #can't rbind because they don't have the same columns but all could have useful data so I'm checking all 3
mqChain=c("MINOCQUA LAKE", "MADELINE LAKE", "MID LAKE", "TOMAHAWK LAKE", "KAWAGUESAGA LAKE", "CARROL LAKE")
mq1=wly[wly$county=="ONEIDA",]
mq1=mq1[mq1$waterbody%in%mqChain,]
mq2=wly2[wly2$county=="ONEIDA",]
mq2=mq2[mq2$waterbody%in%mqChain,]
mq3=wly3[wly3$county=="ONEIDA",]
mq3=mq3[mq3$waterbody%in%mqChain,]

#finding unique sampling dates
unique(mq1$surveyYear)
unique(mq2$surveyYear)
unique(mq3$surveyYear)

mqPE=mq1%>%
  group_by(waterbody)%>%
  summarize(n_distinct(surveyYear))
mq2CR=mq2%>%
  group_by(waterbody)%>%
  summarize(n_distinct(surveyYear))
mq3CR=mq3%>%
  group_by(waterbody)%>%
  summarize(n_distinct(surveyYear))

#looking for creel data on minocqua chain lakes
ciAll1=gdriveURL("https://drive.google.com/open?id=1UYhbGH28WXjmi-4BzhfwO4KYwrBCNO2Q") #both ciAll and ciAll2 have minoqua chain data
ciAll2=gdriveURL("https://drive.google.com/open?id=1lxUd742QZMXDQunyFBnENKMYZ1XNM_Pc")

#creel interfiew fish data for minoqua chain lakes (minocqua lake, mid lake, kawaguesaga lake, madeline lake, tomahawk lake)
ciAll=rbind(ciAll1, ciAll2)
mqChain=c("MINOCQUA LAKE", "MADELINE LAKE", "MID LAKE", "TOMAHAWK LAKE", "KAWAGUESAGA LAKE", "CARROL LAKE") #Chain lakes that have creel data
ciMC=ciAll[which(ciAll$lakeName%in%mqChain),]

cSum=ciMC%>%
  group_by(lakeName)%>%
  summarize(n_distinct(surveyYear))

#just look now to see which lakes where creeled and surveyed in the same year

