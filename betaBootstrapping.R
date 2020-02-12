rm(list=ls())
setwd("~/../BoxSync/NDstuff/Dissertation/1/Hyperstability Exp/Rscripts/")
d=read.csv("iCS.CPUE.csv",header=TRUE,stringsAsFactors=FALSE)
d=d[d$lakeID=="CS",]
z=d[!duplicated(d$PE),]
agg_logCPUE=log(z$anCPUE)
agg_logN=log(z$PE)
aggFit_wLK=glm(agg_logCPUE~agg_logN+z$LKanCPUE)
summary(aggFit_wLK)

#### BOOTSTRAPPING ####

betas=numeric(1000) #betas from model fit to simulated data
ps=numeric(1000) #difference in AIC values between the simulated data model fit and the experimental data model fit

for(i in 1:1000){
  pe=rlnorm(n=length(z$PE), meanlog = log(z$PE), sdlog = log((z$PE.ucl-z$PE.lcl)/4)) #rlnorm from chpt 14-45 of RMark book
  pe[1]=352 #becuase our first PE doesn't have CIs it always estimates a NaN so I just set it back to the point estimate. This is also why you get warnings when you run the loop.
  fit=glm(agg_logCPUE ~ log(pe)+z$LKanCPUE)
  betas[i]=fit$coefficients[2]
  comp=abs(fit$aic - aggFit_wLK$aic)
  ps[i]=comp
}

plot(betas, ps)
hist(betas)
hist(ps)
