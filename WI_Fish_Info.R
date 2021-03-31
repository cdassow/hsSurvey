#life hisotry predictors of beta (hyperstability)
#loading in CSV from Weber book, need to clean up habitat and prey item variables
Fish<-read.csv("Fishes of WI_Becker_data.csv")

nullfit=lm(Fish$beta~1)

summary(Fish_length<-lm(Fish$beta~Fish$max.length.mm., Fish)) #p 0.058 .
anova(Fish_length, nullfit)

summary(Fish_length2<-lm(Fish$beta~Fish$min.length.mm., Fish)) # p 0.010 **
anova(Fish_length2,nullfit)

summary(Fish_spawn_time<-lm(Fish$beta~Fish$spawn.start, Fish)) #spawn start month : april p 0.020 *, may p 0.025 *
anova(Fish_spawn_time,nullfit) #0.03312

summary(Fish_spawn_end<-lm(Fish$beta~Fish$spawn.end, Fish)) #nope

summary(Fish_min_temp<-lm(Fish$beta~Fish$min.preffered.water.temp..Celsius.)) #nope

summary(Fish_max_temp<-lm(Fish$beta~Fish$max.preffered.water.temp..Celsius.)) #nope

summary(Fish_YOY<-lm(Fish$beta~Fish$YOY.average.size..mm.)) #0.0219
anova(Fish_YOY,nullfit)

summary(Fish_hatch<-lm(Fish$beta~Fish$hatching.time.average..days.)) #nope

summary(Fish_Stemp_max<-lm(Fish$beta~Fish$max.spawning.temp..Celsius.)) #nope

summary(Fish_Stemp_min<-lm(Fish$beta~Fish$min.spawning.temp..Celsius.)) #nope

# checking life history and catch ability 
Fish$q <- c(0.287, 0.228, 1.272, 0.765, 0.040, 0.675)

nullfitq=lm(Fish$q~1)

summary(Fish_length<-lm(Fish$q~Fish$max.length.mm., Fish)) #small sig 0.009
summary(Fish_length2<-lm(Fish$q~Fish$min.length.mm., Fish)) #almost/small 0.035
summary(Fish_spawn_time<-lm(Fish$q~Fish$spawn.start, Fish)) #no
summary(Fish_spawn_end<-lm(Fish$q~Fish$spawn.end, Fish)) #no
summary(Fish_min_temp<-lm(Fish$q~Fish$min.preffered.water.temp..Celsius.))#no
summary(Fish_max_temp<-lm(Fish$q~Fish$max.preffered.water.temp..Celsius.)) #no
summary(Fish_YOY<-lm(Fish$q~Fish$YOY.average.size..mm.)) #no
summary(Fish_hatch<-lm(Fish$q~Fish$hatching.time.average..days.)) #almost
summary(Fish_Stemp_max<-lm(Fish$q~Fish$max.spawning.temp..Celsius.))#no
summary(Fish_Stemp_min<-lm(Fish$q~Fish$min.spawning.temp..Celsius.))#no

anova(Fish_length,nullfitq)
anova(Fish_length2,nullfitq)

#catchability and beta relationship
anova(lm(Fish$q~1),lm(Fish$q~Fish$beta)) #q note predictor of beta?


