#life hisotry predictors of beta (hyperstability)
#loading in CSV from Weber book, need to clean up habitat and prey item variables
Fish<-read.csv("Fishes of WI_Becker_data.csv")

summary(Fish_length<-lm(Fish$beta~Fish$max.length.mm., Fish)) #p 0.058 .
anova(Fish_length)

summary(Fish_length2<-lm(Fish$beta~Fish$min.length.mm., Fish)) # p 0.010 **
anova(Fish_length2)

summary(Fish_spawn_time<-lm(Fish$beta~Fish$spawn.start, Fish)) #spawn start month : april p 0.020 *, may p 0.025 *
anova(Fish_spawn_time) #0.03312

summary(Fish_spawn_end<-lm(Fish$beta~Fish$spawn.end, Fish)) #nope

summary(Fish_min_temp<-lm(Fish$beta~Fish$min.preffered.water.temp..Celsius.)) #nope

summary(Fish_max_temp<-lm(Fish$beta~Fish$max.preffered.water.temp..Celsius.)) #nope

summary(Fish_YOY<-lm(Fish$beta~Fish$YOY.average.size..mm.)) #0.0219
anova(Fish_YOY)

summary(Fish_hatch<-lm(Fish$beta~Fish$hatching.time.average..days.)) #nope

summary(Fish_Stemp_max<-lm(Fish$beta~Fish$max.spawning.temp..Celsius.)) #nope

summary(Fish_Stemp_min<-lm(Fish$beta~Fish$min.spawning.temp..Celsius.)) #nope

        