rm(list=ls()) # clears workspace

##Week 11##

###1. GLMS###

liz = read.csv("lizards.csv")

unique(liz$time)

#what distribution to choose???
hist(liz$N)
unique(liz$N)

#note - I am using poisson - you need to pick the appropriate distribution for your variable!
g6 = glm(N~time,data=liz, family="poisson")
summary(g6)

library(effects)
plot(allEffects(g6))

library(emmeans)
emmeans(g6, pairwise~time, type="response")

#we have count data, so it might be a good idea to check for overdisperison
#one option - calculate directly
summary(g6)
326.27 / 20

library(AER)
#performs a dispersion test
dispersiontest(g6)

#ok - we have some indication that our data our overdispersed

#lets use a negative binomial instead
library(MASS)
#run a negative binomial glm - no need to use family
g7 = glm.nb(N~time,data=liz)
summary(g7)

#could also check out Dharma plots!!!

#predict on negative binomial model (g7)
liz$yhat = predict(g7,type="response")
head(liz)

head(liz)

library(ggplot2)
plot1=ggplot(data=liz,aes(x=time,y=N))+
  geom_point(size=2,shape =1) +
  geom_point(data=liz, aes(x=time,y=yhat), color="red", size=3)+
  theme_bw()
plot1
