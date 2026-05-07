
rm(list=ls()) # clears workspace

#ex12_mixed_models
##load important packages##
library(ggplot2)
library(MASS)
library(reshape2)
library(tidyverse)

#probably a new package
#install.packages("lme4")
library(lme4)
head(sleepstudy)
#another package
#install.packages("glmmTMB")
library(glmmTMB)



##RUN A A GLMM##
bat = read.csv("bat_data.csv")
head(bat)

#make a year column
bat$date = as.Date(bat$date, "%m/%d/%y")
bat$year = format(bat$date, "%y")
bat$year = as.numeric(bat$year)
bat$year = bat$year - min(bat$year)

#drop a species that dies too early
bat = subset(bat, species!="MYSE")
#and substrate
bat = subset(bat, species!="SUBSTRATE")

library(lme4)
#glms - binomial
#Question: how does the probability of infection differ among species?
gm1 = glmer(gd~species + (1|site),data=bat, family = "binomial")
summary(gm1)
#the GLM version of this will give us p-values
library(car)
Anova(gm1, type=3)

#Q: How does the prob. of infection differ over time among species?
gm2 = glmer(gd~species*year + (1|site),data=bat, family = "binomial")
summary(gm2)
library(effects)
plot(allEffects(gm2))

anova(gm1,gm2)

#let's predict g2
newdat = expand.grid(species = unique(bat$species),
                     year=seq(min(bat$year),max(bat$year),by = .05),
                     site = unique(bat$site)
                     )
newdat$yhat = predict(gm2,newdata= newdat,re.form=NA,type="response")
#note - I use response to transform from logit space to response variable space
#I used re.form to drop random effects because I don't care about the site effects

#let's plot the prediction
r=ggplot(data=newdat, aes(x=year,y=yhat,col=species))+ 
  geom_line(linewidth=1)+
  geom_point(data = bat, aes(x = jitter(year), y = gd),size=3, shape = 1)+
  ylab("Pd Prevalence")+
  xlab("Year")+
  coord_cartesian(ylim=c(-0.1,1.1))+ #zoom in
  theme_bw() + 
  theme(axis.title=element_text(size=23),axis.text=element_text(size=15),panel.grid = element_blank(), axis.line=element_line(),legend.position=c(.9,.55),legend.text = element_text(size=12,face="italic"))
print(r)

#what if I hadn't dropped the site effects?

newdat$yhat = predict(gm2,newdata= newdat,type="response")
r=ggplot(data=newdat, aes(x=year,y=yhat,col=species))+ 
  geom_line(size=1)+
  geom_point(data = bat, aes(x = jitter(year), y = gd),size=3, shape = 1)+
  ylab("Pd Prevalence")+
  xlab("Year")+
  coord_cartesian(ylim=c(-0.1,1.1))+
  theme_bw() + 
  theme(axis.title=element_text(size=23),axis.text=element_text(size=15),panel.grid = element_blank(), axis.line=element_line(),legend.position=c(.9,.55),legend.text = element_text(size=12,face="italic"))
print(r)

#yikes

##what if we want to predict categorical
newdat2 = expand.grid(species = unique(bat$species),
                     site = unique(bat$site)
)
newdat2$yhat = predict(gm1,newdata= newdat2,re.form=NA,type="response")
#note - I use response to transform from logit space to response variable space
#I used re.form to drop random effects because I don't care about the site effects

#let's plot the prediction
r=ggplot(data=newdat2, aes(x=species,y=yhat))+ 
geom_point(color="red")  +
geom_point(data = bat, aes(x = species, y = gd),size=3, shape = 1)+
  ylab("Pd Prevalence")+
  xlab("Species")+
  coord_cartesian(ylim=c(-0.1,1.1))+
  theme_bw() + 
  theme(axis.title=element_text(size=23),axis.text=element_text(size=15),panel.grid = element_blank(), axis.line=element_line(),legend.position=c(.9,.55),legend.text = element_text(size=12,face="italic"))
print(r)


## same model but using glmmTMB
tmb.mod1 = glmmTMB(gd ~ species*year + (1|site), data=bat, family=binomial())
summary(tmb.mod1)
#similar output but not exactly the same


#diagnosing models with Dharma
library(DHARMa)

simulationOutput <- simulateResiduals(fittedModel = tmb.mod1, plot = T) 
# calculates calculates randomized quantile residuals
#To interpret the residuals, a scaled residual value of 0.5 means that half of the simulated data
#are higher than the observed value, and half of them lower. (This would be good)
#A value of 0.99 would mean that nearly all simulated data are lower than the observed value.
#The minimum/maximum values for the residuals are 0 and 1. 
#For a correctly specified model we would expect a flat distribution of the scaled residuals

##plot on the left 
#this is interpreted like a qqPLOT 



