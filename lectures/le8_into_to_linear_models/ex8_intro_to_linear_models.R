library(tidyverse)
library(performance)
library(ggplot2)


batdat=read.csv("bat_data.csv")
head(batdat)  

#transform
batdat$lgdL = log10(batdat$gdL)
batdat$lcount = log10(batdat$count)

#run linear model - continuous predictor
mod1 = lm(lgdL~lcount, data = batdat)
summary(mod1)

#y = ax + b 
#lgdL = -0.7043x + (-1.6718)
#Predictor is *continuous* so value is slope (-0.704) (or coef)

# Running a linear model in R - categorical predictor
# note - you only need to run one model for the assignment - not 2!
mod2 = lm(lgdL~species, data = batdat)
summary(mod2)
#First line (labeled INTERCEPT) is actually the REFERENCE
#This is the omitted group; (EPFU) in this case
#Each other parameter listed is in REFERENCE to the estimated value of the this variable!
#e.g. speciesMYLU is saying that MYLU have, on average, 0.61607 more fungus than EPFU

hist(resid(mod1))

plot(mod1)

#install.packages(performance)

#load performance
library(performance)

check_model(mod1)

###### ---------------plot model---------------####
# continuous!!
library(ggplot2)

r=ggplot(data=batdat, aes(x=lcount, y=lgdL))+ 
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw() + 
  theme(axis.title=element_text(size=20),axis.text=element_text(size=10),panel.grid = element_blank(), axis.line=element_line(),legend.position="top",legend.title=element_blank())
print(r)

# categorical
r=ggplot(data=batdat, aes(x=species, y=lgdL))+ 
  geom_point()+
  stat_summary(fun.data = "mean_se", colour="red", size=1)+ 
  theme_bw() + 
  theme(axis.title=element_text(size=20),axis.text=element_text(size=10),panel.grid = element_blank(), axis.line=element_line(),legend.position="top",legend.title=element_blank())
print(r)
