# 20240816-- CODE FOR DETERMINING ASSOCIATIONS BETWEEN MARE GROUP CHANGING BEHAVIOR AND FEMALE DOMINANCE HIERARCHY STABILITY

# packages ####
library(tidyverse)
library(MuMIn)
library(nlme)
library(lme4)
library(lmerTest)

elostability<- read.csv("C:/Users/cmnunez/OneDrive - The University of Memphis/Current horse research projects/Shackleford mare aggression/Mare aggression analyses/Datasets and Code BIOLOGY LETTERS 20240816/elo hierarchy stability by male 20231113_BIOLOGY LETTERS.csv")
names(elostability)
View(elostability)

# investigating associations between mare group changing behavior and elo uncertainty score (hierarchy stability) ####

# using kendall's tau-- appropriate test for correlations with non-normal data
cor.test(elostability$elo_uncertainty, elostability$total_changes_per_sighting, method = 'kendall') 

# calculating somer's delta-- is appropriate when there is a perceived independent and dependent variable
install.packages("DescTools")
library(DescTools)

SomersDelta(elostability$total_changes_per_sighting, elostability$elo_uncertainty)


# mare group changing behavior and female dominance hierarchy stability figure (not included in the manuscript) ####

# making year a factor so can show both years in same plot with their own trend lines
elostability$year <- as.factor(elostability$year) 

ggplot(data = elostability, aes(x = total_changes_per_sighting, y = elo_uncertainty, color=year, label = male)) + 
  geom_point(size = 4) +
  geom_text(hjust=1, vjust=1) +
  geom_smooth(method = 'lm')

