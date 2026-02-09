# 2024 CODE FOR BIOLOGY LETTERS SUBMISSION-- ASSOCIATIONS BETWEEN MARE GROUP CHANGING BEHAVIOR AND MARE RANK AND THE AGGRESSION RECEIVED AND INITIATED BY MARES

# packages ####
library(tidyverse)
library(MuMIn)
library(lme4)
library(lmerTest)
library(outliers)

ch_agg<-read.csv("C:/Users/cmnunez/OneDrive - The University of Memphis/Current horse research projects/Shackleford mare aggression/Mare aggression analyses/Datasets and Code BIOLOGY LETTERS 20240816/mare changes agg all factors 20240821_BIO LETTERS_DATA ONLY.csv")
setwd("./Datasets and Code BIOLOGY LETTERS 20240816")
ch_agg<-read.csv("mare changes agg all factors 20240821_BIO LETTERS_DATA ONLY.csv")
names(ch_agg)

# data filtering ####

# filtering for animals I watched for at least 3 hours
ch_agghrs3<-ch_agg[ch_agg$total_time_watched_hrs >= 3,]
View(ch_agghrs3)

# filtering out one outlier individual (delphi) in 2021 -- in that year, she was considerably more aggressive than any other mare and changed groups substantially more frequently (see tests and descriptive stats below showing how far above all other data from 2021).  
# when this one individual is included in the analysis, the results shift so that mares changing groups do not receive more aggression but initiate more aggression. 
# so we see an association regardless, however, given that delphi was so different in 2021 from every other mare we studied, we deem it reasonable that the patterns presented in our manuscript accurately reflect the responses of the vast majority of animals we studied and are more likely to apply in other systems.

# statistical justification for excluding delphi ####

delphi <- ch_agghrs3 %>% filter(horse == 'delphi') %>% select(horse, year, ch_per_hour, agg_given_per_hrs_obs)
#   horse year ch_per_hour agg_given_per_hrs_obs
#1 delphi 2021   1.8823529             6.8235294
#2 delphi 2023   0.3636364             0.5454545

# delphi ch_per_hour is 1.88 
(1.88 - mean(ch_agghrs3$ch_per_hour, na.rm = T))/sd(ch_agghrs3$ch_per_hour, na.rm = T)
# 4.86 sds above the mean for changes

# delphi agg_given_per_hrs_obs is 6.82 
(6.82 - mean(ch_agghrs3$agg_given_per_hrs_obs, na.rm = T))/sd(ch_agghrs3$agg_given_per_hrs_obs, na.rm = T)
# 5.41 sds above the mean for agg

hist(ch_agghrs3$ch_per_hour, breaks = 30)
hist(ch_agghrs3$agg_given_per_hrs_obs, breaks = 30)
# shows she's far away from rest of population
# also indicates non-normal, but no standard outlier tests for non-normal data. Using grubbs, suggests outlier
outliers::grubbs.test(ch_agghrs3$ch_per_hour, type = 10)
#	Grubbs test for one outlier

#data:  ch_agghrs3$ch_per_hour
#G = 4.86939, U = 0.72433, p-value = 6.634e-06
#alternative hypothesis: highest value 1.882352941 is an outlier

outliers::grubbs.test(ch_agghrs3$agg_given_per_hrs_obs, type = 10)
#Grubbs test for one outlier

#data:  ch_agghrs3$agg_given_per_hrs_obs
#G = 5.41588, U = 0.65898, p-value = 1.03e-07
#alternative hypothesis: highest value 6.823529412 is an outlier


# excluding delphi in 2021
ch_agghrs3nod<-ch_agghrs3[-which(ch_agghrs3$horse=="delphi" & ch_agghrs3$year == "2021"),]
View(ch_agghrs3nod)

# making year a factor in the dataframe
ch_agghrs3nod$year <- as.factor(ch_agghrs3nod$year)

# investigating associations between mare group changing behavior and mare rank ####

# filtering out NAs for mare rank so that dredge can be run on this model 
ch_agg_noNA_rank<-ch_agghrs3nod[is.na(ch_agghrs3nod$avg_rel_rank)==F,]

# mare group changing behavior and mare rank analysis ####
# maximal model fit by maximum likelihood 
# to allow model comparison among models with different fixed effects
rank_lmer<-lmer(avg_rel_rank~ch_per_hour + region + year + mareage + ave_rump+ 
                  ch_per_hour:region +
                  ch_per_hour:year +
                  ch_per_hour:mareage +
                  ch_per_hour:ave_rump +(1|horse),
                data=ch_agg_noNA_rank,na.action = na.pass, REML = F)
# residuals
hist(resid(rank_lmer, type = 'pearson')) # minimal deviations from normal
shapiro.test(resid(rank_lmer, type = 'pearson'))
#data:  resid(rank_lmer, type = "pearson")
#W = 0.98943, p-value = 0.7519
# fails to reject null hypothesis that resids are derived from a normal distribution

# moreover, the paper below shows that
# deviations from normality much more severe than the above
# yielded accurate estimates from linear mixed effects models
#https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13434

# checking for variance inflation issues
car::vif(rank_lmer) # some high VIF, but these are variables involved in interactions and the VIF for the interaction is high, which is expected

summary(rank_lmer)

# model comparison
rank_lmer_weights <- dredge(rank_lmer)

# view top models within 2 delta AICc of top and show their summaries.
length(get.models(rank_lmer_weights, subset = delta < 2))
# 2

# top model: shows no effect of group changing on rank
# only parameters are intercept and average condition (rump) score
# now parameters estimated by REML, which should be more precise
rank_lmer.1 <- get.models(rank_lmer_weights, subset = delta < 2, REML = T)[[1]]
summary(rank_lmer.1)

# second model (dAICc = 1.3): shows no effect of group changing on rank
# parameters are intercept, average condition (rump) score, year
# now parameters estimated by REML, which should be more precise
rank_lmer.2 <- get.models(rank_lmer_weights, subset = delta < 2, REML = T)[[2]]
summary(rank_lmer.2)

# third model (dAICc = 2.02): condition (rump) score and group changes
# now parameters estimated by REML, which should be more precise
rank_lmer.3 <- get.models(rank_lmer_weights, subset = delta < 4, REML = T)[[3]]
summary(rank_lmer.3)


# investigating associations between mare group changing behavior and the aggression they receive from other mares ####

# filtering out NAs for rump score so can include this variable and so dredge can be run on this model 
ch_agg_noNA_rump<-ch_agghrs3nod[is.na(ch_agghrs3nod$ave_rump)==F,]

# mare group changing behavior and the aggression they received analysis ####
# maximal model fit by maximum likelihood 
# to allow model comparison among models with different fixed effects
rump_rec_lmer<-lmer(agg_rcd_per_hrs_obs~ch_per_hour + region + year + mareage + ave_rump + no_trts +
                      ch_per_hour:no_trts +
                      ch_per_hour:ave_rump +
                      ch_per_hour:region +
                      ch_per_hour:year +
                      ch_per_hour:mareage + (1|horse),
                    data=ch_agg_noNA_rump,na.action = na.pass, REML = F)
hist(resid(rump_rec_lmer)) # some deviation from normality
shapiro.test((resid(rump_rec_lmer))) #
# data:  (resid(rump_rec_lmer))
# W = 0.90389, p-value = 9.287e-06
# does suggest deviation from normality

# but, the paper below shows that
# deviations from normality much more severe than the above
# yielded accurate estimates from linear mixed effects models
#https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13434

car::vif(rump_rec_lmer) # same as above only high values are involved in interactions, to be expected
summary(rump_rec_lmer)

rump_rec_lmer_weights <- dredge(rump_rec_lmer)

# view top models within 2 delta AICc of top and show their summaries.
length(get.models(rump_rec_lmer_weights, subset = delta < 2))
#7

# top model: group changes, region, and their interaction, year, condition (rump) score
# now parameters estimated by REML, which should be more precise
rump_rec_lmer.1 <- get.models(rump_rec_lmer_weights, subset = delta < 2, REML = T)[[1]]
summary(rump_rec_lmer.1)

# second model (dAICc = 0.5): group changes, region, and their interaction; year
# now parameters estimated by REML, which should be more precise
rump_rec_lmer.2 <- get.models(rump_rec_lmer_weights, subset = delta < 2, REML = T)[[2]]
summary(rump_rec_lmer.2)

# third model (dAICc = 1.09): group changes, year, and their interaction
# now parameters estimated by REML, which should be more precise
rump_rec_lmer.3 <- get.models(rump_rec_lmer_weights, subset = delta < 2, REML = T)[[3]]
summary(rump_rec_lmer.3)

# fourth model (dAICc = 1.35): group changes, region, and their interaction; condition (rump) score and interaction with group changes; year
# now parameters estimated by REML, which should be more precise
rump_rec_lmer.4 <- get.models(rump_rec_lmer_weights, subset = delta < 2, REML = T)[[4]]
summary(rump_rec_lmer.4)

# fifth model (dAICc = 1.55): group changes, region, and their interaction
rump_rec_lmer.5 <- get.models(rump_rec_lmer_weights, subset = delta < 2, REML = T)[[5]]
summary(rump_rec_lmer.5)

# sixth model (dAICc = 1.64): group changes, year, and their interaction; condition (rump score)
rump_rec_lmer.6 <- get.models(rump_rec_lmer_weights, subset = delta < 2, REML = T)[[6]]
summary(rump_rec_lmer.6)

# seventh model (dAICc = 1.99): group changes, region, and their interaction; year and interaction with group changes; condition (rump) score
rump_rec_lmer.7 <- get.models(rump_rec_lmer_weights, subset = delta < 2, REML = T)[[7]]
summary(rump_rec_lmer.7)

# eighth model (dAICc = 2.02): group changes, region, and their interaction; year and its interaction with group changes
rump_rec_lmer.8 <- get.models(rump_rec_lmer_weights, subset = delta < 4, REML = T)[[8]]
summary(rump_rec_lmer.8)

# predictions from top model to make figure
# put a column for all vars in full model in case needed for mod averaging below

rec_pred_data <- expand.grid(ave_rump = mean(ch_agg_noNA_rump$ave_rump, na.rm = T),
                             ch_per_hour = seq(from = min(ch_agg_noNA_rump$ch_per_hour, na.rm = T), to = max(ch_agg_noNA_rump$ch_per_hour, na.rm = T), by = 0.01),
                             region = factor(c('A_west', 'B_central', 'C_east'), levels = c('A_west', 'B_central', 'C_east')),
                             year = factor('2021', levels = c('2021','2023')),
                             no_trts = mean(ch_agg_noNA_rump$no_trts, na.rm = T),
                             mareage = mean(ch_agg_noNA_rump$mareage, na.rm = T))

rec_pred_data$yr_adj_preds_best <- predict(rump_rec_lmer.1, newdata = rec_pred_data, re.form = ~0, type = 'response') + fixef(rump_rec_lmer.1)[6]/2
# gives predictions based on fixed effects only, with average rump score and average year effect (i.e., intercepts will be intermediate between 2021 and 2023 estimates)

# mare group changes and the aggression they received figure 1 ####
paperplot<-ggplot(ch_agg_noNA_rump, aes(x = ch_per_hour, y = agg_rcd_per_hrs_obs, col=region)) +
  geom_point(size = 5) +
  theme_classic() +
  geom_line(data = rec_pred_data, aes(x = ch_per_hour, y = yr_adj_preds_best, col = region)) +
  theme(legend.position="none") + 
  scale_color_manual(values=c("#2b83ba", "#fdae61", "#d7191c"))
  
paperplot 

#ggsave('C:/Users/jsalocal/The University of Memphis/Cassandra Maria Victoria Nunez (cmnunez) - Mare aggression analyses/Biol Lett Resub Figures/raw fig agg rec 20241020.eps', plot = paperplot, height = 90, width = 70, units = 'mm')

# model averaging predictions from top agg rec ML models, see if they gibe with REML top models ####
# all models
rec_list_ML_top_mods <- get.models(rump_rec_lmer_weights, delta < 2)
rec_lmer_ML_mod_avg <- model.avg(rec_list_ML_top_mods, fit = T)

rec_pred_data$raw_preds_best <- predict(rump_rec_lmer.1, newdata = rec_pred_data, re.form = ~0, type = 'response')
rec_pred_data$yr_adj_preds_best <- predict(rump_rec_lmer.1, newdata = rec_pred_data, re.form = ~0, type = 'response') + fixef(rump_rec_lmer.1)[6]/2
rec_pred_data$avg_preds <- predict(rec_lmer_ML_mod_avg, re.form = ~0, rec_pred_data, full = TRUE)

paperplot_avg<-ggplot(ch_agg_noNA_rump, aes(x = ch_per_hour, y = agg_rcd_per_hrs_obs, col=region)) +
  geom_point(size = 5) +
  theme_classic() +
  geom_line(data = rec_pred_data, aes(x = ch_per_hour, y = avg_preds, col = region)) +
  theme(legend.position="none") + 
  scale_color_manual(values=c("#2b83ba", "#fdae61", "#d7191c"))

paperplot_avg 
#ggsave('C:/Users/jsalocal/The University of Memphis/Cassandra Maria Victoria Nunez (cmnunez) - Mare aggression paper/Submitted files/RESUBMISSION 20241004/FINAL SUBMISSION 20241113/Figures/raw fig agg rec MOD AVG 20241115.eps', plot = paperplot_avg, height = 90, width = 70, units = 'mm')


# supplemental analysis for group changing behavior and the aggression mares received excluding mares that changed groups at the highest rates ####

# filtering out mares changing groups at the highest rates 
# (keeping only <=1.0 changes per hour)
ecmax<-ch_agg_noNA_rump[ch_agg_noNA_rump$ch_per_hour <= 1.0,]
View(ecmax)

ecmax_rec_lmer<-lmer(agg_rcd_per_hrs_obs~ch_per_hour + region + year + mareage + ave_rump + no_trts + 
                       ch_per_hour:no_trts +
                       ch_per_hour:ave_rump +
                       ch_per_hour:region +
                       ch_per_hour:year +
                       ch_per_hour:mareage + (1|horse),
                     data=ecmax,na.action = na.pass, REML = F)
hist(resid(ecmax_rec_lmer)) # same caveats as above
car::vif(ecmax_rec_lmer)
summary(ecmax_rec_lmer)

ecmax_rec_lmer_weights <- dredge(ecmax_rec_lmer)

# view top models within 2 delta AICc of top and show their summaries.
length(get.models(ecmax_rec_lmer_weights, subset = delta < 2))
# 13

# top model: group changes, region, and their interaction, year, condition (rump) score
# now parameters estimated by REML, which should be more precise
ecmax_rec_lmer.1 <- get.models(ecmax_rec_lmer_weights, subset = delta < 2, REML = T)[[1]]
summary(ecmax_rec_lmer.1)

# second model (dAICc = 0.42): group changes, year
# now parameters estimated by REML, which should be more precise
ecmax_rec_lmer.2 <- get.models(ecmax_rec_lmer_weights, subset = delta < 2, REML = T)[[2]]
summary(ecmax_rec_lmer.2)

# third model (dAICc = 0.63): group changes, region, and their interaction; year 
# now parameters estimated by REML, which should be more precise
ecmax_rec_lmer.3 <- get.models(ecmax_rec_lmer_weights, subset = delta < 2, REML = T)[[3]]
summary(ecmax_rec_lmer.3)

# fourth model (dAICc = 0.70): group changes, condition (rump) score and interaction with group changes; year
# now parameters estimated by REML, which should be more precise
ecmax_rec_lmer.4 <- get.models(ecmax_rec_lmer_weights, subset = delta < 2, REML = T)[[4]]
summary(ecmax_rec_lmer.4)

# fifth model (dAICc = 0.87): group changes, condition (rump) score, year
ecmax_rec_lmer.5 <- get.models(ecmax_rec_lmer_weights, subset = delta < 2, REML = T)[[5]]
summary(ecmax_rec_lmer.5)

# sixth model (dAICc = 0.95): group changes, year, and their interaction
ecmax_rec_lmer.6 <- get.models(ecmax_rec_lmer_weights, subset = delta < 2, REML = T)[[6]]
summary(ecmax_rec_lmer.6)

# seventh model (dAICc = 1.35): group changes
ecmax_rec_lmer.7 <- get.models(ecmax_rec_lmer_weights, subset = delta < 2, REML = T)[[7]]
summary(ecmax_rec_lmer.7)

# eight model (dAICc = 1.38): group changes, region, and their interaction; year; condition (rump) score and its interaction with group changes
# now parameters estimated by REML, which should be more precise
ecmax_rec_lmer.8 <- get.models(ecmax_rec_lmer_weights, subset = delta < 2, REML = T)[[8]]
summary(ecmax_rec_lmer.8)

# ninth model (dAICc = 1.43): group changes, region, and their interaction; condition (rump) score; year and its interaction with group changes
# now parameters estimated by REML, which should be more precise
ecmax_rec_lmer.9 <- get.models(ecmax_rec_lmer_weights, subset = delta < 2, REML = T)[[9]]
summary(ecmax_rec_lmer.9)

# 10th model (dAICc = 1.51): group changes, number of treatments, year 
# now parameters estimated by REML, which should be more precise
ecmax_rec_lmer.10 <- get.models(ecmax_rec_lmer_weights, subset = delta < 2, REML = T)[[10]]
summary(ecmax_rec_lmer.10)

# 11th model (dAICc = 1.53): group changes; condition (rump) score; year and its interaction with group changes
# now parameters estimated by REML, which should be more precise
ecmax_rec_lmer.11 <- get.models(ecmax_rec_lmer_weights, subset = delta < 2, REML = T)[[11]]
summary(ecmax_rec_lmer.11)

# 12th model (dAICc = 1.56): group changes, region, and their interaction; year and its interaction with group changes
# now parameters estimated by REML, which should be more precise
ecmax_rec_lmer.12 <- get.models(ecmax_rec_lmer_weights, subset = delta < 2, REML = T)[[12]]
summary(ecmax_rec_lmer.12)

# lucky 13th model (dAICc = 1.86): group changes, region, and their interaction
# now parameters estimated by REML, which should be more precise
ecmax_rec_lmer.13 <- get.models(ecmax_rec_lmer_weights, subset = delta < 2, REML = T)[[13]]
summary(ecmax_rec_lmer.13)

# 14th model (dAICc = 2.31): group changes, year, and their interaction; condition (rump) score and its interaction with group changes
# now parameters estimated by REML, which should be more precise
ecmax_rec_lmer.14 <- get.models(ecmax_rec_lmer_weights, subset = delta < 4, REML = T)[[14]]
summary(ecmax_rec_lmer.14)

# mare group changing and the aggression received supplemental figure 1 ####
# predictions from top ecmax model to make figure
ecmax_rec_pred_data <- expand.grid(ave_rump = mean(ecmax$ave_rump, na.rm = T),
                             ch_per_hour = seq(from = min(ecmax$ch_per_hour, na.rm = T), to = max(ecmax$ch_per_hour, na.rm = T), by = 0.01),
                             region = factor(c('A_west', 'B_central', 'C_east'), levels = c('A_west', 'B_central', 'C_east')),
                             year = factor('2021', levels = c('2021','2023')))

ecmax_rec_pred_data$preds <- predict(ecmax_rec_lmer.1, newdata = ecmax_rec_pred_data, re.form = ~0, type = 'response') + fixef(ecmax_rec_lmer.1)[6]/2
# gives predictions based on fixed effects only, with average rump score and average year effect (i.e., intercepts will be intermediate between 2021 and 2023 estimates)

# mare group changes and the aggression, excluding highest values of group changes
supp_plot<-ggplot(ecmax, aes(x = ch_per_hour, y = agg_rcd_per_hrs_obs, col=region)) +
  geom_point(size = 5) +
  theme_classic() +
  geom_line(data = ecmax_rec_pred_data, aes(x = ch_per_hour, y = preds, col = region)) +
  theme(legend.position="none") + 
  scale_color_manual(values=c("#2b83ba", "#fdae61", "#d7191c"))

supp_plot 

#ggsave('C:/Users/jsalocal/The University of Memphis/Cassandra Maria Victoria Nunez (cmnunez) - Mare aggression analyses/Biol Lett Resub Figures/raw supp fig agg rec 20241020.eps', plot = supp_plot, height = 90, width = 70, units = 'mm')

# investigating associations between mare group changing behavior and the aggression they initiate with other mares ####
# maximal model fit by maximum likelihood 
# to allow model comparison among models with different fixed effects
rump_given_lmer<-lmer(agg_given_per_hrs_obs~ch_per_hour + region + year + mareage + ave_rump + no_trts +
                        ch_per_hour:no_trts +
                        ch_per_hour:ave_rump +
                        ch_per_hour:region +
                        ch_per_hour:year +
                        ch_per_hour:mareage + (1|horse),
                      data=ch_agg_noNA_rump,na.action = na.pass, REML = F)
hist(resid(rump_given_lmer)) # similar to above, some deviation from normality
shapiro.test(resid(rump_given_lmer))
#data:  resid(rump_given_lmer)
#W = 0.87549, p-value = 6.141e-07
# again, does suggest deviation from normality

# but, as above the paper below shows that
# deviations from normality much more severe than the above
# yielded accurate estimates from linear mixed effects models
#https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13434
car::vif(rump_given_lmer) # as above, variance inflation only in variables involved in interactions, to be expected
summary(rump_given_lmer)

rump_given_lmer_weights <- dredge(rump_given_lmer)

# view top models within 2 delta AICc of top and show their summaries.
length(get.models(rump_given_lmer_weights, subset = delta < 2))
# 12

# top model: condition (rump) score
# now parameters estimated by REML, which should be more precise
rump_given_lmer.1 <- get.models(rump_given_lmer_weights, subset = delta < 2, REML = T)[[1]]
summary(rump_given_lmer.1)

# second model (dAICc = 0.55): condition (rump) score, group changes
# now parameters estimated by REML, which should be more precise
rump_given_lmer.2 <- get.models(rump_given_lmer_weights, subset = delta < 2, REML = T)[[2]]
summary(rump_given_lmer.2)

# third model (dAICc = 1.10): condition (rump) score, number of treatments 
# now parameters estimated by REML, which should be more precise
rump_given_lmer.3 <- get.models(rump_given_lmer_weights, subset = delta < 2, REML = T)[[3]]
summary(rump_given_lmer.3)

# fourth model (dAICc = 1.27): condition (rump) score; year
# now parameters estimated by REML, which should be more precise
rump_given_lmer.4 <- get.models(rump_given_lmer_weights, subset = delta < 2, REML = T)[[4]]
summary(rump_given_lmer.4)

# fifth model (dAICc = 1.32): group changes
rump_given_lmer.5 <- get.models(rump_given_lmer_weights, subset = delta < 2, REML = T)[[5]]
summary(rump_given_lmer.5)

# sixth model (dAICc = 1.36): group changes, year
rump_given_lmer.6 <- get.models(rump_given_lmer_weights, subset = delta < 2, REML = T)[[6]]
summary(rump_given_lmer.6)

# seventh model (dAICc = 1.36): condition (rump) score, mare age
rump_given_lmer.7 <- get.models(rump_given_lmer_weights, subset = delta < 2, REML = T)[[7]]
summary(rump_given_lmer.7)

# eight model (dAICc = 1.73): group changes; year; condition (rump) score
# now parameters estimated by REML, which should be more precise
rump_given_lmer.8 <- get.models(rump_given_lmer_weights, subset = delta < 2, REML = T)[[8]]
summary(rump_given_lmer.8)

# ninth model (dAICc = 1.80): region, condition (rump) score
# now parameters estimated by REML, which should be more precise
rump_given_lmer.9 <- get.models(rump_given_lmer_weights, subset = delta < 2, REML = T)[[9]]
summary(rump_given_lmer.9)

# 10th model (dAICc = 1.92): year
rump_given_lmer.10 <- get.models(rump_given_lmer_weights, subset = delta < 2, REML = T)[[10]]
summary(rump_given_lmer.10)

# 11th model (dAICc = 1.98): group changes; condition (rump) score; number of treatments
# now parameters estimated by REML, which should be more precise
rump_given_lmer.11 <- get.models(rump_given_lmer_weights, subset = delta < 2, REML = T)[[11]]
summary(rump_given_lmer.11)

# 12th model (dAICc = 1.99): intercept only
# now parameters estimated by REML, which should be more precise
rump_given_lmer.12 <- get.models(rump_given_lmer_weights, subset = delta < 2, REML = T)[[12]]
summary(rump_given_lmer.12)

# lucky 13th model (dAICc = 2.13): region and year
# now parameters estimated by REML, which should be more precise
rump_given_lmer.13 <- get.models(rump_given_lmer_weights, subset = delta < 4, REML = T)[[13]]
summary(rump_given_lmer.13)
