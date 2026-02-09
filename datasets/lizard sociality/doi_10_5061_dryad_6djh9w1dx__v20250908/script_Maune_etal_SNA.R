###############################################
##### Title:City lizards are more social ####
#   authors: Avery L. Maune, Tobias Wittenbreder, Duje Lisičić, Barbara A. Caspers, Ettore Camerlenghi, Isabel Damas-Moreira



### Import Data ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Social network data ##
# Only includes individuals that were observed at least 3 times N=69

# GBI matricies #
p1_gbi <- as.matrix(read.csv("P1_GBI.csv", header = TRUE, stringsAsFactors = FALSE))
p2_gbi <- as.matrix(read.csv("P2_GBI.csv", header = TRUE, stringsAsFactors = FALSE))
p3_gbi <- as.matrix(read.csv("P3_GBI.csv", header = TRUE, stringsAsFactors = FALSE))
p4_gbi <- as.matrix(read.csv("P4_GBI.csv", header = TRUE, stringsAsFactors = FALSE))
p5_gbi <- as.matrix(read.csv("P5_GBI.csv", header = TRUE, stringsAsFactors = FALSE))
p6_gbi <- as.matrix(read.csv("P6_GBI.csv", header = TRUE, stringsAsFactors = FALSE))

# Attribute tables #
p1_att <- read.csv("P1_attributes.csv", header=TRUE, row.names=NULL)
p2_att <- read.csv("P2_attributes.csv", header=TRUE, row.names=NULL)
p3_att <- read.csv("P3_attributes.csv", header=TRUE, row.names=NULL)
p4_att <- read.csv("P4_attributes.csv", header=TRUE, row.names=NULL)
p5_att <- read.csv("P5_attributes.csv", header=TRUE, row.names=NULL)
p6_att <- read.csv("P6_attributes.csv", header=TRUE, row.names=NULL)


## Association data ##

asso <- read.csv("associations_data.csv", header=TRUE, row.names=NULL)

asso$habitat <- factor(asso$habitat, levels = c("urban", "non_urban"))


## Population density data ##

pop_den <- read.csv("population_density.csv", header = TRUE, stringsAsFactors = FALSE)

pop_den$habitat <- factor(pop_den$habitat, levels = c("urban", "non_urban"))


#### Install libraries #### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(ggplot2)
library(asnipe)
library(sna)
library(dplyr)
library(lme4)
library(DHARMa)
library(performance)
library(glmmTMB)
library(patchwork)



#### NETWORK CONSTRUCTION ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create adjaceny matrix (NxN matrix) using get_network function in asnipe

net_p1<-get_network(p1_gbi, data_format = "GBI", association_index = "SRI")
net_p2<-get_network(p2_gbi, data_format = "GBI", association_index = "SRI")
net_p3<-get_network(p3_gbi, data_format = "GBI", association_index = "SRI")
net_p4<-get_network(p4_gbi, data_format = "GBI", association_index = "SRI")
net_p5<-get_network(p5_gbi, data_format = "GBI", association_index = "SRI")
net_p6<-get_network(p6_gbi, data_format = "GBI", association_index = "SRI")


## Measure weighted and binary degrees for each network using sna library ##
library(sna)


# Weighted degree (sum of edge weights)
deg_p1 <- degree(net_p1, gmode="graph")
deg_p2 <- degree(net_p2, gmode="graph")
deg_p3 <- degree(net_p3, gmode="graph")
deg_p4 <- degree(net_p4, gmode="graph")
deg_p5 <- degree(net_p5, gmode="graph")
deg_p6 <- degree(net_p6, gmode="graph")

# Binary degree (number of edges per node)
deg_bin_p1 <- degree(net_p1, gmode="graph",ignore.eval=TRUE)
deg_bin_p2 <- degree(net_p2, gmode="graph",ignore.eval=TRUE)
deg_bin_p3 <- degree(net_p3, gmode="graph",ignore.eval=TRUE)
deg_bin_p4 <- degree(net_p4, gmode="graph",ignore.eval=TRUE)
deg_bin_p5 <- degree(net_p5, gmode="graph",ignore.eval=TRUE)
deg_bin_p6 <- degree(net_p6, gmode="graph",ignore.eval=TRUE)

# Add degrees to attribute tables
p1_att$weighted_degree <- deg_p1
p2_att$weighted_degree <- deg_p2
p3_att$weighted_degree <- deg_p3
p4_att$weighted_degree <- deg_p4
p5_att$weighted_degree <- deg_p5
p6_att$weighted_degree <- deg_p6

p1_att$binary_degree <- deg_bin_p1
p2_att$binary_degree <- deg_bin_p2
p3_att$binary_degree <- deg_bin_p3
p4_att$binary_degree <- deg_bin_p4
p5_att$binary_degree <- deg_bin_p5
p6_att$binary_degree <- deg_bin_p6


#### Create SNA dataset ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Combine all attribute tables from urban and non_urban transects into one dataset
sna <- bind_rows(p1_att, p2_att, p3_att, p4_att, p5_att, p6_att)

sna$habitat <- factor(sna$habitat, levels = c("urban", "non_urban"))


#### Variables ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Response variables 
# a) binary_degree - number of unique connections per individual
#    dataset = sna
# b) weighted_degree - sum of edge weights per individual
#    dataset = sna
# c) asso_count - number of times each marked lizard was observed in an association (with marked or unmarked lizard)
#    dataset = asso
# d) no_liz - count of lizards per line transect survey
#    dataset = pop_den


### Explanatory variables 
# habitat                          #urban, non_urban
# sex                              # F, M, Sub

### Random effects
# transect                         # P1, P2, P3, P4, P5, P6



#### Observation effort ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check that there is no observation bias between habitats

par(mfrow=c(1,1))
hist(asso$obs_count)
summary(asso$obs_count)


M1 <- glmer(obs_count ~ habitat + sex + (1|transect), family = poisson, data=asso)


check_overdispersion(M1) # Overdispersion detected.

# Model overdispersed so use a negative binomial distribution instead

M1.1 <- glmmTMB(obs_count ~ habitat + sex + (1|transect), family = nbinom2, data=asso)


check_overdispersion(M1.1) # No overdispersion detected.
check_model(M1.1)
sim_res <- simulateResiduals(M1.1)
plot(sim_res)

summary(M1.1)
# Family: nbinom2  ( log )
# Formula:          obs_count ~ habitat + sex + (1 | transect)
# Data: asso

# AIC       BIC    logLik -2*log(L)  df.resid 
# 504.4     519.7    -246.2     492.4        88 

# Random effects:
  
# Conditional model:
# Groups   Name        Variance  Std.Dev. 
# transect (Intercept) 5.925e-10 2.434e-05
# Number of obs: 94, groups:  transect, 6

# Dispersion parameter for nbinom2 family (): 3.31 

# Conditional model:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        1.5085     0.1323  11.405   <2e-16 ***
# habitatnon_urban  -0.1391     0.1575  -0.883   0.3771    
# sexM               0.3765     0.1577   2.388   0.0169 *  
# sexSub             0.3522     0.2879   1.224   0.2211    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



#### MODELS ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##### a) Binary degree ####

par(mfrow=c(1,1))
hist(sna$binary_degree)
summary(sna$binary_degree)

# Count data - fit a Poisson GLMM

Ma <- glmer(binary_degree ~ habitat + sex + (1|transect), family = poisson, data=sna)


check_overdispersion(Ma) # No overdispersion detected.
check_model(Ma)
sim_res <- simulateResiduals(Ma)
plot(sim_res)
testZeroInflation(sim_res) # no zero inflation


summary(Ma)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: poisson  ( log )
# Formula: binary_degree ~ habitat + sex + (1 | transect)
# Data: sna

# AIC      BIC   logLik deviance df.resid 
# 201.6    212.8    -95.8    191.6       64 

# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -1.5562 -0.5338 -0.4138  0.6163  3.2373 

# Random effects:
# Groups   Name        Variance Std.Dev.
# transect (Intercept) 0.0641   0.2532  
# Number of obs: 69, groups:  transect, 6

# Fixed effects:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        0.6145     0.2337   2.630 0.008545 ** 
# habitatnon_urban  -1.8479     0.4791  -3.857 0.000115 ***
# sexM              -0.1399     0.2186  -0.640 0.522208    
# sexSub            -0.1673     0.3416  -0.490 0.624372    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
# (Intr) HABITA sexM  
# habitatnn_r -0.291              
# sexM        -0.493 -0.102       
# sexSub      -0.297  0.035  0.336



##### b) Weighted degree ####

par(mfrow=c(1,1))
hist(sna$weighted_degree)
summary(sna$weighted_degree)

# Data is right-skewed and contains a lot of zeros
# Use a tweedie distribution which allows for continuous zero-inflated data


Mb <- glmmTMB(weighted_degree ~ habitat + sex + (1 | transect), data = sna, family = tweedie(link = "log"))


check_overdispersion(Mb) # No overdispersion detected.
check_model(Mb)
sim_res <- simulateResiduals(Mb)
plot(sim_res)
testZeroInflation(sim_res) # no zero inflation

summary(Mb)
# Family: tweedie  ( log )
# Formula:          weighted_degree ~ habitat + sex + (1 | transect)
# Data: sna
# AIC       BIC    logLik -2*log(L)  df.resid 
# 13.5      29.2       0.2      -0.5        62 
# Random effects:
# Conditional model:
# Groups   Name        Variance  Std.Dev. 
# transect (Intercept) 8.541e-12 2.922e-06
# Number of obs: 69, groups:  transect, 6

# Dispersion parameter for tweedie family (): 0.172 

# Conditional model:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -1.5140     0.1839  -8.234  < 2e-16 ***
# habitatnon_urban  -1.7239     0.4249  -4.058 4.96e-05 ***
# sexM              -0.3550     0.2575  -1.379    0.168    
# sexSub            -0.6162     0.4618  -1.334    0.182    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


##### c) Total associations ####

par(mfrow=c(1,1))
hist(asso$asso_count)
summary(asso$asso_count)

# Count data - fit a Poisson GLMM

Mc <- glmer(asso_count ~ habitat + sex + (1|transect), family = poisson, data=asso)

check_overdispersion(Mc) # Overdispersion detected.

#Re-fit with negative binomial distribution

Mc.1 <- glmmTMB(asso_count ~ habitat + sex + (1|transect), family = nbinom2, data=asso)

check_overdispersion(Mc.1) #No overdispersion detected.
check_model(Mc.1)
sim_res <- simulateResiduals(Mc.1)
plot(sim_res)
testZeroInflation(sim_res) # no zero inflation

summary(Mc.1)

# Family: nbinom2  ( log )
# Formula:          asso_count ~ habitat + sex + (1 | transect)
# Data: asso

# AIC       BIC    logLik -2*log(L)  df.resid 
# 375.8     391.0    -181.9     363.8        88 

# Random effects:
  
# Conditional model:
# Groups   Name        Variance Std.Dev.
# transect (Intercept) 0.02157  0.1469  
# Number of obs: 94, groups:  transect, 6

# Dispersion parameter for nbinom2 family (): 1.74 

# Conditional model:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -0.3047     0.3036  -1.004    0.316    
# habitaturban   1.3141     0.3014   4.360  1.3e-05 ***
# sexM           0.1743     0.2325   0.750    0.453    
# sexSub         0.2345     0.3958   0.592    0.554    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



##### d) Population density ####
hist(pop_den$no_liz)
summary(pop_den$no_liz)

# Count data - fit a Poisson GLMM

Md <- glmer(no_liz ~ habitat + (1|transect), family = poisson, data=pop_den)

check_overdispersion(Md) # No overdispersion detected.
check_model(Md)
sim_res <- simulateResiduals(Md)
plot(sim_res)
testZeroInflation(sim_res) # no zero inflation

summary(Md)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: poisson  ( log )
# Formula: no_liz ~ habitat + (1 | transect)
# Data: pop_den

# AIC      BIC   logLik deviance df.resid 
# 257.9    264.2   -126.0    251.9       57 

# Scaled residuals: 
# Min       1Q   Median       3Q      Max 
# -1.69818 -0.74045 -0.01814  0.35120  2.31300 

# Random effects:
# Groups   Name        Variance Std.Dev.
# transect (Intercept) 0.1257   0.3545  
# Number of obs: 60, groups:  transect, 6

# Fixed effects:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    1.1563     0.2287   5.057 4.27e-07 ***
# habitaturban   0.5716     0.3172   1.802   0.0716 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
# (Intr)
# habitaturbn -0.720




#### RE-RUN W/O P2 ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Exclude transect P2 and sub-adults and re-run models

## Association data set
# Remove transect P2
new_asso <- asso %>% 
  filter(transect != "P2")

# Remove sub-adults
new_asso <- new_asso[new_asso$sex != "Sub", ]



## SNA data set
# Remove transect P2
new_sna <- sna %>% 
  filter(!transect %in% c("P2"))

# Remove sub adults because only 3 are left after removing P2
new_sna <- new_sna[new_sna$sex != "Sub", ]



##### a) Binary degree ####

Ma.2 <- glmer(binary_degree ~ habitat + sex + (1 | transect), family = poisson, data=new_sna)
#boundary (singular) fit: see help('isSingular')

summary(Ma.2)
## transect has a variance of 0 so remove to avoid singular fit

Ma.2 <- glm(binary_degree ~ habitat + sex, family = poisson, data=new_sna)

check_overdispersion(Ma.2) #No overdispersion detected.
check_model(Ma.2)
sim_res <- simulateResiduals(Ma.2)
plot(sim_res)
testZeroInflation(sim_res) # no zero inflation

summary(Ma.2)
# Call:
# glm(formula = binary_degree ~ habitat + sex, family = poisson, 
# data = new_sna)
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        0.3737     0.2816   1.327 0.184449    
# habitatnon_urban  -1.6647     0.4599  -3.620 0.000295 ***
# sexM              -0.0108     0.3655  -0.030 0.976419    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Dispersion parameter for poisson family taken to be 1)
# Null deviance: 56.917  on 39  degrees of freedom
# Residual deviance: 39.104  on 37  degrees of freedom
# AIC: 89.696
# Number of Fisher Scoring iterations: 6


##### b) Weighted degree ####

Mb.2 <- glmmTMB(weighted_degree ~ habitat + sex + (1 | transect), data = new_sna, family = tweedie(link = "log"))

check_overdispersion(Mb.2) #No overdispersion detected.
check_model(Mb.2)
sim_res <- simulateResiduals(Mb.2)
plot(sim_res)
testZeroInflation(sim_res) # no zero inflation


summary(Mb.2)

# Family: tweedie  ( log )
# Formula:          weighted_degree ~ habitat + sex + (1 | transect)
# Data: new_sna

# AIC       BIC    logLik -2*log(L)  df.resid 
# 26.7      36.8      -7.3      14.7        34 

# Random effects:
  
# Conditional model:
# Groups   Name        Variance Std.Dev.
# transect (Intercept) 0.0934   0.3056  
# Number of obs: 40, groups:  transect, 5

# Dispersion parameter for tweedie family (): 0.217 

# Conditional model:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -1.5479     0.3773  -4.102 4.09e-05 ***
# habitatnon_urban  -1.7426     0.5660  -3.079  0.00208 ** 
# sexM              -0.3649     0.3973  -0.919  0.35834    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


##### c) Total associations ####

Mc.2 <- glmmTMB(asso_count ~ habitat + sex + (1|transect), family = nbinom2, data=new_asso)

check_overdispersion(Mc.2) # No overdispersion detected.
check_model(Mc.2)
sim_res <- simulateResiduals(Mc.2)
plot(sim_res)
testZeroInflation(sim_res) # no zero inflation


summary(Mc.2)
# Family: nbinom2  ( log )
# Formula:          asso_count ~ habitat + sex
# Data: new_asso

# AIC       BIC    logLik -2*log(L)  df.resid 
# 191.6     199.7     -91.8     183.6        52 


# Dispersion parameter for nbinom2 family (): 1.18 

# Conditional model:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       0.90834    0.31483   2.885  0.00391 ** 
# habitatnon_urban -1.14368    0.34077  -3.356  0.00079 ***
# sexM              0.09199    0.35705   0.258  0.79668    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



##### Summaries #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## a) Binary degree ##

# Calculate mean and standard error of binary degree by habitat
binary_degree_summary <- sna %>%
  group_by(habitat) %>%
  summarise(
    mean_binary_degree = mean(binary_degree, na.rm = TRUE),
    se_binary_degree = sd(binary_degree, na.rm = TRUE) / sqrt(n())
  )

binary_degree_summary
# habitat   mean_binary_degree se_binary_degree
# <fct>                  <dbl>            <dbl>
# 1 urban                  1.91             0.223
# 2 non_urban              0.273            0.117

# Percentage of urban lizards with at least one social connection
# Subset only urban lizards
urban_lizards <- sna %>% filter(habitat == "urban")

# Calculate the percentage of lizards with at least 1 connection

#urban
urban_lizards <- sna %>% filter(habitat == "urban")
percentage_urban_with_degree <- (sum(urban_lizards$binary_degree >= 1) / nrow(urban_lizards)) * 100
percentage_urban_with_degree #81% of urban lizards have a social connection
sum(urban_lizards$binary_degree >= 1) #38 urban lizards with a connection
nrow(urban_lizards) #47 total urban lizards

#non-urban lizards
non_urban_lizards <- sna %>% filter(habitat == "non_urban")
percentage_non_urban_with_degree <- (sum(non_urban_lizards$binary_degree >= 1) / nrow(non_urban_lizards)) * 100
percentage_non_urban_with_degree # 23% of non-urban lizards have a social connection
sum(non_urban_lizards$binary_degree >= 1) # 5
nrow(non_urban_lizards) # 22


## b) Weighted degree ##

# Calculate mean and standard error of binary degree by habitat
weighted_degree_summary <- sna %>%
  group_by(habitat) %>%
  summarise(
    mean_weighted_degree = mean(weighted_degree, na.rm = TRUE),
    se_weighted_degree = sd(weighted_degree, na.rm = TRUE) / sqrt(n())
  )

weighted_degree_summary
# habitat   mean_weighted_degree se_weighted_degree
# <fct>                    <dbl>              <dbl>
# 1 urban                   0.175              0.0238
# 2 non_urban               0.0303             0.0140


## c) Total associations ##

length(asso$id)  # 94 individuals 
length(asso$habitat[asso$habitat=="urban"]) # 62 urban lizards
length(asso$habitat[asso$habitat=="non_urban"]) #  32 non-urban lizards


associations_summary <- asso %>%
  group_by(habitat) %>%
  summarise(
    mean_associations = mean(asso_count, na.rm = TRUE),
    se_associations = sd(asso_count, na.rm = TRUE) / sqrt(n())
  )

associations_summary
# habitat   mean_associations se_associations
# <fct>                 <dbl>           <dbl>
# 1 urban                 3.23            0.374
# 2 non_urban             0.844           0.246


# Percentage of observations spent in association
asso <- asso %>%
  mutate(association_percentage = (asso_count / obs_count) * 100)

assoc_summary <- asso %>%
  group_by(habitat) %>%
  summarise(
    mean_association_percentage = mean(association_percentage, na.rm = TRUE),
    se_association_percentage = sd(association_percentage, na.rm = TRUE) / sqrt(n())
  )

assoc_summary
# habitat   mean_association_percentage se_association_percentage
# <fct>                           <dbl>                     <dbl>
# 1 urban                            53.2                      3.94
# 2 non_urban                        15.0                      4.24


