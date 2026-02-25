rm(list=ls())

##Code corresponding to LE7 - Intro to Statistics


forest <- c(9, 6, 4, 6, 7, 10)
field  <- c(12, 9, 12, 10)
mean(field) - mean(forest)

ants <- data.frame(
  place=rep(c("field","forest"),
            c(length(field), length(forest))),
  colonies=c(field,forest)
)

#visualize data
library(ggplot2)
ggplot(ants,aes(place,colonies))+
  geom_point(aes(place,jitter(colonies,factor=1.5)),shape=2,size=3)+
  #jitter uses factor to control the amount of the jitter, 
  #shape specifies triangles, and size is the size of the points
  geom_boxplot(fill=NA)

###########t-tests####################
#one-sample t-test - is the mean equal to 0?
tt_one <- t.test(field)
tt_one


#true student t-test - two sample t-test
#y variable is on the left side, x variables on the right
#~ says "vary by"
tt <- t.test(colonies~place,data=ants,var.equal=TRUE)
tt

#welch's t-test
tt <- t.test(colonies~place,data=ants)
tt

#a paired t-test doesn't make sense for this data because they aren't the same
#imagine we did an experiment where we cut down the trees in the forest, and then re-surveyed our plots
forest_pre=forest
forest_post=c(9+2,  6+1,  4+1,  6+1,  7+1, 10+1)

ttp<- t.test(forest_pre,forest_post,paired=T)
ttp
##see how flexible t-test is? We didn't even need to create a dataframe!

forest_treat <- data.frame(
  trmt=rep(c("forest_pre","forest_post"),
           c(length(forest_pre), length(forest_post))),
  colonies=c(forest_pre,forest_post)
)
forest_treat


#what if we didn't specify paired?
ttp<- t.test(colonies~trmt,data=forest_treat,var.equal=T)
ttp
