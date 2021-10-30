#' ---
#' title: "Case Study I: Relaxing the Assumptions of Linear Regression"
#' output: html_document
#' ---

#' ## Objectives
#' 
#' This example demonstrates how:
#' 
#' 1. a cluster-level bootstrap can be used for repeated measures
#' data with equal-sized clusters. This approach is most applicable
#' to problems where predictors of interest do not vary within a cluster.
#' 
#' 2. to use functions in the boot package to calculate different bootstrap
#' confidence intervals, including the BCa interval, which has better
#' statistical properties than percentile-based intervals.  
#' 

#' ## Document Preamble
#+ docPreamble, warning=FALSE, message=FALSE
# Load Libraries
 library(knitr)
 library(mosaic)
 library(ggplot2)
 library(ggfortify)

# Set knitr options
opts_chunk$set(fig.width = 6, fig.height=5)

# Clear Environment (optional)
remove(list=ls())

# Set seed 
set.seed(314159)

#' ## Section 1. Bootstrapping RIKZ data
#' 
#' Read in data from .csv and look at first 6 rows
#+ dataEntry01
rikzData <- read.csv("data/RIKZdat.csv")
head(rikzData)

#' Fit a linear regression model relating species richness to exposure level
#+ linReg01
# Simple linear regression and summary
lm.RIKZ <- lm(Richness~exposure, data=rikzData)
summary(lm.RIKZ)
 
#' ## Check Assumptions

#' Here, we see that the residuals do not appear Normally distributed.
#' We also know the independence assumption is problematic.
#+fig.width=8, fig.height=4, echo=FALSE, include=FALSE
pdf("Figure6.pdf", width = 8, height = 4)
par(mfrow=c(1,2))
with(rikzData, plot(exposure, Richness, pch=16, bty="L", cex.lab=1.4, xlab="Exposure"))
abline(lm.RIKZ)
title("A)", adj=0)
xdat<-range(lm.RIKZ$resid) 
hist(lm.RIKZ$resid, col="gray", xlab="Residuals", freq=FALSE, bty="L", cex.lab=1.4, main="")
title("B)", adj=0)
curve(dnorm(x, mean(lm.RIKZ$resid), sd(lm.RIKZ$resid)), from =xdat[1], to=xdat[2], 
      add=TRUE, col="black")
dev.off()

#+fig.width=8, fig.height=4 
par(mfrow=c(1,2))
with(rikzData, plot(exposure, Richness, pch=16, bty="L", cex.lab=1.4))
abline(lm.RIKZ)
title("A)", adj=0)
xdat<-range(lm.RIKZ$resid) 
hist(lm.RIKZ$resid, col="gray", xlab="Residuals", freq=FALSE, bty="L", cex.lab=1.4, main="")
title("B)", adj=0)
curve(dnorm(x, mean(lm.RIKZ$resid), sd(lm.RIKZ$resid)), from =xdat[1], to=xdat[2], 
      add=TRUE, col="black")
 


#' Cluster-level bootstrap example. The code below shows how to create
#' a single bootstrap data set where we resample clusters.
#+ bootstrap01
# Data processing
uid <- unique(rikzData$Beach) # unique id for each beach
nBeach <- length(uid) # number of beaches

### One bootstrap:
# Take a sample from x (uid) of size nBeach with replacement:
bootIDs <- data.frame(Beach = sample(x = uid, size = nBeach, replace = TRUE))
bootIDs

# Use this to sample from original data by beach
bootDat <- merge(bootIDs, rikzData)
table(bootDat$Beach) ## this table shows how many obs are drawn for each beach in bootstrap sample.

# Double check sample sizes worked (these should match):
length(rikzData$Beach) # original data
length(bootDat$Beach) # bootstrap sample

#' Now, repeat this process several times and store results. 
#+ bootstrap02
nBoots <- 5000 # number of bootstraps
coef.ests <- matrix(NA, ncol=2, nrow=nBoots) # to hold bootstrap estimates
for(i in 1:nBoots){
  # create bootstrap
  bootIDs <- data.frame(Beach = sample(x = uid, size = nBeach, replace = TRUE))
  bootDat <- merge(bootIDs, rikzData)
  
  # Estimate coefficients
  lmBoot <- lm(Richness ~ exposure, data=bootDat)
  coef.ests[i,] <- lmBoot$coefficients
}

# Look at histograms of the bootstrap estimates
par(mfrow=c(1,2))
hist(coef.ests[,1], main="", xlab="intercept")
hist(coef.ests[,2], main="", xlab="slope")

# Bootstrap confidence intervals using the percentile method
# Intercept:
quantile(x = coef.ests[,1], probs = c(0.025,0.975), na.rm=TRUE)

# Slope:
quantile(x = coef.ests[,2], probs = c(0.025,0.975), na.rm=TRUE)

# Compare to linear regression confidence intervals:
confint(lm.RIKZ)

#' 
#' ## Better bootstrap confidence intervals 
#'   
#' Note:  Although it is common to use percentile-based bootstrap confidence intervals,
#'  there are better ways to calculate confidence intervals, particularly when the 
#'  bootstrap distribution is not symmetric  (as is the case here).  A nice discussion of
#'  alternative bootstrap confidence intervals is Hesterberg (2015). What Teachers 
#'  Should Know about the Bootstrap: Resampling in the Undergraduate Statistics Curriculum,
#'  The American Statistician 69:371-386. 
#'  
#'  Below, I illustrate how one can use the boot library to calculate some alternative intervals. 
#'  
#' ### Other alternatives using boot.ci in the boot library
#' 
#' - Normal = estimate +/- 1.96*SE(bootstrap distribution)
#' - Percentile = percentile-based 
#' - Basic = calculates differences between theta[boot] and original estimate and uses
#'    this information to construct the interval
#' - BCa = attempts to correct for bias and skewness in the bootstrap distribution.
#'

#' Load boot library
#+warning=FALSE, message=FALSE 
library(boot)

#' To use the functions in the boot library, we have to create a function with first
#'  two arguments = data, indices 
#' 
#' - data = the data that will be resampled
#' - indices are the vector of indices that will be resampled
#+ bootfunc01  
slope.exposure<-function(data, indices, formula, obsdat){
    # data will contain the data to be resampled (here, beach ids)
    # indices = used to select clusters
    # formula = allows flexibility w/ the model that is fit
    # obsdat = our data set containing all observations
    
    bootbeaches<-data[indices]
    bootids<-data.frame(Beach=bootbeaches)
    bootdat<-merge(bootids, obsdat)  
  
# Fit the linear model with exposurec and return the fitted coefficients  
   lm.boot<- lm(formula, data = bootdat)
   return(coef(lm.boot))
}

# Call the function to do the bootstrapping  
results<-boot(data=uid, statistic = slope.exposure,  R=9999, 
              formula=Richness ~ exposure, obsdat=rikzData)

#' Lets look at the results. Note: the warning, below, tells us that we are unable to calculate 
#' studentized intervals (these are discussed in Hesterberg (2015)) and tend to work well in many
#' situations but require an estimate of variance of the statistic to go along with each bootstrap replicate.
#' Nonetheless, we get several other intervals all from the same set of 
#' bootstrapped coefficients.  Of these, the BCa interval is arguably the most defensible approach. 
#+ Bootresults01
# Look at results: Intercept   
boot.ci(results, index=1)
  

# Look at results: Slope   
boot.ci(results, index=2)
  
#' The percentile-based intervals are very similar to the intervals we calculated previously. 
#'  Note, however the BCA interval (preferred) is highly asymmetric. All 
#'  intervals are quite a bit wider than those calculated assuming independence. 
#'   Lets plot the results for a visual comparison using ggplot.
#+ message=FALSE, warning=FALSE
library(ggplot2) 

#' Gather the data.
#+ datagather01, warning=FALSE   
intervals<-matrix(NA,10,2)

# Interepts
intervals[1,]<-confint(lm.RIKZ)[1,] # CI assuming independence 
intervals[2,]<- boot.ci(results, index=1)$normal[2:3] #normal based
intervals[3,]<- boot.ci(results, index=1)$basic[4:5] # basic
intervals[4,]<- boot.ci(results, index=1)$percent[4:5] # percentile
intervals[5,]<- boot.ci(results, index=1)$bca[4:5] # bias-corrected and accelerated

# Slopes
intervals[6,]<-confint(lm.RIKZ)[2,] # CI assuming independence 
intervals[7,]<- boot.ci(results, index=2)$normal[2:3] # normal
intervals[8,]<- boot.ci(results, index=2)$basic[4:5] #basic
intervals[9,]<- boot.ci(results, index=2)$percent[4:5] #percentil
intervals[10,]<- boot.ci(results, index=2)$bca[4:5]
  

#' Reformat the data for plotting
intervals.df<-data.frame(lowerCL=intervals[,1], upperCL=intervals[,2], 
                         parameter=c(rep("Intercept",5), rep("Slope",5)),
                         type=rep(c("Independence", "Normal", "Basic", "Percentile", "BCa"), 2), 
                         PE=rep(coef(lm.RIKZ), each=5)) 

#' Plot
#+ ggplotInt01, fig.width=10, fig.height=6
  ggplot(intervals.df, aes(x=type, y=PE, group=type)) +
    geom_errorbar(width=.1, aes(ymin=lowerCL, ymax=upperCL), colour="blue") + 
    geom_point(shape=21, size=3, fill="white")+
    facet_wrap(~parameter, ncol=2, scales="free") + xlab("")+ylab("Estimate and 95% CI")


#' ## Conclusions
#' 
#' Using a cluster-level bootstrap results in wider confidence intervals than naive intervals 
#' that assume independence.  Also, the BCa intervals, which have better statistical properties
#' than percentile-based intervals when estimators exhibit bias or the sampling distribution is 
#' skewed, result is a highly asymmetric confidence interval.
#' 
#' ## Document footer
#' 
#' Session Information:
#+ sessionInfo
sessionInfo()