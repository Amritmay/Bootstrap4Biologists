#' ---
#' title: "Supplementary Appendix A: Linear Regression Example"
#' output: html_document
#' ---

#' ## Objective
#'    
#' This simulation example demonstrates how to conduct a permutation-based 
#' test for a partial regression coefficient in a multiple linear regression 
#' model. 
#'   
#' ## Document Preamble
#+ docPreamble, warning=FALSE, message=FALSE
# Load Libraries
library(knitr)
library(mosaic)
library(ggplot2) 
library(MASS)

# Set knitr options
opts_chunk$set(fig.width = 6, fig.height=5)

# Clear Environment (optional)
remove(list=ls())

# Set seed 
set.seed(314159) 

#' ## Simulation Example
#' 
#' Here we will consider a simple simulation where a response variable, y,
#' is related to two predictor variables, x1 and x2.  The predictors are
#' themselves correlated.  We will illustrate a simple permutation-based
#' test for the effect of x1, adjusted for x2. 
#' 
#' Steps:
#' 
#' 1. Fit a linear regression model relating x1 to x2. 
#' 2. Add the residuals from this model to the original data set.
#' 3. Create the permutation distribution by shuffling these residuals.
#' 4. Determine the p-value by comparing the t-statistic from the 
#' fit to the original data set to the permutation-based distribution
#' of this same statistic.
#' 
#' Simulation parameters
#' 
#' - Sigma (variance/covariance matrix of x1 and x2). 
#' - We will assume mean of x1 and x2 =0
#' - Beta = vector of regression parameters (with intercept=0)
#' 
Sigma <- matrix(c(10,3,3,2),2,2) 
Beta <- c(0.2, -0.5)

#' Create correlated predictors
X<- mvrnorm(n = 100, rep(0, 2), Sigma)
cor(X)

#' Form response variables
y<-X%*%Beta+rnorm(100,0,2)
Mydata<-data.frame(y=y, x1=X[,1], x2=X[,2])

#' Fit regression model to the data
lmsim<-lm(y~x1+x2, data=Mydata)
summary(lmsim)

#' Step 1: capture the part of x1 that is not related to x2
lm1<-lm(x1~x2, data=Mydata)
Mydata<-Mydata %>% mutate(x1resid=lm1$resid)

#' Demonstrate that using the residuals here results in the same
#' coefficient, standard error, t-statistic and p-value for x1
#' as in our original regression (lmsim)
lmsim2<-lm(y~x1resid+x2, data=Mydata)
summary(lmsim2)

#' Store the t-statistic for x1 from this model
(tstat<-summary(lmsim)$coefficients[2,3])

#' Step 2: create the permutation distribution
randsims<-do(10000)*{  
  lmrand<-lm(y~shuffle(x1resid)+x2, data=Mydata)
  summary(lmrand)$coefficients[2,3]
}
head(randsims)

#' Plot the randomization distribution with our original statistic 
histogram(~result, data=randsims, v=tstat, col="gray")

#' Determine our p-value
prop(~I(abs(result)>=tstat), data=randsims) 

#' ## Conclusions
#' 
#' The permutation-based approach allows us to relax the Normality assumption.
#' Our randomization-based p-value is really similar to the p-value
#' of the original t-test. This result is not surprising given that 
#' the assumptions of linear regression (constant variance, normality, 
#' linearity) all hold in the simulation example. 
#' 

#' ## Document footer
#' 
#' Session Information:
#+ sessionInfo
sessionInfo()