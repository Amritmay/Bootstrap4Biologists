#' ---
#' title: "Case Study III: Model Selection Uncertainty" 
#' output: html_document
#' ---

#' ## Objective
#'    
#' This example demonstrates how the bootstrap can be used to explore model uncertainty.  
#'  
#' #### Load R libraries	
#' 	
#+warning=FALSE, message=FALSE 	
library(knitr)	
library(rms)	# for validate function
library(MASS) # for stepAIC


#' ## Setting the seed of the random number generator	
#' 	
#' Use the **set.seed()** function in R to initialize the random number generator.  	
#' 	
set.seed(2041971)	

#' ## Modeling abundance of longnose dace	
#' 	
#' Read in the data:	
dace<- read.csv("data/longnosedace.csv")	

#' ### Predictors
#' 
#'  - acreage = area (in acres) drained by the stream
#'  - do2 = the dissolved oxygen (in mg/liter)
#'  - depth = the maximum depth (in cm) of the 75-meter segment of stream
#'  - no3 = nitrate concentration (mg/liter)
#'  - so4 = sulfate concentration (mg/liter)
#'  - temp = water temperature on the sampling date (in degrees C).
#'  

#' Fit a model using all 6 predictors, then use stepAIC to implement backwards
#' selection to choose a "best" model.
fullmod.lm<-lm(longnosedace~acreage+do2+maxdepth+no3+so4+temp,data=dace)	
stepAIC(fullmod.lm)


#' ##  Bootstrap validation	
#' 	
#' Validate will use the bootstrap to calculate "honest" measures of model fit. 
#'  We can also visualize "model uncertainty" in the "best model" by using bw=T 
#'  (which tells R to use backwards selection to choose the best model). The
#'  "*" below indicate, which variables are included in the "optimal model"
#'  for each bootstrap replicate.
#'  
#' After applying a backwards model selection algorithm, we end up with 
#' a model containing only acreage and no3. The $R^2$ of this model = 0.24, 
#' which describes the variance in longnosedace explained by these two predictors.
#'  If we were to apply this same model to a new data set, we would expect
#' the amount of variance that would be explained to be much lower. We can
#' obtain a more "honest" measure of the variance by: a) creating 2 bootstrap
#' data sets (one for model training and one for model testing); b) applying our
#' model selection algorithm to the training data set and calculating
#' the resulting $R^2$; c) use the same model to predict the response in the
#' bootstrap test data set and use these predictions to calculate
#' a second $R^2$;  d) calculate a measure of "optimism" by subtracting the 
#' average $R^2$ from part c from the average $R^2$ in part b; e) subtract
#' this estimate of optimism from the $R^2$ obtained from our original data set. 
#' The validate function will do this for us!
#' 
fullmod.ols<-ols(longnosedace~acreage+do2+maxdepth+no3+so4+temp,data=dace, x=T, y=T)	
validate(fullmod.ols, bw=T, B=100)

#' ## Conclusions
#' 
#' 1. We see that the different bootstrap samples result in different models 
#' being chosen as optimal. The number of predictor variables included ranges 
#' from 1 (in 6 models) to 6 (in 1 model). 
#' 
#' 2. We see that our original estimate of $R^2$ (0.24) is likely quite
#' optimistic (our estimate of optimism = 0.20).  Thus, we end up
#' with a corrected estimate of $R^2$ = 0.037 (quite depressing!).  
#'

#' ## Footer	
#' 	
#' 	
# Session Information:	
sessionInfo()   	

