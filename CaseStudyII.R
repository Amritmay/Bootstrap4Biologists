#' ---
#' title: "Case Study II: Combining results from multiple models" 
#' output: html_document
#' ---
#' 
#' ## Objectives
#'    
#' This example demonstrates how:
#' 
#' 1. the bootstrap can be used in applications that involve multiple
#' response measures from the same set of cases.  
#' 
#' 2. the bootstrap can provide estimates of uncertainty for 
#' non-linear functions of model parameters. In such cases, there will
#' usually not be an analytical formula for calculating the SE of our
#' estimator. Alternatives are to use the delta method with a Normal
#' approximation of the sampling distribution or Bayesian methods.  
#'   
#' This example comes from:
#' 
#' Zicus, M. C., D. P. Rave, and J. Fieberg.  2006.  Cost effectiveness of single- vs.
#'  double-cylinder over-water nest structures.  Wildlife Society Bulletin 34:647-655.
#' 
#+ warning=FALSE, message=FALSE 
library(geepack)
library(gmodels)
library(mgcv)
library(splines) 
library(dplyr)
library(ggplot2)
library(ggfortify)

#' Set seed of random number generator
set.seed(10)

#' Read in survival data and duckling data
  ddata<-read.csv("data/costeff.csv")
  names(ddata)<-tolower(names(ddata))
  ddata$deply<-as.factor(ddata$deply)
  ddata$year<-as.factor(ddata$year)

#' Variables of interest:
#' 
#' - deply = 0 for single cylinders and 1 for double cylinders
#' - strtno = structure ID (unique to each nesting structure)
#' - period = (1-4) categorical variable capturing seasonal effects
#' - size = size of the wetland where the structure is placed
#' - year = year of observation
#' - yng = number of ducks produced 
#'
  
#' Make sure the observations are ordered by structure ID (strtno);
  x<-order(ddata$strtno)
  ddata<-ddata[x,]

#' ## Structure survival model
#' 
#'  Place knots at size = 3 and size = 10 and create spline basis vectors when
#'  modeling the (non-linear) effect of wetland size
  bsize<-ns(ddata$size,df=3, knots=c(3,10))
  ddata2<-cbind(ddata,bsize[,1:3])
  names(ddata2)[8:10]<-c("sz1", "sz2", "sz3")

#' Fit the discrete time survival model to capture influence of structure type (deply)
#' and wetland size. 
  glmsurv<-glm(surv~year+deply +sz1+sz2+sz3, family=binomial(link=cloglog), data=ddata2)

#' Now, lets get predictions for both cylinder types, a range of sizes, and all 8 years.
  pdat<-expand.grid(size = seq(0,28,0.1), deply=unique(ddata2$deply), year=unique(ddata2$year))
  
#' Create the spline basis vectors again, using the same set of knots
  pdatsize<-predict(bsize, pdat$size)  
  pdat2<-cbind(pdat,pdatsize[,1:3])
  names(pdat2)[4:6]<-c("sz1", "sz2", "sz3")
  
#' Now, predictions on prob of failure (accounting for the fact that our model is fit on the cloglog scale)
  pdat2$pfail<-predict(glmsurv, newdata=pdat2, type="resp")
  
#' Plot survival for each year and deployment type. We see that structure
#' failure rates increase with the size of the wetland. These failure rates
#' also varied considerably from year to year.
#+ fig.width=8, fig.height=4   
  ggplot(pdat2, aes(x=size, y=pfail, color=year))+geom_line()+facet_wrap(~deply)+
   xlab("Size of wetland (ha)")+ ylab("Probability of Structure Failure")

#' Get predictions averaged across years. We see that failure rates 
#' were slightly higher in double-cylinder than single-cylinder structures.
   pfail<- pdat2 %>% group_by(size, deply) %>% summarize(meanfail=mean(pfail))
   ggplot(pfail, aes(x=size, y=meanfail, color=deply))+geom_line() +
     xlab("Size of wetland (ha)")+ ylab("Probability of Structure Failure")
   
#' Costs were estimated as fixed ($25/cylinder + variable depending on survival and cylinder type)
#' 
#' - $28 if single cylinder fails, $48 if double cylinder fails
   vcost<-ifelse(pfail$deply==0,28,48)
   pfail$ec<-25+vcost*pfail$meanfail  
   ggplot(pfail, aes(x=size, y=ec, color=deply))+geom_line() +
     xlab("Size of wetland (ha)")+ ylab("Expected cost")
   
#' ## Duckling production model

#' Since the predictors do not change across years for this model, Zicus et al. just
#' modeled the mean number of ducks as the response.  Determine the mean   
  tdata<-ddata %>% group_by(strtno) %>% summarize(
    yng= mean(yng), size=mean(size), deply=unique(deply)
  ) 
  
#' We can now model how the number of ducks depends on size of the wetland and cynlinder type
  dmod<-lm(yng~size+I(size^2)+deply, data=tdata)
  summary(dmod)
 
#' Note, the assumptions of linear regression are not met. In particular,
#' the assumption that the residuals are Normally distributed seems suspect
#' as evidenced by points falling off the line in the qqplot (top right).
#' That's OK, we will use a non-parametric bootstrap for inference 
#' (resampling structures with replacement).
#+ fig.width=6, fig.height=6
  autoplot(dmod)     
  
#' Predictions for the same range of wetland sizes and both deployment types
  ducks<-expand.grid(size =seq(0,28,0.1), deply=unique(tdata$deply))
  ducks$phat<-predict(dmod, newdata=ducks)

#' Order the ducks data set the same way as the structure failure data set
pfail
ducks<-ducks %>% arrange(size, deply)
head(ducks)

#' Add plot showing expected ducks versus wetland size. 
#' Here, we see that larger wetlands
#' may result in higher production rates of ducklings, but the data are 
#' highly variable and there is not much data available for structures
#' placed in the largest wetlands.
#+ fig.width=8, fig.height=4 
  ggplot(ducks, aes(x=size, y=phat)) + geom_line() + 
    geom_point(data=tdata, aes(size, y=yng))+ facet_wrap(~deply) +
    xlab("Size of wetland (ha)")+ ylab("Expected number of ducks")

#' ## Get cost effectiveness by talking E[ducks]/E[cost]
  costef<-inner_join(pfail, ducks)
  costef$ce<- costef$ec/costef$phat

#' Plot cost-effectiveness versus wetland size for both cylinder types. We
#' see that cost-effectiveness appears to be highest for deeper wetlands
#' (due to higher duck production in these wetlands, despite higher 
#' failure probabilities associated with larger wetlands).  However, we need to 
#' calculate a measure of uncertainty to help interpret these results.    
  ggplot(costef, aes(x=size, y=ce))+geom_line()+facet_wrap(~deply) +
    xlab("Size of wetland (ha)")+ ylab("Expected cost/expected ducks")
  
#' ## Cluster-level bootstrap to get estimates of uncertainty
#'   
#' Now that we have shown how to get an estimate of everything with the original data, to 
#' determine uncertainty, we just need to:
#' 
#' 1. Resample structures with replacement.,
#' 2. Refit our 2 models.
#' 3. Recalculate our statistic of interest (ducks/expected cost)

#' We will select structures with replacement
  uid <- unique(ddata2$strtno) # unique id for each strtucture
  nstrtno <- length(uid) # number of structures
  
#' Set up matrices to hold bootstrap results  
  nboot<-5000
  pfail.b<-matrix(NA, nboot, nrow(ducks))
  costs.b<-matrix(NA, nboot, nrow(ducks))
  ducks.b<-matrix(NA, nboot, nrow(ducks)) 
  pfail.temp<-pdat2
  
#' Code for the bootstrap is given below. This takes some time to run (and could be sped up by 
#' avoiding loops...).
  for(i in 1:nboot){
    
    # Take a sample from x (uid) of size nstrtno with replacement:
    bootIDs <- data.frame(strtno = sample(x = uid, size = nstrtno, replace = TRUE))
    
    # Use this to sample from original data by beach
    bootDat <- merge(bootIDs,ddata2)
    
    # Now, fit survival model
    glmsurv.b<-glm(surv~year+deply +sz1+sz2+sz3, family=binomial(link=cloglog), data=bootDat)
    
    # Now, predictions on prob of failure (accounting for the
    # fact that our model is fit on the cloglog scale)
    pfail.temp$pfail<-predict(glmsurv.b, newdata=pdat2, type="resp")
    
    # Get predictions averaged across years
    pfail.temp2<- pfail.temp %>% group_by(size, deply) %>%
      summarize(meanfail=mean(pfail))  
    pfail.b[i,]<-pfail.temp2$meanfail
    
    # Costs were estimated as fixed ($25/cylinder + variable depending on survival and cylinder type)
    costs.b[i,]<-25+vcost*pfail.b[i,] 
    
    # ## Duckling production model
    
    # Since the predictors do not change across years for this model, Zicus et al. just
    # modeled the mean number of ducks as the response.  Determine the mean   
    tdata.boot<-merge(bootIDs,tdata)
   
     # We can now model how the number of ducks depends on size
     # of the wetland and cynlinder type
    dmod.b<-lm(yng~size+I(size^2)+deply, data=tdata.boot)
    ducks.b[i,]<-predict(dmod.b, newdata=ducks)
    
} 
  
    
#' Now, calculate pointwise 90% CI and plot.
  ducks$upducks<-apply(ducks.b,2, quantile, probs=0.95) 
  ducks$lowducks<-apply(ducks.b,2, quantile, probs=0.05)
  pfail$upcost<-apply(costs.b, 2,quantile, probs=0.95)
  pfail$lowcost<-apply(costs.b, 2,quantile, probs=0.05)
  costef$upce<-apply(costs.b/ducks.b, 2, quantile, probs=0.95)
  costef$lowce<-apply(costs.b/ducks.b, 2, quantile, probs=0.05)
  costef$meanbootce<-apply(costs.b/ducks.b, 2, mean)
  
#+ fig.width=8, fig.height=4    
  ggplot(ducks, aes(x=size, y=phat)) + 
    geom_ribbon(aes(ymin=lowducks, ymax=upducks), fill="grey70") + 
    geom_line() + geom_point(data=tdata, aes(size, y=yng))+ facet_wrap(~deply) +
    xlab("Size of wetland (ha)")+ ylab("Expected number of ducks")

#+ fig.width=8, fig.height=4       
  ggplot(pfail, aes(x=size, y=ec))+geom_ribbon(aes(ymin=lowcost, ymax=upcost), fill="grey70") + 
    facet_wrap(~deply) + geom_line() + 
    xlab("Size of wetland (ha)")+ ylab("Expected cost")
  
  
#+ fig.width=8, fig.height=4    
  ggplot(costef, aes(x=size, y=ce))+geom_ribbon(aes(ymin=lowce, ymax=upce), fill="grey70") +
    facet_wrap(~deply) +geom_line()+
    xlab("Size of wetland (ha)")+ ylab("Expected cost/expected ducks")

#' ## Use the bootstrap to check for bias
#'
#' We can compare the mean of the bootstrap distribution to the point
#' estimate as a measure of bias. 
costef$boot.bias<-apply(costs.b/ducks.b, 2, mean) - costef$ce

#' Comparing the estimated bias to the estimated SE, we see that the 
#' relative bias is < 0.25, which Efron and Tibshirani (1993) suggest
#' as a general rule of thumb for when it is not worth worrying about.
costef$boot.se<-apply(costs.b/ducks.b, 2, sd) 
summary(costef$boot.bias/costef$boot.se)

#' ## Biological Conclusions
#' 
#' We conclude that cost-effectiveness of nest structures is highest
#' in large wetlands (cost/duck is minimized). Cost-effectiveness also
#' appears to be slightly higher for single-cylinder structures though
#' confidence intervals for single- and double-cylinder structures largely
#' overlap. Single-cylinder structures were less likely to fall over,
#' so were less costly to maintain.
#'
 
#' ## Document footer
#' 
#' Session Information:
#+ sessionInfo
  sessionInfo()  