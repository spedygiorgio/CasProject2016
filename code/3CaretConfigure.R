##################################
#defining the list of variables
#to be easily called when defining
#models' inputs
##################################

#categorical predictors
predictors.categorical<-c("bonusMalus","policyLimit","vehicleFuelType",
         "territoryBigCity","policyholderGender","policyholderOccupation","policyholderMaritalStatus",
         "previousCompany","policyholderTerritory","vehicleKm","vehicleUsage",
         "vehicleMakeAndModel"
)

#continuous ones
predictors.continuous<-c("vehiclePower","vehicleAge","vehiclePurchaseAge","policyholderAge","quoteTimeToPolicy",
                         "ratioCompanyMkt","deltaCompanyMkt")

#binned version of continuous ones
predictors.binned<-c("vehiclePowerBin", "vehicleAgeBin", "policyholderAgeBin",
                     "quoteTimeToPolicyBin", "ratioCompanyMktBin", "deltaCompanyMktBin")

#the dummy version of the above ones
predictors.binned.dummified<-grep(pattern="Bin\\.",x=colnames(dbtrain),value=TRUE)
#all dummies
predictors.dummy<-grep(pattern="\\.",x=colnames(dbtrain),value=TRUE)

###################################
### caret package configuration
###################################

require(caret)
#caret general configuration
caretTrainCtrl <- trainControl(method = "cv", #use of cross validation for resampling
                               number=5, #five folds cross validation
                               summaryFunction = twoClassSummary, #use AUC for performance assessment
                               classProbs = TRUE, #this option is needed to compute raw class probability estimates
                               returnData =FALSE)  #do not save data (reduce object size)

#caret configuration for models with rebalancing
caretTrainCtrlRebalancing<-caretTrainCtrl
caretTrainCtrlRebalancing$sampling='down' #standard rebalancing approach: downsampling
caretTrainCtrlRebalancing$summaryFunction<-defaultSummary #this is needed to return Kappa statistis


###################################
#### parallel backend configuration
###################################

#configure foreach and doParalle parallel backends
library(foreach) 
library(doParallel)
cores<-max(1,detectCores()-1) #use all cores - 1


################################
#PERFORMANCE METRIC DEFINITIONS
################################

#LOG LOSS DEFINITION
LogLoss <- function(actual, predicted, eps=0.00001) {
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}

#AUC DEFINITION
Auc <- function (actual, predicted) 
{
  r <- rank(predicted)
  n_pos <- as.numeric(sum(actual == 1));
  n_neg <- as.numeric(length(actual) - n_pos)
  auc <- (sum(r[actual == 1]) - n_pos * (n_pos + 1)/2)/(n_pos * n_neg);
  auc
}

#GINI DEFINITION
Gini <- function(auc) {
  out<-2*auc-1
  return(out)
}

#KAPPA ?

.kappaInt<-function(x,cutoff=0.5) {
  if (x<cutoff) return(0) else return(1)
}

Kappa <- function(actual, predicted, cutoff=0.5) {
  E = 1-mean(predicted) #expected accuracy
  my_classification<-sapply(predicted,  .kappaInt, cutoff=0.5)
  confDf<-data.frame(act=actual, pred=my_classification)
  confMatr<-with(confDf, table(pred, act)) %>% as.matrix()
  if (nrow(confMatr)==1) return(0)
  O=sum(diag(confMatr))/sum(confMatr)
  K <- (O-E)/(1-E)
  return(K)
}
