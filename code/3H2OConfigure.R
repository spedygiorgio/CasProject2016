#INITIALIZING H2O
library(h2o)
#library(h2oEnsemble)

localH2O <- h2o.init(nthreads = 6, max_mem_size="8g") 

#loading the train set
dbtrainh2o <-  as.h2o(dbtrain[,c(predictors.categorical,predictors.continuous,predictors.binned,
                                 "converted"),with=FALSE], 
                      destination_frame="dbtrainh2o")
#loading the test set
dbtesth2o <-  as.h2o(dbtest[,c(predictors.categorical, predictors.binned,
                               predictors.continuous,"converted"),with=FALSE], 
                     destination_frame="dbtesth2o")
#loading the validation set
dbcalibrationh2o<-as.h2o(dbcalibration[,c(predictors.categorical,
                                   predictors.continuous,
                                   predictors.binned,"converted","premiumCompany",
                                   "premiumMarket","burningCost"),with=FALSE], 
                         destination_frame="dbcalibrationh2o")

#save also a small data set for fast training
dbtrainsmallh2o<- as.h2o(dbtrainsmall[,c(predictors.categorical,predictors.continuous,
                                         predictors.binned,"converted"),with=FALSE], 
                         destination_frame="dbtrainsmallh2o")

#define the number of cross - validation samples
nfolds_par=10