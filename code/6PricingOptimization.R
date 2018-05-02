#loading the models
## h2o
final_glm<-h2o.loadModel(path = "./H2OModels/GLM/final_glm") # glm: elasticnet (lasso + ridge and non binning of continuous predicots)
base_glm<-h2o.loadModel(path = "./H2OModels/GLM/base_glm") # glm: elasticnet (lasso + ridge and non binning of continuous predicots)
final_gbm<-h2o.loadModel(path = "./H2OModels/GBM/final_gbm") #h2o gbm

## xgboost
require(xgboost)
require(Matrix)
#load(file="./caretModels/modelsxgboost.RData")
load(file="./caretModels/xgboostG.RData")

## helper function to get xgboost matrices

predictorsMatrix2xgbmatrix<-function(df){
  # Sparse Matrix creation (one hot-encoded categorical variables)
  previous_na_action <- options('na.action')
  options(na.action='na.pass')
  #convert in sparse matrix
  df.sparse=sparse.model.matrix(~.-1, data = df)
  #convert in dmatrix 4 xgboost
  cols2keep<-setdiff(colnames(df.sparse) ,"convertedY")
  Target="convertedY"
  df.DMatrix <- xgb.DMatrix(data =df.sparse[,cols2keep] , label = df.sparse[,Target])
  options(na.action=previous_na_action$na.action)
  return(df.DMatrix)
}

dbcalibration.xgb<-predictorsMatrix2xgbmatrix(df=dbcalibration[,c(predictors.categorical,predictors.continuous,"converted"),with=FALSE])

#baseline predictions
## probabilities
preds.baseglm<-as.data.frame(predict(object = base_glm, dbcalibrationh2o)$Y)[,1]
preds.glm<-as.data.frame(predict(object = final_glm, dbcalibrationh2o)$Y)[,1]
preds.gbm<-as.data.frame(predict(object = final_gbm, dbcalibrationh2o)$Y)[,1]
preds.xgb<-predict(object = model.xgboost, dbcalibration.xgb)
## numbers
conv.true=sum(dbcalibration$converted=="Y")
conv.baseglm<-sum(preds.baseglm)
conv.glm<-sum(preds.glm)
conv.gbm<-sum(preds.gbm)
conv.xgb<-sum(preds.xgb)
## premiums (volume)
prem.true<-dbcalibration[converted=="Y",sum(premiumCompany)]
prem.baseglm<-sum(dbcalibration$premiumCompany*preds.baseglm); prem.baseglm/prem.true-1
prem.glm<-sum(dbcalibration$premiumCompany*preds.glm); prem.glm/prem.true-1
prem.gbm<-sum(dbcalibration$premiumCompany*preds.gbm); prem.gbm/prem.true-1
prem.xgb<-sum(dbcalibration$premiumCompany*preds.xgb); prem.xgb/prem.true-1
## margin (volume)
mgn.true<-dbcalibration[converted=="Y",sum(premiumCompany-burningCost)]
mgn.baseglm<-sum(preds.baseglm*(dbcalibration$premiumCompany-dbcalibration$burningCost))
mgn.glm<-sum(preds.glm*(dbcalibration$premiumCompany-dbcalibration$burningCost))
mgn.gbm<-sum(preds.gbm*(dbcalibration$premiumCompany-dbcalibration$burningCost))
mgn.xgb<-sum(preds.xgb*(dbcalibration$premiumCompany-dbcalibration$burningCost))

models.list<-c("actual","glm base","glm elasticnet","gbm","xgb")
conversions.list<-c(conv.true,conv.baseglm,conv.glm,conv.gbm,conv.xgb)
margins.list<-c(mgn.true,mgn.baseglm,mgn.glm,mgn.gbm,mgn.xgb)


# general hyphotheses
## defining the baseline delta range
premiumDeltaRange<-seq(from=0.90,to=1.10,by=.01)

#defining the premium (company and market) matrices 
##all possible premium proposes by the company
matrpremiumCompany<-as.matrix(dbcalibration[,"premiumCompany",with=FALSE])%*%premiumDeltaRange 
##all possible premium proposes by the company (constant)
matrpremiumMarket<-matrix(NA, nrow=nrow(matrpremiumCompany),ncol=length(premiumDeltaRange))
for( i in 1:length(premiumDeltaRange)) matrpremiumMarket[,i]<-as.matrix(dbcalibration[,"premiumMarket",with=FALSE])
#recompute the competitive position
matrratioCompanyMkt<-matrpremiumCompany/matrpremiumMarket
matrdeltaCompanyMkt<-matrpremiumCompany-matrpremiumMarket

## allocate a matrix to compute conversion probabilities
matrProb<-matrix(NA, nrow=nrow(matrpremiumCompany),ncol=length(premiumDeltaRange))
## set the burning cost for all risks
matrburningCost<-matrix(NA, nrow=nrow(matrpremiumCompany),ncol=length(premiumDeltaRange))
for (i in seq_along(premiumDeltaRange)) {
  matrburningCost[,i]<-dbcalibration$burningCost
}




#first model: glm

for(i in 1:length(premiumDeltaRange)) {
  cat("Simulating :",premiumDeltaRange[i]," variation ","\n")
  trialData<-dbcalibration
  
  trialData[,"premiumCompany"]<-matrpremiumCompany[,i]
  trialData[,"premiumMarket"]<-matrpremiumMarket[,i]
  trialData[,"ratioCompanyMkt"]<-matrratioCompanyMkt[,i]
  trialData[,"deltaCompanyMkt"]<-matrdeltaCompanyMkt[,i]
  
  dbtrialh2o <-  as.h2o(trialData[,c(predictors.categorical,predictors.continuous,predictors.binned,
                                     "converted","premiumCompany","premiumMarket","burningCost"),with=FALSE], destination_frame="dbtrialh2o")
  
  conversion.pred = h2o.predict(object = final_glm, newdata = dbtrialh2o)
  prediction<-as.data.frame(conversion.pred)
  matrProb[,i]<-prediction$Y
  
  h2o.rm(dbtrialh2o)
  h2o.rm(conversion.pred)
}

#calculating the expected margin at baseline

matrExpectedMargin<-(matrpremiumCompany-matrburningCost)*matrProb
baselineMargin<-sum(matrExpectedMargin[,which(premiumDeltaRange==1)]) #this is the baseline

#calculating the optimized variation

bestPremium<-numeric(nrow(matrExpectedMargin))
bestVar<-numeric(nrow(matrExpectedMargin))
bestExpecteMargin<-numeric(nrow(matrExpectedMargin))

for(i in 1:nrow(matrExpectedMargin)) {
  ithChoice<-which.max(matrExpectedMargin[i,])
  bestVar[i] <-  premiumDeltaRange[ithChoice]
  bestPremium[i] <- matrpremiumCompany[i,ithChoice]
  bestExpecteMargin[i] <- matrExpectedMargin[i,ithChoice]
}


glm.bestExpectedMargin<-sum(bestExpecteMargin)
glm.bestVar<-mean(bestVar)


#zero model: base glm

for(i in 1:length(premiumDeltaRange)) {
  cat("Simulating :",premiumDeltaRange[i]," variation ","\n")
  trialData<-dbcalibration
  
  trialData[,"premiumCompany"]<-matrpremiumCompany[,i]
  trialData[,"premiumMarket"]<-matrpremiumMarket[,i]
  trialData[,"ratioCompanyMkt"]<-matrratioCompanyMkt[,i]
  trialData[,"deltaCompanyMkt"]<-matrdeltaCompanyMkt[,i]
  
  dbtrialh2o <-  as.h2o(trialData[,c(predictors.categorical,predictors.continuous,predictors.binned,
                                     "converted","premiumCompany","premiumMarket","burningCost"),with=FALSE], destination_frame="dbtrialh2o")
  
  conversion.pred = h2o.predict(object = base_glm, newdata = dbtrialh2o)
  prediction<-as.data.frame(conversion.pred)
  matrProb[,i]<-prediction$Y
  
  h2o.rm(dbtrialh2o)
  h2o.rm(conversion.pred)
}

#calculating the expected margin at baseline

matrExpectedMargin<-(matrpremiumCompany-matrburningCost)*matrProb
baselineMargin<-sum(matrExpectedMargin[,which(premiumDeltaRange==1)]) #this is the baseline

#calculating the optimized variation

bestPremium<-numeric(nrow(matrExpectedMargin))
bestVar<-numeric(nrow(matrExpectedMargin))
bestExpecteMargin<-numeric(nrow(matrExpectedMargin))

for(i in 1:nrow(matrExpectedMargin)) {
  ithChoice<-which.max(matrExpectedMargin[i,])
  bestVar[i] <-  premiumDeltaRange[ithChoice]
  bestPremium[i] <- matrpremiumCompany[i,ithChoice]
  bestExpecteMargin[i] <- matrExpectedMargin[i,ithChoice]
}


baseglm.bestExpectedMargin<-sum(bestExpecteMargin)
baseglm.bestVar<-mean(bestVar)



#second model: gbm

for(i in 1:length(premiumDeltaRange)) {
  cat("Simulating :",premiumDeltaRange[i]," variation ","\n")
  trialData<-dbcalibration
  
  trialData[,"premiumCompany"]<-matrpremiumCompany[,i]
  trialData[,"premiumMarket"]<-matrpremiumMarket[,i]
  trialData[,"ratioCompanyMkt"]<-matrratioCompanyMkt[,i]
  trialData[,"deltaCompanyMkt"]<-matrdeltaCompanyMkt[,i]
  
  dbtrialh2o <-  as.h2o(trialData[,c(predictors.categorical,predictors.continuous,predictors.binned,
                                     "converted","premiumCompany","premiumMarket","burningCost"),with=FALSE], destination_frame="dbtrialh2o")
  
  conversion.pred = h2o.predict(object = final_gbm, newdata = dbtrialh2o)
  prediction<-as.data.frame(conversion.pred)
  matrProb[,i]<-prediction$Y
  
  h2o.rm(dbtrialh2o)
  h2o.rm(conversion.pred)
}

#calculating the expected margin at baseline

matrExpectedMargin<-(matrpremiumCompany-matrburningCost)*matrProb
baselineMargin<-sum(matrExpectedMargin[,which(premiumDeltaRange==1)]) #this is the baseline

#calculating the optimized variation

bestPremium<-numeric(nrow(matrExpectedMargin))
bestVar<-numeric(nrow(matrExpectedMargin))
bestExpecteMargin<-numeric(nrow(matrExpectedMargin))

for(i in 1:nrow(matrExpectedMargin)) {
  ithChoice<-which.max(matrExpectedMargin[i,])
  bestVar[i] <-  premiumDeltaRange[ithChoice]
  bestPremium[i] <- matrpremiumCompany[i,ithChoice]
  bestExpecteMargin[i] <- matrExpectedMargin[i,ithChoice]
}


gbm.bestExpectedMargin<-sum(bestExpecteMargin)
gbm.bestVar<-mean(bestVar)


#xgboost

## simulating over the premium variation ranges

for(i in 1:length(premiumDeltaRange)) {
  cat("Simulating :",premiumDeltaRange[i]," variation ","\n")
  #generating the premium scenario predictor matrix
  
  trialData<-dbcalibration[,c(predictors.categorical,predictors.continuous,"converted"),with=FALSE]
  #changing premiums & market positions
  trialData$premiumCompany=matrpremiumCompany[,i]
  trialData$premiumMarket=matrpremiumMarket[,i]
  trialData$ratioCompanyMkt=matrratioCompanyMkt[,i]
  trialData$deltaCompanyMkt=matrdeltaCompanyMkt[,i]
  #simulating new scenarios
  
  trials.xgb<-predictorsMatrix2xgbmatrix(df=trialData)
  
  probXGBoost<-predict(model.xgboost, newdata=trials.xgb)
  
  matrProb[,i]<-probXGBoost
  

}

##recomputing premium and margins

matrExpectedMargin<-(matrpremiumCompany-matrburningCost)*matrProb
baselineMargin<-sum(matrExpectedMargin[,which(premiumDeltaRange==1)]) #this is the baseline

##calculating the optimized variation

bestPremium<-numeric(nrow(matrExpectedMargin))
bestVar<-numeric(nrow(matrExpectedMargin))
bestExpecteMargin<-numeric(nrow(matrExpectedMargin))

for(i in 1:nrow(matrExpectedMargin)) {
  ithChoice<-which.max(matrExpectedMargin[i,])
  bestVar[i] <-  premiumDeltaRange[ithChoice]
  bestPremium[i] <- matrpremiumCompany[i,ithChoice]
  bestExpecteMargin[i] <- matrExpectedMargin[i,ithChoice]
}


xgb.bestExpectedMargin<-sum(bestExpecteMargin)
xgb.bestVar<-mean(bestVar)



#saving all results
optimized_margins<-c(NA,baseglm.bestExpectedMargin,glm.bestExpectedMargin,gbm.bestExpectedMargin,xgb.bestExpectedMargin )
pricing.optimization.results<-data.frame(models=models.list, 
                                 conversions=conversions.list, 
                                 baseline_uw_margin=margins.list,
                                 optimized_uw_margin=optimized_margins)


save(list = c("pricing.optimization.results"),file="./Data/finalOptimization.RData")
