#scoring caret models
source(file="./Code/3CaretConfigure.R")
##loading models
load(file="./caretModels/modelsLda.RData")
load(file="./caretModels/modelsKnn.RData")
load(file="./caretModels/modelsNb.RData")
load(file="./caretModels/modelsFda.RData")
load(file="./caretModels/modelsC50.RData")
load(file="./caretModels/modelsxgboost.RData")
load(file="./caretModels/modelsSvmRadial.RData")


##predicting conversion probabilities
dbtest$probLda<-predict(lda.model.unbinned.downsample,newdata = 
                          dbtest[,c(predictors.continuous,predictors.dummy),
                                 with=FALSE],type = "prob")["Y"][,1]*lda.unbinned.correction
dbtest$probFda<-predict(model.fda.down, newdata=dbtest,type="prob")["Y"][,1]*model.fda.down.correction
dbtest$probC50<-predict(C5.0.model, newdata=dbtest,type="prob")["Y"][,1]*c50.correction
dbtest$probNb<-predict(model.nb.down, newdata=dbtest,type="prob")["Y"][,1]*model.nb.correction
dbtest$probKnn<-predict(model.knn.down, newdata=dbtest[,predictors.continuous,with=FALSE],type="prob")["Y"][,1]*model.knn.correction
dbtest$probXGBoost<-predict(model.xgboost, newdata=sparse_test)
dbtest$probSvmRadial<-predict(model.svm, newdata=dbtest[,preescreended.svm.candidates,with=FALSE],type="prob")["Y"][,1]*model.svm.correction

#scoring h2o models
source(file="./Code/3H2OConfigure.R")
##loading models
base_glm<-h2o.loadModel(path = paste(workDir,"/H2OModels/GLM/","base_glm",sep=''))
final_glm<-h2o.loadModel(path = paste(workDir,"/H2OModels/GLM/","final_glm",sep=''))
final_gbm<-h2o.loadModel(path = paste(workDir,"/H2OModels/GBM/","final_gbm",sep=''))
final_rf<-h2o.loadModel(path = paste(workDir,"/H2OModels/DRF/","final_rf",sep=''))
final_dl<-h2o.loadModel(path = "./H2OModels/DEEPLEARNING/final_dl")
#final_ensemble_gbm<-h2o.load_ensemble( path = "./H2OModels/ENSEMBLES/ensemble_gbm/")
#final_ensemble_glm<-h2o.load_ensemble( path = "./H2OModels/ENSEMBLES/ensemble_glm/")
##scoring models
h2o.preds.glm<-predict(object = base_glm, newdata=dbtesth2o)$Y; names(h2o.preds.glm)="probGlmBase"
h2o.preds.glmfin<-predict(object = final_glm, newdata=dbtesth2o)$Y; names(h2o.preds.glmfin)="probGlmFinal"
h2o.preds.rf<-predict(object = final_rf, newdata=dbtesth2o)$Y; names(h2o.preds.rf)="probRandomForest"
h2o.preds.dl<-predict(object = final_dl, newdata=dbtesth2o)$Y; names(h2o.preds.dl)="probDeepLearning"
h2o.preds.gbm<-predict(object = final_gbm, newdata=dbtesth2o)$Y; names(h2o.preds.gbm)="probGbm"

# h2o.preds.ensemble_gbm<-predict.h2o.ensemble(object = final_ensemble_gbm, newdata=dbtesth2o); 
# h2o.preds.ensemble_gbm<-h2o.preds.ensemble_gbm$pred$Y; names(h2o.preds.ensemble_gbm)="probEnsembleGbm"
# 
# h2o.preds.ensemble_glm<-predict.h2o.ensemble(object = final_ensemble_glm, newdata=dbtesth2o); 
# h2o.preds.ensemble_glm<-h2o.preds.ensemble_glm$pred$Y; names(h2o.preds.ensemble_glm)="probEnsembleGlm"

h2o.preds<-as.data.frame(h2o.cbind(h2o.preds.glm,h2o.preds.glmfin,h2o.preds.rf,
                                   h2o.preds.dl,h2o.preds.gbm
#                                   ,h2o.preds.ensemble_gbm,
#                                   h2o.preds.ensemble_glm
                                   ))

h2o.rm(h2o.preds.rf)
h2o.rm(h2o.preds.glmfin)
h2o.rm(h2o.preds.glm)
h2o.rm(h2o.preds.gbm)
h2o.rm(h2o.preds.dl)
#h2o.rm(h2o.preds.ensemble_glm)
#h2o.rm(h2o.preds.ensemble_gbm)
dbtest<-cbind(dbtest, h2o.preds)

#assessing predictive performance 
dbtest$convertedBin=0
dbtest[converted=='Y',convertedBin:=1]

probCols <- grep(pattern="prob",x=names(dbtest),value = TRUE)
library(stringr)
library(dtplyr)
modelNames<-str_replace_all(string=probCols,pattern="prob",replacement = "")
##calculating logloss auc and total predicted converted quotes
dbLogLoss<-dbtest[,(lapply(.SD,LogLoss,actual=dbtest$convertedBin)),.SDcols=probCols][, t(.SD)]
dbAuc<-dbtest[,(lapply(.SD,Auc ,actual=dbtest$convertedBin)),.SDcols=probCols][, t(.SD)]
dbSums<-dbtest[,(lapply(.SD,sum)),.SDcols=probCols][, t(.SD)]
#dbKappa<-dbtest[,(lapply(.SD,ModelMetrics::kappa,actual=dbtest$convertedBin,cutoff=0.5)),.SDcols=probCols][, t(.SD)]

##binding all together
temp<-cbind(dbSums,dbAuc,dbLogLoss
#            ,dbKappa
            ) %>% as.data.frame()
names(temp)<-c("quotes","auc","logloss"
               #,"kappa"
               )
temp$models<-modelNames
comparison.df<-dplyr::select(temp, models, quotes,logloss,  auc,kappa ) %>% dplyr::arrange(logloss)
comparison.df<-rbind(data.frame(models="actual",quotes=sum(dbtest$convertedBin), logloss=NA, auc=NA, kappa=NA),comparison.df)
##creating lift and calibration charts
calibration.plot<-calibration(converted~probGlmFinal+probGbm+probXGBoost,data = dbtest,class = "Y",cuts=20)
lift.plot<-lift(converted~probGlmFinal+probGbm+probXGBoost,data = dbtest,class = "Y",cuts = 100)

#renaming the scoring df

dbscored<-dbtest[,c(probCols,"convertedBin"),with=FALSE]

save(list=c("comparison.df","calibration.plot","lift.plot","dbscored"),file="./Data/finalsummary.RData")
