#load the caret's helper corde
source(file="./Code/3CaretConfigure.R")
#lda predictors
ldaPreds<-c(predictors.binned.dummified,predictors.dummy)

#variable importance analysis for initial predictors' strength assessment

library(CORElearn)

reliefValues <- attrEval(converted ~ ., data = dbtrainsmall[,c(predictors.continuous,
                                                               predictors.categorical,
                                                               'converted'),with=FALSE],
                          estimator = "ReliefFequalK",ReliefIterations = 50)
reliefValues.df<-data.frame(variable=names(reliefValues),relief=reliefValues) %>% arrange(desc(relief))
important.variables<-as.character(reliefValues.df$variable[reliefValues.df$relief>.1])

selected.lda.vars<-character()

for (i in important.variables) {
  selected.binned.vars<-grep(pattern = i,x = ldaPreds,value=TRUE)
  selected.lda.vars <- c(selected.lda.vars,selected.binned.vars)
}

#MODEL TRAINING
##fitting standard models
cl<-makeCluster(cores)
registerDoParallel(cl)
##on binned data
lda.model.bin<-train(x=dbtrain[,ldaPreds,with=FALSE],
                 y=dbtrain$converted,
                 preProc = c("center", "scale"),
                 method="lda",
                 trControl=caretTrainCtrl,
                 metric = "ROC")
##on unbinned data
lda.model.unbinned<-train(x=dbtrain[,c(predictors.continuous,predictors.dummy),with=FALSE],
                     y=dbtrain$converted,
                     preProc = c("center", "scale"),
                     method="lda",
                     trControl=caretTrainCtrl,
                     metric = "ROC")
##on selected vars
lda.model.selected<-train(x=dbtrain[,selected.lda.vars,with=FALSE],
                          y=dbtrain$converted,
                          preProc = c("center", "scale"),
                          method="lda",
                          trControl=caretTrainCtrl,
                          metric = "ROC")

stopCluster(cl)

#get performance measures
getTrainPerf(lda.model.bin)
getTrainPerf(lda.model.unbinned) #best
getTrainPerf(lda.model.selected)

##CHECKING PERFORMANCE WITH DOWN / UPSAMPLING
##
cl<-makeCluster(cores)
registerDoParallel(cl)
#downsample

lda.model.unbinned.downsample<-train(x=dbtrain[,c(predictors.continuous,predictors.dummy),with=FALSE],
                          y=dbtrain$converted,
                          preProc = c("center", "scale"),
                          method="lda",
                          trControl=caretTrainCtrlRebalancing,
                          metric = "ROC")
#upsample (not run - too slow)
# caretTrainCtrl2$sampling<-"up"
# lda.model.unbinned.upsample<-train(x=dbtrain[,c(predictors.continuous,predictors.dummy),with=FALSE],
#                                      y=dbtrain$converted,
#                                      preProc = c("center", "scale"),
#                                      method="lda",
#                                      trControl=caretTrainCtrl2,
#                                      metric = "ROC")

#smote (not run - too slow)
caretTrainCtrl2<-caretTrainCtrlRebalancing
 caretTrainCtrl2$sampling<-"smote"
 lda.model.unbinned.smote<-train(x=dbtrain[,c(predictors.continuous,predictors.dummy),with=FALSE],
                                    y=dbtrain$converted,
                                    preProc = c("center", "scale"),
                                    method="lda",
                                    trControl=caretTrainCtrl2,
                                    metric = "ROC")

stopCluster(cl)

getTrainPerf(lda.model.unbinned.downsample) #best
getTrainPerf(lda.model.unbinned)
#getTrainPerf(lda.model.unbinned.smote)
# 
# getting scaling factor
preds<-predict(lda.model.unbinned.downsample,
                          newdata = dbtrain[,c(predictors.continuous,predictors.dummy),with=FALSE],
                          type = "prob")["Y"][,1]

lda.unbinned.correction<-(sum(dbtrain$converted=='Y'))/sum(predict(lda.model.unbinned.downsample,newdata = 
                                                                     dbtrain[,c(predictors.continuous,predictors.dummy),
                                                                             with=FALSE],type = "prob")["Y"][,1])

save(list=c("lda.model.unbinned","lda.model.unbinned.downsample","lda.model.unbinned.smote",
            "lda.unbinned.correction"),file="./caretModels/modelsLda.RData")
