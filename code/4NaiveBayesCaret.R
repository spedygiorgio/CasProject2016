#loading naive bayes
source(file="./Code/3CaretConfigure.R")

#define predictors
nb.preds<-c(predictors.continuous,predictors.categorical)
#define grid
nbGrid = expand.grid(fL=seq(from=0,to=10, by=1),
                     usekernel=c(TRUE,FALSE),
                     adjust=c(0.5,1,2))


#running the base model

cl<-makeCluster(cores)
registerDoParallel(cl)
model.nb.base<-train(x=dbtrainsmall[,nb.preds, with=FALSE],
                    y=dbtrainsmall$converted,
                 method="nb",metric="ROC",
                   tuneGrid = nbGrid,
                    trControl = caretTrainCtrl)
stopCluster(cl)

#downsampling

cl<-makeCluster(cores)
registerDoParallel(cl)
model.nb.down<-train(x=dbtrainsmall[,nb.preds, with=FALSE],
                     y=dbtrainsmall$converted,
                     method="nb",metric="ROC",
                     tuneGrid = nbGrid,
                     trControl = caretTrainCtrlRebalancing)
stopCluster(cl)


#smote
# caretTrainCtrl2<-caretTrainCtrl
# caretTrainCtrl2$sampling<-"smote"
# 
# 
# cl<-makeCluster(cores)
# registerDoParallel(cl)
# model.nb.smote<-train(x=dbtrain[,nb.preds, with=FALSE],
#                      y=dbtrain$converted,
#                      method="nb",metric="Kappa",
#                      tuneGrid = nbGrid,
#                      trControl = caretTrainCtrl2)
# stopCluster(cl)



#check perf

getTrainPerf(model.nb.down) #best
#getTrainPerf(model.nb.smote)
getTrainPerf(model.nb.base) 


model.nb.correction<-(sum(dbtrain$converted=='Y'))/sum(predict(model.nb.down,
                                                               newdata = dbtrain[,nb.preds,with=FALSE],
                                                               type = "prob")["Y"][,1])



save(list=c("model.nb.correction","model.nb.down","model.nb.base"),
     file="./caretModels/modelsNb.RData",compress="xz")

#TRYING RECURSIVE FEATURE ELIMINATION
#  rfe.ctrl<- rfeControl(functions = nbFuncs,
#                               method = "cv",
#                               number=2,
#                               verbose = FALSE)
# # 
#  subsets <- c(1:5, 10, 15, length(nb.preds))
#  cl<-makeCluster(cores)
#  registerDoParallel(cl)
#  nb.rfe<-rfe(x = dbsmall[,nb.preds, with=FALSE],
#              y=dbsmall$converted,
#              sizes=subsets,
#              metric = "Kappa",
#              rfeControl = rfe.ctrl)
#  stopCluster(cl)


#load(file="./caretModels/retentionNb.RData")
