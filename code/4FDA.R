#flexible discriminant analysis
source(file="./Code/3CaretConfigure.R")

#define predictors
fda.preds<-c(predictors.continuous,predictors.categorical)
#define grid
fdaGrid = expand.grid(degree = 1:2, nprune = 2:length(fda.preds))

#create a small set
dbtrain_small<-sample_n(dbtrain,size=200e3)

#fitting the baseline model
cl<-makeCluster(cores)
registerDoParallel(cl)
model.fda.time<-Sys.time()
model.fda.base<-train(x=dbtrain_small[,fda.preds, with=FALSE],
                    y=dbtrain_small$converted,
                 method="fda",metric="ROC",
                   tuneGrid = fdaGrid,
                    trControl = caretTrainCtrl)
model.fda.time<-Sys.time()-model.fda.time
stopCluster(cl)


#fitting the downsampled model

cl<-makeCluster(cores)
registerDoParallel(cl)
model.fda.time.down<-Sys.time()
model.fda.down<-train(x=dbtrain_small[,fda.preds, with=FALSE],
                 y=dbtrain_small$converted,
                 method="fda",metric="Kappa",
                 tuneGrid = fdaGrid,
                 trControl = caretTrainCtrlRebalancing)
model.fda.time.down<-Sys.time()-model.fda.time.down
stopCluster(cl)

#upsample
# caretTrainCtrl2<-caretTrainCtrl
# caretTrainCtrl2$sampling<-"up"
# 
# cl<-makeCluster(cores)
# registerDoParallel(cl)
# model.fda.time.up<-Sys.time()
# model.fda.up<-train(x=dbtrain_small[,fda.preds, with=FALSE],
#                       y=dbtrain_small$converted,
#                       method="fda",metric="Kappa",
#                       tuneGrid = fdaGrid,
#                       trControl = caretTrainCtrl2)
# model.fda.time.up<-Sys.time()-model.fda.time.up
# stopCluster(cl)
# 
# 
# 
# #smote
# caretTrainCtrl2<-caretTrainCtrl
# caretTrainCtrl2$sampling<-"smote"
# 
# cl<-makeCluster(cores)
# registerDoParallel(cl)
# model.fda.time.smothe<-Sys.time()
# model.fda.smote<-train(x=dbtrain_small[,fda.preds, with=FALSE],
#                       y=dbtrain_small$converted,
#                       method="fda",metric="Kappa",
#                       tuneGrid = fdaGrid,
#                       trControl = caretTrainCtrl2)
# model.fda.time.smothe<-Sys.time()-model.fda.time.smothe
# stopCluster(cl)


getTrainPerf(model.fda.base)
getTrainPerf(model.fda.down) #best model
#getTrainPerf(model.fda.smote)
#calculate correction
model.fda.down.correction<-sum(dbtrain$converted=='Y')/sum(predict(model.fda.down, 
                                                                   newdata=dbtrain,type="prob")["Y"][,1])


save(list=grep(pattern = "model\\.fda",x=ls(),value = TRUE),file="./caretModels/modelsFda.RData")
#load(file="./caretModels/modelsFda.RData")
