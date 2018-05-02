
#load caret package configuration
source(file="./Code/3CaretConfigure.R")
#define knn grid: min 4 to 26
knnGrid <- expand.grid(k=seq(from=4,to=26, by=2))
#train base knn model
cl<-makeCluster(cores)
registerDoParallel(cl)
model.knn.base<-train(x=dbtrainsmall[,c(predictors.continuous),with=FALSE], 
                 y=dbtrainsmall$converted,
                 preProc = c("center", "scale"),
                    method="knn",metric="ROC",
                 trControl=caretTrainCtrl,
                    tuneGrid = knnGrid)
stopCluster(cl)



#train downsampled knn model


cl<-makeCluster(cores)
registerDoParallel(cl)
model.knn.down<-train(x=dbtrainsmall[,c(predictors.continuous),with=FALSE], 
                      y=dbtrainsmall$converted,
                      preProc = c("center", "scale"),
                      method="knn",metric="Kappa",
                      trControl=caretTrainCtrlRebalancing,
                      tuneGrid = knnGrid)
stopCluster(cl)



model.knn.correction<-(sum(dbtrain$converted=='Y'))/sum(predict(model.knn.down,
                                                               newdata = dbtrain[,predictors.continuous,with=FALSE],
                                                               type = "prob")["Y"][,1])

# getTrainPerf(model.knn.base)
# getTrainPerf(model.knn.down) #best one
#save the list
save(list = c("model.knn.correction","model.knn.base","model.knn.down"),file="./caretModels/modelsKnn.RData")
