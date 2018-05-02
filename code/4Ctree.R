
#ctree models


source(file="./Code/3CaretConfigure.R")


library(party) #ctree model is implemented in party package
cl<-makeCluster(cores)
registerDoParallel(cl)
ctree.model<- train(x=dbtrain[,c(predictors.continuous,predictors.categorical),with=FALSE], 
                            y=dbtrain$converted, 
                            method = "ctree", 
                            trControl = caretTrainCtrl,
                            metric = "ROC")
stopCluster(cl)


caretTrainCtrl2<-caretTrainCtrl
caretTrainCtrl2$sampling='down'

cl<-makeCluster(cores)
registerDoParallel(cl)
ctree.model.down<- train(x=dbtrain[,c(predictors.continuous,predictors.categorical),with=FALSE], 
                    y=dbtrain$converted, 
                    method = "ctree", 
                    trControl = caretTrainCtrl2,
                    metric = "Kappa")
stopCluster(cl)


#plot(ctree.model$finalModel) #for plotting the final model
getTrainPerf(ctree.model)
ctree.correction<-sum(dbtrain$converted=='Y')/sum(predict(ctree.model.down, newdata=dbtrain,type="prob")["Y"][,1])

save(list = c("ctree.model.down","ctree.model","ctree.correction"),file="./caretModels/modelCtree.RData")

