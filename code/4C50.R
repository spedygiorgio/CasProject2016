############
#C5.0 model#
############

#configure
source(file="./Code/3CaretConfigure.R")
library(C50)

#set out the grid
c50Grid <- expand.grid(winnow = c(TRUE,FALSE), #winnowing: preliminary variable selection
                       trials=c(1,5,10,20),  #judgmentally set
                       model=c("tree","rules")) #choose between tree and rules


#C5.0 is sensible to unbalanced datasets (no split was found)


#tuning with rebalanced version
cl<-makeCluster(cores)
registerDoParallel(cl)
C5.0.model<- train(x=dbtrain[,c(predictors.continuous,predictors.categorical),with=FALSE],
                               y=dbtrain$converted,
                               method = "C5.0",
                               tuneGrid = c50Grid,
                               trControl = caretTrainCtrlRebalancing,
                               metric = "Kappa", #use Kappa since more appropriate for impalanced sample
                               importance=TRUE)  
stopCluster(cl)

#computing the unbalancing correction
c50.correction<-sum(dbtrain$converted=='Y')/sum(predict(C5.0.model, newdata=dbtrain,type="prob")["Y"][,1])

#get performance measures
getTrainPerf(C5.0.model)
save(list = c("C5.0.model","c50.correction"),file="./caretModels/modelsC50.RData")
#load(file="./caretModels/C50.RData")
