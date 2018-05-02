source(file="./Code/3CaretConfigure.R")


#configurations for SVM
caretTrainCtrl$number<-5  #reducing the number of CVs
predictors.candidates<-c(predictors.continuous,predictors.categorical.dummy)

#initial screening
#from http://stackoverflow.com/questions/21088825/feature-selection-in-caret-rfe-sum-with-roc

### finding optimal value of a tuning parameter

dbtrainverysmall<-sample_frac(dbtrainsmall,size=.5)

#pre - screening: variable importance
library(CORElearn)
reliefValues <- attrEval(converted ~ ., data = dbtrainverysmall[,c(predictors.continuous,
                                                               predictors.categorical,
                                                               'converted'),with=FALSE],
                         estimator = "ReliefFequalK",ReliefIterations = 100)

reliefValues.df<-data.frame(variable=names(reliefValues),relief=reliefValues) %>% arrange(desc(relief))


important.variables<-as.character(reliefValues.df$variable[reliefValues.df$relief>.10])
important.variables<-setdiff(important.variables, "vehicleMakeAndModel")
preescreended.svm.candidates<-character()
for (i in important.variables) {
  selected.binned.vars<-grep(pattern = i,x = predictors.candidates,value=TRUE)
  preescreended.svm.candidates <- c(preescreended.svm.candidates,selected.binned.vars)
}

# pre - estimating the sigma
require(kernlab)
sigma.estimation <- sigest(converted ~ ., data = dbtrain[,c(preescreended.svm.candidates,'converted'),with=FALSE], frac = 0.5)
svmGrid=expand.grid(sigma=seq(from=sigma.estimation[1],to=sigma.estimation[3],length.out = 5), #taking the median
                     C=2^seq(from=-3, to=3)) #standard range


#fitting the model
cl<-makeCluster(cores)
registerDoParallel(cl)
model.svm<-train(x=dbtrainverysmall[,preescreended.svm.candidates, with=FALSE],
                     y=dbtrainverysmall$converted,
                     method="svmRadial",metric="Kappa",
                     tuneGrid =  svmGrid,
                      preProc = c("center", "scale"),
                     trControl = caretTrainCtrlRebalancing)
stopCluster(cl)


model.svm.correction<-(sum(dbtrain$converted=='Y'))/sum(predict(model.svm,
                                                                newdata = dbtrain[,preescreended.svm.candidates,with=FALSE],
                                                                type = "prob")["Y"][,1])


save(list=c("model.svm","model.svm.correction","preescreended.svm.candidates"),file="./caretModels/modelsSvmRadial.RData",compress="xz")



 
# #bayesian optimization
#  caretTrainCtrl2<-caretTrainCtrl
#  caretTrainCtrl2$number<-2
# # 
#  svm_fit_bayes <- function(logC, logSigma) {
#       ## Use the same model code but for a single (C, sigma) pair. 
#                 mod <- train(x=dbtrainverysmall[,preescreended.candidates, with=FALSE],
#                              y=dbtrainverysmall$converted,
#                              method = "svmRadial",
#                              preProc = c("center", "scale"),
#                              metric = "ROC",
#                              trControl = caretTrainCtrl2,
#                              tuneGrid = data.frame(C = exp(logC), sigma = exp(logSigma)))
#           list(Score = -getTrainPerf(mod)[, "ROC"], Pred = 0)
#  }
# # 
# # 
#  lower_bounds <- c(logC = -5, logSigma = -9)
#  upper_bounds <- c(logC = 10, logSigma = -0.75)
# # 
#  bounds <- list(logC = c(lower_bounds[1], upper_bounds[1]),logSigma = c(lower_bounds[2], upper_bounds[2]))
# # 
# # 
#  ## Create a grid of values as the input into the BO code
#  initial_grid <- model.svm$results[, c("C", "sigma", "ROC")]
#  initial_grid$C <- log(initial_grid$C)
#  initial_grid$sigma <- log(initial_grid$sigma)
#  names(initial_grid) <- c("logC", "logSigma", "Value")
#  
#save(initial_grid,file="./caretModels/svmgrid.RData") 

# 
# library(rBayesianOptimization)
# 
# cl<-makeCluster(cores)
# registerDoParallel(cl)
# 
#  ba_search <- BayesianOptimization(svm_fit_bayes,
#  bounds = bounds,
#  init_grid_dt = initial_grid, 
# init_points = 0, 
#  n_iter = 5,
#  acq = "ucb", 
#  kappa = 1, 
#  eps = 0.0,
#  verbose = TRUE)
#  
#  stopCluster(cl)
