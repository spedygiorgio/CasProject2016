require(Matrix)
require(data.table)
require(pbapply)
require(xgboost)

# Preliminary
rounds_grid=10000
dbtrain=dbtrain[,c(predictors.categorical,predictors.continuous,"converted"),with=FALSE]
dbtest=dbtest[,c(predictors.categorical,predictors.continuous,"converted"),with=FALSE]

# Sparse Matrix creation (one hot-encoded categorical variables)
previous_na_action <- options('na.action')
options(na.action='na.pass')

sparse_train=sparse.model.matrix(~.-1, data = dbtrain)
sparse_test=sparse.model.matrix(~.-1, data = dbtest)

cols2keep<-setdiff(colnames(sparse_train) ,c("convertedY"))
Target="convertedY"

# XGB dedicated objects creation
dtrain <- xgb.DMatrix(data =sparse_train[,cols2keep] , label = sparse_train[,Target])
dtest <- xgb.DMatrix(data =sparse_test[,cols2keep] , label = sparse_test[,Target])

options(na.action=previous_na_action$na.action)

# Watchlist to perform early stopping
watchlist=list(train=dtrain, test=dtest)

# First Step: Tuning of major parameters, fixed ETA=0.1 and delta_step=1
searchGrid <- expand.grid(subsample = c(0.5,  0.8, 0.9),
                          colsample_bytree = c(0.5,  0.8, 0.9),
                          max_depth = c(6,8,10,12,14),
                          eta = 0.1,
                          min_child_weight=c(1,5,10,20),
                          alpha=0,
                          gamma=0 
                         )

xgb.tuner.1 <- pbapply(searchGrid, 1, function(parameterList){
  # extract parameters to test
  current_subsample_rate <- parameterList[["subsample"]]
  current_colsample_rate <- parameterList[["colsample_bytree"]]
  current_max_depth <- parameterList[["max_depth"]]
  current_eta <- parameterList[["eta"]]
  current_min_child <- parameterList[["min_child_weight"]]
  current_gamma <- parameterList[["gamma"]]
  current_alpha <- parameterList[["alpha"]]
  
 cat("\n","Training . . . ", "\n", sep=" ")
  
  fit <- xgb.train(data =dtrain  , watchlist=watchlist, nround = rounds_grid,
                             print_every_n = 30,early_stopping_rounds = 15,eval_metric = "logloss",
                             eta = current_eta,objective = "binary:logistic", verbose = T, maximize = F,
                             max.depth = current_max_depth, subsample = current_subsample_rate,
                             colsample_bytree = current_colsample_rate,
                             min_child_weight=current_min_child, base_score=mean(sparse_train[,"convertedY"]),
                             alpha=current_alpha, gamma=current_gamma, max_delta_step=1
                             )
  
 logloss=min(fit$evaluation_log$test_logloss) 
 idx=fit$best_iteration
 preds=predict(fit,dtest)
 preds=sum(preds)
 
  
  return(c("metric"=logloss, "iter"=idx, "pred"=preds,  "depth"= current_max_depth , "subsample"= current_subsample_rate, 
           "colsample"=current_colsample_rate, "child"=current_min_child, "alpha"=current_alpha,
           "gamma"=current_gamma
           ))
})
g1=which.min(xgb.tuner.1["metric",])

# Second Step: Tuning of regularization parameters, fixed ETA
searchGrid <- expand.grid(subsample = xgb.tuner.1["subsample",g1],
                          colsample_bytree = xgb.tuner.1["colsample",g1],
                          max_depth = xgb.tuner.1["depth",g1],
                          min_child_weight= xgb.tuner.1["child",g1],
                          eta = 0.1,
                          alpha=c(0,0.01,0.03,1),
                          gamma=c(0,0.01,0.03,1),
                          max_delta_step=c(0,1,2,3,4,10)
)

# iter 299
# pred 6825.584 (99.6%)
# depth 6
# subsample .9
# colsample .9
# child 10
# alpha 0
# gamma 0
# step 1
# BEST ETA: 0.05 - 650 rounds
# FOLLOWING TO BE DELETED AFTER RUN
searchGrid <- expand.grid(subsample = .9,
                          colsample_bytree = .9,
                          max_depth = 6,
                          min_child_weight= 10,
                          eta = 0.1,
                          alpha=c(0,0.01,0.03,1),
                          gamma=c(0,0.01,0.03,1),
                          max_delta_step=c(0,1,2,3,4,10)
)

xgb.tuner.2 <- pbapply(searchGrid, 1, function(parameterList){
  # extract parameters to test
  current_subsample_rate <- parameterList[["subsample"]]
  current_colsample_rate <- parameterList[["colsample_bytree"]]
  current_max_depth <- parameterList[["max_depth"]]
  current_eta <- parameterList[["eta"]]
  current_min_child <- parameterList[["min_child_weight"]]
  current_gamma <- parameterList[["gamma"]]
  current_alpha <- parameterList[["alpha"]]
  current_step <- parameterList[["max_delta_step"]]
  
  cat("\n","Training . . . ", "\n", sep=" ")
  
  fit <- xgb.train(data =dtrain  , watchlist=watchlist, nround = rounds_grid,
                   print_every_n = 30,early_stopping_rounds = 15,eval_metric = "logloss",
                   eta = current_eta,objective = "binary:logistic", verbose = T, maximize = F,
                   max.depth = current_max_depth, subsample = current_subsample_rate,
                   colsample_bytree = current_colsample_rate,
                   min_child_weight=current_min_child, base_score=mean(sparse_train[,"convertedY"]),
                   alpha=current_alpha, gamma=current_gamma, max_delta_step=current_step
  )
  
  logloss=min(fit$evaluation_log$test_logloss) 
  idx=fit$best_iteration
  preds=predict(fit,dtest)
  preds=sum(preds)
  
  
  return(c("metric"=logloss, "iter"=idx, "pred"=preds,  "depth"= current_max_depth , "subsample"= current_subsample_rate, 
           "colsample"=current_colsample_rate, "child"=current_min_child, "alpha"=current_alpha,
           "gamma"=current_gamma, "step"=current_step
  ))
})
g2=which.min(xgb.tuner.2["metric",])

# Third Step: Tuning of ETA/nround at the same time
# Note: nround is tuned based upon best iteration
xgb.eta.big<-  xgb.train(nround = 30000,eta = 0.1,
                        
                         data =dtrain  , watchlist=watchlist, 
                         print_every_n = 30,early_stopping_rounds = 15,eval_metric = "logloss",
                         objective = "binary:logistic", verbose = T, maximize = F,
                         subsample = xgb.tuner.2["subsample",g2],
                         colsample_bytree = xgb.tuner.2["colsample",g2],
                         max_depth = xgb.tuner.2["depth",g2],
                         min_child_weight=xgb.tuner.2["child",g2],
                         gamma=xgb.tuner.2["gamma",g2],
                         alpha=xgb.tuner.2["alpha",g2],
                         max_delta_step=xgb.tuner.2["delta",g2],
                         base_score=mean(sparse_train[,"convertedY"]))

xgb.eta.std<-  xgb.train(nround = 30000,eta = 0.05,
                         
                         data =dtrain  , watchlist=watchlist, 
                         print_every_n = 30,early_stopping_rounds = 15,eval_metric = "logloss",
                         objective = "binary:logistic", verbose = T, maximize = F,
                         subsample = xgb.tuner.2["subsample",g2],
                         colsample_bytree = xgb.tuner.2["colsample",g2],
                         max_depth = xgb.tuner.2["depth",g2],
                         min_child_weight=xgb.tuner.2["child",g2],
                         gamma=xgb.tuner.2["gamma",g2],
                         alpha=xgb.tuner.2["alpha",g2],
                         max_delta_step=xgb.tuner.2["delta",g2],
                         base_score=mean(sparse_train[,"convertedY"]))

xgb.eta.small<- xgb.train(nround = 30000,eta = 0.01,
                          
                         data =dtrain  , watchlist=watchlist, 
                         print_every_n = 30,early_stopping_rounds = 15,eval_metric = "logloss",
                         objective = "binary:logistic", verbose = T, maximize = F,
                         subsample = xgb.tuner.2["subsample",g2],
                         colsample_bytree = xgb.tuner.2["colsample",g2],
                         max_depth = xgb.tuner.2["depth",g2],
                         min_child_weight=xgb.tuner.2["child",g2],
                         gamma=xgb.tuner.2["gamma",g2],
                         alpha=xgb.tuner.2["alpha",g2],
                         max_delta_step=xgb.tuner.2["delta",g2],
                         base_score=mean(sparse_train[,"convertedY"]))

xgb.eta.med<-  xgb.train(nround = 30000,eta = 0.025,
                         
                         data =dtrain  , watchlist=watchlist, 
                         print_every_n = 30,early_stopping_rounds = 15,eval_metric = "logloss",
                         objective = "binary:logistic", verbose = T, maximize = F,
                         subsample = xgb.tuner.2["subsample",g2],
                         colsample_bytree = xgb.tuner.2["colsample",g2],
                         max_depth = xgb.tuner.2["depth",g2],
                         min_child_weight=xgb.tuner.2["child",g2],
                         gamma=xgb.tuner.2["gamma",g2],
                         alpha=xgb.tuner.2["alpha",g2],
                         max_delta_step=xgb.tuner.2["delta",g2],
                         base_score=mean(sparse_train[,"convertedY"]))

# Preliminary for choosing the best model
# (standard)
iter.std= which.min(xgb.eta.std$evaluation_log$test_logloss) # Best Iter (nround "tuning")
metric.std= min(xgb.eta.std$evaluation_log$test_logloss) # LogLoss at Best Iter

# (big)
iter.big= which.min(xgb.eta.big$evaluation_log$test_logloss) 
metric.big= min(xgb.eta.big$evaluation_log$test_logloss) 

# (small)
iter.small= which.min(xgb.eta.small$evaluation_log$test_logloss) 
metric.small= min(xgb.eta.small$evaluation_log$test_logloss) 

# (medium)
iter.med= which.min(xgb.eta.med$evaluation_log$test_logloss) 
metric.med= min(xgb.eta.med$evaluation_log$test_logloss) 

# Choose the best model 
best=(as.data.frame(which.min(c("std"=metric.std,"big"=metric.big,"small"=metric.small,"medium"=metric.med))))
if(best==1) bestmod=xgb.eta.std
if(best==2) bestmod=xgb.eta.big
if(best==3) bestmod=xgb.eta.small
if(best==3) bestmod=xgb.eta.med
model.xgboost<-bestmod

# Save the model for future reload and prediction 
save(list=c("sparse_test","model.xgboost.fixed_alpha_gamma"),file="./caretModels/xgboost.RData")

# BEST Model: (xgb.tuner.1[,99])
# iter 299
# pred 6825.584 (99.6%)
# depth 6
# subsample .9
# colsample .9
# child 10
# alpha 0
# gamma 0
# step 1
# BEST ETA: 0.05 - 650 rounds

load("modelsxgboost.RData")
importance <- xgb.importance(feature_names = sparse_train@Dimnames[[2]][-length(sparse_train@Dimnames[[2]])], model = model.xgboost)

xgb.plot.importance(importance_matrix = importance[1:20], measure="Gain",rel_to_first = TRUE, xlab="Relative Importance")





