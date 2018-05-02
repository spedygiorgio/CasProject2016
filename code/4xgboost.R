require(Matrix)
require(data.table)
require(pbapply)
require(xgboost)

# Preliminary
rounds_grid=10000
dbtrain$convertedY<-0
dbtest$convertedY<-0
dbtrain[converted=='Y',convertedY:=1]
dbtest[converted=='Y',convertedY:=1]


# creating the XGBOOST datasets

dtrain<-prepare_db_xgboost(df=dbtrain,x_vars =c(predictors.categorical,predictors.continuous),y_var ="convertedY"  )
dtest<-prepare_db_xgboost(df=dbtest,x_vars =c(predictors.categorical,predictors.continuous),y_var ="convertedY"  )

# Watchlist to perform early stopping
watchlist<-list(train=dtrain, test=dtest)

# First Step: Tuning of major parameters, fixed ETA=0.1 and delta_step=1
searchGrid <- expand.grid(subsample = seq(from=0.3,to=1,length.out = 10),
                          colsample_bytree = seq(from=0.5,to=0.95,length.out = 10),
                          max_depth = c(6L,8L,10L,12L,14L),
                          eta = 0.1,
                          min_child_weight=c(0L,1L,5L,10L,20L),
                          alpha=0,
                          gamma=0,
                          lambda=1,
                          max_delta_step=1
                         ) %>% sample_n(20)

xgb.tuner.1 <- pbapply(searchGrid, 1, function(parameterList){
  # extract parameters to test
  current_subsample_rate <- parameterList[["subsample"]]
  current_colsample_rate <- parameterList[["colsample_bytree"]]
  current_max_depth <- parameterList[["max_depth"]]
  current_eta <- parameterList[["eta"]]
  current_min_child <- parameterList[["min_child_weight"]]
  current_gamma <- parameterList[["gamma"]]
  current_alpha <- parameterList[["alpha"]]
  current_lambda <- parameterList[["lambda"]]
  current_max_delta_step <- parameterList[["max_delta_step"]]
  
 cat("\n","Training . . . ", "\n", sep=" ")
  
  fit <- xgb.train(data =dtrain  , watchlist=watchlist, nround = rounds_grid,
                             print_every_n = 100,early_stopping_rounds = 15,eval_metric = "logloss",
                             eta = current_eta,objective = "binary:logistic", verbose = T, maximize = FALSE,
                             max.depth = current_max_depth, subsample = current_subsample_rate,
                             colsample_bytree = current_colsample_rate,
                             min_child_weight=current_min_child,lambda=current_lambda,
                             alpha=current_alpha, gamma=current_gamma, max_delta_step=current_max_delta_step
                             )
  
 logloss=min(fit$evaluation_log$test_logloss) 
 idx=fit$best_iteration
 preds=predict(fit,dtest)
 preds=sum(preds)
 
  
  return(c("metric"=logloss, "iter"=idx, "pred"=preds,  "depth"= current_max_depth , 
           "subsample"= current_subsample_rate, 
           "colsample"=current_colsample_rate, "child"=current_min_child, "alpha"=current_alpha,
           "gamma"=current_gamma,
           "max_delta_step"=current_max_delta_step,
           "lambda"=current_lambda
           ))
})
g1<-which.min(xgb.tuner.1["metric",])

# Second Step: Tuning of regularization parameters, fixed ETA
searchGrid2 <- expand.grid(subsample = xgb.tuner.1["subsample",g1],
                          colsample_bytree = xgb.tuner.1["colsample",g1],
                          max_depth = xgb.tuner.1["depth",g1],
                          min_child_weight= xgb.tuner.1["child",g1],
                          eta = 0.1,
                          alpha=runif(n = 10,min = 0,1),
                          gamma=runif(n = 10,min = 0,1),
                          max_delta_step=c(0,1,2,4,7,10),
                          lambda=runif(n=10,min=0,max=3)
) %>% sample_n(10)



xgb.tuner.2 <- pbapply(searchGrid2, 1, function(parameterList){
  # extract parameters to test
  current_subsample_rate <- parameterList[["subsample"]]
  current_colsample_rate <- parameterList[["colsample_bytree"]]
  current_max_depth <- parameterList[["max_depth"]]
  current_eta <- parameterList[["eta"]]
  current_min_child <- parameterList[["min_child_weight"]]
  current_gamma <- parameterList[["gamma"]]
  current_alpha <- parameterList[["alpha"]]
  current_lambda <- parameterList[["lambda"]]
  current_step <- parameterList[["max_delta_step"]]
  
  cat("\n","Training . . . ", "\n", sep=" ")
  
  fit <- xgb.train(data =dtrain  , watchlist=watchlist, nround = rounds_grid,
                   print_every_n = 30,early_stopping_rounds = 15,eval_metric = "logloss",
                   eta = current_eta,objective = "binary:logistic", verbose = T, maximize = F,
                   max.depth = current_max_depth, subsample = current_subsample_rate,
                   colsample_bytree = current_colsample_rate,
                   lambda=current_lambda,
                   min_child_weight=current_min_child, 
                   alpha=current_alpha, gamma=current_gamma, max_delta_step=current_step
  )
  
  logloss=min(fit$evaluation_log$test_logloss) 
  idx=fit$best_iteration
  preds=predict(fit,dtest)
  preds=sum(preds)
  
  
  return(c("metric"=logloss, "iter"=idx, "pred"=preds,  "depth"= current_max_depth , "subsample"= current_subsample_rate, 
           "colsample"=current_colsample_rate, "child"=current_min_child, "alpha"=current_alpha,
           "gamma"=current_gamma, "step"=current_step, "lambda"=current_lambda))
})
g2=which.min(xgb.tuner.2["metric",])

#collect the results for bayesian optimization initializing grid

full_grid_results<-as.data.frame(t(cbind(xgb.tuner.1,xgb.tuner.2)))
initial_grid<-dplyr::select(full_grid_results, min_child_weight= child, max_depth=depth,gamma,alpha,lambda, subsample,
                            colsample_bytree=colsample,max_delta_step,Value=metric)
initial_grid$Value<-initial_grid$Value*-1



#--------------------------------
# Bayesian Grid Configuration
#--------------------------------

library(rBayesianOptimization)
Bayes_kappa <-  4.5
init_bayes <- 50
iter_bayes <- 150
rounds <- 2000

#define hyperparameters' bounds of search
xgb_bin_bounds_list <- list(min_child_weight= c(0L, 50L),
                            max_depth= c(3L, 16L), 
                            gamma= c(0, 1),
                            alpha=c(0, 1),
                            lambda = c(0, 5),
                            subsample=c(0.3,0.95), 
                            colsample_bytree=c(0.5,0.95),
                            max_delta_step=c(0,7))



#function to be optimized
xgb_bayes_bin <- function(min_child_weight, max_depth, subsample, colsample_bytree,max_delta_step,gamma, alpha, lambda) {
  
  fit_round <- xgb.train(params = list(max_depth = max_depth, subsample = subsample, colsample_bytree = colsample_bytree,
                                       min_child_weight=min_child_weight,gamma=gamma, alpha=alpha,  lambda = lambda, max_delta_step=max_delta_step,
                                       booster = "gbtree", eta=0.05),
                         data = dtrain, nround = rounds, watchlist = watchlist, 
                         objective = "binary:logistic",eval_metric="logloss",
                         early_stopping_rounds = 15, maximize = FALSE, verbose = TRUE)
  
  predictions_round<-predict(fit_round,dtest)
  
  metric_round <- fit_round$best_score
  
  list(Score = -metric_round, Pred = predictions_round)
}

#performing bayesian optimization


Results_bayes_tot <- BayesianOptimization(FUN=xgb_bayes_bin,
                                            bounds = xgb_bin_bounds_list,
                                            #init_grid_dt=initial_grid,
                                            init_points = init_bayes, 
                                            n_iter = iter_bayes, 
                                            acq = "ucb", 
                                            kappa = Bayes_kappa, 
                                          verbose = TRUE)


# Third Step: Tuning of ETA/nround at the same time

xgb.eta.big<-  xgb.train(nround = 30000,eta = 0.1,
                         data =dtrain  , watchlist=watchlist, 
                         print_every_n = 30,early_stopping_rounds = 15,eval_metric = "logloss",
                         objective = "binary:logistic", verbose = T, maximize = F,
                         subsample = Results_bayes_tot$Best_Par["subsample"],
                         colsample_bytree = Results_bayes_tot$Best_Par["colsample_bytree"],
                         max_depth = Results_bayes_tot$Best_Par["max_depth"],
                         min_child_weight=Results_bayes_tot$Best_Par["min_child_weight"],
                         gamma=Results_bayes_tot$Best_Par["gamma"],
                         alpha=Results_bayes_tot$Best_Par["alpha"],
                         lambda=Results_bayes_tot$Best_Par["lambda"],
                         max_delta_step=Results_bayes_tot$Best_Par["max_delta_step"])

xgb.eta.std<- xgb.train(nround = 30000,eta = 0.05,
                        data =dtrain  , watchlist=watchlist, 
                        print_every_n = 30,early_stopping_rounds = 15,eval_metric = "logloss",
                        objective = "binary:logistic", verbose = T, maximize = F,
                        subsample = Results_bayes_tot$Best_Par["subsample"],
                        colsample_bytree = Results_bayes_tot$Best_Par["colsample_bytree"],
                        max_depth = Results_bayes_tot$Best_Par["max_depth"],
                        min_child_weight=Results_bayes_tot$Best_Par["min_child_weight"],
                        gamma=Results_bayes_tot$Best_Par["gamma"],
                        alpha=Results_bayes_tot$Best_Par["alpha"],
                        lambda=Results_bayes_tot$Best_Par["lambda"],
                        max_delta_step=Results_bayes_tot$Best_Par["max_delta_step"])
xgb.eta.small<- xgb.train(nround = 30000,eta = 0.01,
                          data =dtrain  , watchlist=watchlist, 
                          print_every_n = 30,early_stopping_rounds = 15,eval_metric = "logloss",
                          objective = "binary:logistic", verbose = T, maximize = F,
                          subsample = Results_bayes_tot$Best_Par["subsample"],
                          colsample_bytree = Results_bayes_tot$Best_Par["colsample_bytree"],
                          max_depth = Results_bayes_tot$Best_Par["max_depth"],
                          min_child_weight=Results_bayes_tot$Best_Par["min_child_weight"],
                          gamma=Results_bayes_tot$Best_Par["gamma"],
                          alpha=Results_bayes_tot$Best_Par["alpha"],
                          lambda=Results_bayes_tot$Best_Par["lambda"],
                          max_delta_step=Results_bayes_tot$Best_Par["max_delta_step"])


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


# Choose the best model 
best=(as.data.frame(which.min(c("std"=metric.std,"big"=metric.big,"small"=metric.small))))
if(best==1) bestmod=xgb.eta.std
if(best==2) bestmod=xgb.eta.big
if(best==3) bestmod=xgb.eta.small
model.xgboost<-bestmod

# Save the model for future reload and prediction 
save(list=c("Results_bayes_tot","model.xgboost"),file="./caretModels/modelXgboostFinal.RData")
