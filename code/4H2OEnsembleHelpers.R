
myworkdir=getwd()
base_glm<-h2o.loadModel(path=paste(myworkdir,"/H2OModels/GLM/","base_glm",sep=''))
final_glm<-h2o.loadModel(path=paste(myworkdir,"/H2OModels/GLM/","final_glm",sep=''))
final_gbm<-h2o.loadModel(path=paste(myworkdir,"/H2OModels/GBM/","final_gbm",sep=''))
final_rf<-h2o.loadModel(path=paste(myworkdir,"/H2OModels/DRF/","final_rf",sep=''))
final_dl<-h2o.loadModel(path=paste(myworkdir,"/H2OModels/DEEPLEARNING/","final_dl",sep=''))

#helpers for glms
best_glm_alpha<-final_glm@parameters$alpha
best_glm_lambda<-final_glm@parameters$lambda

h2o.glm.wrapper.1 <- function(..., alpha = best_glm_alpha, lambda = best_glm_lambda, seed = 1) {
   h2o.glm.wrapper(..., alpha = alpha, lambda = lambda, seed = seed)
 }
 
#helpers for random forest
best_rf_ntrees<-final_rf@parameters$ntrees
best_rf_max_depth<-final_rf@parameters$max_depth
best_rf_min_rows<-final_rf@parameters$min_rows
best_rf_sample_rate<-final_rf@parameters$sample_rate
best_rf_mtries<-final_rf@parameters$mtries
best_rf_histogram_type<-final_rf@parameters$histogram_type
best_rf_binomial_double_trees<-final_rf@parameters$binomial_double_trees
best_rf_balance_classes<-final_rf@allparameters$balance_classes

 
h2o.rf.wrapper.1 <- function(..., ntrees = best_rf_ntrees, 
                             max_depth = best_rf_max_depth, 
                             min_rows=best_rf_min_rows,sample_rate=best_rf_sample_rate, 
                             mtries=best_rf_mtries, 
                             histogram_type=best_rf_histogram_type,
                             binomial_double_trees=best_rf_binomial_double_trees,
                             balance_classes=best_rf_balance_classes,
                             seed = 1) {
  
  h2o.randomForest.wrapper(..., ntrees = ntrees, 
                           max_depth = max_depth, 
                           min_rows=min_rows, 
                           sample_rate=sample_rate, 
                           mtries=mtries,
                           histogram_type=histogram_type,
                           binomial_double_trees=binomial_double_trees,
                           balance_classes=balance_classes,
                           seed = seed)
}

#helpers for deep learning
#final_dl<-h2o.loadModel(path = "./H2OModels/DEEPLEARNING/final_dl")

best_dl_activation=final_dl@parameters$activation
best_dl_hidden=final_dl@parameters$hidden
best_dl_input_dropout_ratio = final_dl@parameters$input_dropout_ratio
best_dl_hidden_dropout_ratio=final_dl@parameters$hidden_dropout_ratios
best_dl_epsilon=final_dl@allparameters$epsilon
best_dl_rho=final_dl@parameters$rho
best_dl_l1=final_dl@parameters$l1
best_dl_l2=final_dl@parameters$l2
best_dl_epochs=final_dl@parameters$epochs
best_dl_max_w2=final_dl@parameters$max_w2
best_dl_balance_classes<-final_dl@allparameters$balance_classes


h2o.dl.wrapper.1 <- function(..., activation=best_dl_activation,
                             hidden=best_dl_hidden,
                             input_dropout_ratio=best_dl_input_dropout_ratio,
                             hidden_dropout_ratio=best_dl_hidden_dropout_ratio,
                             epsilon=best_dl_epsilon,
                             rho=best_dl_rho,
                             l1=best_dl_l1,
                             l2= best_dl_l2,
                             epochs=best_dl_epochs,
                             max_w2=best_dl_max_w2,
                             balance_classes=best_dl_balance_classes,
                             seed = 1) {
  
  h2o.deeplearning.wrapper(..., activation = activation, 
                           hidden = hidden, 
                           input_dropout_ratio=input_dropout_ratio,
                           hidden_dropout_ratio=hidden_dropout_ratio,
                           epsilon=epsilon,
                           rho=rho,
                           l1=l1,
                           l2=l2,
                           epochs = epochs,
                           max_w2=max_w2,
                           balance_classes=balance_classes,
                           seed = seed)
}


#GBM

best_gbm_distribution=final_gbm@allparameters$distribution
best_gbm_max_depth=final_gbm@allparameters$max_depth
best_gbm_sample_rate=final_gbm@allparameters$sample_rate
best_gbm_col_sample_rate=final_gbm@allparameters$col_sample_rate
best_gbm_col_sample_rate_per_tree=final_gbm@allparameters$col_sample_rate_per_tree
best_gbm_nbins_cats=final_gbm@allparameters$nbins_cats
best_gbm_min_rows=final_gbm@allparameters$min_rows
best_gbm_min_split_improvement=final_gbm@allparameters$min_split_improvement
best_gbm_histogram_type=final_gbm@allparameters$histogram_type
best_gbm_learn_rate=final_gbm@allparameters$learn_rate
best_gbm_learn_rate_annealing = final_gbm@allparameters$learn_rate_annealing
best_gbm_balance_classes = final_gbm@allparameters$balance_classes



h2o.gbm.wrapper.1 <- function(..., max_depth=best_gbm_max_depth,
                              sample_rate=best_gbm_sample_rate,
                              col_sample_rate=best_gbm_col_sample_rate,
                              col_sample_rate_per_tree=best_gbm_col_sample_rate_per_tree,
                              nbins_cats=best_gbm_nbins_cats,
                              min_rows=best_gbm_min_rows,
                              min_split_improvement=best_gbm_min_split_improvement,
                              histogram_type=best_gbm_histogram_type,
                              distribution=best_gbm_distribution,
                              learn_rate=best_gbm_learn_rate,
                              learn_rate_annealing = best_gbm_learn_rate_annealing,
                              balance_classes=best_gbm_balance_classes,
                             seed = 1) {
  
  h2o.gbm.wrapper(..., max_depth=max_depth,
                  sample_rate=sample_rate,
                  col_sample_rate=col_sample_rate,
                  col_sample_rate_per_tree=col_sample_rate_per_tree,
                  min_rows=min_rows,
                  nbins_cats=nbins_cats,
                  min_split_improvement=min_split_improvement,
                  histogram_type=histogram_type,
                  distribution=distribution,
                  learn_rate=learn_rate,
                  learn_rate_annealing=learn_rate_annealing,
                  balance_classes=balance_classes,
                           seed = seed)
}