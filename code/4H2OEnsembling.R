
source(file="./Code/3H2OConfigure.R")


final_glm<-h2o.loadModel(path = "./H2OModels/GLM/final_glm")
final_rf<-h2o.loadModel(path = "./H2OModels/DRF/final_rf")
final_gbm<-h2o.loadModel(path = "./H2OModels/GBM/final_gbm")
final_dl<-h2o.loadModel(path = "./H2OModels/DEEPLEARNING/final_dl")

ensemble <- h2o.stackedEnsemble(x = c(predictors.categorical,predictors.continuous),
                                y = "converted",
                                training_frame = dbtrainh2o,

                                model_id = "ensemble_gbm_grid_binomial",
                                base_models =list(final_dl@model_id,
                                                  final_glm@model_id,
                                                  final_gbm@model_id,
                                                  final_rf@model_id)
                                )







source(file="./Code/4H2OEnsembleHelpers.R")


# USING ALL MODELS

# 
h2o.ensemble.gbm <- h2o.ensemble(x = c(predictors.categorical,predictors.continuous),
                                 y = "converted",
                                 training_frame = dbtrainh2o,
                                 family = "binomial",
                                 learner = c("h2o.dl.wrapper.1","h2o.rf.wrapper.1", "h2o.glm.wrapper.1","h2o.gbm.wrapper.1"),
                                 metalearner = "h2o.gbm.wrapper",
                                 cvControl = list(V = nfolds_par, shuffle = TRUE))

h2o.ensemble.glm <- h2o.ensemble(x = c(predictors.categorical,predictors.continuous),
                     y = "converted",
                     training_frame = dbtrainh2o,
                     family = "binomial",
                     learner = c("h2o.rf.wrapper.1", "h2o.glm.wrapper.1","h2o.gbm.wrapper.1"),
                     metalearner = "h2o.glm.wrapper",
                     cvControl = list(V = nfolds_par, shuffle = TRUE))




 perf <- h2o.ensemble_performance(h2o.ensemble.glm, newdata = dbtesth2o)
 print(perf)

 perf <- h2o.ensemble_performance(h2o.ensemble.gbm, newdata = dbtesth2o)
 print(perf)
 
 h2o.save_ensemble(object=h2o.ensemble.glm, path = "./H2OModels/ENSEMBLES/ensemble_glm", force = TRUE, export_levelone = FALSE)
 h2o.save_ensemble(object=h2o.ensemble.gbm, path = "./H2OModels/ENSEMBLES/ensemble_gbm", force = TRUE, export_levelone = FALSE)
 

#reload the models
## GLMs
# temp<-h2o.loadModel(path = "./H2OModels/GLM/final_glm")
# glm_pars<-temp@parameters
# glm_pars$model_id="final_glm"
# glm_pars$fold_assignment = "Modulo"
# glm_pars$nfolds = nfolds_par
# glm_pars$keep_cross_validation_predictions = TRUE
# final_glm <- do.call(h2o.glm,glm_pars)
# h2o.rm(temp)
# 
# 
# ## Random Forest
# temp<-h2o.loadModel(path = "./H2OModels/DRF/final_rf")
# rf_pars<-temp@parameters
# rf_pars$model_id="final_rf"
# rf_pars$fold_assignment = "Modulo"
# rf_pars$nfolds = nfolds_par
# rf_pars$keep_cross_validation_predictions = TRUE
# final_rf <- do.call(h2o.randomForest,rf_pars)
# h2o.rm(temp)
# 
# ## GBM
# temp<-h2o.loadModel(path = "./H2OModels/GBM/final_gbm")
# gbm_pars<-temp@parameters
# gbm_pars$model_id="final_gbm"
# gbm_pars$fold_assignment = "Modulo"
# gbm_pars$nfolds = nfolds_par
# gbm_pars$keep_cross_validation_predictions = TRUE
# final_gbm <- do.call(h2o.gbm,gbm_pars)
# h2o.rm(temp)
# 
# ## DEEP LEARNING
# temp<-h2o.loadModel(path = "./H2OModels/DEEPLEARNING/final_dl")
# dl_pars<-temp@parameters
# dl_pars$model_id="final_dl"
# dl_pars$fold_assignment = "Modulo"
# dl_pars$nfolds = nfolds_par
# dl_pars$keep_cross_validation_predictions = TRUE
# final_dl <- do.call(h2o.deeplearning,dl_pars)
# h2o.rm(temp)
# 
# 
# # Create a list of all the base models
# #models <- c(final_rf,final_glm,final_dl, final_gbm)
# models <- c(final_glm, final_rf)#, final_gbm, final_dl) 
# # Let's stack!: try two superlearners, a glm and a gbm 
# model.h2oensemble.glm <- h2o.stack(models = models, response_frame = dbtrainh2o[,"converted"],metalearner = "h2o.glm.wrapper")
# model.h2oensemble.rf <- h2o.stack(models = models, response_frame = dbtrainh2o[,"converted"],metalearner = "h2o.randomForest.wrapper")
# model.h2oensemble.gbm <- h2o.stack(models = models, response_frame = dbtrainh2o[,"converted"],metalearner = "h2o.gbm.wrapper")
# 
# #check performance
# perf.gbm <- h2o.ensemble_performance(model.h2oensemble.gbm, newdata = dbtesth2o)
# perf.glm <- h2o.ensemble_performance(model.h2oensemble.glm, newdata = dbtesth2o)
# perf.rf <- h2o.ensemble_performance(model.h2oensemble.rf, newdata = dbtesth2o)
# 
# print(perf.glm)
# print(perf.gbm) #best
# print(perf.rf)
# 
# final_ensemble<-model.h2oensemble.gbm
# 
# 
# h2o.save_ensemble(object=final_ensemble, path = "./H2OModels/ENSEMBLES/", force = TRUE, export_levelone = TRUE)
# 
# 
#  
#  h2o.ensemble.glm.NODL <- h2o.ensemble(x = c(predictors.categorical,predictors.continuous),
#                                   y = "converted",
#                                   training_frame = dbtrainh2o,
#                                   family = "binomial",
#                                   learner = c("h2o.rf.wrapper.1", "h2o.glm.wrapper.1","h2o.gbm.wrapper.1"),
#                                   metalearner = "h2o.glm.wrapper",
#                                   cvControl = list(V = nfolds_par, shuffle = TRUE))
#  
#  # 
#  h2o.ensemble.gbm.NODL <- h2o.ensemble(x = c(predictors.categorical,predictors.continuous),
#                                   y = "converted",
#                                   training_frame = dbtrainh2o,
#                                   family = "binomial",
#                                   learner = c("h2o.rf.wrapper.1", "h2o.glm.wrapper.1","h2o.gbm.wrapper.1"),
#                                   metalearner = "h2o.gbm.wrapper",
#                                   cvControl = list(V = nfolds_par, shuffle = TRUE))
#  
#  
#  perf <- h2o.ensemble_performance(h2o.ensemble.glm.NODL, newdata = dbtesth2o)
#  print(perf)
#  
#  perf <- h2o.ensemble_performance(h2o.ensemble.gbm.NODL, newdata = dbtesth2o)
#  print(perf)
#  
#  h2o.save_ensemble(object=h2o.ensemble.glm.NODL, path = "./H2OModels/ENSEMBLES/ensemble_glm_nodl", force = TRUE, export_levelone = FALSE)
#  h2o.save_ensemble(object=h2o.ensemble.gbm.NODL, path = "./H2OModels/ENSEMBLES/ensemble_gbm_nodl", force = TRUE, export_levelone = FALSE)
#  