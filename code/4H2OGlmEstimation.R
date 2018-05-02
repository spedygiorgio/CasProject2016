#######################
######GLMs with H2O####
#######################


source(file="./Code/3H2OConfigure.R")

#BASELINE: binned no alpha no lamda
base_glm<-h2o.glm(model_id = "base_glm",
                   x = c(predictors.binned, predictors.categorical),
                   y = "converted", 
                   alpha=0,
                   lambda=0,
                   lambda_search = FALSE,
                   training_frame = dbtrainh2o,
                   nfolds = nfolds_par,
                   family = "binomial",
                   keep_cross_validation_predictions = TRUE,
                   fold_assignment="Modulo"
)
summary(base_glm)


############################GRID SEARCH FOR ELASTICNET MODELS (LASSO + RIDGE) ######################


alpha_opt <-seq(from=0, to=1, by=.05)

#setting the extremes to 1-eps
alpha_opt[1]<-0.01
alpha_opt[21]<-0.99

hyper_parameters <- list(alpha=alpha_opt)

#training on binned and unbinned vars

#NOT BINNED
glm_grid <-
  h2o.grid("glm",
           grid_id = "glm_grid_unbinned",
           x = c(predictors.continuous, predictors.categorical),
           y = "converted", lambda_search = TRUE,
           training_frame = dbtrainh2o,
           nfolds=nfolds_par,
           family = "binomial",
           lambda_search=TRUE,
           #nlambdas=10,
           hyper_params = hyper_parameters
  )
summary(glm_grid)

glm_grid_unbinned <- h2o.getGrid(grid_id = "glm_grid_unbinned", sort_by = "logloss")
final_glm_unbinned<- h2o.getModel(glm_grid_unbinned@model_ids[[1]])
h2o.performance(final_glm_unbinned)

#BINNED
glm_grid <-
  h2o.grid("glm", 
           grid_id = "glm_grid_binned",
           x = c(predictors.binned, predictors.categorical),
           y = "converted", lambda_search = TRUE,
           training_frame = dbtrainh2o,
           nfolds = nfolds_par,
           family = "binomial",
          lambda_search=TRUE,
           hyper_params = hyper_parameters
  )
summary(glm_grid)

glm_grid_binned <- h2o.getGrid(grid_id = "glm_grid_binned", sort_by = "logloss")
final_glm_binned<- h2o.getModel(glm_grid_binned@model_ids[[1]])



#choose the glm based on performance metrics
h2o.performance(base_glm,newdata = dbtesth2o)
h2o.performance(final_glm_binned,newdata = dbtesth2o)
h2o.performance(final_glm_unbinned,newdata = dbtesth2o) #best


final_glm<-final_glm_unbinned

#saving models
h2o.saveModel(final_glm,path = paste(workDir,"/H2OModels/GLM/",sep=''),force=TRUE)
h2o.saveModel(base_glm,path = paste(workDir,"/H2OModels/GLM/",sep=''),force=TRUE)

#final_glm=h2o.loadModel(path = paste(workDir,"/H2OModels/GLM/final_glm",sep=''))

