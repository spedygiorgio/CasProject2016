
source(file="./Code/3H2OConfigure.R")

# define the random forest hyperparameter hyperspace
ntrees_opts = c(50,100,200,400)       # number of trees
max_depth_opts = c(10,15,20,25) # max dept of each tree
min_rows_opts = c(3,5,10,15,20) #min number of observation 
mtries_opts = c(-1,4,7,10) #available predictors at each sample
sample_rate_opts=seq(from=0.5, to=1, by=0.1)
#binomial_double_trees_opts=c(FALSE, TRUE)
histogram_type_opts = c( "UniformAdaptive", "Random", 
                         "QuantilesGlobal","RoundRobin") #histogram type
hyper_params = list( ntrees = ntrees_opts, 
                     max_depth = max_depth_opts, 
                     min_rows = min_rows_opts, 
                     mtries = mtries_opts,
                     sample_rate = sample_rate_opts,
                     histogram_type=histogram_type_opts
                      #,binomial_double_trees=binomial_double_trees_opts
)
# search criteria
search_criteria = list(strategy = "RandomDiscrete", 
                       max_runtime_secs = 4*60*60, 
                       max_models = 200, 
                       stopping_metric = "AUTO", 
                       stopping_tolerance = 0.001, 
                       stopping_rounds = 3, 
                       seed = 123456)
# grid tuning
rf_grid <- h2o.grid("randomForest", 
                     grid_id = "rf_grid",
                     x = c(predictors.categorical,predictors.continuous), 
                     y = "converted", 
                     training_frame = dbtrainh2o,
                     nfolds = nfolds_par,
                     stopping_metric = "logloss",
                     seed = 123456,
                    binomial_double_trees=TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)
# sorting the grid
rf_sorted_grid <- h2o.getGrid(grid_id = "rf_grid", sort_by = "logloss")
print(rf_sorted_grid)
#pick the best configuration

#fine tuning

search_criteria = list(strategy = "RandomDiscrete", 
                       max_runtime_secs = 10*60*60, 
                       max_models = 200, 
                       stopping_metric = "AUTO", 
                       stopping_tolerance = 0.001, 
                       stopping_rounds = 3, 
                       seed = 123456)
# grid tuning

# define the random forest hyperparameter hyperspace
ntrees_opts = seq(from=100, to=300, by=20)       # number of trees
max_depth_opts = c(8,10,12) # max dept of each tree
min_rows_opts = c(10,17,15,13,20) #min number of observation 
mtries_opts = c(-1,4,5) #available predictors at each sample
sample_rate_opts=seq(from=0.6, to=.8, by=0.1)

histogram_type_opts = c( "QuantilesGlobal") #histogram type

hyper_params = list( ntrees = ntrees_opts, 
                     max_depth = max_depth_opts, 
                     min_rows = min_rows_opts, 
                     mtries = mtries_opts,
                     sample_rate = sample_rate_opts,
                     histogram_type=histogram_type_opts
)

rf_grid2 <- h2o.grid("randomForest", 
                    grid_id = "rf_grid2",
                    x = c(predictors.categorical,predictors.continuous), 
                    y = "converted", 
                    training_frame = dbtrainh2o,
                    nfolds = nfolds_par,
                    stopping_metric = "logloss",
                    seed = 123456,
                    binomial_double_trees=TRUE,
                    hyper_params = hyper_params,
                    search_criteria = search_criteria)
# sorting the grid
rf_sorted_grid2 <- h2o.getGrid(grid_id = "rf_grid2", sort_by = "logloss")
print(rf_sorted_grid2)


final_rf <- h2o.getModel(rf_sorted_grid2@model_ids[[1]])
myModelRF<-h2o.saveModel(final_rf,path = "./H2OModels/DRF/",force=TRUE)

#final_rf<-h2o.saveModel(final_rf,path = "./H2OModels/DRF/",force=TRUE)



