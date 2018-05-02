
source(file="./Code/3H2OConfigure.R")

# Construct a large Cartesian hyper-parameter space

# in this dataset
laplace_opts = seq(from=0, to=10, by=1)

hyper_params = list( laplace = laplace_opts)


# Search a random subset of these hyper-parmameters. Max runtime 
# and max models are enforced, and the search will stop after we 
# don't improve much over the best 5 random models.
search_criteria = list(strategy = "RandomDiscrete", 
                       max_runtime_secs = 5*60, 
                       max_models = 15, 
                       stopping_metric = "AUTO", 
                       stopping_tolerance = 0.001, 
                       stopping_rounds = 3, 
                       seed = 123456)

nb_grid <- h2o.grid("naivebayes", 
                     grid_id = "nb_grid",
                     x = c(predictors.categorical,predictors.continuous), 
                     y = "converted",
                     # faster to use a 80/20 split
                     training_frame = dbtrainh2o,
                    fold_assignment="Modulo",
                     nfolds = 5,
                     stopping_metric = "AUC",
                    ## seed to control the sampling of the 
                     ## Cartesian hyper-parameter space:
                     seed = 123456,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

nb_sorted_grid <- h2o.getGrid(grid_id = "nb_grid", sort_by = "auc", decreasing = TRUE)
print(nb_sorted_grid)

best_nb <- h2o.getModel(nb_sorted_grid@model_ids[[1]])
summary(best_nb)
myModelNB<-h2o.saveModel(best_nb,path = "./H2OModels/",force=TRUE)
