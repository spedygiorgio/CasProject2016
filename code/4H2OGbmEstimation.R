
source(file="./Code/3H2OConfigure.R")

#first run: train the depth

## Depth 10 is usually plenty of depth for most datasets, but you never know
 hyper_params = list( max_depth = seq(from=4,to=15) ) 
 search_criteria_0 = list(strategy = "RandomDiscrete", 
                       max_runtime_secs = 60*60*4,
                       max_models = 100,
                       stopping_metric = "AUTO",
                       stopping_tolerance = 0.0001,
                       stopping_rounds = 5,
                       seed = 123456)

#first grid defines original depth, some parameters kept to default
grid0 <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  ## which algorithm to run
  algorithm="gbm",
  search_criteria=search_criteria_0,
  ## identifier for the grid, to later retrieve it
  grid_id="grid0",
  ## standard model parameters
  x = c(predictors.categorical,predictors.continuous),
  y = "converted",
  # faster to use a 80/20 split
  training_frame = dbtrainh2o,
  validation_frame = dbtesth2o,
  ## more trees is better if the learning rate is small enough
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,
  ## learning rate annealing: learning_rate shrinks by 1% after every tree
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.995,
  ## sample 80% of rows per tree
  sample_rate = 0.8,
  ## sample 80% of columns per split
  col_sample_rate = 0.8,
  ## fix a random number generator seed for reproducibility
  seed = 1234,
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "logloss",
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,
  distribution="bernoulli"
)
# 
# ## by default, display the grid search results sorted by increasing logloss (since this is a classification task)
# 
# 

sortedGrid <- h2o.getGrid("grid0", sort_by="logloss", decreasing = FALSE)    
sortedGrid
 
final_gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
summary(final_gbm)
 
 
myModelGBM<-h2o.saveModel(final_gbm,path = "./H2OModels/GBM/",force=TRUE)
 
source(file="./Code/3H2OConfigure.R")


hyper_params_full = list( 
  ## restrict the search to the range of max_depth established above
  max_depth = seq(4,8,1),                                      
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.7,0.95,0.05),                                             
  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.7,0.95,0.05),                                         
  ## search a large space of column sampling rates per tree
  col_sample_rate_per_tree = c(0.9,1,.95),                                
  ## search a large space of how column sampling per split should change as a function of the depth of the split
  col_sample_rate_change_per_level = seq(0.9,1.0,0.01),                      
  ## search a large space of the number of min rows in a terminal node
  min_rows = c(8,10,12),                                 
  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = 2^seq(4,10,1),                                                     
  ## search a large space of the number of bins for split-finding for categorical columns
  #nbins_cats = 2^seq(4,12,1),                                                
  ## search a few minimum required relative error improvement thresholds for a split to happen
  min_split_improvement = c(0,1e-8,1e-6,1e-4),                               
  ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin","Random","AUTO"),
  balance_classes=c(FALSE,TRUE)
)


# Search a random subset of these hyper-parmameters. Max runtime 

search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",      
  ## limit the runtime to 60 minutes
  max_runtime_secs = 12*60*60,         
  ## build no more than 100 models
  max_models = 1000,                  
  ## random number generator seed to make sampling of parameter combinations reproducible
  seed = 1234,                        
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 5,

  stopping_metric = "logloss",
  stopping_tolerance = 1e-4
)

#run the second grid

conversion_gbm_grid <- h2o.grid("gbm", 
                     grid_id = "conversion_gbm_grid",
                     x = c(predictors.categorical,predictors.continuous), 
                     y = "converted", 
                     training_frame = dbtrainh2o,
                     nfolds = nfolds_par,
                     distribution="bernoulli",
                     stopping_rounds = 5,
                     stopping_tolerance = 1e-4,
                     stopping_metric = "logloss",
                     ntrees = 10000,
                     learn_rate=0.01,
                     learn_rate_annealing = 1, 
                     ## seed to control the sampling of the 
                     ## Cartesian hyper-parameter space:
                     seed = 123456,
                     hyper_params = hyper_params_full,
                     search_criteria = search_criteria)

gbm_sorted_grid <- h2o.getGrid(grid_id = "conversion_gbm_grid", sort_by = "logloss", decreasing = FALSE)
print(gbm_sorted_grid)

final_gbm <- h2o.getModel(gbm_sorted_grid@model_ids[[1]])
final_gbm

h2o.saveModel(final_gbm,path = "./H2OModels/GBM/",force=TRUE)
final_gbm<-h2o.loadModel(path = "./H2OModels/GBM/final_gbm")
