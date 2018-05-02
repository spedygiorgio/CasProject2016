source(file="./Code/3H2OConfigure.R")


#initialize the layers grid
# 120 number distinct categories
# 20 jugdmental number of minimum layers
my_hidden_architecture<-list()
max.models= 500
for (i in 1:max.models) {
  rand_numlayers <- sample(2:3,1)
  rand_hidden <- c(sample(120:20, rand_numlayers, T))
  my_hidden_architecture[[i]]<-rand_hidden
}


#first "wide" grid search

hyper_params <- list(
    activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout",
                 "TanhWithDropout","MaxoutWithDropout"),
    hidden=my_hidden_architecture,
    input_dropout_ratio=c(0,0.05,0.1,.2),
    l1=seq(0,1e-4,1e-6),
    l2=seq(0,1e-4,1e-6),
    rho=seq(from=0.9,to=0.999,length.out = 5),
    epsilon=10^-(seq(from=8,to=4))
  )
 # ## defining search criteria ofver the grid
  search_criteria = list(strategy = "RandomDiscrete",
                         max_runtime_secs = 60*60*4,
                         max_models = max.models,
                         seed=1234567,
                         stopping_rounds=5,
                         stopping_tolerance=1e-2)
 dl_grid_1 <- h2o.grid(
   algorithm="deeplearning",
   grid_id = "dl_grid_1",
   x = c(predictors.categorical,predictors.continuous), 
   y = "converted", 
   training_frame = dbtrainh2o,
   #nfolds=nfolds_par,
   epochs=50,
   stopping_metric="logloss",
   stopping_tolerance=1e-3,
   stopping_rounds=2,
   score_validation_samples=10000,
   score_duty_cycle=0.025,   # don't score more than 2.5% of the total time
   max_w2=20,  # can help improve stability for Rectifier
   hyper_params = hyper_params,
   search_criteria = search_criteria
 )
 dl_grid_1 <- h2o.getGrid("dl_grid_1",sort_by="logloss",decreasing=FALSE)
 dl_grid_1

best_model_1 <- h2o.getModel(dl_grid_1@model_ids[[1]]) ## model with highest auc
best_model_1

grid1_model=h2o.loadModel(path = "./H2OModels/DEEPLEARNING/final_dl")



#second more refined grid

search_criteria = list(strategy = "RandomDiscrete",
                       max_runtime_secs = 60*60*12,
                       max_models = max.models,
                       seed=1234567,
                       stopping_rounds=4,
                       stopping_tolerance=1e-4)



numlayers <- length( grid1_model@parameters$hidden)
input_dropout_ratio_pars<-grid1_model@parameters$input_dropout_ratio
activation_pars<-grid1_model@parameters$activation
epsilon_pars<-10^seq(from=-8,to=-6,length.out = 10)
l1_pars=seq(from=2, to=4, length.out = 15) * 1e-5
l2_pars=seq(from=5, to=6, length.out = 15) * 1e-5
rho_pars=seq(from=0.999,to=0.9999, length.out = 15)
max_w2_pars=10^seq(0.7,2,length.out = 10)

hidden_opts<-list(grid1_model@parameters$hidden)
hidden_dropout_ratios_pars<-list()
for (i in 1:max.models) {
  rand_hidden <- runif(n=numlayers,min=0,max=0.5)
  hidden_dropout_ratios_pars[[i]]<-rand_hidden
}

# 
 hyper_params2 <- list(
   activation=activation_pars,
   hidden=hidden_opts,
   input_dropout_ratio=input_dropout_ratio_pars,
   hidden_dropout_ratios=hidden_dropout_ratios_pars,
   l1=l1_pars,
   l2=l2_pars,
   rho=rho_pars,
   epsilon=epsilon_pars,
   balance_classes=c(TRUE,FALSE),
   max_w2=max_w2_pars
 )
 
# 
# 
dl_grid_2 <- h2o.grid(
  algorithm="deeplearning",
  grid_id = "dl_grid_2",
  x = c(predictors.categorical,predictors.continuous),
  y = "converted",
  training_frame = dbtrainh2o,
  validation_frame = dbtesth2o,
  #nfolds = nfolds_par,
  stopping_metric="logloss",
  stopping_tolerance=1e-4,        # stop when logloss does not improve by >=1% for 2 scoring events
  stopping_rounds=4,
  epochs=50,
  hyper_params = hyper_params2,
  search_criteria = search_criteria
)
 dl_grid_2 <- h2o.getGrid("dl_grid_2",sort_by="logloss",decreasing=FALSE)
# dl_grid_2

final_dl <- h2o.getModel(dl_grid_2@model_ids[[1]]) ## model with lowest log - loss
final_dl

myModelDL<-h2o.saveModel(final_dl,path = "./H2OModels/DEEPLEARNING/",force=TRUE)
final_dl=h2o.loadModel(path = "./H2OModels/DEEPLEARNING/final_dl")

