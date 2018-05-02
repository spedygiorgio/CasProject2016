library(h2o)

h2o.init(nthreads = -1, max_mem_size="12g") 
h2o.removeAll() 

#loading the train set
up_train_resample=sample_frac(up_train,0.5)

dbtrainh2o <-  as.h2o(up_train_resample, destination_frame="dbtrainh2o")
#loading the test set
dbtesth2o <-  as.h2o(dbtest,destination_frame="dbtesth2o")
colnames(dbtesth2o)[20] <- "Class"

######################################################################
### parameter tuning with random search
######################################################################
models <- c()

for (i in 1:30) {
  rand_activation <- c("RectifierWithDropout", "MaxoutWithDropout","TanhWithDropout")[sample(1:3,1)]
  rand_numlayers <- sample(2:3,1)
  rand_hidden <- c(sample(25:300, rand_numlayers, T))
  rand_l1 <- runif(1, 0, 1e-3)
  rand_l2 <- runif(1, 0, 1e-2)
  rand_hidden_dropout <- c(runif(rand_numlayers, 0, 0.5))
  rand_input_dropout <- runif(1, 0, 0.2)
  rand_rho <- runif(1, 0.9, 0.999)
  rand_epsilon <- runif(1, 1e-10, 1e-4)
  # rand_rate <- runif(1, 0.005, 0.02)
  # rand_rate_decay <- runif(1, 0, 0.66)
  # rand_momentum <- runif(1, 0, 0.5)
  # rand_momentum_ramp <- runif(1, 1e-7, 1e-5)
  dlmodel <- h2o.deeplearning(x = 1:19, y = 20, 
                             training_frame = dbtrainh2o, 
                              validation_frame = dbtesth2o,
                              rho = rand_rho, epsilon = rand_epsilon, 
                              #rate = rand_rate,
                              #rate_decay = rand_rate_decay, 
                              nesterov_accelerated_gradient = T, 
                              #momentum_start = rand_momentum,
                              #momentum_ramp = rand_momentum_ramp,
                              epochs = 15, 
                              activation = rand_activation, 
                              hidden = rand_hidden, 
                              l1 = rand_l1, 
                              l2 = rand_l2,
                              input_dropout_ratio = rand_input_dropout, 
                              hidden_dropout_ratios = rand_hidden_dropout 
                                )
  models <- c(models, dlmodel)
}

# Find the best model (lowest logloss on the validation holdout set)
best_err <- 1e3 
for (i in 1:length(models)) {
  err <- h2o.logloss( h2o.performance(models[[i]], dbtesth2o))
  if (err < best_err) {
    best_err <- err
    best_model <- models[[i]]
  }
}

h2o.shutdown(prompt=FALSE)
