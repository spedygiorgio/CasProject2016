#H2O AUTOML


source(file="./Code/3H2OConfigure.R")

final_automl<-h2o.automl(x = c(predictors.categorical,predictors.continuous), 
                         y = "converted", 
                         training_frame = dbtrainh2o,
                         validation_frame = NULL,
           leaderboard_frame = dbtesth2o, 
           max_runtime_secs = 60*60*3, stopping_metric ="logloss", 
           stopping_rounds = 3, seed = 123, project_name = "conversion_automl")

h2o.preds.automl<-predict(object = final_automl, newdata=dbtesth2o)$Y;  names(h2o.preds.automl)="probAutoML"

dbtest$convertedBin=0
dbtest[converted=='Y',convertedBin:=1]

ciao<-cbind(as.data.frame(h2o.preds.automl),actual=dbtest$convertedBin)
with(ciao, ModelMetrics::logLoss(actual=actual, predicted = probAutoML))

h2o.saveModel(object = final_automl@leader, path = paste(myworkdir,"./H2OModels/ENSEMBLES/","automl_leader",sep=''),force=TRUE)