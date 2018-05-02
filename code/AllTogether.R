rm(list=ls()) #clean working environment
workDirWork='C:/Users/UGA04625/Dropbox/Retention/CAS Call of Paper'
workDirSpedyHome='E:/Dropbox/Retention/CAS Call of Paper'
workDirNotebook='C:/Users/Giorgio1/Dropbox/Retention/CAS Call of Paper'
workDirChris='/Users/dutangc/drop-Giorgio/Dropbox/retention/CAS Call of Paper'
workDirSpedyHome2='D:/GiorgioDropbox/Dropbox/retention/CAS Call of Paper'
workDirSpedyWork="C:/Users/UGA04625/Dropbox/Retention/CAS Call of Paper"
#setwd(workDirChris)
workDir<-workDirSpedyHome
if(length(list.files(workDirChris)) > 0)
  setwd(workDirChris)
if(length(list.files(workDir)) > 0)
  setwd(workDir)

library(tidyverse)
library(dtplyr)
library(data.table)
library(pander)
library(caret)

set.seed(123) #sets seed
options("scipen"=100, "digits"=3) #remove scientific notation
options(contrasts=c("contr.treatment","contr.treatment")) #fissa i 
#source(file="./Code/1load.R") #prepare the data into RData from csv (to be run only once)
load(file="././data/R/conversionData.RData")
#source(file="./Code/1preprocess.R")
source(file="./Code/2createTrainAndTestSet.R") #split train and test
source(file="./Code/1descriptives.R") #perform descriptive
#source(file="./Code/3CaretConfigure.R")  #will be called by each model
#source(file="./Code/3H2OConfigure.R") #this function should be run once when a H2O model is wanted
source(file="./Code/4H2OGlmEstimation.R") #h2o model
source(file="./Code/4LinearDiscriminantAnalysis.R") #caret model (requires MASS)
source(file="./Code/4H2ODeepLearning.R") #h2o model
source(file="./Code/4FDA.R") #caret model, requires earth
source(file="./Code/4knn.R") #caret model, requires klaR
source(file="./Code/4NaiveBayesCaret.R") #caret model, requires e1071
source(file="./Code/4SvmRadial.R") #caret model, requires kernlab
source(file="./Code/4C50.R") #caret model, requires C5.0
source(file="./Code/4H2ODRFEstimation.R") #h2o model
source(file="./Code/4H2OGbmEstimation.R") #h2o model
source(file="./Code/4B_xgboost_mods.R") #xgboost (non caret)
source(file="./Code/5ModelComparison.R") #model comparisons...
source(file="./Code/6PricingOptimization.R") #pricing optimizal


#load(file="././data/R/conversionData.RData")
source(file="./Code/0utilityFunctions.R")  #utility function
source(file="./Code/2createTrainAndTestSet.R")
