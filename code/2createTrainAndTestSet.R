
######################################
#Crea train e test set
######################################
load(file="./Data/R/conversionDataPreprocessed.RData")
#calibration test: select data only that will be available for modelign
dbcalibration<-subset(dbconversion, burningCost>0 )
quotesCalibration<-dbcalibration$quoteId
#defining train and test set
dbtest<-subset(dbconversion, !(quoteId %in% quotesCalibration  )) %>% sample_frac(size=.20)
quoteTest<-dbtest$quoteId
dbtrain<-subset(dbconversion, !(quoteId %in% c(quoteTest,quotesCalibration) )  )
#creating a small data set
dbtrainsmall<-sample_n(dbconversion,size=50e3)

rm(dbconversion)

###########################
#DEFINING VARIABLES GROUPINGS
##########################

#list of native categorical predictors
predictors.categorical<-c("bonusMalus","policyLimit","vehicleFuelType",
                          "territoryBigCity","policyholderGender","policyholderOccupation","policyholderMaritalStatus",
                          "previousCompany","policyholderTerritory","vehicleKm","vehicleUsage",
                          "vehicleMakeAndModel"
)

#list of native numerical predictors
predictors.continuous<-c("vehiclePower","vehicleAge","vehiclePurchaseAge","policyholderAge","quoteTimeToPolicy",
                         "ratioCompanyMkt","deltaCompanyMkt")

#list of binned numerical predictors
predictors.binned<-c("vehiclePowerBin", "vehicleAgeBin", "policyholderAgeBin",
                     "quoteTimeToPolicyBin", "ratioCompanyMktBin", "deltaCompanyMktBin")

#list of dummies for models that do not encode categorical predictors
predictors.binned.dummified<-grep(pattern="Bin\\.",x=colnames(dbtrain),value=TRUE)
predictors.dummy<-grep(pattern="\\.",x=colnames(dbtrain),value=TRUE)

predictors.categorical.dummy<-setdiff(predictors.dummy,predictors.binned.dummified)