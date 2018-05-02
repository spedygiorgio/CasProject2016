##############################################
# PREPROCESSING STAGE#########################
##############################################
load(file="./Data/R/conversionData.RData")
#creating a small data set to test pre - processing
#dbconversion.small<-sample_frac(dbconversion,size = .2)
require(caret)

#identify strongly correlated variables
var2check<-c("premiumCompany", "premiumMarket","vehiclePower","vehicleAge", "vehiclePurchaseAge",
             "policyholderAge","quoteTimeToPolicy","vehicleKm")
myCorr<-cor(x=as.matrix(dbconversion[,var2check,with=FALSE]))
highCorr <- findCorrelation(myCorr, cutoff = .85)
var2check[highCorr] #we keep all

#identify zero var
nzvVars<-nearZeroVar(dbconversion)
names(dbconversion)[nzvVars]

dbconversion[,policyholderPreviousClaims:=NULL]


#################################################
#creating a function to perform bivariate summaries

#see the function bivariateSummary in 0utilityFunctions.R

# checking the values of some variables

#  bivariateSummary(varname = "policyLimit",db=dbconversion.small) # maybe Eletric to be collapsed
#  bivariateSummary(varname = "territoryBigCity",db=dbconversion.small)   #ok
#  bivariateSummary(varname = "policyholderOccupation",db=dbconversion.small)  #ok
#  bivariateSummary(varname = "policyholderMaritalStatus",db=dbconversion.small) #ok 
#  bivariateSummary(varname = "previousCompany",db=dbconversion.small) #ok
#  bivariateSummary(varname = "policyholderTerritory",db=dbconversion.small) #ok
#  bivariateSummary(varname = "vehicleKm",db=dbconversion.small) #ok
#  bivariateSummary(varname = "vehicleUsage",db=dbconversion.small) #ok
#  bivariateSummary(varname = "vehicleMakeAndModel",db=dbconversion.small) #ok




#################################################
#adding dummy indicators

dummyFier<-dummyVars(~policyLimit+vehicleFuelType+territoryBigCity+policyholderOccupation+
                       policyholderMaritalStatus+previousCompany+policyholderTerritory+vehicleUsage+vehicleMakeAndModel,
                     data=dbconversion,fullRank = TRUE)

#create dummy vars
dummyVars.df<-predict(dummyFier,newdata = dbconversion)
temp<-cbind(dbconversion,dummyVars.df)
#find dummy vars
allDummyVars<-grep(pattern="\\.",x=colnames(temp),value=TRUE)
dbconversion<-temp
rm(temp)
#################################################
#CREATE BINNED VARIABLES
dbconversion[,vehiclePowerBin:=Hmisc::cut2(vehiclePower,g=5)]
dbconversion[,vehicleAgeBin:=Hmisc::cut2(vehicleAge,g=5)]
dbconversion[,policyholderAgeBin:=Hmisc::cut2(policyholderAge,g=10)]
dbconversion[,quoteTimeToPolicyBin:=Hmisc::cut2(quoteTimeToPolicy,g=7)]
dbconversion[,ratioCompanyMktBin:=Hmisc::cut2(ratioCompanyMkt,g=10)]
dbconversion[,deltaCompanyMktBin:=Hmisc::cut2(deltaCompanyMkt,g=10)]

#identify binned variables
allBinnedVars<-grep(pattern="Bin$",x=colnames(dbconversion),value=TRUE, perl = TRUE)
formulForBinning<-paste("~",paste(allBinnedVars,collapse="+"),sep="")
dummyFier<-dummyVars(as.formula(formulForBinning),data=dbconversion,fullRank = TRUE)
dummyVars.df<-predict(dummyFier,newdata=dbconversion)
temp<-cbind(dbconversion,dummyVars.df)
dbconversion<-temp
rm(temp)


save(list="dbconversion",file="./Data/R/conversionDataPreprocessed.RData")