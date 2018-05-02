####################################
#LOADING AND PREPROC  #############
####################################


#reading data base
dbconversion_orig <- fread(input = "./data/Raw/dbconversion.csv", na.strings="")
#small fixings
dbconversion_orig[bonusMalus==0,bonusMalus:=1]
setnames(x=dbconversion_orig,"BURNINGCOST","burningCost")

#making rare levels to be absorbed by more frequent ones (using data.table syntax)
dbconversion_orig[vehicleFuelType==3,vehicleFuelType:=4] #moving eletric vehicles to Lpl (very few cases)
dbconversion_orig[policyLimit==1012,policyLimit:=1011] #moving policy limit top plus in top (very few cases)
dbconversion_orig[bonusMalus>15,bonusMalus:=15] #rare bonus malus classes

#recoding factors
## not using ordered factors caused not read by h2o
dbconversion<-mutate(dbconversion_orig,
                     converted=factor(convertedYn,levels=c(0,1),labels=c("N","Y")),
                     territoryBigCity=factor(territoryBigCity,levels=c(0,1),labels=c("N","Y")),
                     policyLimit=factor(policyLimit, levels=c(1009:1011),labels=c('Minimum','Medium','Top')),
                     vehicleFuelType=factor(vehicleFuelType,levels =c(1,2,4,5),labels=c("Petrol","Diesel","Lpl","NaturalGas")),
                     policyholderPreviousClaims=factor(policyholderPreviousClaims, levels=c(0,1),labels=c("N","Y")),
                     policyholderGender=factor(policyholderGender),
                     vehicleUsage=factor(vehicleUsage),
                     previousCompany=factor(previousCompany),
                     policyholderTerritory=factor(policyholderTerritory),
                     policyholderOccupation=factor(policyholderOccupation),
                     policyholderMaritalStatus=factor(policyholderMaritalStatus),
                     territoryProvince=factor(territoryProvince),
                     #calculate prospect premium positions with respect to market
                     ratioCompanyMkt=premiumCompany/premiumMarket,
                     deltaCompanyMkt=premiumCompany-premiumMarket
                     )
dbconversion[,convertedYn:=NULL]

library(sqldf)
#determining most important vehicles
load(file="./data/R/marcheModelli.RData")
setnames(marcheModelliBig,"marca","vehicleMake")
setnames(marcheModelliBig,"modello","vehicleModel")
setnames(marcheModelliBig,"marcaModello","vehicleMakeAndModel")

temp1<-as(dbconversion, "data.frame")
temp2<-as(marcheModelliBig, "data.frame")
temp<-merge(x=temp1,y=temp2,all.x=TRUE,by=c("vehicleMake","vehicleModel"))
rm(temp1,temp2)
dbconversion<-as(temp,"data.table");rm(temp)
dbconversion[is.na(vehicleMakeAndModel),vehicleMakeAndModel:='OTHER']

dbconversion[,vehicleMakeAndModel:=factor(vehicleMakeAndModel,ordered=FALSE)]
dbconversion[,vehicleMakeNew:=vehicleMake]
dbconversion[,vehicleModelNew:=vehicleModel]
dbconversion[as.character(vehicleMake)=='OTHER',vehicleMakeNew:='OTHER']
dbconversion[as.character(vehicleModel)=='OTHER',vehicleModelNew:='OTHER']
dbconversion[,vehicleMakeNew:=factor(vehicleMakeNew)]
dbconversion[,vehicleModelNew:=factor(vehicleModelNew)]

#kills unuseful variables

dbconversion[,c("territoryProvince","kmRec","vehicleMake","vehicleMakeNew",
                "vehicleModelNew","vehicleModel"):=NULL]
#saving final data
save(list=c("dbconversion"),file="./data/R/conversionData.RData",compress="xz")
