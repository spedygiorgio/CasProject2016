# premium and 
library(data.table)
require(tidyverse)

#summarise premium and loss cost data

premiumsTable<-dplyr::summarize(dbcalibration, 
                       companyPremium = mean(premiumCompany, na.rm = T),
                       marketPremium = mean(premiumMarket, na.rm = T),
                       burningCost = mean(burningCost, na.rm = T))

premiumCompanyPlot<-ggplot(dbcalibration,aes(premiumCompany))+geom_histogram(fill="steelblue",binwidth = 50)+labs(x="amount",y="frequency",title="Company Premium Distribution")
premiumMarketPlot<-ggplot(dbcalibration,aes(premiumMarket))+geom_histogram(fill="steelblue",binwidth = 50)+labs(x="amount",y="frequency",title="Market Premium Distribution")
burningCostPlot<-ggplot(dbcalibration,aes(premiumMarket))+geom_histogram(fill="steelblue",binwidth = 50)+labs(x="amount",y="frequency",title="Burning Cost Distribution")
conversionTable<-group_by(dbcalibration,converted) %>% summarise(Num=length(premiumCompany)) %>% mutate(Freq=Num/nrow(dbcalibration))
