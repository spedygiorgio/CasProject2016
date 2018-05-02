rm(list=ls()) #clean working environment
workDirWork='C:\\Users\\UGA04625\\Dropbox\\Retention\\CAS Call of Paper\\'
workDirSpedyHome='E:\\Dropbox\\Retention\\CAS Call of Paper\\'
workDirNotebook='C:\\Users\\Giorgio1\\Dropbox\\Retention\\CAS Call of Paper\\'
workDirluca='C:\\Users\\LOMBARDI\\Dropbox\\retention\\CAS Call of Paper\\'
workDirSpedyHome2='D:\\GiorgioDropbox\\Dropbox\\retention\\CAS Call of Paper\\'
setwd(workDirSpedyHome)
setwd(workDirWork)

library(dplyr)
library(dtplyr)
library(data.table)
library(ggplot2)
library(pander)
library(caret)
#setting options
panderOptions('digits', 5)
panderOptions('round', 10)
panderOptions('keep.trailing.zeros', TRUE)

set.seed(123) #sets seed
options("scipen"=100, "digits"=3) #remove scientific notation
options(contrasts=c("contr.treatment","contr.treatment")) #fissa i 
#source(file="./Code/1load.R")
#load(file="././data/R/conversionData.RData")
source(file="./Code/0utilityFunctions.R")
source(file="./Code/2createTrainAndTestSet.R")
