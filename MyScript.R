# Read the dataset
rm(list=ls())
setwd("/Users/mohammad/Documents/GitHub/Mortality-Risk-Dashboard")

#_____________________________________Library___________________________________

#install.packages(c("dplyr", "ranger", "caret", "ggplot2"))
library(dplyr)
library(ranger)
library(caret)
library(ggplot2)
library(nnet)
library(xgboost)
library(janitor)
library(Metrics)
library(rpart)
library(rpart.plot)  
library(e1071)
library(class)
library(keras)
library(reticulate)
#_____________________________________Full Database_____________________________

hospdata<-read.csv("Hospital_Inpatient_Discharges__SPARCS_2022_20250313.csv",stringsAsFactors = F,header=T,sep=",")
head(hospdata)

hospdata <- hospdata[hospdata$APR.MDC.Description == "DISEASES AND DISORDERS OF THE CIRCULATORY SYSTEM", ]
hospdata$Length.of.Stay[hospdata$Length.of.Stay == "120 +"] <- 120
hospdata <- hospdata[!is.na(as.numeric(hospdata$Length.of.Stay)), ]

hospdata <- hospdata[hospdata$Type.of.Admission != "Newborn", ]
hospdata <- hospdata[hospdata$Type.of.Admission != "Not Available", ]

ny_hospdata <- hospdata
ny_hospdata$Length.of.Stay_Class <- cut(as.numeric(hospdata$Length.of.Stay), breaks=c(0, 3, 120), labels=c("Short", "Long"))
write.csv(ny_hospdata,"hospdata_2022.csv")
