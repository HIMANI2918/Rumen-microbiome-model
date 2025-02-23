---
  title: "Heat Stress Prediction"
author: "Himani Joshi"
date: "`r Sys.Date()`"
output:
  word_document: default
html_document: default
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)


library(mlr)
library(dplyr)
library(parallelMap)


Train <- read.csv("./Train_AP.csv", header=TRUE)
Test <- read.csv("./Test_AP.csv", header=TRUE)
Train$Response=as.factor(Train$Response)
Train$Sequencing_Depths=as.numeric(Train$Sequencing_Depths)
#convert Presence_Absence to factors
Train[,1:985]=lapply(Train[,1:985],factor)
str(Train)

Test$Response=as.factor(Test$Response)
Test$Sequencing_Depths=as.numeric(Test$Sequencing_Depths)
#convert Presence_Absence to factors
Test[,1:985]=lapply(Test[,1:985],factor)
str(Test)

HSTrain <- makeClassifTask(data=Train, target="Response")
HSTest <- makeClassifTask(data=Test, target="Response")
rfLearn <- makeLearner("classif.ranger", predict.type = "prob")


listMeasures("classif")
#possibilities: ber, bac, brier, mcc,kappa, f1
msrs = list(ber)


#Tuning the model
PSrf <- makeParamSet(
  makeIntegerParam("mtry", lower=2, upper=round(sum(HSTrain$task.desc$n.feat)/2))
)

contrlRF <-makeTuneControlGrid(resolution=round(sum(HSTrain$task.desc$n.feat)/2-1))
tunedRF <- tuneParams(rfLearn, HSTrain, par.set = PSrf, control=contrlRF, cv5, measures = msrs)
rfLearn <- setHyperPars(rfLearn, par.vals = list(mtry = tunedRF$x$mtry))
rfTrained <- train(rfLearn, HSTrain)
#getting accuracy of the trained model
rfPredTrain <- predict(rfTrained, HSTrain)
roc_measures_train <- calculateROCMeasures(rfPredTrain)
print(roc_measures_train)  
#getting accuracy of trained model on test data
rfPred <- predict(rfTrained, HSTest)
print(calculateROCMeasures(rfPred))


```

