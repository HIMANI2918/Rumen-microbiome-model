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

```{r}
svmLearn = makeLearner("classif.svm", predict.type = "prob")
svmConstant <- makeRemoveConstantFeaturesWrapper(svmLearn)
svmDummy <- svmLearn %>% 
  makeDummyFeaturesWrapper() %>%
  makeRemoveConstantFeaturesWrapper()

PSsvm <- makeParamSet(
  makeNumericParam("cost", lower=0, upper=5, trafo = function(x) 10^x),
  makeNumericParam("gamma", lower=-5, upper = -1, trafo = function(x)10^x)
)

contrlSVM <-makeTuneControlMBO(budget=50)
tunedSVM <- tuneParams(svmDummy, HSTrain, par.set = PSsvm, control=contrlSVM, cv5, measures = msrs)

svmDummy <- setHyperPars(svmDummy, par.vals = tunedSVM$x)
svmTrain <- mlr::train(svmDummy, HSTrain)
#getting accuracy of trained model
predsvmTrain <- predict(svmTrain, HSTrain)
roc_measures_train <- calculateROCMeasures(predsvmTrain)
print(roc_measures_train)  
#getting accuracy of trained model
predSVM <- predict(svmTrain, HSTest)
print(calculateROCMeasures(predSVM))


```