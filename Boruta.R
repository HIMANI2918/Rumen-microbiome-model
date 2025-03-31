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
library(Boruta)
library(pROC)

HSData <- read.csv("./Absence_Presence.csv", header=TRUE)

#Boruta algorithm will be trained on the whole dataset for best feature selection
HSData$Response=as.factor(HSData$Response)
HSData$Animal_Age=as.factor(HSData$Animal_Age)
HSData$Lactation_Stage=as.factor(HSData$Lactation_Stage)
HSData$Study_ID=as.factor(HSData$Study_ID)
HSData$Sequencing_Depths=as.numeric(HSData$Sequencing_Depths)
#convert Presence_Absence to factors
HSData[,1:985]=lapply(HSData[,1:985],factor)
str(HSData)

Use Boruta to estimate feature importance
```{r run Boruta to select important features}
library(Boruta) 

features <- FullData[,-c(ncol(HSData):(ncol(HSData)-1))]

Li.train <- Boruta(y =(HSData[,ncol(HSData)]),
                   x = features,
                   doTrace = 0, #Verbosity, 0 = min, 3 = max
                   maxRuns=300, 
                   ntree=1000)

Li.final <- TentativeRoughFix(Li.train)
print(Li.final)
plot(Li.final)

#Remove the "Rejected" features
tmp <- Li.final$finalDecision=="Rejected"
data_Boruta <- features[,!tmp]

#Reattach the Species column
HSData <- cbind(Response = HSData$Response, data_Boruta)
```
List features ranked by importance
```{r}
mImp <- attStats(Li.final)
smImp <- order(mImp$meanImp, decreasing = T)
mImp[smImp[1:20],]
```
