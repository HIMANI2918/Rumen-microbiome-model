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
library(mlr3)
library(mlr3verse)
library(mlr3tuning)
library(future)
library(mlr3mbo)
library(mlr3viz)
library(mlr3extralearners)
library(mlr3pipelines)
library(bbotk)
library(kknn)
library(multcomp)
```
