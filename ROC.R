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


```{r}
library(pROC)
#If using two algorithm random forest(rf) and support vector machine(svm)
roc_rose <- plot(roc(predSVM$data$truth, predSVM$data$prob.Heat_Stress), print.auc = TRUE, col = "blue", 
                 lty=1,print.auc.cex=1.2)
roc_rose2 <- plot(roc(rfPred$data$truth, rfPred$data$prob.Heat_Stress), print.auc = TRUE, 
                  col = "green", print.auc.y = .4, add = TRUE, lty=2, print.auc.cex=1.2)

legend("topleft",
       legend=c("SVM", "RF"),
       col=c("blue", "green"),
       lty = c(1,2),
       lwd=2, cex =1, xpd = TRUE, horiz = TRUE)

#Do these two auc values differ?
print(roc.test(roc_rose, roc_rose2))


#Prediction model using only one algorithm, for example, random forest(rf)

roc_rose <- plot(roc(rfPred2$data$truth, rfPred2$data$prob.Heat_Stress), 
                 print.auc = TRUE, col = "blue", lty=1, 
                 print.auc.x = 0.8, print.auc.y = 0.6, print.auc.cex=1.2)     #Model 1
roc_rose2 <- plot(roc(rfPred3$data$truth, rfPred3$data$prob.Heat_Stress), 
                  print.auc = TRUE, col = "red", lty=2, add = TRUE, 
                  print.auc.x = 0.8, print.auc.y = 0.5, print.auc.cex=1.2)    #Model 2

roc_rose3 <- plot(roc(rfPred$data$truth, rfPred$data$prob.Heat_Stress), 
                  print.auc = TRUE, col = "green", lty=2, add = TRUE, 
                  print.auc.x = 0.8, print.auc.y = 0.4, print.auc.cex=1.2)    #Model 3


legend("topleft",
       legend=c("Model 1", "Model 2", "Model 3"),
       col=c("blue", "red", "green"),
       lty = c(1,2),
       lwd=2, 
       cex =0.7, 
       xpd = TRUE, 
       horiz = TRUE)

#Do these two auc values differ?
print(roc.test(roc_rose, roc_rose2))
print(roc.test(roc_rose, roc_rose3))
print(roc.test(roc_rose2, roc_rose3))
```

