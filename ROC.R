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
```

