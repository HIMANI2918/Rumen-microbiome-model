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
library(mlr3)
library(mlr3verse)
library(mlr3tuning)
library(future)
library(mlr3mbo)
library(mlr3viz)
library(mlr3extralearners)
```

```{r Read in data, combine preprocess and create task}
HSData <- read.csv("./Absence_Presence.csv", header=TRUE)
HSData$Response=as.factor(HSData$Response)
HSData$Animal_Age=as.factor(HSData$Animal_Age)
HSData$Lactation_Stage=as.factor(HSData$Lactation_Stage)
HSData$Study_ID=as.factor(HSData$Study_ID)
HSData$Sequencing_Depths=as.numeric(HSData$Sequencing_Depths)
HSData[,1:985]=lapply(HSData[,1:985],factor)
str(HSData)

task = as_task_classif(HSData, target = "Response")
task$col_roles$stratum = task$target_names  
```


lrn_ranger = lrn("classif.ranger", importance = "impurity")
flt_importance = flt("importance", learner = lrn_ranger)
flt_importance$calculate(task)
as.data.table(flt_importance)
lrn_ranger$train(task)

keep = names(head(flt_importance$scores, 20))
task2 = task$clone()
task2$select(keep)
task2$feature_names
```

library(mlr3pipelines)
importance_ranger = lrn("classif.ranger", importance="impurity", predict_type="prob")


removeConstants = po("removeconstants")

graph = po("filter", filter = flt("importance", learner=importance_ranger), filter.frac=to_tune(0.1,1)) %>>%
  po("learner", mlr3::lrn("classif.ranger", predict_type="prob"), mtry.ratio =to_tune(0.05, 1))
graph$plot(horizontal = T)
glrnImRF = as_learner(graph)

graph2 = po("learner", mlr3::lrn("classif.ranger", predict_type="prob"), mtry.ratio=to_tune(0.05,1))
glrnRF = as_learner(graph2)


graph4 = removeConstants %>>% po("filter", filter = flt_importance, filter.frac=to_tune(0.1,1)) %>>%
  po("learner", mlr3::lrn("classif.kknn", predict_type="prob"), k=to_tune(1,15), 
     kernel = to_tune(c("rectangular", 
                        "triangular",
                        "gaussian",
                        "optimal")))
glrnKnn = as_learner(graph4)

lrn_svm = lrn("classif.svm" , predict_type="prob", scale=F,
              type = "C-classification",
              kernel = "radial",
              gamma = to_tune(1e-4, 1, logscale=T),
              cost = to_tune(1e-1, 1e5, logscale=T)
)

dummy= po("encode", method = "one-hot",
          affect_columns = selector_type("factor"),
          id = "dummify")

graphSVM = removeConstants %>>% dummy %>>% po("learner", lrn_svm)
glrnSVM = as_learner(graphSVM)

graph3 = removeConstants %>>% po("filter", filter = flt_importance, filter.frac=to_tune(0.1,1)) %>>%
  dummy %>>% po("learner", lrn_svm)
graph3$plot(horizontal=T)
glrnImSVM = as_learner(graph3)

graph5 = removeConstants %>>% po("filter", filter = flt_importance, filter.frac=to_tune(0.1,1)) %>>% po("learner", mlr3::lrn("classif.lda", predict_type="prob"))
glrnlda = as_learner(graph5)


graph6 = po("learner", mlr3::lrn("classif.cv_glmnet", predict_type="prob"))
glrnglm = as_learner(graph6)

graph7 = removeConstants %>>% po("filter", filter = flt_importance, filter.frac=to_tune(0.1,1)) %>>% po("learner", mlr3::lrn("classif.log_reg", predict_type="prob"))
glrnlog = as_learner(graph7)

graph8 = removeConstants %>>% po("filter", filter = flt_importance, filter.frac=to_tune(0.1,1)) %>>% po("learner", mlr3::lrn("classif.featureless", predict_type="prob", method="sample"))
glrnftl = as_learner(graph8)


resampling_outer = rsmp("repeated_cv", folds = 5, repeats=40)
resampling_inner = rsmp("cv", folds = 5)

```

```{r}

library(bbotk)
library(kknn)
tuner = TunerMbo$new(
  acq_optimizer =acqo(opt("random_search"), terminator = trm("evals", n_evals=50))
)

measure = msr("classif.bacc")
terminator = trm("evals", n_evals = 30)
atImRF = AutoTuner$new(
  learner = glrnImRF, 
  resampling = resampling_inner,
  measure = measure,
  terminator = terminator,
  tuner = tuner,
  store_models = TRUE)

atRF = AutoTuner$new(
  learner = glrnRF,       
  resampling = resampling_inner,
  measure = measure,
  terminator = terminator,
  tuner = tuner,
  store_models = TRUE)

atImSVM = AutoTuner$new(
  learner = glrnImSVM,       
  resampling = resampling_inner,
  measure = measure,
  terminator = terminator,
  tuner = tuner,
  store_models = TRUE)

atSVM = AutoTuner$new(
  learner = glrnSVM,       
  resampling = resampling_inner,
  measure = measure,
  terminator = terminator,
  tuner = tuner,
  store_models = TRUE)

atKnn = AutoTuner$new(
  learner = glrnKnn,       
  resampling = resampling_inner,
  measure = measure,
  terminator = terminator,
  tuner = tuner,
  store_models = TRUE)

atlda = AutoTuner$new(
  learner = glrnlda,       
  resampling = resampling_inner,
  measure = measure,
  terminator = terminator,
  tuner = tnr("random_search"),
  store_models = TRUE)

atglm = AutoTuner$new(
  learner = glrnglm,       
  resampling = resampling_inner,
  measure = measure,
  terminator = terminator,
  tuner = tnr("random_search"),
  store_models = TRUE)

atlog = AutoTuner$new(
  learner = glrnlog,       
  resampling = resampling_inner,
  measure = measure,
  terminator = terminator,
  tuner = tuner,
  store_models = TRUE)

atftl = AutoTuner$new(
  learner = glrnftl,       
  resampling = resampling_inner,
  measure = measure,
  terminator = terminator,
  tuner = tuner,
  store_models = TRUE)

learners = c(atImRF,atImSVM, atKnn, atlda, atftl) 
design = benchmark_grid(task, learners, resamplings = resampling_outer)
future::plan("multisession", workers=availableCores()/1.5)

bmr = benchmark(design, store_models = F)
dt2 = as.data.table(bmr$aggregate(msr("classif.acc")))
bmr$score(msr("classif.acc"))
dt=as.data.frame(extract_inner_tuning_results(bmr))
sd(1-bmr$resample_results$resample_result[[1]]$score()$classif.ce)
sd(dt[dt$experiment==1,]$classif.bacc)
nLearners = length(learners)
for (j in 1:nLearners){
  dt2$SEM[j] =  sd(1-bmr$resample_results$resample_result[[j]]$score()$classif.ce)/
    sqrt(bmr$resample_results$resample_result[[j]]$iters)
}

```

```{r Frequentist analysis of results using binomial glm}
t <- bmr$score()
dat <- data.frame(learn=t$learner_id, acc=(1-t$classif.ce))

rows_to_fix <- dat[["learn"]] %in% c("importance.classif.ranger.tuned")
dat[["learn"]][rows_to_fix] <- "ranger"

rows_to_fix <- dat[["learn"]] %in% c("removeconstants.importance.dummify.classif.svm.tuned")
dat[["learn"]][rows_to_fix] <- "svm"

rows_to_fix <- dat[["learn"]] %in% c("removeconstants.importance.classif.kknn.tuned")
dat[["learn"]][rows_to_fix] <- "kknn"

rows_to_fix <- dat[["learn"]] %in% c("removeconstants.importance.classif.lda.tuned")
dat[["learn"]][rows_to_fix] <- "lda"

rows_to_fix <- dat[["learn"]] %in% c("removeconstants.importance.classif.featureless.tuned")
dat[["learn"]][rows_to_fix] <- "featureless"


for( j in 1:nrow(dat)){
  dat[j,3] = dat[j,2] * length(t$prediction[[j]]$data$truth)
  dat[j,4] = length(t$prediction[[j]]$data$truth) - dat[j,3]
  dat[j,5] = length(t$prediction[[j]]$data$truth)
}


library(multcomp)
y <- cbind(dat$V3, dat$V4)
dat$learn <- as.factor(dat$learn)
mylogit <- glm(y ~learn, data = dat, family = "binomial")
my.mod.mc=glht(mylogit, mcp(learn="Tukey"))


summary(my.mod.mc)
cld(my.mod.mc, level=0.05, decreasing = T)


par(mar= c(3, 10, 2, 0) + 0.1)
plot(my.mod.mc)

```
