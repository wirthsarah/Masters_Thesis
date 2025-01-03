#Modelling ----
#This file contains all models. As the models take hours to days to compute, I recommend splitting this file into several files and running them independently. Next File: Export.R

library("readr")
library("dplyr")
library("tidyr")
library("DescTools")
library("ranger")
library("mlr3")
library("mlr3tuning") 
library("mlr3learners") 
library("tibble")
library("purrr")
set.seed(131)

##define learner----
ranger <-lrn("regr.ranger", importance = "permutation", predict_type = "response", 
             respect.unordered.factors= to_tune(c("order","partition")))#might change importance later

##define tuning strategy----
tuner = tnr("random_search")

##for cefi----
### cefi fie ----
set.seed(131)
#load datasets
field_cefi<-read_csv2("model_data/field_cefi.csv")
#create task
task_fie_cefi <- as_task_regr(field_cefi, target="cefi", id="fie_cefi")

#define resampling strategy---
rsmp_fie_cefi = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_cefi$instantiate(task_fie_cefi, f = folds)

#create tuner
at_fie_cefi = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_cefi,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_cefi$train(task_fie_cefi)#

#Export
results_fie_cefi<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_cefi<-as.data.frame(list(model="fie_cefi",result = at_fie_cefi$tuning_result$respect.unordered.factors, rmse = at_fie_cefi$tuning_result$regr.rmse))

write_csv2(results_fie_cefi, file = "model_results/preliminary/results_fie_cefi.csv")

imp_fie_cefi <- tibble::rownames_to_column(data.frame(at_fie_cefi$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_cefi, file = "model_results/preliminary/imp_fie_cefi.csv")


### cefi lab ----
#load datasets
set.seed(131)
lab_cefi<-read_csv2("model_data/lab_cefi.csv")
#create task
task_lab_cefi <- as_task_regr(lab_cefi, target="cefi", id="lab_cefi")

#define resampling strategy---
rsmp_lab_cefi = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_cefi$instantiate(task_lab_cefi, f = folds)

#create tuner
at_lab_cefi = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_cefi,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_cefi$train(task_lab_cefi)#

#Export
results_lab_cefi<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_cefi<-as.data.frame(list(model="lab_cefi",result = at_lab_cefi$tuning_result$respect.unordered.factors, rmse = at_lab_cefi$tuning_result$regr.rmse))

write_csv2(results_lab_cefi, file = "model_results/preliminary/results_lab_cefi.csv")

imp_lab_cefi <- tibble::rownames_to_column(data.frame(at_lab_cefi$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_cefi, file = "model_results/preliminary/imp_lab_cefi.csv")



##for DI----
### DI fie ----
set.seed(131)
#load datasets
field_DI<-read_csv2("model_data/field_DI.csv")
#create task
task_fie_DI <- as_task_regr(field_DI, target="DI", id="fie_DI")

#define resampling strategy---
rsmp_fie_DI = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_DI$instantiate(task_fie_DI, f = folds)

#create tuner
at_fie_DI = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_DI,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_DI$train(task_fie_DI)#

#Export
results_fie_DI<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_DI<-as.data.frame(list(model="fie_DI",result = at_fie_DI$tuning_result$respect.unordered.factors, rmse = at_fie_DI$tuning_result$regr.rmse))

write_csv2(results_fie_DI, file = "model_results/preliminary/results_fie_DI.csv")

imp_fie_DI <- tibble::rownames_to_column(data.frame(at_fie_DI$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_DI, file = "model_results/preliminary/imp_fie_DI.csv")


### DI lab ----
#load datasets
set.seed(131)
lab_DI<-read_csv2("model_data/lab_DI.csv")
#create task
task_lab_DI <- as_task_regr(lab_DI, target="DI", id="lab_DI")

#define resampling strategy---
rsmp_lab_DI = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_DI$instantiate(task_lab_DI, f = folds)

#create tuner
at_lab_DI = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_DI,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_DI$train(task_lab_DI)#

#Export
results_lab_DI<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_DI<-as.data.frame(list(model="lab_DI",result = at_lab_DI$tuning_result$respect.unordered.factors, rmse = at_lab_DI$tuning_result$regr.rmse))

write_csv2(results_lab_DI, file = "model_results/preliminary/results_lab_DI.csv")

imp_lab_DI <- tibble::rownames_to_column(data.frame(at_lab_DI$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_DI, file = "model_results/preliminary/imp_lab_DI.csv")


##for DK_IBCH----
### DK_IBCH fie ----
set.seed(131)
#load datasets
field_DK_IBCH<-read_csv2("model_data/field_DK_IBCH.csv")
#create task
task_fie_DK_IBCH <- as_task_regr(field_DK_IBCH, target="DK_IBCH", id="fie_DK_IBCH")

#define resampling strategy---
rsmp_fie_DK_IBCH = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_DK_IBCH$instantiate(task_fie_DK_IBCH, f = folds)

#create tuner
at_fie_DK_IBCH = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_DK_IBCH,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_DK_IBCH$train(task_fie_DK_IBCH)#

#Export
results_fie_DK_IBCH<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_DK_IBCH<-as.data.frame(list(model="fie_DK_IBCH",result = at_fie_DK_IBCH$tuning_result$respect.unordered.factors, rmse = at_fie_DK_IBCH$tuning_result$regr.rmse))

write_csv2(results_fie_DK_IBCH, file = "model_results/preliminary/results_fie_DK_IBCH.csv")

imp_fie_DK_IBCH <- tibble::rownames_to_column(data.frame(at_fie_DK_IBCH$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_DK_IBCH, file = "model_results/preliminary/imp_fie_DK_IBCH.csv")


### DK_IBCH lab ----
#load datasets
set.seed(131)
lab_DK_IBCH<-read_csv2("model_data/lab_DK_IBCH.csv")
#create task
task_lab_DK_IBCH <- as_task_regr(lab_DK_IBCH, target="DK_IBCH", id="lab_DK_IBCH")

#define resampling strategy---
rsmp_lab_DK_IBCH = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_DK_IBCH$instantiate(task_lab_DK_IBCH, f = folds)

#create tuner
at_lab_DK_IBCH = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_DK_IBCH,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_DK_IBCH$train(task_lab_DK_IBCH)#

#Export
results_lab_DK_IBCH<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_DK_IBCH<-as.data.frame(list(model="lab_DK_IBCH",result = at_lab_DK_IBCH$tuning_result$respect.unordered.factors, rmse = at_lab_DK_IBCH$tuning_result$regr.rmse))

write_csv2(results_lab_DK_IBCH, file = "model_results/preliminary/results_lab_DK_IBCH.csv")

imp_lab_DK_IBCH <- tibble::rownames_to_column(data.frame(at_lab_DK_IBCH$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_DK_IBCH, file = "model_results/preliminary/imp_lab_DK_IBCH.csv")



##for dom_inter----
### dom_inter fie ----
set.seed(131)
#load datasets
field_dom_inter<-read_csv2("model_data2/field_dom_inter.csv")
#create task
task_fie_dom_inter <- as_task_regr(field_dom_inter, target="dom_inter", id="fie_dom_inter")

#define resampling strategy---
rsmp_fie_dom_inter = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_dom_inter$instantiate(task_fie_dom_inter, f = folds)

#create tuner
at_fie_dom_inter = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_dom_inter,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_dom_inter$train(task_fie_dom_inter)#

#Export
results_fie_dom_inter<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_dom_inter<-as.data.frame(list(model="fie_dom_inter",result = at_fie_dom_inter$tuning_result$respect.unordered.factors, rmse = at_fie_dom_inter$tuning_result$regr.rmse))

write_csv2(results_fie_dom_inter, file = "model_results/preliminary/results_fie_dom_inter.csv")

imp_fie_dom_inter <- tibble::rownames_to_column(data.frame(at_fie_dom_inter$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_dom_inter, file = "model_results/preliminary/imp_fie_dom_inter.csv")


### dom_inter lab ----
#load datasets
set.seed(131)
lab_dom_inter<-read_csv2("model_data2/lab_dom_inter.csv")
#create task
task_lab_dom_inter <- as_task_regr(lab_dom_inter, target="dom_inter", id="lab_dom_inter")

#define resampling strategy---
rsmp_lab_dom_inter = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_dom_inter$instantiate(task_lab_dom_inter, f = folds)

#create tuner
at_lab_dom_inter = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_dom_inter,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_dom_inter$train(task_lab_dom_inter)#

#Export
results_lab_dom_inter<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_dom_inter<-as.data.frame(list(model="lab_dom_inter",result = at_lab_dom_inter$tuning_result$respect.unordered.factors, rmse = at_lab_dom_inter$tuning_result$regr.rmse))

write_csv2(results_lab_dom_inter, file = "model_results/preliminary/results_lab_dom_inter.csv")

imp_lab_dom_inter <- tibble::rownames_to_column(data.frame(at_lab_dom_inter$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_dom_inter, file = "model_results/preliminary/imp_lab_dom_inter.csv")

##for dom_lentic----
### dom_lentic fie ----
set.seed(131)
#load datasets
field_dom_lentic<-read_csv2("model_data/field_dom_lentic.csv")
#create task
task_fie_dom_lentic <- as_task_regr(field_dom_lentic, target="dom_lentic", id="fie_dom_lentic")

#define resampling strategy---
rsmp_fie_dom_lentic = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))
rsmp_fie_dom_lentic$instantiate(task_fie_dom_lentic, f = folds)

#create tuner
at_fie_dom_lentic = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_dom_lentic,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_dom_lentic$train(task_fie_dom_lentic)

#Export
results_fie_dom_lentic<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_dom_lentic<-as.data.frame(list(model="fie_dom_lentic",result = at_fie_dom_lentic$tuning_result$respect.unordered.factors, rmse = at_fie_dom_lentic$tuning_result$regr.rmse))

write_csv2(results_fie_dom_lentic, file = "model_results/preliminary/results_fie_dom_lentic.csv")

imp_fie_dom_lentic <- tibble::rownames_to_column(data.frame(at_fie_dom_lentic$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_dom_lentic, file = "model_results/preliminary/imp_fie_dom_lentic.csv")


### dom_lentic lab ----
#load datasets
set.seed(131)
lab_dom_lentic<-read_csv2("model_data/lab_dom_lentic.csv")
#create task
task_lab_dom_lentic <- as_task_regr(lab_dom_lentic, target="dom_lentic", id="lab_dom_lentic")

#define resampling strategy---
rsmp_lab_dom_lentic = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_dom_lentic$instantiate(task_lab_dom_lentic, f = folds)

#create tuner
at_lab_dom_lentic = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_dom_lentic,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_dom_lentic$train(task_lab_dom_lentic)#

#Export
results_lab_dom_lentic<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_dom_lentic<-as.data.frame(list(model="lab_dom_lentic",result = at_lab_dom_lentic$tuning_result$respect.unordered.factors, rmse = at_lab_dom_lentic$tuning_result$regr.rmse))

write_csv2(results_lab_dom_lentic, file = "model_results/preliminary/results_lab_dom_lentic.csv")

imp_lab_dom_lentic <- tibble::rownames_to_column(data.frame(at_lab_dom_lentic$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_dom_lentic, file = "model_results/preliminary/imp_lab_dom_lentic.csv")

##for dom_lotic----
### dom_lotic fie ----
set.seed(131)
#load datasets
field_dom_lotic<-read_csv2("model_data/field_dom_lotic.csv")
#create task
task_fie_dom_lotic <- as_task_regr(field_dom_lotic, target="dom_lotic", id="fie_dom_lotic")

#define resampling strategy---
rsmp_fie_dom_lotic = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_dom_lotic$instantiate(task_fie_dom_lotic, f = folds)

#create tuner
at_fie_dom_lotic = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_dom_lotic,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_dom_lotic$train(task_fie_dom_lotic)#

#Export
results_fie_dom_lotic<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_dom_lotic<-as.data.frame(list(model="fie_dom_lotic",result = at_fie_dom_lotic$tuning_result$respect.unordered.factors, rmse = at_fie_dom_lotic$tuning_result$regr.rmse))

write_csv2(results_fie_dom_lotic, file = "model_results/preliminary/results_fie_dom_lotic.csv")

imp_fie_dom_lotic <- tibble::rownames_to_column(data.frame(at_fie_dom_lotic$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_dom_lotic, file = "model_results/preliminary/imp_fie_dom_lotic.csv")


### dom_lotic lab ----
#load datasets
set.seed(131)
lab_dom_lotic<-read_csv2("model_data/lab_dom_lotic.csv")
#create task
task_lab_dom_lotic <- as_task_regr(lab_dom_lotic, target="dom_lotic", id="lab_dom_lotic")

#define resampling strategy---
rsmp_lab_dom_lotic = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_dom_lotic$instantiate(task_lab_dom_lotic, f = folds)

#create tuner
at_lab_dom_lotic = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_dom_lotic,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_dom_lotic$train(task_lab_dom_lotic)#

#Export
results_lab_dom_lotic<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_dom_lotic<-as.data.frame(list(model="lab_dom_lotic",result = at_lab_dom_lotic$tuning_result$respect.unordered.factors, rmse = at_lab_dom_lotic$tuning_result$regr.rmse))

write_csv2(results_lab_dom_lotic, file = "model_results/preliminary/results_lab_dom_lotic.csv")

imp_lab_dom_lotic <- tibble::rownames_to_column(data.frame(at_lab_dom_lotic$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_dom_lotic, file = "model_results/preliminary/imp_lab_dom_lotic.csv")



##for dom_surf----
### dom_surf fie ----
set.seed(131)
#load datasets
field_dom_surf<-read_csv2("model_data/field_dom_surf.csv")
#create task
task_fie_dom_surf <- as_task_regr(field_dom_surf, target="dom_surf", id="fie_dom_surf")

#define resampling strategy---
rsmp_fie_dom_surf = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_dom_surf$instantiate(task_fie_dom_surf, f = folds)

#create tuner
at_fie_dom_surf = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_dom_surf,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_dom_surf$train(task_fie_dom_surf)#

#Export
results_fie_dom_surf<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_dom_surf<-as.data.frame(list(model="fie_dom_surf",result = at_fie_dom_surf$tuning_result$respect.unordered.factors, rmse = at_fie_dom_surf$tuning_result$regr.rmse))

write_csv2(results_fie_dom_surf, file = "model_results/preliminary/results_fie_dom_surf.csv")

imp_fie_dom_surf <- tibble::rownames_to_column(data.frame(at_fie_dom_surf$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_dom_surf, file = "model_results/preliminary/imp_fie_dom_surf.csv")


### dom_surf lab ----
#load datasets
set.seed(131)
lab_dom_surf<-read_csv2("model_data/lab_dom_surf.csv")
#create task
task_lab_dom_surf <- as_task_regr(lab_dom_surf, target="dom_surf", id="lab_dom_surf")

#define resampling strategy---
rsmp_lab_dom_surf = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_dom_surf$instantiate(task_lab_dom_surf, f = folds)

#create tuner
at_lab_dom_surf = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_dom_surf,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_dom_surf$train(task_lab_dom_surf)#

#Export
results_lab_dom_surf<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_dom_surf<-as.data.frame(list(model="lab_dom_surf",result = at_lab_dom_surf$tuning_result$respect.unordered.factors, rmse = at_lab_dom_surf$tuning_result$regr.rmse))

write_csv2(results_lab_dom_surf, file = "model_results/preliminary/results_lab_dom_surf.csv")

imp_lab_dom_surf <- tibble::rownames_to_column(data.frame(at_lab_dom_surf$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_dom_surf, file = "model_results/preliminary/imp_lab_dom_surf.csv")

##for dom_surf_lentic----
### dom_surf_lentic fie ----
set.seed(131)
#load datasets
field_dom_surf_lentic<-read_csv2("model_data2/field_dom_surf_lentic.csv")
#create task
task_fie_dom_surf_lentic <- as_task_regr(field_dom_surf_lentic, target="dom_surf_lentic", id="fie_dom_surf_lentic")

#define resampling strategy---
rsmp_fie_dom_surf_lentic = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_dom_surf_lentic$instantiate(task_fie_dom_surf_lentic, f = folds)

#create tuner
at_fie_dom_surf_lentic = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_dom_surf_lentic,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_dom_surf_lentic$train(task_fie_dom_surf_lentic)#

#Export
results_fie_dom_surf_lentic<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_dom_surf_lentic<-as.data.frame(list(model="fie_dom_surf_lentic",result = at_fie_dom_surf_lentic$tuning_result$respect.unordered.factors, rmse = at_fie_dom_surf_lentic$tuning_result$regr.rmse))

write_csv2(results_fie_dom_surf_lentic, file = "model_results/preliminary/results_fie_dom_surf_lentic.csv")

imp_fie_dom_surf_lentic <- tibble::rownames_to_column(data.frame(at_fie_dom_surf_lentic$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_dom_surf_lentic, file = "model_results/preliminary/imp_fie_dom_surf_lentic.csv")


### dom_surf_lentic lab ----
#load datasets
set.seed(131)
lab_dom_surf_lentic<-read_csv2("model_data2/lab_dom_surf_lentic.csv")
#create task
task_lab_dom_surf_lentic <- as_task_regr(lab_dom_surf_lentic, target="dom_surf_lentic", id="lab_dom_surf_lentic")

#define resampling strategy---
rsmp_lab_dom_surf_lentic = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_dom_surf_lentic$instantiate(task_lab_dom_surf_lentic, f = folds)

#create tuner
at_lab_dom_surf_lentic = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_dom_surf_lentic,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_dom_surf_lentic$train(task_lab_dom_surf_lentic)#

#Export
results_lab_dom_surf_lentic<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_dom_surf_lentic<-as.data.frame(list(model="lab_dom_surf_lentic",result = at_lab_dom_surf_lentic$tuning_result$respect.unordered.factors, rmse = at_lab_dom_surf_lentic$tuning_result$regr.rmse))

write_csv2(results_lab_dom_surf_lentic, file = "model_results/preliminary/results_lab_dom_surf_lentic.csv")

imp_lab_dom_surf_lentic <- tibble::rownames_to_column(data.frame(at_lab_dom_surf_lentic$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_dom_surf_lentic, file = "model_results/preliminary/imp_lab_dom_surf_lentic.csv")


##for Egat----
### Egat fie ----
set.seed(131)
#load datasets
field_Egat<-read_csv2("model_data/field_Egat.csv")
#create task
task_fie_Egat <- as_task_regr(field_Egat, target="Egat", id="fie_Egat")

#define resampling strategy---
rsmp_fie_Egat = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_Egat$instantiate(task_fie_Egat, f = folds)

#create tuner
at_fie_Egat = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_Egat,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_Egat$train(task_fie_Egat)#

#Export
results_fie_Egat<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_Egat<-as.data.frame(list(model="fie_Egat",result = at_fie_Egat$tuning_result$respect.unordered.factors, rmse = at_fie_Egat$tuning_result$regr.rmse))

write_csv2(results_fie_Egat, file = "model_results/preliminary/results_fie_Egat.csv")

imp_fie_Egat <- tibble::rownames_to_column(data.frame(at_fie_Egat$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_Egat, file = "model_results/preliminary/imp_fie_Egat.csv")


### Egat lab ----
#load datasets
set.seed(131)
lab_Egat<-read_csv2("model_data/lab_Egat.csv")
#create task
task_lab_Egat <- as_task_regr(lab_Egat, target="Egat", id="lab_Egat")

#define resampling strategy---
rsmp_lab_Egat = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_Egat$instantiate(task_lab_Egat, f = folds)

#create tuner
at_lab_Egat = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_Egat,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_Egat$train(task_lab_Egat)#

#Export
results_lab_Egat<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_Egat<-as.data.frame(list(model="lab_Egat",result = at_lab_Egat$tuning_result$respect.unordered.factors, rmse = at_lab_Egat$tuning_result$regr.rmse))

write_csv2(results_lab_Egat, file = "model_results/preliminary/results_lab_Egat.csv")

imp_lab_Egat <- tibble::rownames_to_column(data.frame(at_lab_Egat$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_Egat, file = "model_results/preliminary/imp_lab_Egat.csv")


##for Ewei----
### Ewei fie ----
set.seed(131)
#load datasets
field_Ewei<-read_csv2("model_data/field_Ewei.csv")
#create task
task_fie_Ewei <- as_task_regr(field_Ewei, target="Ewei", id="fie_Ewei")

#define resampling strategy---
rsmp_fie_Ewei = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_Ewei$instantiate(task_fie_Ewei, f = folds)

#create tuner
at_fie_Ewei = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_Ewei,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_Ewei$train(task_fie_Ewei)#

#Export
results_fie_Ewei<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_Ewei<-as.data.frame(list(model="fie_Ewei",result = at_fie_Ewei$tuning_result$respect.unordered.factors, rmse = at_fie_Ewei$tuning_result$regr.rmse))

write_csv2(results_fie_Ewei, file = "model_results/preliminary/results_fie_Ewei.csv")

imp_fie_Ewei <- tibble::rownames_to_column(data.frame(at_fie_Ewei$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_Ewei, file = "model_results/preliminary/imp_fie_Ewei.csv")


### Ewei lab ----
#load datasets
set.seed(131)
lab_Ewei<-read_csv2("model_data/lab_Ewei.csv")
#create task
task_lab_Ewei <- as_task_regr(lab_Ewei, target="Ewei", id="lab_Ewei")

#define resampling strategy---
rsmp_lab_Ewei = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_Ewei$instantiate(task_lab_Ewei, f = folds)

#create tuner
at_lab_Ewei = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_Ewei,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_Ewei$train(task_lab_Ewei)#

#Export
results_lab_Ewei<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_Ewei<-as.data.frame(list(model="lab_Ewei",result = at_lab_Ewei$tuning_result$respect.unordered.factors, rmse = at_lab_Ewei$tuning_result$regr.rmse))

write_csv2(results_lab_Ewei, file = "model_results/preliminary/results_lab_Ewei.csv")

imp_lab_Ewei <- tibble::rownames_to_column(data.frame(at_lab_Ewei$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_Ewei, file = "model_results/preliminary/imp_lab_Ewei.csv")



##for hp_sen----
### hp_sen fie ----
set.seed(131)
#load datasets
field_hp_sen<-read_csv2("model_data2/field_hp_sen.csv")
#create task
task_fie_hp_sen <- as_task_regr(field_hp_sen, target="hp_sen", id="fie_hp_sen")

#define resampling strategy---
rsmp_fie_hp_sen = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_hp_sen$instantiate(task_fie_hp_sen, f = folds)

#create tuner
at_fie_hp_sen = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_hp_sen,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_hp_sen$train(task_fie_hp_sen)#

#Export
results_fie_hp_sen<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_hp_sen<-as.data.frame(list(model="fie_hp_sen",result = at_fie_hp_sen$tuning_result$respect.unordered.factors, rmse = at_fie_hp_sen$tuning_result$regr.rmse))

write_csv2(results_fie_hp_sen, file = "model_results/preliminary/results_fie_hp_sen.csv")

imp_fie_hp_sen <- tibble::rownames_to_column(data.frame(at_fie_hp_sen$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_hp_sen, file = "model_results/preliminary/imp_fie_hp_sen.csv")


### hp_sen lab ----
#load datasets
set.seed(131)
lab_hp_sen<-read_csv2("model_data2/lab_hp_sen.csv")
#create task
task_lab_hp_sen <- as_task_regr(lab_hp_sen, target="hp_sen", id="lab_hp_sen")

#define resampling strategy---
rsmp_lab_hp_sen = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_hp_sen$instantiate(task_lab_hp_sen, f = folds)

#create tuner
at_lab_hp_sen = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_hp_sen,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_hp_sen$train(task_lab_hp_sen)#

#Export
results_lab_hp_sen<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_hp_sen<-as.data.frame(list(model="lab_hp_sen",result = at_lab_hp_sen$tuning_result$respect.unordered.factors, rmse = at_lab_hp_sen$tuning_result$regr.rmse))

write_csv2(results_lab_hp_sen, file = "model_results/preliminary/results_lab_hp_sen.csv")

imp_lab_hp_sen <- tibble::rownames_to_column(data.frame(at_lab_hp_sen$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_hp_sen, file = "model_results/preliminary/imp_lab_hp_sen.csv")


##for IBCH----
### IBCH fie ----
set.seed(131)
#load datasets
field_IBCH<-read_csv2("model_data/field_IBCH.csv")
#create task
task_fie_IBCH <- as_task_regr(field_IBCH, target="IBCH", id="fie_IBCH")

#define resampling strategy---
rsmp_fie_IBCH = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_IBCH$instantiate(task_fie_IBCH, f = folds)

#create tuner
at_fie_IBCH = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_IBCH,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_IBCH$train(task_fie_IBCH)#

#Export
results_fie_IBCH<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_IBCH<-as.data.frame(list(model="fie_IBCH",result = at_fie_IBCH$tuning_result$respect.unordered.factors, rmse = at_fie_IBCH$tuning_result$regr.rmse))

write_csv2(results_fie_IBCH, file = "model_results/preliminary/results_fie_IBCH.csv")

imp_fie_IBCH <- tibble::rownames_to_column(data.frame(at_fie_IBCH$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_IBCH, file = "model_results/preliminary/imp_fie_IBCH.csv")


### IBCH lab ----
#load datasets
set.seed(131)
lab_IBCH<-read_csv2("model_data/lab_IBCH.csv")
#create task
task_lab_IBCH <- as_task_regr(lab_IBCH, target="IBCH", id="lab_IBCH")

#define resampling strategy---
rsmp_lab_IBCH = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_IBCH$instantiate(task_lab_IBCH, f = folds)

#create tuner
at_lab_IBCH = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_IBCH,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_IBCH$train(task_lab_IBCH)#

#Export
results_lab_IBCH<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_IBCH<-as.data.frame(list(model="lab_IBCH",result = at_lab_IBCH$tuning_result$respect.unordered.factors, rmse = at_lab_IBCH$tuning_result$regr.rmse))

write_csv2(results_lab_IBCH, file = "model_results/preliminary/results_lab_IBCH.csv")

imp_lab_IBCH <- tibble::rownames_to_column(data.frame(at_lab_IBCH$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_IBCH, file = "model_results/preliminary/imp_lab_IBCH.csv")


##for k_index----
### k_index fie ----
set.seed(131)
#load datasets
field_k_index<-read_csv2("model_data/field_k_index.csv")
#create task
task_fie_k_index <- as_task_regr(field_k_index, target="k_index", id="fie_k_index")

#define resampling strategy---
rsmp_fie_k_index = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_k_index$instantiate(task_fie_k_index, f = folds)

#create tuner
at_fie_k_index = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_k_index,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_k_index$train(task_fie_k_index)#

#Export
results_fie_k_index<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_k_index<-as.data.frame(list(model="fie_k_index",result = at_fie_k_index$tuning_result$respect.unordered.factors, rmse = at_fie_k_index$tuning_result$regr.rmse))

write_csv2(results_fie_k_index, file = "model_results/preliminary/results_fie_k_index.csv")

imp_fie_k_index <- tibble::rownames_to_column(data.frame(at_fie_k_index$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_k_index, file = "model_results/preliminary/imp_fie_k_index.csv")


### k_index lab ----
#load datasets
set.seed(131)
lab_k_index<-read_csv2("model_data/lab_k_index.csv")
#create task
task_lab_k_index <- as_task_regr(lab_k_index, target="k_index", id="lab_k_index")

#define resampling strategy---
rsmp_lab_k_index = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_k_index$instantiate(task_lab_k_index, f = folds)

#create tuner
at_lab_k_index = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_k_index,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_k_index$train(task_lab_k_index)#

#Export
results_lab_k_index<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_k_index<-as.data.frame(list(model="lab_k_index",result = at_lab_k_index$tuning_result$respect.unordered.factors, rmse = at_lab_k_index$tuning_result$regr.rmse))

write_csv2(results_lab_k_index, file = "model_results/preliminary/results_lab_k_index.csv")

imp_lab_k_index <- tibble::rownames_to_column(data.frame(at_lab_k_index$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_k_index, file = "model_results/preliminary/imp_lab_k_index.csv")


##for lzi----
### lzi fie ----
set.seed(131)
#load datasets
field_lzi<-read_csv2("model_data/field_lzi.csv")
#create task
task_fie_lzi <- as_task_regr(field_lzi, target="lzi", id="fie_lzi")

#define resampling strategy---
rsmp_fie_lzi = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_lzi$instantiate(task_fie_lzi, f = folds)

#create tuner
at_fie_lzi = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_lzi,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_lzi$train(task_fie_lzi)#

#Export
results_fie_lzi<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_lzi<-as.data.frame(list(model="fie_lzi",result = at_fie_lzi$tuning_result$respect.unordered.factors, rmse = at_fie_lzi$tuning_result$regr.rmse))

write_csv2(results_fie_lzi, file = "model_results/preliminary/results_fie_lzi.csv")

imp_fie_lzi <- tibble::rownames_to_column(data.frame(at_fie_lzi$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_lzi, file = "model_results/preliminary/imp_fie_lzi.csv")


### lzi lab ----
#load datasets
set.seed(131)
lab_lzi<-read_csv2("model_data/lab_lzi.csv")
#create task
task_lab_lzi <- as_task_regr(lab_lzi, target="lzi", id="lab_lzi")

#define resampling strategy---
rsmp_lab_lzi = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_lzi$instantiate(task_lab_lzi, f = folds)

#create tuner
at_lab_lzi = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_lzi,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_lzi$train(task_lab_lzi)#

#Export
results_lab_lzi<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_lzi<-as.data.frame(list(model="lab_lzi",result = at_lab_lzi$tuning_result$respect.unordered.factors, rmse = at_lab_lzi$tuning_result$regr.rmse))

write_csv2(results_lab_lzi, file = "model_results/preliminary/results_lab_lzi.csv")

imp_lab_lzi <- tibble::rownames_to_column(data.frame(at_lab_lzi$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_lzi, file = "model_results/preliminary/imp_lab_lzi.csv")


##for mar_div----
### mar_div fie ----
set.seed(131)
#load datasets
field_mar_div<-read_csv2("model_data/field_mar_div.csv")
#create task
task_fie_mar_div <- as_task_regr(field_mar_div, target="mar_div", id="fie_mar_div")

#define resampling strategy---
rsmp_fie_mar_div = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_mar_div$instantiate(task_fie_mar_div, f = folds)

#create tuner
at_fie_mar_div = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_mar_div,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_mar_div$train(task_fie_mar_div)#

#Export
results_fie_mar_div<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_mar_div<-as.data.frame(list(model="fie_mar_div",result = at_fie_mar_div$tuning_result$respect.unordered.factors, rmse = at_fie_mar_div$tuning_result$regr.rmse))

write_csv2(results_fie_mar_div, file = "model_results/preliminary/results_fie_mar_div.csv")

imp_fie_mar_div <- tibble::rownames_to_column(data.frame(at_fie_mar_div$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_mar_div, file = "model_results/preliminary/imp_fie_mar_div.csv")


### mar_div lab ----
#load datasets
set.seed(131)
lab_mar_div<-read_csv2("model_data/lab_mar_div.csv")
#create task
task_lab_mar_div <- as_task_regr(lab_mar_div, target="mar_div", id="lab_mar_div")

#define resampling strategy---
rsmp_lab_mar_div = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_mar_div$instantiate(task_lab_mar_div, f = folds)

#create tuner
at_lab_mar_div = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_mar_div,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_mar_div$train(task_lab_mar_div)#

#Export
results_lab_mar_div<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_mar_div<-as.data.frame(list(model="lab_mar_div",result = at_lab_mar_div$tuning_result$respect.unordered.factors, rmse = at_lab_mar_div$tuning_result$regr.rmse))

write_csv2(results_lab_mar_div, file = "model_results/preliminary/results_lab_mar_div.csv")

imp_lab_mar_div <- tibble::rownames_to_column(data.frame(at_lab_mar_div$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_mar_div, file = "model_results/preliminary/imp_lab_mar_div.csv")


##for MMI_HP----
### MMI_HP fie ----
set.seed(131)
#load datasets
field_MMI_HP<-read_csv2("model_data/field_MMI_HP.csv")
#create task
task_fie_MMI_HP <- as_task_regr(field_MMI_HP, target="MMI_HP", id="fie_MMI_HP")

#define resampling strategy---
rsmp_fie_MMI_HP = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_MMI_HP$instantiate(task_fie_MMI_HP, f = folds)

#create tuner
at_fie_MMI_HP = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_MMI_HP,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_MMI_HP$train(task_fie_MMI_HP)#

#Export
results_fie_MMI_HP<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_MMI_HP<-as.data.frame(list(model="fie_MMI_HP",result = at_fie_MMI_HP$tuning_result$respect.unordered.factors, rmse = at_fie_MMI_HP$tuning_result$regr.rmse))

write_csv2(results_fie_MMI_HP, file = "model_results/preliminary/results_fie_MMI_HP.csv")

imp_fie_MMI_HP <- tibble::rownames_to_column(data.frame(at_fie_MMI_HP$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_MMI_HP, file = "model_results/preliminary/imp_fie_MMI_HP.csv")


### MMI_HP lab ----
#load datasets
set.seed(131)
lab_MMI_HP<-read_csv2("model_data/lab_MMI_HP.csv")
#create task
task_lab_MMI_HP <- as_task_regr(lab_MMI_HP, target="MMI_HP", id="lab_MMI_HP")

#define resampling strategy---
rsmp_lab_MMI_HP = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_MMI_HP$instantiate(task_lab_MMI_HP, f = folds)

#create tuner
at_lab_MMI_HP = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_MMI_HP,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_MMI_HP$train(task_lab_MMI_HP)#

#Export
results_lab_MMI_HP<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_MMI_HP<-as.data.frame(list(model="lab_MMI_HP",result = at_lab_MMI_HP$tuning_result$respect.unordered.factors, rmse = at_lab_MMI_HP$tuning_result$regr.rmse))

write_csv2(results_lab_MMI_HP, file = "model_results/preliminary/results_lab_MMI_HP.csv")

imp_lab_MMI_HP <- tibble::rownames_to_column(data.frame(at_lab_MMI_HP$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_MMI_HP, file = "model_results/preliminary/imp_lab_MMI_HP.csv")


##for nr_ept_tax----
### nr_ept_tax fie ----
set.seed(131)
#load datasets
field_nr_ept_tax<-read_csv2("model_data/field_nr_ept_tax.csv")
#create task
task_fie_nr_ept_tax <- as_task_regr(field_nr_ept_tax, target="nr_ept_tax", id="fie_nr_ept_tax")

#define resampling strategy---
rsmp_fie_nr_ept_tax = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_nr_ept_tax$instantiate(task_fie_nr_ept_tax, f = folds)

#create tuner
at_fie_nr_ept_tax = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_nr_ept_tax,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_nr_ept_tax$train(task_fie_nr_ept_tax)#

#Export
results_fie_nr_ept_tax<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_nr_ept_tax<-as.data.frame(list(model="fie_nr_ept_tax",result = at_fie_nr_ept_tax$tuning_result$respect.unordered.factors, rmse = at_fie_nr_ept_tax$tuning_result$regr.rmse))

write_csv2(results_fie_nr_ept_tax, file = "model_results/preliminary/results_fie_nr_ept_tax.csv")

imp_fie_nr_ept_tax <- tibble::rownames_to_column(data.frame(at_fie_nr_ept_tax$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_nr_ept_tax, file = "model_results/preliminary/imp_fie_nr_ept_tax.csv")


### nr_ept_tax lab ----
#load datasets
set.seed(131)
lab_nr_ept_tax<-read_csv2("model_data/lab_nr_ept_tax.csv")
#create task
task_lab_nr_ept_tax <- as_task_regr(lab_nr_ept_tax, target="nr_ept_tax", id="lab_nr_ept_tax")

#define resampling strategy---
rsmp_lab_nr_ept_tax = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_nr_ept_tax$instantiate(task_lab_nr_ept_tax, f = folds)

#create tuner
at_lab_nr_ept_tax = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_nr_ept_tax,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_nr_ept_tax$train(task_lab_nr_ept_tax)#

#Export
results_lab_nr_ept_tax<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_nr_ept_tax<-as.data.frame(list(model="lab_nr_ept_tax",result = at_lab_nr_ept_tax$tuning_result$respect.unordered.factors, rmse = at_lab_nr_ept_tax$tuning_result$regr.rmse))

write_csv2(results_lab_nr_ept_tax, file = "model_results/preliminary/results_lab_nr_ept_tax.csv")

imp_lab_nr_ept_tax <- tibble::rownames_to_column(data.frame(at_lab_nr_ept_tax$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_nr_ept_tax, file = "model_results/preliminary/imp_lab_nr_ept_tax.csv")


##for nr_ple_taxa----
### nr_ple_taxa  fie ----
set.seed(131)

#load datasets
field_nr_ple<-read_csv2("model_data/field_nr_ple_taxa.csv")

#create task
task_fie_nr_ple <- as_task_regr(field_nr_ple, target="nr_ple_taxa", id="fie_nr_ple")

#define resampling strategy---
rsmp_fie_nr_ple = rsmp("custom_cv")

folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))#

rsmp_fie_nr_ple$instantiate(task_fie_nr_ple, f = folds)

#create tuner
at_fie_nr_ple = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_nr_ple,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_nr_ple$train(task_fie_nr_ple)

#Export
results_fie_nr_ple<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_nr_ple<-as.data.frame(list(model="fie_nr_ple",result = at_fie_nr_ple$tuning_result$respect.unordered.factors, rmse = at_fie_nr_ple$tuning_result$regr.rmse))

write_csv2(results_fie_nr_ple, file = "model_results/preliminary/results_fie_nr_ple.csv")

imp_fie_nr_ple <- tibble::rownames_to_column(data.frame(at_fie_nr_ple$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_nr_ple, file = "model_results/preliminary/imp_fie_nr_ple.csv")

### nr_ple_taxa  lab ----
#load datasets
set.seed(131)
lab_nr_ple<-read_csv2("model_data/lab_nr_ple_taxa.csv")

#create task
task_lab_nr_ple <- as_task_regr(lab_nr_ple, target="nr_ple_taxa", id="lab_nr_ple")

#define resampling strategy---
rsmp_lab_nr_ple = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))

rsmp_lab_nr_ple$instantiate(task_lab_nr_ple, f = folds)

#create tuner
at_lab_nr_ple = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_nr_ple,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_nr_ple$train(task_lab_nr_ple)#

#Export
results_lab_nr_ple<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_nr_ple<-as.data.frame(list(model="lab_nr_ple",result = at_lab_nr_ple$tuning_result$respect.unordered.factors, rmse = at_lab_nr_ple$tuning_result$regr.rmse))

write_csv2(results_lab_nr_ple, file = "model_results/preliminary/results_lab_nr_ple.csv")

imp_lab_nr_ple <- tibble::rownames_to_column(data.frame(at_lab_nr_ple$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_nr_ple, file = "model_results/preliminary/imp_lab_nr_ple.csv")


##for nr_taxa----
### nr_taxa fie ----
set.seed(131)
#load datasets
field_nr_taxa<-read_csv2("model_data/field_nr_taxa.csv")
#create task
task_fie_nr_taxa <- as_task_regr(field_nr_taxa, target="nr_taxa", id="fie_nr_taxa")

#define resampling strategy---
rsmp_fie_nr_taxa = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_nr_taxa$instantiate(task_fie_nr_taxa, f = folds)

#create tuner
at_fie_nr_taxa = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_nr_taxa,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_nr_taxa$train(task_fie_nr_taxa)#

#Export
results_fie_nr_taxa<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_nr_taxa<-as.data.frame(list(model="fie_nr_taxa",result = at_fie_nr_taxa$tuning_result$respect.unordered.factors, rmse = at_fie_nr_taxa$tuning_result$regr.rmse))

write_csv2(results_fie_nr_taxa, file = "model_results/preliminary/results_fie_nr_taxa.csv")

imp_fie_nr_taxa <- tibble::rownames_to_column(data.frame(at_fie_nr_taxa$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_nr_taxa, file = "model_results/preliminary/imp_fie_nr_taxa.csv")

### nr_taxa lab ----
#load datasets
set.seed(131)
lab_nr_taxa<-read_csv2("model_data/lab_nr_taxa.csv")
#create task
task_lab_nr_taxa <- as_task_regr(lab_nr_taxa, target="nr_taxa", id="lab_nr_taxa")

#define resampling strategy---
rsmp_lab_nr_taxa = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_nr_taxa$instantiate(task_lab_nr_taxa, f = folds)

#create tuner
at_lab_nr_taxa = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_nr_taxa,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_nr_taxa$train(task_lab_nr_taxa)#

#Export
results_lab_nr_taxa<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_nr_taxa<-as.data.frame(list(model="lab_nr_taxa",result = at_lab_nr_taxa$tuning_result$respect.unordered.factors, rmse = at_lab_nr_taxa$tuning_result$regr.rmse))

write_csv2(results_lab_nr_taxa, file = "model_results/preliminary/results_lab_nr_taxa.csv")

imp_lab_nr_taxa <- tibble::rownames_to_column(data.frame(at_lab_nr_taxa$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_nr_taxa, file = "model_results/preliminary/imp_lab_nr_taxa.csv")


##for sha_div----
### sha_div fie ----
set.seed(131)
#load datasets
field_sha_div<-read_csv2("model_data/field_sha_div.csv")
#create task
task_fie_sha_div <- as_task_regr(field_sha_div, target="sha_div", id="fie_sha_div")

#define resampling strategy---
rsmp_fie_sha_div = rsmp("custom_cv")
folds = as.factor(c(1,1,1,1,1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,6,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,11,11,11,11,11))

rsmp_fie_sha_div$instantiate(task_fie_sha_div, f = folds)

#create tuner
at_fie_sha_div = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_fie_sha_div,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_fie_sha_div$train(task_fie_sha_div)#

#Export
results_fie_sha_div<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_fie_sha_div<-as.data.frame(list(model="fie_sha_div",result = at_fie_sha_div$tuning_result$respect.unordered.factors, rmse = at_fie_sha_div$tuning_result$regr.rmse))

write_csv2(results_fie_sha_div, file = "model_results/preliminary/results_fie_sha_div.csv")

imp_fie_sha_div <- tibble::rownames_to_column(data.frame(at_fie_sha_div$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_fie_sha_div, file = "model_results/preliminary/imp_fie_sha_div.csv")

### sha_div lab ----
#load datasets
set.seed(131)
lab_sha_div<-read_csv2("model_data/lab_sha_div.csv")
#create task
task_lab_sha_div <- as_task_regr(lab_sha_div, target="sha_div", id="lab_sha_div")

#define resampling strategy---
rsmp_lab_sha_div = rsmp("custom_cv")
folds = as.factor(c(1,1,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,8,9,9,9,10,10,11,11))
rsmp_lab_sha_div$instantiate(task_lab_sha_div, f = folds)

#create tuner
at_lab_sha_div = auto_tuner(
  tuner = tuner,
  learner = ranger,
  resampling = rsmp_lab_sha_div,
  measure = msr("regr.rmse"),
  terminator = trm("evals", n_evals = 15) )

at_lab_sha_div$train(task_lab_sha_div)#

#Export
results_lab_sha_div<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("model", "result", "rmse"))

results_lab_sha_div<-as.data.frame(list(model="lab_sha_div",result = at_lab_sha_div$tuning_result$respect.unordered.factors, rmse = at_lab_sha_div$tuning_result$regr.rmse))

write_csv2(results_lab_sha_div, file = "model_results/preliminary/results_lab_sha_div.csv")

imp_lab_sha_div <- tibble::rownames_to_column(data.frame(at_lab_sha_div$learner$importance()), "predictor")#extract variable importances
write_csv2(imp_lab_sha_div, file = "model_results/preliminary/imp_lab_sha_div.csv")