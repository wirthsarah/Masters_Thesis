#Export ----
# This file should be used, to merge the results from All_Modelling.R. Next File: Evaluation.R

library("readr")
library("dplyr")
library("tidyr")
library("tibble")
library("purrr")
library("stringr")

##Export Results ----
list_results <- list.files(path= "model_results/preliminary", pattern = "results", full.names = TRUE)
all_results <- read_csv2(list_results, id = "file_name")
all_results <- subset(all_results, select=-c(file_name))
write_csv2(all_results, file = "model_results/definitive/all_results.csv")

##Export Variable importance ----
ldf <- list() 
listcsv <- dir(path= "model_results/preliminary", pattern = "imp", full.names = TRUE) 
for (k in 1:length(listcsv)){
  ldf[[k]] <- read_csv2(listcsv[k])}

all_imp<-Reduce(function(x, y) merge(x, y, all = TRUE),ldf)
all_imp<-all_imp |> 
  rename_with(~ str_remove(., ".learner.importance.."), everything())
all_imp<-all_imp |> 
  rename_with(~ str_remove(., "at_"), everything())

write_csv2(all_imp, file = "model_results/definitive/all_variable_importances.csv")

