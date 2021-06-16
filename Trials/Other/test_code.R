library(tidymodels)
library(workflowsets)
library(tidyposterior)
library(themis)
library(magrittr)

setwd("J:/My Drive/Miami/Code/GitHub/subsampling")

datasets = readRDS("Data/results/datasets.rds") %>% .[-31,] %>% arrange(imbalance_ratio)

finalResults = vector(mode = "list")

for (i in c(14, 20, 61, 82)) {
  set.seed(2021)
  Data = datasets$data[[i]]
  Data$Class = relevel(Data$Class, ref = "positive")
  colnames(Data) <- make.names(colnames(Data))
  splitData = initial_split(data = Data, prop = 0.8)
  trainData = training(splitData)
  testData = testing(splitData)
  
  set.seed(1)
  dataFolds = vfold_cv(trainData, v = 5)
  
  base_recipe = recipe(Class ~ ., data = trainData)
  
  down_recipe = base_recipe %>% step_downsample(Class)
  
  up_recipe = base_recipe %>% step_upsample(Class)
  
  smote_recipe = base_recipe %>% step_smote(Class)
  
  #adasyn_recipe = base_recipe %>% step_adasyn(Class)
  
  preprocessors <-
    list(
      plain = Class ~ .,
      down = down_recipe,
      up = up_recipe,
      #adasyn = adasyn_recipe
      smote = smote_recipe
    )
  
  
  log_spec = logistic_reg() %>% set_engine("glm", control = list(maxit = 50)) %>% set_mode("classification")
  cart_spec = decision_tree() %>% set_engine("rpart") %>% set_mode("classification")
  nnet_spec = mlp() %>% set_engine("nnet") %>% set_mode("classification")
  rf_spec = rand_forest() %>% set_engine("ranger") %>% set_mode("classification") 
  
  models = list(
    logReg = log_spec,
    cart = cart_spec,
    nnet = nnet_spec,
    rf = rf_spec
  )
  
  wflow_set = workflow_set(preprocessors, models, cross = TRUE) %>% 
    workflow_map(resamples = dataFolds,
                 grid =  10,
                 seed = 2021,
                 metrics = metric_set(accuracy, roc_auc, sens, spec, pr_auc, recall, precision))%>%
    collect_predictions(select_best=TRUE, metric="roc_auc")
  
  results = rank_results(wflow_set) %>% 
    select(-rank) %>% 
    mutate(dataset = datasets$dataset[[i]]) %>% 
    relocate(dataset, mean)
  
  finalResults[[ datasets$dataset[[i]] ]] = results

  print(paste("Datasets analyzed", i))
}

#ecoli1 (IR: 3.36), yeast3 (IR:8.1), yeast-1-4-5-8_vs_7 (IR: 22.1), abalone19 (IR: 129.44), yeast6 (IR:41.4)

which(datasets$dataset %in% c("ecoli1", "yeast3", "yeast-1-4-5-8_vs_7", "abalone19", "yeast6"))

saveRDS(finalResults, "CrossValidation_result.rds")
