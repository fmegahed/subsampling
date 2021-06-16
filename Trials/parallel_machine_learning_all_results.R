
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# This code is written to examine the performance of popular machine learning methods on 5- popular imbalanced datasets
# with the following parameters:
# Subsampling strategies: subsampling = c(NULL, 'down', 'up', 'smote')
# models: glm, rpart
# metrics: Accuracy, ROC, and AUC

if(require(pacman) == FALSE) install.packages('pacman')
pacman::p_load(tidyverse, magrittr, conflicted)

conflict_prefer("arrange", "dplyr")

# * Main Function Prepation  
mainfunction <- function(i, dataset, cases){
  trainData = dataset$data[[cases$ID[i]]]
  best_metric <- cases$metrics[i]
  subsampling <- cases$subsampling[i]
  if (subsampling=="NULL") subsampling <- NULL
  
  # * Required Packages and Custom Functions --------------------------------
  
  # * * Packages ------------------------------------------------------------
  pacman::p_load(tidyverse, magrittr, janitor,
                 caret, caretEnsemble, recipes,
                 rpart, glmnet, PRROC, conflicted)
  

  # * * Custom Functions ----------------------------------------------------
  
  get_all <- function(model){
    resamples <- paste0("Resample", 1:5)
    defaultS <- sapply(resamples, function(x) defaultSummary(model$pred[model$pred$Resample==x,], c("positive", "negative"), model$finalModel))
    twoClassS <- sapply(resamples, function(x) twoClassSummary(model$pred[model$pred$Resample==x,], c("positive", "negative"), model$finalModel))
    prS <- sapply(resamples, function(x) prSummary(model$pred[model$pred$Resample==x,], c("positive", "negative"), model$finalModel))
    Resample_result <- cbind.data.frame(Resample=resamples, t(defaultS), t(twoClassS), t(prS))
    rownames(Resample_result) <- NULL
    colnames(Resample_result)[-1] <- paste0(model$method, "~", colnames(Resample_result)[-1])
    return(Resample_result)
  }

  trainData %<>% mutate_if(is.character, as.factor) %>% 
    clean_names() %>% 
    mutate(class = relevel(class, ref = "positive"))
  
  #table(trainData$class) %>% prop.table()
  
  # * Data Preprocessing ----------------------------------------------------
  recSteps = recipe(class ~ ., data = trainData) %>% 
    step_nzv( all_predictors() ) %>% 
    # step_BoxCox(all_numeric()) %>% 
    step_YeoJohnson(all_numeric()) %>%
    step_normalize( all_numeric() ) %>% 
    step_dummy(all_nominal(), -all_outcomes() ) # create dummy variables for categorical predictors
  
  if (best_metric=="Accuracy") outputFun = defaultSummary
  if (best_metric=="ROC") outputFun = twoClassSummary
  if (best_metric=="AUC") outputFun = prSummary
  
  if (cases$subsampling[i]=="NULL") set.seed((11111+cases$ID[i]))
  if (cases$subsampling[i]=="down") set.seed((22222+cases$ID[i]))
  if (cases$subsampling[i]=="up") set.seed((33333+cases$ID[i]))
  if (cases$subsampling[i]=="smote") set.seed((44444+cases$ID[i]))
  
  seeds <- vector(mode = "list", length = 5)
  for(j in 1:5) seeds[[j]] <- sample.int(nrow(trainData), 1)
  
  ## For the last model:
  seeds[[6]] <- sample.int(nrow(trainData), 1)
  
  # * Cross Validation Setup ------------------------------------------------
  
  fitControl = trainControl(
    method = "cv", # k-fold cross validation
    number = 5, # Number of Folds
    search = "grid", # Default grid search for parameter tuning
    sampling = subsampling, # If none, please set to NULL
    summaryFunction = outputFun, # see custom function in this file
    classProbs = T, # should class probabilities be returned
    selectionFunction = "best", # best fold
    savePredictions = 'final',
    index = createResample(trainData$class, 5), seeds = seeds)
  
  # Model Training ----------------------------------------------------------

  models = caretList(recSteps, data = trainData, metric=best_metric, 
                     tuneList= list(
                       cart = caretModelSpec(method="rpart", tuneLength=30),
                       glm = caretModelSpec(method = "glm", family= "binomial", maxit = 100)
                     ),  
                     trControl=fitControl, continue_on_fail = T
  )
  
  
  #cvResults = resamples(models) # cross-validation results for best fold
  
  
  # How to extract the cross validation Results ----
  temp = cbind(get_all(models$cart), get_all(models$glm)[,-1])
  temp$subsampling = toupper(cases$subsampling[i]) # needs to be automatically read from the input to fitControl
  temp$metric = best_metric # needs to be automatically read for the input to caretList
  temp$dataset = dataset$dataset[cases$ID[i]] # needs to be automatically read for the dataset name
  temp$imbalance_ratio = dataset$imbalance_ratio[cases$ID[i]]
  
  temp2 = temp %>% pivot_longer(cols = contains('~'))
  temp2$method = str_detect(temp2$name, 'glm')
  temp2$method = ifelse(temp2$method == 'TRUE', 'GLM', 'CART')
  temp2$measure = str_replace(temp2$name, '.*~', '')
  temp2$measure <- ifelse(temp2$measure=="ROC", "ROC_AUC", temp2$measure)
  temp2$measure <- ifelse(temp2$measure=="AUC", "PR_AUC", temp2$measure)
  temp2 %<>% select(dataset, imbalance_ratio, subsampling, metric, method, Resample, measure, value)
  
  return(temp2)
}


# * Datasets and Experimental Parameters ------------------------------------

# * * Datasets -------------------------------------------------------------
dataset = readRDS("../Data/results/all_datasets.rds") %>% .[-31,] %>% arrange(imbalance_ratio)

X1 <- unlist(lapply(dataset$response, function(x) ifelse(table(x)[1]<table(x)[2], 1, 0)))
X2 <- unlist(lapply(dataset$response, function(x) ifelse(table(x)[2]<25, 1, 0)))
index <- which(X1==1|X2==1)
dataset <- dataset[-index,]

# * * Experimental Setup --------------------------------------------------

dataset_id <- seq(1:nrow(dataset))
subsampling_methods <- c("NULL", "down", "up", "smote")
best_metrics <- c("Accuracy", "AUC", "ROC")
cases <- expand.grid(subsampling=subsampling_methods, metrics=best_metrics, ID=dataset_id,
                     stringsAsFactors=FALSE)

# * Parallel Computing --------------------------------------------------

library(snow)
time.begin <- proc.time()[3]
cl = makeCluster(4, type="SOCK")
ncases = nrow(cases)
Result <- parSapply(cl, 1:ncases, mainfunction, dataset, cases)
stopCluster(cl)

time.end <- proc.time()[3] - time.begin
paste("It took", time.end, "seconds to run the program.")

# k=1074

All_Result <- data.frame()

for (j in 1:ncol(Result)){
  temp <- cbind.data.frame(Result["dataset", j], Result["imbalance_ratio", j],
                           Result["subsampling", j], Result["metric", j],
                           Result["method", j], Result["Resample", j],
                           Result["measure", j], Result["value", j])
  
  All_Result <- rbind.data.frame(All_Result, temp)
}

colnames(All_Result) <- rownames(Result)

saveRDS(All_Result, "All_Result_new_5fold.RDS")


#result <- mainfunction(1, dataset, cases)

#  [1] "ecoli-0-1-3-7_vs_2-6"         "glass-0-1-6_vs_2"            
#  [3] "glass4"                       "lymphography-normal-fibrosis"
#  [5] "poker-8-9_vs_5"               "poker-8-9_vs_6"              
#  [7] "poker-8_vs_6"                 "poker-9_vs_7"                
#  [9] "winequality-red-3_vs_5"       "winequality-white-9_vs_4"    
#  [11] "yeast-2_vs_8"                 "zoo-3"           