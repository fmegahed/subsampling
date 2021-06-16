
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# This code is written to examine the performance of popular machine learning methods on 5- popular imbalanced datasets
# with the following parameters:
# Subsampling strategies: subsampling = c(NULL, 'down', 'up', 'smote')
# models: glm, rpart
# metrics: Accuracy, ROC, and AUC



# * Required Packages and Custom Functions --------------------------------


# * * Packages ------------------------------------------------------------
if(require(pacman) == FALSE) install.packages('pacman')
pacman::p_load(tidyverse, magrittr, janitor,
               caret, caretEnsemble, recipes,
               rpart, glmnet)


# * * Custom Functions ----------------------------------------------------

outputFun = function(data, lev = c("positive", "negative"), model = NULL) {
  defaultS = defaultSummary(data, lev, model) # returns Accuracy and Kappa
  twoClassS = twoClassSummary(data, lev, model) # returns AUC, sensitivity and specificity
  prS = prSummary(data, lev, model)
  out = c(defaultS, twoClassS, prS)
  out['F'] = ifelse(!is.finite(out['F']), NA, out['F'])
  out['AUC'] = ifelse(!is.finite(out['AUC']), NA, out['AUC'])
  return(out)
}



# * Datasets and Experimental Parameters ------------------------------------

# * * Datasets -------------------------------------------------------------
dataset = readRDS("../Data/results/datasets.rds")


# * * Experimental Setup --------------------------------------------------
trainData = dataset$data[[1]]

trainData %<>% mutate_if(is.character, as.factor) %>% 
  clean_names() %>% 
  mutate(class = relevel(class, ref = 'positive'))

table(trainData$class) %>% prop.table()




# * Data Preprocessing ----------------------------------------------------
recSteps = recipe(class ~ ., data = trainData) %>% 
  step_nzv( all_predictors() ) %>% 
  step_BoxCox(all_numeric()) %>% 
  step_normalize( all_numeric() ) %>% 
  step_dummy(all_nominal(), -all_outcomes() ) # create dummy variables for categorical predictors


# * Cross Validation Setup ------------------------------------------------

fitControl = trainControl(
  method = "cv", # k-fold cross validation
  number = 5, # Number of Folds
  search = "grid", # Default grid search for parameter tuning
  sampling = NULL, # If none, please set to NULL
  summaryFunction = outputFun, # see custom function in this file
  classProbs = T, # should class probabilities be returned
  selectionFunction = "best", # best fold
  savePredictions = 'final',
  index = createResample(trainData$class, 5)  )



# Model Training ----------------------------------------------------------
# 
# numCores = detectCores() - 2
# cl = makePSOCKcluster(numCores , outfile ="../Verbose/trainLog.txt") # Telling R to run on # cores
# registerDoParallel(cl)

models = caretList(recSteps, data = trainData, metric="Accuracy", 
                   tuneList= list(
                     cart = caretModelSpec(method="rpart", tuneLength=30),
                     glm = caretModelSpec(method = "glm", family= "binomial")
                   ),  
                   trControl=fitControl, continue_on_fail = T
)

cvResults = resamples(models) # cross-validation results for best fold


# How to extract the cross validation Results ----
temp = cvResults$values
temp$subsampling = 'down' # needs to be automatically read from the input to fitControl
temp$metric = 'ROC' # needs to be automatically read for the input to caretList
temp$dataset = 'abalone' # needs to be automatically read for the dataset name

temp %>% pivot_longer(cols = contains('~')) -> temp2
temp2$method = str_detect(temp2$name, 'glm')
temp2$method = ifelse(temp2$method == 'TRUE', 'GLM', 'CART')
temp2$measure = str_replace(temp2$name, '.*~', '')

temp2 %<>% select(dataset, subsampling, metric, method, Resample, measure, value)

# stopCluster(cl)

# pdf("Data/results/cvBoxPlot_NULL_AUC.pdf", width = 6.5, height = 6.5)
# scales = list(x=list(relation="free"), y=list(relation="free"))
# bwplot(cvResults, scales=scales) %>% print()
# dev.off()
