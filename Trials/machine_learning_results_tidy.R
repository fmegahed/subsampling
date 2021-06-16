
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# This code is written to examine the performance of popular machine learning methods on 5- popular imbalanced datasets
# with the following parameters:
# Subsampling strategies: subsampling = c('none', 'down', 'up', 'smote')
# models: logistic, cart, nnet, random forest and SVM
# metrics: accuracy, AUC, AUCPRC, sensitivity




# * Required Packages and Custom Functions --------------------------------


# * * Packages ------------------------------------------------------------
if(require(pacman) == FALSE) install.packages('pacman')
pacman::p_load(tidyverse, magrittr, janitor,
               doParallel,
               caret, caretEnsemble, recipes,
               rpart, glmnet, naivebayes, randomForest, kernlab)


# * * Custom Functions ----------------------------------------------------

outputFun = function(data, lev = "positive", model = NULL) {
  defaultS = defaultSummary(data, lev, model) # returns Accuracy and Kappa
  twoClassS = twoClassSummary(data, lev, model) # returns AUC, sensitivity and specificity
  prS = prSummary(data, lev, model)
  out = c(defaultS, twoClassS, prS)
  out
}



# * Datasets and Experimental Parameters ------------------------------------

# * * Datasets -------------------------------------------------------------
dataset = readRDS("Data/results/datasets.rds")
sample = c('ecoli1', 'yeast3', 'yeast-1-4-5-8_vs_7', 'abalone19', 'yeast6') # selected to have diff imbalanced ratios
finalDatasets = dataset %>% filter(dataset %in% sample)


# * * Experimental Setup --------------------------------------------------
subsampling = c('none', 'down', 'up', 'smote')
metrics = c('accuracy', 'precision', 'recall', 'sensitivity', 'specificity', 'f1', 'ROC', 'AUC')


ecoli1 = finalDatasets$data[[1]]

ecoli1 %<>% mutate_if(is.character, as.factor) %>% 
  clean_names() %>% 
  mutate(class = relevel(class, ref = 'positive'))



# * Data Splitting and Resampling -----------------------------------------
set.seed(2021)

trainRowNumbers = createDataPartition(ecoli1$class,
                                      p = 0.8,
                                      list = F) %>% as.vector()
trainData = ecoli1[trainRowNumbers, ]
testData = ecoli1[-trainRowNumbers, ]



# * Data Preprocessing ----------------------------------------------------
recSteps = recipe(class ~ ., data = trainData) %>% 
  step_nzv( all_predictors() ) %>% 
  step_dummy( all_nominal_predictors() ) %>% # create dummy variables for categorical predictors
  step_normalize( all_numeric() )


# * Cross Validation Setup ------------------------------------------------

fitControl = trainControl(
  method = "cv", # k-fold cross validation
  number = 10, # Number of Folds
  sampling = NULL, # no resampling
  search = "grid", # Default grid search for parameter tuning
  summaryFunction = outputFun, # see functions.R file
  classProbs = T, # should class probabilities be returned
  selectionFunction = "best", # best fold
  savePredictions = 'final',
  index = createResample(trainData$class, 10)  )



# Model Training ----------------------------------------------------------

numCores = detectCores() - 2
cl = makePSOCKcluster(numCores , outfile ="Verbose/trainLog.txt") # Telling R to run on # cores
registerDoParallel(cl)

models = caretList(recSteps, data = trainData, metric="AUC", 
                   tuneList= list(
                     cart= caretModelSpec(method="rpart", tuneLength=30),
                     glm = caretModelSpec(method = "glm", family= "binomial"),
                     lasso = caretModelSpec(method = "glmnet", family= "binomial", 
                                            tuneGrid = expand.grid(.alpha=1,
                                                                   .lambda= seq(0.00001, 2, length=30))),
                     nb = caretModelSpec(method="naive_bayes", tuneLength=30),
                     rf = caretModelSpec(method="rf", tuneLength=30),
                     ridge = caretModelSpec(method = "glmnet", family= "binomial", 
                                            tuneGrid = expand.grid(.alpha=0,
                                                                   .lambda= seq(0.00001, 2, length=30))),
                     svm = caretModelSpec(method="svmRadial", tuneLength=30) 
                   ),  
                   trControl=fitControl, continue_on_fail = T
)

cvResults = resamples(models) # cross-validation results for best fold

stopCluster(cl)

pdf("Data/results/cvBoxPlot_NULL_AUC.pdf", width = 6.5, height = 6.5)
scales = list(x=list(relation="free"), y=list(relation="free"))
bwplot(cvResults, scales=scales) %>% print()
dev.off()
