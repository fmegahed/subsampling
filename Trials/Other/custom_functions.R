# Combining the outputs from the defaultSummary and twoClassSummary caret functions
outputFun = function(data, lev = "positive", model = NULL) {
  defaultS = defaultSummary(data, lev, model) # returns Accuracy and Kappa
  twoClassS = twoClassSummary(data, lev, model) # returns AUC, sensitivity and specificity
  prS = prSummary(data, lev, model) # returns precision, recall, area under pr-recall curve
  out = c(defaultS, twoClassS, prSummary)
  out
}

# Suppressing output from cat() by Hadeley Wickham per
# https://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-tp859876p859882.html
quiet = function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 


# Unzipping and reading from csv
unzip_read = function(fileName, ext = ".dat"){
  # read all file names in the subfolder
  path = paste0(here::here("Data/unzipDatasets"), "/" )
  all_files = gsub(path, "", as.character(unzip(fileName, exdir=here::here("Data/unzipDatasets"))))[1]
  
  # find the .dat data file
  find_ext = str_detect(all_files, ext)
  
  # the following code needs to modify if there are more than one dataset in the folder
  if (length(find_ext[find_ext==TRUE])==1){
    # find data and names for predictors
    filetext = readLines(unz(fileName,  all_files[find_ext==TRUE]))
    unlink(unz(fileName,  all_files[find_ext==TRUE]))
    lines2Skip = which(str_detect(filetext, "@data"))
    find_predictor_names = str_remove_all(filetext[str_detect(filetext, "@inputs|@input")], "@inputs |@input ")
    find_predictor_names = str_replace_all(find_predictor_names, " ", "")
    predictor_names = unlist(str_split(find_predictor_names, ","))
    predictor_names = predictor_names[predictor_names != ""]
    
    # read the dataset
    df = read.csv(unz(fileName,  all_files[find_ext==TRUE]), 
                     skip = lines2Skip, header=F, stringsAsFactors = T)
    colnames(df) = c(predictor_names, "Class")
    df %<>% mutate_if(is.factor, ~factor(trimws(.)))
    return(df)
  }else{
    return(NA)
  }
}


# Generating a list of data.frames containing all keelDatasets
keelData = function(kDataFullLinks, datasetNames, numAttributes, dest = here::here("Data/imbalancedDatasets")){
    # Downloading the data into 
  Map(function(u, d) download.file(u, d, mode="wb"), 
      kDataFullLinks, paste0(dest,"/" , datasetNames, ".zip") )

  subfolder_names = list.files(path = dest, pattern = ".zip", full.names = T)
  
  List_of_dfs = list()
  for (counter in 1:length(subfolder_names)) {
    List_of_dfs[[counter]] = unzip_read(subfolder_names[counter])
    df_name = gsub(".zip", "", gsub(".*/", "", subfolder_names[counter]))
    names(List_of_dfs)[counter] = df_name
  }
  return(List_of_dfs)
}


# Creating a function for applying multiple machine learning models
mlFuncFact = function(ml_method) {
  function(data, label, samplingMethod){
    if(samplingMethod != "none"){
        caret::train(x = data, y = label, metric = "ROC", 
                     method = ml_method,
                     trControl = trainControl(method = "cv", number = 5, search = "grid",
                                              selectionFunction = "best", sampling = samplingMethod,
                                              summaryFunction = outputFun, classProbs = TRUE) )
      
    }
    else{
        caret::train(x = data, y = label, metric = "ROC", 
                     method = ml_method,
                     trControl = trainControl(method = "cv", number = 5, search = "grid",
                                              selectionFunction = "best", sampling = NULL,
                                              summaryFunction = outputFun, classProbs = TRUE) )
    }
  }
}
