setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# * Loading Packages and Dataset -----------------------------------------
pacman::p_load(tidyverse, magrittr, imbalance, ggtext, ggpubr, skimr, kableExtra, magick, webshot)
webshot::install_phantomjs()

count_numeric = function(x){map_lgl(x, is.numeric) %>% sum()}
count_factor = function(x){map_lgl(x, is.numeric) %>% sum()}

all_datasets = readRDS("Data/results/all_datasets.rds")



summaryTable = all_datasets %>% 
  filter(dataset != 'flare-F') %>% 
  transmute(factorizedIR = map_chr(.x = imbalance_ratio, .f = cut,
                                   breaks = c(0, 9, 15, 35, 150), 
                                   labels = c('IR <= 9', '9 < IR <= 15',
                                              "15 < IR <= 35", 'IR > 35'),
                                   right = TRUE),
            IR = imbalance_ratio,
            number_Obs = map_dbl(.x = predictors, .f = nrow),
            num_Pred = map_dbl(.x = predictors, .f = ncol ),
            num_Numeric_Pred = map_dbl(.x = predictors, .f = count_numeric),
            num_Categorical_Pred = map_dbl(.x = predictors, .f = count_factor))

summaryTable$factorizedIR %<>% as.factor()

summaryTable %<>% group_by(factorizedIR) 

skim(summaryTable) %>%  focus(skim_type, skim_variable, factorizedIR, 
                              numeric.mean, numeric.sd, 
                              numeric.p0, numeric.p25, numeric.p50, numeric.p75, numeric.p100, 
                              numeric.hist) -> table1

table1 %<>% select(-skim_type) %>% mutate(across(is.numeric, ~ round(., 1))) %>% 
  arrange(factorizedIR) %>% select(factorizedIR, everything())

colnames(table1) = c('IR Category', 'Statistic', 'Mean', 'SD', 'p0', 'p25', 'p50', 'p75', 'p100', 'Hist')

knitr::kable(table1, 'html') %>% kable_styling(full_width = F) %>% as_image(file = "table1.png", width = 3)
