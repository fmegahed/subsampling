setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# * Loading Packages and Dataset -----------------------------------------
pacman::p_load(tidyverse, magrittr, imbalance, ggtext, ggpubr, skimr, kableExtra, magick, webshot)


all_results = readRDS("Trials/All_Result.RDS")
glimpse(all_results)

all_results %<>% mutate(factorizedIR = map_chr(.x = imbalance_ratio, .f = cut,
                                               breaks = c(0, 9, 15, 35, 150), 
                                               labels = c('IR <= 9', '9 < IR <= 15',
                                                          "15 < IR <= 35", 'IR > 35'),
                                               right = TRUE) )

groupedWideResults = all_results %>%  group_by(factorizedIR, subsampling, metric, method, measure) %>% 
  pivot_wider(names_from = 'measure', values_from = 'value') %>% 
  select(dataset, factorizedIR, subsampling, metric, method, Resample, Accuracy, Sens, Spec, ROC_AUC, PR_AUC)

wideSummary = groupedWideResults %>% 
  summarise_if(is.numeric, 
               list(min = ~ min(.x, na.rm = T), 
                    mean = ~ mean(.x, na.rm = T), 
                    q1 = ~ quantile(.x, probs = 0.25, na.rm = T),
                    median = ~ median(.x, na.rm = T),
                    q3 = ~ quantile(.x, probs = 0.75, na.rm = T),
                    max = ~ max(.x, na.rm = T),
                    sd = ~sd(.x, na.rm = T)) )

groupedResults = all_results %>%  
  group_by(factorizedIR, subsampling, metric, method, measure) %>% 
  select(-imbalance_ratio)

summary = groupedResults %>% 
  summarise_if(is.numeric, 
               list(min = ~ min(.x, na.rm = T), 
                    mean = ~ mean(.x, na.rm = T), 
                    q1 = ~ quantile(.x, probs = 0.25, na.rm = T),
                    median = ~ median(.x, na.rm = T),
                    q3 = ~ quantile(.x, probs = 0.75, na.rm = T),
                    max = ~ max(.x, na.rm = T),
                    sd = ~sd(.x, na.rm = T)) ) %>% 
  filter(measure %in% c('Accuracy', 'Sens', 'Spec', 'ROC_AUC', 'PR_AUC'))

summary %<>% ungroup() %>% mutate_if(is.character, factor) 


# summaryLong = summary %>% pivot_longer(min:sd, names_to = "statistic", values_to = "value")

p =  ggplot( aes(x = factorizedIR, y = metric, ))