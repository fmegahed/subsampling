setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load(car, tidyverse, magrittr, RColorBrewer, ggsci, ggplot2, 
               DataExplorer, DT, conflicted, magick)

conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("group_by", "dplyr")
conflict_prefer("select", "dplyr")

All_Result <- readRDS("All_Results_0628.RDS")
All_Result$measure <- ifelse(All_Result$measure=="Sensitivity", "Sens", All_Result$measure)
All_Result$measure <- ifelse(All_Result$measure=="Specificity", "Spec", All_Result$measure)
All_Result$subsampling <- ifelse(All_Result$subsampling=="DOWN", "UNDER", All_Result$subsampling)
All_Result$subsampling <- ifelse(All_Result$subsampling=="UP", "OVER", All_Result$subsampling)
All_Result$metric <- ifelse(All_Result$metric=="AUC", "AUPRC", All_Result$metric)
All_Result$metric <- ifelse(All_Result$metric=="ROC", "AUROC", All_Result$metric)


All_Result %<>% filter(measure %in% c("Accuracy", "AUPRC", "AUROC", "Sens", "Spec"))
All_Result$subsampling <- factor(All_Result$subsampling, levels=c("NULL", "UNDER", "OVER", "SMOTE")) 

result <- All_Result %>% group_by(dataset, imbalance_ratio, subsampling, metric,
                                  method, measure) %>% dplyr::summarise(
                                    n = n(),
                                    M_value = mean(value, na.rm=T)
                                  )

result <- result %>% mutate(IR_category=cut(imbalance_ratio, breaks=c(-Inf, 5, 10, 30, Inf), labels=1:4))


set.seed(2021)
data_indices <- sapply(c(13, rep(15,3)), function(x) sample(1:x, 1))
S <- result %>% data.frame() %>% select(dataset, IR_category) %>% group_by(dataset, IR_category) %>% unique()

Data <- sapply(1:4, function(x) as.character((S %>% filter(IR_category==as.character(x)))[data_indices[x],1]))

IR <- sort(unique(result$imbalance_ratio[result$dataset %in% Data]))
data_title <- paste0("(", letters[1:4], ") Data: ", Data, " (IR: ", IR, ")")          

p <- vector(mode = "list", length = 4)

for (k in 1:4){
  temp_data <- All_Result %>% filter(dataset==Data[[k]])
  p[[k]] <- temp_data %>% filter(method=="CART") %>% group_by(metric, subsampling) %>%
    ggplot(aes(x = measure, y = value)) +
    geom_boxplot() +
    facet_grid(cols = vars(metric), rows = vars(subsampling)) +
    theme_bw(base_size = 18)+
    theme(axis.title = element_text(size = 18), axis.text = element_text(size = 12),
          strip.text = element_text(size = 18)) + ylim(0, 1) +
    labs(title = data_title[k]) + 
    theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, size=18)) + scale_color_npg()
}

png(paste0("../Figures/Example4.png"), width = 900, height = 600)
p[[4]]
dev.off()


imgs <- list.files("../Figures/", pattern="Example", full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 1 frame per 5 seconds
img_animated <- image_animate(img_joined, delay=1000)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "../Figures/result_samples.gif")


# * * Combining the Plots -------------------------------------------------

png("../Figures/IR_result.png", width = 18, height = 12, units = 'in', res = 700)
ggarrange(p[[1]], p[[2]], p[[3]], p[[4]], nrow = 2, ncol = 2)
dev.off()

