graphics.off()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load(car, tidyverse, magrittr, RColorBrewer, ggsci, ggplot2, DataExplorer, DT, conflicted,
               gridExtra, lsmeans, sjPlot, effects, jtools, grid, ggpubr, gtable)

conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("group_by", "dplyr")

All_Result <- readRDS("All_Result_new_5fold.RDS")
All_Result$measure <- ifelse(All_Result$measure=="PR_AUC", "AUPRC", All_Result$measure)
All_Result$measure <- ifelse(All_Result$measure=="ROC_AUC", "AUROC", All_Result$measure)
All_Result$subsampling <- ifelse(All_Result$subsampling=="DOWN", "UNDER", All_Result$subsampling)
All_Result$subsampling <- ifelse(All_Result$subsampling=="UP", "OVER", All_Result$subsampling)

All_Result %<>% filter(measure %in% c("Accuracy", "AUPRC", "AUROC", "Sens", "Spec"))
All_Result$subsampling <- factor(All_Result$subsampling, levels=c("NULL", "UNDER", "OVER", "SMOTE")) 

result <- All_Result %>% group_by(dataset, imbalance_ratio, subsampling, metric,
                                  method, measure) %>% dplyr::summarise(
                                    n = n(),
                                    M_value = mean(value, na.rm=T)
                                  )

result <- result %>% mutate(IR_category=cut(imbalance_ratio, breaks=c(-Inf, 5, 10, 30, Inf), labels=1:4))

df1 <- result %>% pivot_wider(names_from = measure, values_from = M_value)

complete <- df1 %>% data.frame() %>%
  select(IR_category, subsampling, method, metric, Sens) 

block1sens <- formula(Sens~IR_category+subsampling+method)
sensmod1 <- lm(block1sens,data=complete)
Anova(sensmod1,type="II")

block2sens <- formula(Sens~(IR_category+subsampling+method)^2)
sensmod2 <- lm(block2sens,data=complete)
result_table <- Anova(sensmod2,type="II")

block3sens <- formula(Sens~(IR_category+subsampling+metric+method)^2)
sensmod3 <- lm(block3sens,data=complete)
Anova(sensmod3,type="II")

block4sens <- formula(Sens~(IR_category*subsampling*method))
sensmod4 <- lm(block4sens,data=complete)
Anova(sensmod4,type="II")

anova(sensmod1, sensmod2, sensmod4, sensmod3)

p <- vector(mode = "list", length = 6)

data_title <- c("(a) Type II Sum of Squares and Anova F-tests", 
                "(b) Main Effect: Imbalance Raito Category", 
                "(c) Main Effect: Subsampling", 
                "(d) Main Effect: Method", 
                "(e) Interaction Effect Plot 1", 
                "(f) Interaction Effect Plot 2")


result_table[,1:3] = apply(result_table[,1:3], 2, function(x) round(x, 3))
result_table$`Pr(>F)` = scales::scientific(result_table$`Pr(>F)`)

A <- tableGrob(result_table, theme=ttheme_minimal(base_size = 8))

# Adding Table borders
A <- gtable_add_grob(A, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                       t = 2, b = nrow(A), l = 1, r = ncol(A))
A <- gtable_add_grob(A,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(A))
grid.draw(A)

## Adjusting the dimensions of the gtable
A$widths = unit(c('0.46', '0.14', '0.1', '0.14', '0.2'), "npc")


#A <- gtable_add_padding(A, unit(0.3, "cm"))

title <- textGrob(data_title[1], gp = gpar(fontsize=12))

p[[1]] <- gtable_add_grob(
  A, list(title),
  t=1, b=1, l = 1, r = ncol(A)
)




p[[2]] <- effect_plot(sensmod2, pred="IR_category") +
    theme(axis.title = element_text(size=22), axis.text = element_text(size=22)) +
    ylim(0, 1) +
    ylab("Sensitivity")+
    xlab("Imbalance Ratio category")+
    labs(title = data_title[2]) + 
    theme_bw() +
    theme(text = element_text(size=22),
          legend.text = element_text(size=22),
          legend.direction = "horizontal",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="top",
          plot.title = element_text(hjust = 0.5, size=22)) +
    scale_x_discrete(limits = c("1", "2", "3", "4"),
                   labels = c("1.82-5", "5-10", 
                              "10-30", "30-129.44"))

p[[3]] <- effect_plot(sensmod2, pred="subsampling") +
  theme(axis.title = element_text(size=22), axis.text = element_text(size=22)) +
  ylim(0, 1) +
  ylab("Sensitivity")+
  xlab("Subsampling")+
  labs(title = data_title[3]) + 
  theme_bw() +
  theme(text = element_text(size=22),
        legend.text = element_text(size=22),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top",
        plot.title = element_text(hjust = 0.5, size=22)) 

p[[4]] <- effect_plot(sensmod2, pred="method") +
  theme(axis.title = element_text(size = 22), axis.text = element_text(size=22)) +
  ylim(0, 1) +
  ylab("Sensitivity")+
  xlab("Method")+
  theme_bw() +
  labs(title = data_title[4]) + 
  theme(text = element_text(size=22),
        legend.text = element_text(size=22),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top",
        plot.title = element_text(hjust = 0.5, size=22)) 


#The Interaction
Inter.Sens.1 <- effect('IR_category*subsampling', sensmod2, se=TRUE)
Inter.Sens.2 <- effect('IR_category*method', sensmod2, se=TRUE)

#Data Frame
Inter.Sens.1.DF <- as.data.frame(Inter.Sens.1)
Inter.Sens.2.DF <- as.data.frame(Inter.Sens.2)

#Create plot
p[[5]] <- ggplot(data=Inter.Sens.1.DF, aes(x=IR_category, y=fit, group=subsampling))+
  geom_line(size=2, aes(color=subsampling))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=subsampling),alpha=.2)+
  coord_cartesian(xlim = c(1.25,3.75))+
  ylab("Sensitivity")+
  xlab("Imbalance Ratio Category")+
  #labs(title="(b) Imbalance Raito and Subsampling as Sensitivity Predictors")+
  ylim(0,1)+
  theme_bw(base_size = 10)+
  theme(text = element_text(size=10),
        legend.title = element_text(size=5),
        legend.text = element_text(size=5),
    legend.position="top",
        plot.title = element_text(hjust = 0.5, size=12)) +
  scale_x_discrete(limits = c("1", "2", "3", "4"),
                   labels = c("[1.82, 5]", "(5, 10]", 
                              "(10, 30]", "(30, 129.44]"))

p[[6]] <- ggplot(data=Inter.Sens.2.DF, aes(x=IR_category, y=fit, group=method))+
  geom_line(size=2, aes(color=method))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=method), alpha=.2)+
  coord_cartesian(xlim = c(1.25,3.75))+
  ylab("Sensitivity")+
  xlab("Imbalance Ratio Category")+
  # labs(title="(c) Imbalance Raito and Method as Sensitivity Predictors")+
  ylim(0,1)+
  theme_bw(base_size = 8)+
  theme(text = element_text(size=10),
        legend.title = element_text(size=5),
        legend.text = element_text(size=5),
    legend.position="top",
        plot.title = element_text(hjust = 0.5, size=12)) +
  scale_x_discrete(limits = c("1", "2", "3", "4"),
                   labels = c("[1.82, 5]", "(5, 10]", 
                              "(10, 30]", "(30, 129.44]"))

# * * Combining the Plots -------------------------------------------------

png("../Figures/Anova_result.png", width = 9, height = 2.5, units = "in", res = 300)
grid.arrange(p[[1]], p[[5]], p[[6]], nrow = 1, ncol = 3)
dev.off()