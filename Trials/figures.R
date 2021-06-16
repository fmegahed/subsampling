setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load(car, tidyverse, magrittr, RColorBrewer, ggsci, ggplot2, DataExplorer, DT, conflicted,
               lsmeans, sjPlot)

conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("group_by", "dplyr")

All_Result <- readRDS("All_Result_new_5fold.RDS")
All_Result$measure <- ifelse(All_Result$measure=="PR_AUC", "AUPRC", All_Result$measure)
All_Result$measure <- ifelse(All_Result$measure=="ROC_AUC", "AUROC", All_Result$measure)


All_Result %<>% filter(measure %in% c("Accuracy", "AUPRC", "AUROC", "Sens", "Spec"))

All_Result$subsampling <- factor(All_Result$subsampling, levels=c("NULL", "DOWN", "UP", "SMOTE")) 

result <- All_Result %>% group_by(dataset, imbalance_ratio, subsampling, metric,
                                 method, measure) %>% dplyr::summarise(
                                   n = n(),
                                   M_value = mean(value, na.rm=T)
                                 )

result <- result %>% mutate(IR_category=cut(imbalance_ratio, breaks=c(-Inf, 5, 10, 30, Inf), labels=1:4))



df1 <- result %>% pivot_wider(names_from = measure, values_from = M_value)
plot_histogram(df1[, c("Accuracy", "AUPRC", "AUROC", "Sens", "Spec")], ncol=3)

plot_missing(All_Result)
plot_boxplot(df1%>%data.frame()%>%select(-imbalance_ratio), by="IR_category", ncol=3)

df1 %>% filter(method=="CART") %>%
  ggplot(aes(x=Sens, y=IR_category)) + 
  geom_boxplot() +
  facet_grid(rows=vars(subsampling), cols=vars(metric)) 


boxplot(Sens~IR_category, data=df1)

complete <- df1 %>% data.frame() %>%
  select(IR_category, subsampling, metric, method, Sens) 


block1sens <- formula(Sens~IR_category+subsampling+method)
sensmod1 <- lm(block1sens,data=complete)
Anova(sensmod1,type="II")

block2sens <- formula(Sens~(IR_category+subsampling+method)^2)
sensmod2 <- lm(block2sens,data=complete)
result_table <- Anova(sensmod2,type="II")

block3sens <- formula(Sens~(IR_category+subsampling+metric+method)^2)
sensmod3 <- lm(block3sens,data=complete)
Anova(sensmod3,type="II")

anova(sensmod1, sensmod2, sensmod3)

catnum <- lsmeans(sensmod2, pairwise~subsampling:method,adjust="tukey")
plot(catnum[[2]])+theme_minimal() 


theme_set(theme_sjplot())
plot_model(sensmod2, type="pred", terms=c("IR_category", "method"), line.size = 1.5, title="", axis.title = c("Subsampling","Sens"), legend.title = "method")+font_size(axis_title.x=14, axis_title.y=14,labels.x=12, labels.y=12)+ylim(0.35, 0.65)+theme(legend.position="top")

library(gridExtra)
png("test.png", height = 400, width = 800)
result_table[,1:3] = apply(result_table[,1:3], 2, function(x) round(x, 3))
result_table$`Pr(>F)` = scales::scientific(result_table$`Pr(>F)`)
p <- tableGrob(result_table, theme=ttheme_minimal(base_size=22))
grid.arrange(p)
dev.off()

#The Interaction
library(effects)
Inter.Sens.2 <- effect('IR_category*method', sensmod2,
                      se=TRUE)
#Data Frame
Inter.Sens.2.DF<-as.data.frame(Inter.Sens.2)


#Create plot
Plot.Sens <- ggplot(data=Inter.Sens.2.DF, aes(x=IR_category, y=fit, group=method))+
  geom_line(size=2, aes(color=method))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=method),alpha=.2)+
  ylab("Sensitivity")+
  xlab("IR_category")+
  ggtitle("Imbalance Raito and Method as Sensitivity Predictors")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
Plot.Sens

library(jtools)
effect_plot(sensmod2, pred=method)

library(stargazer)
stargazer(sensmod1, sensmod2, type="text",
          column.labels = c("Main Effects", "Interaction"), 
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE) 


library(lme4)
lme.1 <- lmer(Sens~1|IR_category, complete)
summary(lme.1)

model_final <- lmer(Sens~(subsampling+method+metric)^2+(1|IR_category), data=complete, REML=FALSE)
Anova(model_final)
anova(lme.1, model_final)

qqnorm(resid(model_final))
qqline(resid(model_final))  


set.seed(2021)
data_indices <- sapply(c(13, rep(15,3)), function(x) sample(1:x, 1))
S <- result %>% data.frame() %>% select(dataset, IR_category) %>% group_by(dataset, IR_category) %>% unique()

Data <- sapply(1:4, function(x) as.character((S %>% filter(IR_category==as.character(x)))[data_indices[x],1]))

data1 <- as.character((S %>% filter(IR_category=="1"))[C1,1])
data2 <- as.character((S %>% filter(IR_category=="2"))[C2,1])
data3 <- as.character((S %>% filter(IR_category=="3"))[C3,1])
data4 <- as.character((S %>% filter(IR_category=="4"))[C4,1])

df2 <- All_Result %>% filter(dataset==Data[[2]])

df2 %<>% filter(measure %in% c("Accuracy", "PR_AUC", "ROC_AUC", "Precision","Sens", "Spec"))

df2 %>% filter(method=="CART") %>% group_by(metric, subsampling) %>%
  ggplot(aes(x = measure, y = value)) +
  geom_boxplot() +
  theme_bw(base_size = 10) + ylim(0, 1) +
  facet_grid(cols = vars(metric), rows = vars(subsampling)) +
  theme(legend.position = "top") + scale_color_npg()
