setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load(tidyverse, magrittr, imbalance, ggtext, ggpubr, png)



# Plots for Imbalanced Data -----------------------------------------------

alldata <- readRDS("../Data/results/all_datasets.RDS")
newthyroid2 <- alldata$data[[which(alldata$dataset=="new-thyroid2")]]

imbalanceRatio = 1/imbalanceRatio(newthyroid2)

table(newthyroid2$Class)

diff = table(newthyroid2$Class)[[1]] - table(newthyroid2$Class)[[2]]


# * * Original Data Plot --------------------------------------------------
newthyroid2 %>% 
  ggplot(aes(x = T3resin, y = Thyroxin, shape = Class)) +
  geom_point(size=5) + theme_bw(base_size = 22) + 
  scale_color_manual(values = c('Original' = '#154360')) + 
  scale_shape_manual(values = c('positive' = '+' , 'negative' =  "o")) +
  theme(legend.position = 'top', plot.title = element_text(hjust = 0.5)) + 
  labs(caption = 'The new-thyroid2 dataset from the KEEL repository, 
       containing 35 positive (+) and 180 negative (o) observations.',
       title = '(a) Original Data') -> p1

original <- readPNG("../Figures/original1.png")
p1 <- p1 + annotation_raster(original, xmin = 62, xmax = 82.8843, ymin = 0, ymax = 12) 
p1


# * * Under Sampling Plot -------------------------------------------------
set.seed(2021)
positive = newthyroid2 %>% filter(Class == 'positive')
negative = setdiff(newthyroid2, positive)

positive$Type = 'Original'

negative$Type = 'Removed'
rowsToBeKept = sample(1:nrow(negative), nrow(positive))
negative$Type[rowsToBeKept] = 'Original'

underSampled = rbind(positive, negative)

caption2 = '<span style="color:black;">The new-thyroid2 dataset, with<br /></span>with  <span style="color:black;">145 observations removed with random under sampling.</span>'


underSampled %>% 
  ggplot(aes(x = T3resin, y = Thyroxin, shape = Class, color = Type)) +
  geom_point(size = 5) + theme_bw(base_size = 22) + 
  scale_color_manual(values = c('Original' = '#154360', 'Removed' = '#439dd6')) + 
  scale_shape_manual(values = c('positive' = '+' , 'negative' = "o")) +
  labs(caption = caption2, title = '(b) Under Sampled Data') + 
  theme(legend.position = 'top', plot.title = element_text(hjust = 0.5),
        plot.caption = element_markdown()) -> p2

under <- readPNG("../Figures/undersampled1.png")
p2 <- p2 + annotation_raster(under, xmin = 62, xmax = 82.8843, ymin = 0, ymax = 12) 
p2


# * * OverSampled Plot ----------------------------------------------------
set.seed(2021)
positive = newthyroid2 %>% filter(Class == 'positive')
negative = setdiff(newthyroid2, positive)

negative$Type = 'Original'
negative$Alpha = 1

obsToBeAdded= sample(1:nrow(positive), diff, replace = T)

positive = rbind(positive, positive[obsToBeAdded,] )
positive$Type = 'Added'
positive$Type[1:35] = 'Original'
positive$Alpha = 1
positive$Alpha[1:35] = 1

overSampled = rbind(positive, negative)



caption3 = '<span style="color:black;">The new-thyroid2 dataset,</span> with  <span style="color:black;">145 observations added using random over <br>sampling</span>. Here, the observations were jittered for visualization purposes.'


overSampled %>% 
  ggplot(aes(x = T3resin, y = Thyroxin, shape = Class, color = Type)) +
  geom_point(position=position_jitter(h=0.5, w=0.5), size = 5) + theme_bw(base_size = 22) +
  scale_color_manual(values = c('Original' = '#154360', 'Added' = '#F88000')) + 
  scale_shape_manual(values = c('positive' = '+' , 'negative' =  "o")) +
  labs(caption = caption3, title = '(c) Over Sampled Data') + 
  theme(legend.position = 'top', plot.title = element_text(hjust = 0.5),
        plot.caption = element_markdown()) + scale_alpha(guide = 'none') -> p3

over <- readPNG("../Figures/oversampled1.png")
p3 <- p3 + annotation_raster(over, xmin = 62, xmax = 82.8843, ymin = 0, ymax = 12) 
p3



# * * SMOTE Plot ----------------------------------------------------------
smoteSamples <- mwmote(dataset = newthyroid2, numInstances = diff,
                     classAttr = "Class")
smoteDataset <- rbind(newthyroid2, smoteSamples)
smoteDataset$Type = c( rep('Original', 215), rep('SMOTE', diff) ) 

caption4 = '<span style="color:black;">The new-thyroid2 dataset </span>with <br> <span style="color:black;">145 synthetic observations generated using a hybrid technique.</span>'


smoteDataset %>% 
  ggplot(aes(x = T3resin, y = Thyroxin, shape = Class, color = Type  )) +
  geom_point(size = 5) + theme_bw(base_size = 22) + 
  scale_color_manual(values = c('Original' = '#154360', 'SMOTE' = '#FF6666')) + 
  scale_shape_manual(values = c('positive' = '+' , 'negative' =  "o")) +
  labs(caption = caption4, title = '(d) Hybrid/SMOTE Sampled Data') + 
  theme(legend.position = 'top', plot.title = element_text(hjust = 0.5),
        plot.caption = element_markdown()) -> p4

smote <- readPNG("../Figures/smotesampled1.png")
p4 <- p4 + annotation_raster(smote, xmin = 62, xmax = 82.8843, ymin = 0, ymax = 12) 
p4



# * * Combining the Plots -------------------------------------------------

png("../Figures/Figure1.png", width = 18, height = 12, units = 'in', res = 700)
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
dev.off()



