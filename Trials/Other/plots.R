setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load(tidyverse, magrittr, imbalance, ggtext, ggpubr)



# Plots for Imbalanced Data -----------------------------------------------

data("newthyroid1")
imbalanceRatio = 1/imbalanceRatio(newthyroid1)

table(newthyroid1$Class)

diff = table(newthyroid1$Class)[[1]] - table(newthyroid1$Class)[[2]]



# * * Original Data Plot --------------------------------------------------
newthyroid1 %>% 
  ggplot(aes(x = T3resin, y = Thyroxin, shape = Class)) +
  geom_point(size = 5) + theme_bw(base_size = 14) + 
  scale_color_manual(values = c('Original' = 'black')) + 
  scale_shape_manual(values = c('positive' = '+' , 'negative' =  '-')) +
  theme(legend.position = 'top', plot.title = element_text(hjust = 0.5)) + 
  labs(caption = 'The new-thyroid1 dataset from the KEEL repository, 
       containing 35 positive (+) and 180 negative (-) observations.',
       title = '(a) Original Data') -> p1

p1


# * * Under Sampling Plot -------------------------------------------------
set.seed(2021)
positive = newthyroid1 %>% filter(Class == 'positive')
negative = setdiff(newthyroid1, positive)

positive$Type = 'Original'

negative$Type = 'Removed'
rowsToBeKept = sample(1:nrow(negative), nrow(positive))
negative$Type[rowsToBeKept] = 'Original'

underSampled = rbind(positive, negative)

caption2 = '<span style="color:black;">The new-thyroid1 dataset, with<br /></span> <span style="color:dimgray;">145 observations removed with random under sampling.</span>'


underSampled %>% 
  ggplot(aes(x = T3resin, y = Thyroxin, shape = Class, color = Type  )) +
  geom_point(size = 5) + theme_bw(base_size = 14) + 
  scale_color_manual(values = c('Original' = 'black', 'Removed' = 'lightgray')) + 
  scale_shape_manual(values = c('positive' = '+' , 'negative' =  '-')) +
  labs(caption = caption2, title = '(b) Under Sampled Data') + 
  theme(legend.position = 'top', plot.title = element_text(hjust = 0.5),
        plot.caption = element_markdown()) -> p2
p2





# * * OverSampled Plot ----------------------------------------------------
set.seed(2021)
positive = newthyroid1 %>% filter(Class == 'positive')
negative = setdiff(newthyroid1, positive)

negative$Type = 'Original'
negative$Alpha = 1

obsToBeAdded= sample(1:nrow(positive), diff, replace = T)

positive = rbind(positive, positive[obsToBeAdded,] )
positive$Type = 'Added'
positive$Type[1:35] = 'Original'
positive$Alpha = 1
positive$Alpha[1:35] = 1

overSampled = rbind(positive, negative)



caption3 = '<span style="color:black;">The new-thyroid1 dataset,</span> with  <span style="color:orange;">145 observations added using random <br>over sampling</span>. Here, the observations were jittered for visualization purposes.'


overSampled %>% 
  ggplot(aes(x = T3resin, y = Thyroxin, shape = Class, color = Type)) +
  geom_point(position=position_jitter(h=0.5, w=0.5), size = 5, aes(alpha = Alpha)) + theme_bw(base_size = 14) +
  scale_color_manual(values = c('Original' = 'black', 'Added' = 'orange')) + 
  scale_shape_manual(values = c('positive' = '+' , 'negative' =  '-')) +
  labs(caption = caption3, title = '(c) Over Sampled Data') + 
  theme(legend.position = 'top', plot.title = element_text(hjust = 0.5),
        plot.caption = element_markdown()) + scale_alpha(guide = 'none') -> p3
p3





# * * SMOTE Plot ----------------------------------------------------------
smoteSamples <- mwmote(dataset = newthyroid1, numInstances = diff,
                     classAttr = "Class")
smoteDataset <- rbind(newthyroid1, smoteSamples)
smoteDataset$Type = c( rep('Original', 215), rep('SMOTE', diff) ) 

caption4 = '<span style="color:black;">The new-thyroid1 dataset </span>with <br> <span style="color:red;">145 synthetic observations generated using a hybrid technique.</span>'


smoteDataset %>% 
  ggplot(aes(x = T3resin, y = Thyroxin, shape = Class, color = Type  )) +
  geom_point(size = 5) + theme_bw(base_size = 14) + 
  scale_color_manual(values = c('Original' = 'black', 'SMOTE' = 'red')) + 
  scale_shape_manual(values = c('positive' = '+' , 'negative' =  '-')) +
  labs(caption = caption4, title = '(d) Hybrid/SMOTE Sampled Data') + 
  theme(legend.position = 'top', plot.title = element_text(hjust = 0.5),
        plot.caption = element_markdown()) -> p4
p4



# * * Combining the Plots -------------------------------------------------
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
