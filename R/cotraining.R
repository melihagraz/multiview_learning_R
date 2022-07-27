rm(list=ls())
cat("\014")

#---------------------------------------------------------------# packages
library(dplyr)
library(xtable)
source("main_cotraining.R")
#---------------------------------------------------------------# environment
setwd("...\data")

#---------------------------------------------------------------# read the data
#labeled data
dataL<-read.csv("lab_view1_2_3_labeled.csv") %>% 
  dplyr::select(-c(X, MaskID))

# unlabeled adta
dataU<-read.csv("unlab_view1_2_3_labeled.csv") %>% 
  dplyr::select(-c(X, MaskID))

# method is Naive Bayes
# feature selection method is FALSE
# this means we are using all the features

res1<- self(dataL, dataU, method = "nb", imbalanced = TRUE,
                  neg_conf_prob = 0.9, pos_conf_prob = 0.1, feature_sel = TRUE,
                  n_fold = 5, seed = 123)

 

# graphs are automatically saved, so we can get the accuracy measures now.

# NPV, PPV, Spec, Sens, Acc, F
res1_fin<-matrix(c(res1[1:2,4],res1[1:2,3], res1[1:2,2], 
                   res1[1:2, 1],res1[1:2, 5],res1[1:2, 6]), 2)

res1_fin<-round(res1_fin,3)# Overleaf Table 2 (Naive Bayes);
# 1st row. Medical Selected 1st iteration of Table 2
# 2nd row. Medical Selected last iteration of Table 2
res1_fin


