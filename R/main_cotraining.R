rm(list=ls())
cat("\014")

#---------------------------------------------------------------# packages
library(dplyr)
library(xtable)
source("cotraining.R")
#---------------------------------------------------------------# environment
setwd("/Users/melihagraz/Documents/GitHub/multiview_learning_R/data")

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

res1<- CoTraining(dataL, dataU, method = "nb", imbalanced = TRUE,
                  neg_conf_prob = 0.9, pos_conf_prob = 0.1, feature_sel = TRUE,
                  n_fold = 5, seed = 123
                  )

res1$accuracy_measures
res1$confusion_matrix

#                 Sensitivity   Specificity   PPV      NPV       Acc        F1      BalAcc
# First iteration     0.40734     0.73992   0.18350  0.894320   0.69560   0.24842   0.57366
# Last iteration      0.74038     0.26508   0.12856  0.868460   0.32572   0.21868   0.50272
# Percentage         81.75971   -64.17451 -29.94005 -2.891582 -53.17424 -11.97166 -12.36621


