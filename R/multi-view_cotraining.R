rm(list=ls())
cat("\014")

#---------------------------------------------------------------# packages
library(dplyr)
library(xtable)
source("main_multi-view_cotraining.R")
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

Co_res1<-CoTrain_CV(dataL, dataU, method = "nb", train_prob = 0.8,
                      n_subPool=75, n_iteration=30, imbalanced=TRUE,
                      feature_sel=TRUE, n_neg=7, n_pos=1,
                      n_fold=5, seed = 123)
Co_res1

res1NB_MS<- matrix( c(Co_res1$first_last[1:3,7],Co_res1$first_last[1:3,8],
                      Co_res1$first_last[1:3,5], Co_res1$first_last[1:3,6],
                      Co_res1$first_last[1:3,3], Co_res1$first_last[1:3,4],
                      Co_res1$first_last[1:3,1], Co_res1$first_last[1:3,2],
                      Co_res1$first_last[1:3,11], Co_res1$first_last[1:3,12],
                      Co_res1$first_last[1:3,9], Co_res1$first_last[1:3,10]),6)


#output


# Sen_v1 Sen_v2    Spec_v1 Spec_v2    PPV_v1 PPV_v2     NPV_v1  NPV_v2     F1_v1 F1_v2
# first_iteration   0.117900      0  0.9697000       1   0.38178    NaN  0.8812000 0.87162  0.176300   NaN
# second iteration  0.111460      0  0.9658000       1   0.34030    NaN  0.8800000 0.87162  0.164280   NaN
# percentage       -5.462256    NaN -0.4021862       0 -10.86490    NaN -0.1361779 0.00000 -6.817924   NaN
# Acc_v1  Acc_v2  BalAcc_v1 BalAcc_v2
# first_iteration   0.8597400 0.87162  0.5438000       0.5
# second iteration  0.8555000 0.87162  0.5386200       0.5
# percentage       -0.4931724 0.00000 -0.9525561       0.0
