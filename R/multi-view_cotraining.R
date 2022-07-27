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

 

res1NB_MS<- matrix( c(Co_res1$first_last[1:3,7],Co_res1$first_last[1:3,8],
                      Co_res1$first_last[1:3,5], Co_res1$first_last[1:3,6],
                      Co_res1$first_last[1:3,3], Co_res1$first_last[1:3,4],
                      Co_res1$first_last[1:3,1], Co_res1$first_last[1:3,2],
                      Co_res1$first_last[1:3,11], Co_res1$first_last[1:3,12],
                      Co_res1$first_last[1:3,9], Co_res1$first_last[1:3,10]),6)

round(res1NB_MS,3)# Table 3 rows (Naive Bayes-Medical Selected-(columns 1-6)

