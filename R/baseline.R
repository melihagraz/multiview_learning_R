rm(list=ls())
cat("\014")

#---------------------------------------------------------------# packages
library(dplyr)
library(xtable)
source("main_baseline.R")

#---------------------------------------------------------------# read the data
setwd("...\data")

dataL<-read.csv("lab_view1_2_3_labeled.csv") %>% 
  dplyr :: select(-c(X, MaskID))



LR <- Conventional_ML(dataL = dataL, method = "logreg", imbalanced = TRUE,
                 feature_sel = "Lasso", n_fold = 5,seed = 123)
LR 


# output 

# NPV       PPV      Spec      Sens       Acc        F1
# [1,] 0.90615 0.1799651 0.6109583 0.5674599 0.6054273 0.2712822
 



