rm(list=ls())
cat("\014")

#---------------------------------------------------------------# packages
library(dplyr)
library(xtable)
source("main_baseline.R")
#---------------------------------------------------------------# environment
setwd("...\data")
 
#---------------------------------------------------------------# read the data
dataL<-read.csv("lab_view1_2_3_labeled.csv") %>% 
  dplyr :: select(-c(X, MaskID))



LR <- Conventional_ML(dataL = dataL, method = "logreg", imbalanced = TRUE,
                 feature_sel = "Lasso", n_fold = 5)


 



