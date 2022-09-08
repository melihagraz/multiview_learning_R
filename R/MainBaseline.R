rm(list=ls())
cat("\014")

#---------------------------------------------------------------# packages
library(dplyr)
library(xtable)
source("baseline.R")

#---------------------------------------------------------------# read the data
setwd("...\data")

# Table 1: Comparison of results for baseline models using the medical selected ACCORD data
# using over-sampling. NPV; Negative predictive model, PPV;Positive predictive model, 
# Spec.; Specificity, Sens.; Sensitivity. 

# Medical Selection
dataL<-read.csv("lab_view1_2_3_labeled.csv") %>% 
  dplyr :: select(-c(X, MaskID))

LR_ms <- ConventionalMacLearn(dataL = dataL, method = "logreg", imbalanced = TRUE,
                           feature_sel = "Medical Selected", n_fold = 5,seed = 123)
LR_ms 

XgB_ms <- ConventionalMacLearn(dataL = dataL, method = "xgb", imbalanced = TRUE,
                            feature_sel = "Medical Selected", n_fold = 5,seed = 123)
XgB_ms 

NB_ms <- ConventionalMacLearn(dataL = dataL, method = "nb", imbalanced = TRUE,
                           feature_sel = "Medical Selected", n_fold = 5,seed = 123)
NB_ms 

SVM_ms <- ConventionalMacLearn(dataL = dataL, method = "svm", imbalanced = TRUE,
                            feature_sel = "Medical Selected", n_fold = 5,seed = 123)
SVM_ms


RF_ms <- ConventionalMacLearn(dataL = dataL, method = "rf", imbalanced = TRUE,
                           feature_sel = "Medical Selected", n_fold = 5,seed = 123)
RF_ms

round(rbind(LR_ms, XgB_ms, NB_ms, SVM_ms, RF_ms),3)




# Lasso Feature Selection  
dataL<-read.csv("lab_view1_2_3_labeled.csv") %>% 
  dplyr :: select(-c(X, MaskID))

LR_ls <- ConventionalMacLearn(dataL = dataL, method = "logreg", imbalanced = TRUE,
                 feature_sel = "Lasso", n_fold = 5,seed = 123)
LR_ls 

XgB_ls <- ConventionalMacLearn(dataL = dataL, method = "xgb", imbalanced = TRUE,
                           feature_sel = "Lasso", n_fold = 5,seed = 123)
XgB_ls 

NB_ls <- ConventionalMacLearn(dataL = dataL, method = "nb", imbalanced = TRUE,
                            feature_sel = "Lasso", n_fold = 5,seed = 123)
NB_ls 

SVM_ls <- ConventionalMacLearn(dataL = dataL, method = "svm", imbalanced = TRUE,
                            feature_sel = "Lasso", n_fold = 5,seed = 123)
SVM_ls


RF_ls <- ConventionalMacLearn(dataL = dataL, method = "rf", imbalanced = TRUE,
                            feature_sel = "Lasso", n_fold = 5,seed = 123)
RF_ls

round(rbind(LR_ls, XgB_ls, NB_ls, SVM_ls, RF_ls),3)



# Boruta Feature Selection  
dataL<-read.csv("lab_view1_2_3_labeled.csv") %>% 
  dplyr :: select(-c(X, MaskID))

LR_br <- ConventionalMacLearn(dataL = dataL, method = "logreg", imbalanced = TRUE,
                              feature_sel = "Boruta", n_fold = 5,seed = 123)
LR_br 

XgB_br <- ConventionalMacLearn(dataL = dataL, method = "xgb", imbalanced = TRUE,
                               feature_sel = "Boruta", n_fold = 5,seed = 123)
XgB_br 

NB_br <- ConventionalMacLearn(dataL = dataL, method = "nb", imbalanced = TRUE,
                              feature_sel = "Boruta", n_fold = 5,seed = 123)
NB_br 

SVM_br <- ConventionalMacLearn(dataL = dataL, method = "svm", imbalanced = TRUE,
                               feature_sel = "Boruta", n_fold = 5,seed = 123)
SVM_br


RF_br <- ConventionalMacLearn(dataL = dataL, method = "rf", imbalanced = TRUE,
                              feature_sel = "Boruta", n_fold = 5,seed = 123)
RF_br

round(rbind(LR_br, XgB_br, NB_br, SVM_br, RF_br),3)




# MRMR Feature Selection  
dataL<-read.csv("lab_view1_2_3_labeled.csv") %>% 
  dplyr :: select(-c(X, MaskID))

LR_mr <- ConventionalMacLearn(dataL = dataL, method = "logreg", imbalanced = TRUE,
                              feature_sel = "MRMR", n_fold = 5,seed = 123)
LR_mr 

XgB_mr <- ConventionalMacLearn(dataL = dataL, method = "xgb", imbalanced = TRUE,
                               feature_sel = "MRMR", n_fold = 5,seed = 123)
XgB_mr 

NB_mr <- ConventionalMacLearn(dataL = dataL, method = "nb", imbalanced = TRUE,
                              feature_sel = "MRMR", n_fold = 5, seed = 123)
NB_mr 

SVM_mr <- ConventionalMacLearn(dataL = dataL, method = "svm", imbalanced = TRUE,
                               feature_sel = "MRMR", n_fold = 5,seed = 123)
SVM_mr


RF_mr <- ConventionalMacLearn(dataL = dataL, method = "rf", imbalanced = TRUE,
                              feature_sel = "MRMR", n_fold = 5,seed = 123)
RF_mr

round(rbind(LR_mr, XgB_mr, NB_mr, SVM_mr, RF_mr),3)
