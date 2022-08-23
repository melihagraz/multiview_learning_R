rm(list=ls())
cat("\014")
dev.off()

setwd(".../R")
library(dplyr)
library(xtable)
source("helper_functions.R")
#---------------------------------------------------------------------------------------------NB
setwd("/Users/melihagraz/Documents/GitHub/multiview_learning_R/data")
setwd(".../data")
lab <- read.csv("lab_view1_2_3_labeled.csv")%>%
  dplyr::select(- c(X, MaskID))

unlabeled<-unlab <- read.csv("unlab_view1_2_3_labeled.csv")%>%
  dplyr::select(- c(X))

n_NEG = 7 # number of negative classes we are selecting
n_POS = 1 # number of positive classes we are selecting

#------------------------------------------------------------------------------------------- Naive Bayes
# Naive Bayes Medical Selected
setwd(".../R")
setwd("/Users/melihagraz/Documents/GitHub/multiview_learning_R/R")
source("main_multi-view_cotraining.R")
Co_res2<-CoTrain_cv_errorBar(lab, unlab,  method = "nb",  train_prob  = 0.8,
                             n_subPool   = 75, n_iteration = 30, imbalanced  = TRUE,
                             feature_sel = FALSE, n_neg = n_NEG, n_pos = n_POS,
                             n_fold = 5, seed=123 )

Co_res2
res1NB_MS2<- matrix( c(Co_res2$first_last[1:3,7],Co_res2$first_last[1:3,8],
                       Co_res2$first_last[1:3,5], Co_res2$first_last[1:3,6],
                       Co_res2$first_last[1:3,3], Co_res2$first_last[1:3,4],
                       Co_res2$first_last[1:3,1], Co_res2$first_last[1:3,2],
                       Co_res2$first_last[1:3,11], Co_res2$first_last[1:3,12],
                       Co_res2$first_last[1:3,9], Co_res2$first_last[1:3,10]),6)


rownames(res1NB_MS2)<- c("first_v1", "last_v1", "perc_v1", "first_v2", "last_v2", "perc_v2")
colnames(res1NB_MS2)<-c("NPV", "PPV", "Spec", "Sen", "Acc", "F1" )
res1NB_MS2



