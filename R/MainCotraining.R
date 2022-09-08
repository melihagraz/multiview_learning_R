rm(list=ls())
cat("\014")

#---------------------------------------------------------------# packages
library(dplyr)
library(xtable)
#---------------------------------------------------------------# environment for cotraining
setwd("/Users/melihagraz/Documents/GitHub/multiview_learning_R/R")
source("cotraining.R")
#---------------------------------------------------------------# environment for data
setwd("/Users/melihagraz/Documents/GitHub/multiview_learning_R/data")

#---------------------------------------------------------------# read the data
#labeled data
dataL<-read.csv("lab_view1_2_3_labeled.csv") %>% 
  dplyr::select(-c(X, MaskID))

# unlabeled data
dataU<-read.csv("unlab_view1_2_3_labeled.csv") %>% 
  dplyr::select(-c(X, MaskID))



# dataL         : Labeled data
# dataU         : Unlabeled data
# method        : naive bayes or random forest
# imbalanced    : balance the training data or not 
# neg_conf_prob : confidence probability to select the positive labels
# pos_conf_prob : confidence probability to select the negative labels
# feature_sel   : MRMR selected fetures (TRUE), medical selected features (FALSE)
# n_fold        : number of fold
# seed          : set seed 

#---------------------------------------------------------------# environment for the output
setwd("/Users/melihagraz/Documents/GitHub/multiview_learning_R/outputCo")

# method is Naive Bayes
# feature selection method is TRUE
# this means we are using all medically selected features

res1 <- CoTraining(dataL, dataU, method = "nb", imbalanced = TRUE,
                  neg_conf_prob = 0.9, pos_conf_prob = 0.1, feature_sel = FALSE,
                  n_fold = 5, seed = 123
)

res1$accuracy_measures
res1$confusion_matrix

#                Sensitivity Specificity       PPV      NPV      Acc        F1    BalAcc
# First iteration     0.97904     0.03816  0.130600  0.92444  0.15898  0.230200  0.508600
# Last iteration      0.86510     0.13326  0.127160  0.90042  0.22794  0.221320  0.499220
# Percentage Gain   -11.63793   249.21384 -2.633997 -2.59833 43.37653 -3.857515 -1.844278


# method is Naive Bayes
# feature selection method is TRUE
# this means we are using all medically selected features
res2 <- CoTraining(dataL, dataU, method = "nb", imbalanced = TRUE,
                  neg_conf_prob = 0.9, pos_conf_prob = 0.1, feature_sel = TRUE,
                  n_fold = 5, seed = 123
                  )

res2$accuracy_measures
res2$confusion_matrix

#                 Sensitivity   Specificity   PPV      NPV       Acc        F1      BalAcc
# First iteration     0.40734     0.73992   0.18350  0.894320   0.69560   0.24842   0.57366
# Last iteration      0.74038     0.26508   0.12856  0.868460   0.32572   0.21868   0.50272
# Percentage         81.75971   -64.17451 -29.94005 -2.891582 -53.17424 -11.97166 -12.36621


# method is random forest
# feature selection method is TRUE
# this means we are using all medically selected features
res3 <- CoTraining(dataL, dataU, method = "rf", imbalanced = TRUE,
                  neg_conf_prob = 0.9, pos_conf_prob = 0.1, feature_sel = FALSE,
                  n_fold = 5, seed = 123
)

res3$accuracy_measures
res3$confusion_matrix



# method is random forest
# feature selection method is FALSE
# this means we are using selected features by MRMR

res4 <- CoTraining(dataL, dataU, method = "rf", imbalanced = TRUE,
                  neg_conf_prob = 0.9, pos_conf_prob = 0.1, feature_sel = TRUE,
                  n_fold = 5, seed = 123
)

res4$accuracy_measures
res$confusion_matrix
