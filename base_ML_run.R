rm(list=ls())
cat("\014")

#---------------------------------------------------------------# packages
library(dplyr)
library(xtable)
#---------------------------------------------------------------# environment
setwd("/Users/melihagraz/Desktop/multiview/march10_2")

#---------------------------------------------------------------# call _main.R code
source("base_ML_main.R")
#---------------------------------------------------------------# read the data
dataL<-read.csv("lab_view1_2_3_labeled.csv") %>% 
  dplyr :: select(-c(X, MaskID))


#-----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------- Medical Selected
#----------------------------------------------------------------------------------------- 

LR<-Baseline_Hyp(dataL = dataL, 
                 method           = "Logistic Regression" ,
                 imbalanced       =  TRUE,
                 feature_sel      =  "NO FS",
                 n_fold           =  5 
)
round(LR,3)



XG<-Baseline_Hyp(dataL = dataL, method = "Xgboost" ,
                 imbalanced       =  TRUE,
                 feature_sel      =  "NO FS",
                 n_fold           =  5 
)
round(XG,3)

NB<-Baseline_Hyp(dataL = dataL, method = "Naive Bayes" ,
                 imbalanced       = TRUE,
                 feature_sel      = "NO FS",
                 n_fold=5 
)
round(NB,3)

SV<-Baseline_Hyp(dataL = dataL, method = "SVM" ,
                 imbalanced       = TRUE,
                 feature_sel      = "NO FS",
                 n_fold=5 
)
round(SV,3)


RF<-Baseline_Hyp(dataL = dataL, method = "Random Forest" ,
                 imbalanced       = TRUE,
                 feature_sel      = "NO FS",
                 n_fold=5 
)
round(RF,3)

fin_res<-rbind(LR, XG, NB, SV,  RF)
fin_res
round(fin_res,3)

# this part is for latex
rownames(fin_res)<-c("Logistic Regression","XGBoost","Naive Bayes", "SVM","Random Forest")
fin_res
Base_ML_Med<- xtable(fin_res, type = "latex", file = "filename11.tex",digits=c(3))
Base_ML_Med

#-----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------- LASSO Feature
#----------------------------------------------------------------------------------------- Selection (FS)


dataL<-read.csv("lab_view1_2_3_labeled.csv") %>% 
  dplyr::select(-c(X, MaskID))

LR_L<-Baseline_Hyp(dataL = dataL, 
                   method           = "Logistic Regression" ,
                   imbalanced       =  TRUE,
                   feature_sel      =  "Lasso",
                   n_fold           =  5 
)
LR_L



XG_L<-Baseline_Hyp(dataL = dataL, method = "Xgboost" ,
                   imbalanced       =  TRUE,
                   feature_sel      =  "Lasso",
                   n_fold           =  5 
)
XG_L

NB_L<-Baseline_Hyp(dataL = dataL, method = "Naive Bayes" ,
                   imbalanced       =  TRUE,
                   feature_sel      =  "Lasso",
                   n_fold           =  5 
)
NB_L

SV_L<-Baseline_Hyp(dataL = dataL, method = "SVM" ,
                   imbalanced       =  TRUE,
                   feature_sel      =  "Lasso",
                   n_fold           =  5 
)
SV_L


RF_L<-Baseline_Hyp(dataL = dataL, method = "Random Forest" ,
                   imbalanced       =  TRUE,
                   feature_sel      =  "Lasso",
                   n_fold           =  5 
)
RF_L

fin_res_L<-rbind(LR_L, XG_L, NB_L, SV_L,  RF_L)
round(fin_res_L,3)


# this part is for Latex
rownames(fin_res_L)<-c("Logistic Regression","XGBoost","Naive Bayes", "SVM","Random Forest")
fin_res_L
Base_ML_L<- xtable(fin_res_L, type = "latex", file = "filename11.tex",digits=c(3))
Base_ML_L


#-----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------- Boruta FS
#----------------------------------------------------------------------------------------- 


dataL<-read.csv("lab_view1_2_3_labeled.csv") %>% 
  dplyr::select(-c(X, MaskID))

LR_B<-Baseline_Hyp(dataL = dataL, 
                   method           = "Logistic Regression" ,
                   imbalanced       =  TRUE,
                   feature_sel      =  "Boruta",
                   n_fold           =  5 
)
LR_B



XG_B<-Baseline_Hyp(dataL = dataL, method = "Xgboost" ,
                   imbalanced       =  TRUE,
                   feature_sel      =  "Boruta",
                   n_fold           =  5 
)
XG_B

NB_B<-Baseline_Hyp(dataL = dataL, method = "Naive Bayes" ,
                   imbalanced       =  TRUE,
                   feature_sel      =  "Boruta",
                   n_fold           =  5 
)
NB_B

SV_B<-Baseline_Hyp(dataL = dataL, method = "SVM" ,
                   imbalanced       =  TRUE,
                   feature_sel      =  "Boruta",
                   n_fold           =  5 
)
SV_B


RF_B<-Baseline_Hyp(dataL = dataL, method = "Random Forest" ,
                   imbalanced       =  TRUE,
                   feature_sel      =  "Boruta",
                   n_fold           =  5 
)
RF_B

fin_res_B<-rbind(LR_B, XG_B, NB_B, SV_B,  RF_B)
fin_res_B


rownames(fin_res_B)<-c("Logistic Regression","XGBoost","Naive Bayes", "SVM","Random Forest")
fin_res_B
Base_ML_B<- xtable(fin_res_B, type = "latex", file = "filename11.tex",digits=c(3))
Base_ML_B


# this part is for Latex
rownames(fin_res_B)<-c("Logistic Regression","XGBoost","Naive Bayes", "SVM","Random Forest")
fin_res_B
Base_ML_B<- xtable(fin_res_B, type = "latex", file = "filename11.tex",digits=c(3))
Base_ML_B

#-----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------- MRMR FS
#-----------------------------------------------------------------------------------------

dataL<-read.csv("lab_view1_2_3_labeled.csv") %>% select(-c(X, MaskID))

LR_M<-Baseline_Hyp(dataL = dataL, 
                   method           = "Logistic Regression" ,
                   imbalanced       =  TRUE,
                   feature_sel      =  "MRMR",
                   n_fold           =  5 
)
LR_M



XG_M<-Baseline_Hyp(dataL = dataL, method = "Xgboost" ,
                   imbalanced       =  TRUE,
                   feature_sel      =  "MRMR",
                   n_fold           =  5 
)
XG_M

NB_M<-Baseline_Hyp(dataL = dataL, method = "Naive Bayes" ,
                   imbalanced       =  TRUE,
                   feature_sel      =  "MRMR",
                   n_fold           =  5 
)
NB_M

SV_M<-Baseline_Hyp(dataL = dataL, method = "SVM" ,
                   imbalanced       =  TRUE,
                   feature_sel      =  "MRMR",
                   n_fold           =  5 
)
SV_M


RF_M<-Baseline_Hyp(dataL = dataL, method = "Random Forest" ,
                   imbalanced       =  TRUE,
                   feature_sel      =  "MRMR",
                   n_fold           =  5 
)
RF_M

fin_res_M<-rbind(LR_M, XG_M, NB_M, SV_M,  RF_M)
fin_res_M


# this part is for latex
rownames(fin_res_M)<-c("Logistic Regression","XGBoost","Naive Bayes", "SVM","Random Forest")
fin_res_M
Base_ML_M<- xtable(fin_res_M, type = "latex", file = "filename11.tex",digits=c(3))
Base_ML_M
