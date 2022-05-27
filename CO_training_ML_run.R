rm(list=ls())
cat("\014")
dev.off()

library(dplyr)
library(xtable)
#---------------------------------------------------------------------------------------------NB 
rm(list=ls())
setwd("/Users/melihagraz/Desktop/multiview/march10_2")

source("CO_training_main.R")


lab <- read.csv("lab_view1_2_3_labeled.csv")%>% 
  dplyr::select(- c(X, MaskID))

unlabeled<-unlab <- read.csv("unlab_view1_2_3_labeled.csv")%>% 
  dplyr::select(- c(X))


n_NEG = 7 # number of negative classes we are selecting
n_POS = 1 # number of positive classes we are selecting

#------------------------------------------------------------------------------------ running co-training 

#------------------------------------------- Description

# method       : Machine learning model we want to run, only for  "Naive Bayes" OR "Random Forest"
# train_prob   : Train-Test split
# n_subPool    : unlabeled sample pool
# n_iteration  : number of iteration 
# imbalanced   : imbalanced for training model
# feature_sel  : feature selection algorithm
# n_neg        : number of negative classes will be selected
# n_pos        : number of positive classes will be selected
# n_fold       : number of folds

#------------------------------ Default of the main function 
# method      = "Naive Bayes"
# train_prob  = 0.8 
# n_subPool   = 75 
# n_iteration = 30 
# imbalanced  = TRUE
# feature_sel = FALSE
# n_neg       = n_NEG
# n_pos       = n_POS
# n_fold      = 5

#-------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------- Naive Bayes
#-------------------------------------------------------------------------------------------
# Naive Bayes Medical Selected 
Co_res1<-CoTrain_cv_errorBar(lab, unlab, 
                             method      = "Naive Bayes",
                             train_prob  = 0.8, 
                             n_subPool   = 75, 
                             n_iteration = 30, 
                             imbalanced  = TRUE,
                             feature_sel = FALSE,
                             n_neg       = n_NEG,
                             n_pos       = n_POS,
                             n_fold      = 5
)

# NB Medical Selected results
res1NB_MS<- matrix( c(Co_res1$first_last[1:3,7],Co_res1$first_last[1:3,8],
                      Co_res1$first_last[1:3,5], Co_res1$first_last[1:3,6],
                      Co_res1$first_last[1:3,3], Co_res1$first_last[1:3,4],
                      Co_res1$first_last[1:3,1], Co_res1$first_last[1:3,2],
                      Co_res1$first_last[1:3,11], Co_res1$first_last[1:3,12],
                      Co_res1$first_last[1:3,9], Co_res1$first_last[1:3,10]),6)

round(res1NB_MS,3)# Table 3 rows (Naive Bayes-Medical Selected-(columns 1-6)
#1st row.  View1 1st.    
#2nd row.  View1 last.
#3rd row.  Percentage. 
#4st row.  View2 1st.   
#5nd row.  View2 last. 
#6th row.  Percentage.  

# NB FS Selected
Co_res2<-CoTrain_cv_errorBar(lab, unlab, 
                             method      = "Naive Bayes",
                             train_prob  = 0.8, 
                             n_subPool   = 75, 
                             n_iteration = 30, 
                             imbalanced  = TRUE,
                             feature_sel = TRUE,
                             n_neg       = n_NEG,
                             n_pos       = n_POS,
                             n_fold      = 5
)




# NB Feature Selection results (MRMR results)
res1NB_FS<- matrix( 
  c(Co_res2$first_last[1:3,7], Co_res2$first_last[1:3,8],
    Co_res2$first_last[1:3,5], Co_res2$first_last[1:3,6],
    Co_res2$first_last[1:3,3], Co_res2$first_last[1:3,4],
    Co_res2$first_last[1:3,1], Co_res2$first_last[1:3,2],
    Co_res2$first_last[1:3,11], Co_res2$first_last[1:3,12],
    Co_res2$first_last[1:3,9], Co_res2$first_last[1:3,10]),6)

round(res1NB_FS,3)# Table 3 rows (Naive Bayes-MRMR-(columns 7-12)
#1st row.  View1 1st.    
#2nd row.  View1 last.
#3rd row.  Percentage. 
#4st row.  View2 1st.   
#5nd row.  View2 last. 
#6th row.  Percentage.


# Latex part
# creating Table 3 Naive bayes part
resNB_fin<- cbind(res1NB_MS, res1NB_FS)
resNB_fin
rownames( resNB_fin)<-c("View1 1st iteration","View1 last iteration", "Percentage",
                        "View2 1st iteration","View2 last iteration", "Percentage")

CO_NB_CONC_RES<- xtable(resNB_fin, 
                        type = "latex", 
                        file = "filename1.tex",
                        digits=c(3))


CO_NB_CONC_RES
#------------------------------------------------------------------------------- AND OR Rule (Table 4)
#Table 4, AND Rule 1st row (Naive-Bayes Medical Selected)
round(Co_res1$Conc_AND,3)
#Table 4, AND Rule 2nd row (Naive-Bayes MRMR)
round( Co_res2$Conc_AND,3)
AND_NB_MS_FS <- rbind(Co_res1$Conc_AND, Co_res2$Conc_AND)
AND_NB_MS_FS
# creating latex 
rownames(AND_NB_MS_FS)<-c("Naive Bayes-Medical Selected", "Naive Bayes-MRMR ")
AND_NB_MS_FS
xtable(AND_NB_MS_FS, type = "latex", file = "filename1.tex",digits=c(3))

#Table 4, OR Rule 1st row (Naive-Bayes Medical Selected) 
round(Co_res1$Conc_OR,3)
#Table 4, OR Rule 2nd row (Naive-Bayes MRMR)
round(Co_res2$Conc_OR,3)
OR_NB_MS_FS <- rbind(Co_res1$Conc_OR, Co_res2$Conc_OR)
#latex part
rownames(OR_NB_MS_FS)<-c("Naive Bayes-Medical Selected", "Naive Bayes-MRMR ")
xtable(OR_NB_MS_FS, type = "latex", file = "filename1.tex",digits=c(3))

#-------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------- Random Forest
#-------------------------------------------------------------------------------------------



Co_res3<-CoTrain_cv_errorBar(lab, unlab, 
                             method      = "Random Forest",
                             train_prob  = 0.8, 
                             n_subPool   = 75, 
                             n_iteration = 30, 
                             imbalanced  = TRUE,
                             feature_sel = FALSE,
                             n_neg       = n_NEG,
                             n_pos       = n_POS,
                             n_fold      = 5
)


res2RF_MS<- matrix( c(Co_res3$first_last[1:3,7], Co_res3$first_last[1:3,8],
                      Co_res3$first_last[1:3,5], Co_res3$first_last[1:3,6],
                      Co_res3$first_last[1:3,3], Co_res3$first_last[1:3,4],
                      Co_res3$first_last[1:3,1], Co_res3$first_last[1:3,2],
                      Co_res3$first_last[1:3,11],Co_res3$first_last[1:3,12],
                      Co_res3$first_last[1:3,9], Co_res3$first_last[1:3,10]),6
)

round(res2RF_MS,3)# Table 3 rows (Random Forest-Medical Selected-(columns 1-6)
#1st row.  View1 1st.    
#2nd row.  View1 last.
#3rd row.  Percentage. 
#4st row.  View2 1st.   
#5nd row.  View2 last. 
#6th row.  Percentage.

Co_res4<-CoTrain_cv_errorBar(lab, unlab, 
                             method      = "Random Forest",
                             train_prob  = 0.8, 
                             n_subPool   = 75, 
                             n_iteration = 30, 
                             imbalanced  = TRUE,
                             feature_sel = TRUE,
                             n_neg       = n_NEG,
                             n_pos       = n_POS,
                             n_fold      = 5
)



res2RF_FS<- matrix( 
  c(Co_res4$first_last[1:3,7], Co_res4$first_last[1:3,8],
    Co_res4$first_last[1:3,5], Co_res4$first_last[1:3,6],
    Co_res4$first_last[1:3,3], Co_res4$first_last[1:3,4],
    Co_res4$first_last[1:3,1], Co_res4$first_last[1:3,2],
    Co_res4$first_last[1:3,11],Co_res4$first_last[1:3,12],
    Co_res4$first_last[1:3,9], Co_res4$first_last[1:3,10]),6
)
round(res2RF_FS,3)# Table 3 rows (Random Forest-Medical Selected-(columns 7-12)
#1st row.  View1 1st.    
#2nd row.  View1 last.
#3rd row.  Percentage. 
#4st row.  View2 1st.   
#5nd row.  View2 last. 
#6th row.  Percentage.

resRF_fin1<- cbind(res2RF_MS, res2RF_FS)
rownames( resRF_fin1)<-c("View1 1st iteration","View1 last iteration", "Percentage",
                         "View2 1st iteration","View2 last iteration", "Percentage")
resRF_fin1

# latex part
CO_RF_CONC_RES<- xtable(resRF_fin1, type = "latex", file = "filename1.tex",digits=c(3))
CO_RF_CONC_RES


#------------------------------------------------------------------------------- AND OR Rule  (Table 4)
#Table 4, AND Rule 3rd row (RandomForest- Medical Selected)
round(Co_res3$Conc_AND,3)
#Table 4, AND Rule 4th row (RandomForest- MRMR)
round(Co_res4$Conc_AND,3)

AND_RF_MS_FS <- rbind(Co_res3$Conc_AND, Co_res4$Conc_AND)
round(AND_RF_MS_FS,3)
# latex 
rownames(AND_RF_MS_FS)<-c("Random Forest-Medical Selected", "Random Forest-MRMR")
xtable(AND_RF_MS_FS, type = "latex", file = "filename1.tex",digits=c(3))

#Table 4, OR Rule 3rd row (RandomForest- Medical Selected) 
round(Co_res3$Conc_OR,3)
#Table 4, OR Rule 4th row (RandomForest- MRMR) 
round(Co_res4$Conc_OR,3)
OR_RF_MS_FS <- rbind(Co_res3$Conc_OR, Co_res4$Conc_OR)
round(OR_RF_MS_FS,3)
#latex
rownames(OR_RF_MS_FS)<-c("Random Forest-Medical Selected", "Random Forest-MRMR")
xtable(OR_RF_MS_FS, type = "latex", file = "filename1.tex",digits=c(3))
#Table 4, OR Rule 3rd row (RandomForest- Medical Selected) #Table 4, OR Rule 3rd row (RandomForest- Medical Selected) #Table 4, OR Rule 3rd row (RandomForest- Medical Selected)#Table 4, OR Rule 3rd row (RandomForest- Medical Selected)#Table 4, OR Rule 3rd row (RandomForest- Medical Selected) 

