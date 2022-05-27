# medical selected
rm(list= ls())
cat("\014")
dev.off()

library(dplyr)
library(xtable)


setwd("/Users/melihagraz/Desktop/multiview/march10_2")

#labeled data
dataL<-read.csv("lab_view1_2_3_labeled.csv") %>% 
  dplyr::select(-c(X, MaskID))

# unlabeled adta
dataU<-read.csv("unlab_view1_2_3_labeled.csv") %>% 
  dplyr::select(-c(X, MaskID))



# we have 4 different parts 
# Naive Bayes Medical Selected:res1_fin
# NB MRMR FS                  :res2_fin
# RF Med Sel                  :res3_fin
# RF MRMR FS                  :res4_fin

#------------------------------------------------------------------------- running self-learning 

#----------------------- description
# method       : Machine learning models we want to run, we are running only "Naive Bayes" OR "Random Forest"
# imbalanced   : imbalanced for training model
# neg_conf_prob: the ones with the higher then "neg_conf_prob" " will be labeled as "pseudo labels" in negative class
# pos_conf_prob: the ones with the higher then "pos_conf_prob" " will be labeled as "pseudo labels" in positive class 
# (default is 0.1 because we are sorting all probabilities descending order, i.e. 0.1 = 1-0.9)
# feature_sel  : if TRUE, we are running the model with selected features
# n_fold       : number of folds in CV


#----------------------- defaults of the main function 
# method         = "Naive Bayes"
# imbalanced     = TRUE
# neg_conf_prob  = 0.9
# pos_conf_prob  = 0.1
# feature_sel    = FALSE
# n_fold         = 5

source("self_training_main.R")

# method is Naive Bayes
# feature selection method is FALSE
# this means we are using all the features
res1<- self_feb20(dataL, 
                  dataU, 
                  method = "Naive Bayes", 
                  imbalanced=TRUE,
                  neg_conf_prob=0.9, 
                  pos_conf_prob=0.1, 
                  feature_sel=FALSE,
                  n_fold=5
)

# graphs are automatically saved, so we can get the accuracy measures now.

# NPV, PPV, Spec, Sens, Acc, F
res1_fin<-matrix(c(res1[1:2,4],res1[1:2,3], res1[1:2,2], 
                   res1[1:2, 1],res1[1:2, 5],res1[1:2, 6]), 2)

res1_fin<-round(res1_fin,3)# Overleaf Table 2 (Naive Bayes);
# 1st row. Medical Selected 1st iteration of Table 2
# 2nd row. Medical Selected last iteration of Table 2
res1_fin

# method is Naive Bayes
# feature selection method is TRUE
# this means, we are running the code on selected features

res2<- self_feb20(dataL, 
                  dataU, 
                  method = "Naive Bayes", 
                  imbalanced=TRUE,
                  neg_conf_prob=0.9, 
                  pos_conf_prob=0.1, 
                  feature_sel=TRUE,
                  n_fold=5
)

res2_fin<-matrix(c(res2[1:2,4],res2[1:2,3],
                   res2[1:2,2],res2[1:2, 1],
                   res2[1:2, 5],res2[1:2, 6]), 2)

res2_fin<-round(res2_fin, 3)# Table 2 rows (Naive Bayes);
# 4th row. MRMR 1st iteration
# 5th row. MRMR last iteration
res2_fin 


# method is Random Forest
# feature selection method is TRUE

res3<- self_feb20(dataL, 
                  dataU, 
                  method = "Random Forest", 
                  imbalanced=TRUE,
                  neg_conf_prob=0.9, 
                  pos_conf_prob=0.1, 
                  feature_sel=FALSE,
                  n_fold=5
)


res3_fin<-matrix(c(res3[1:2,4],res3[1:2,3],
                   res3[1:2,2],res3[1:2, 1],
                   res3[1:2, 5],res3[1:2, 6]), 2)


res3_fin<-round(res3_fin,3)# Table 2 rows (Random Forest);
# 1st row. Medical Selected 1st iteration
# 2nd row. Medical Selected last iteration
res3_fin 

res4<- self_feb20(dataL, 
                  dataU, 
                  method = "Random Forest", 
                  imbalanced=TRUE,
                  neg_conf_prob=0.9, 
                  pos_conf_prob=0.1, 
                  feature_sel=TRUE,
                  n_fold=5
)


res4_fin<-matrix(c(res4[1:2,4],res4[1:2,3],
                   res4[1:2,2],res4[1:2, 1],
                   res4[1:2, 5],res4[1:2, 6]), 2)


res4_fin<-round(res4_fin,3)# Table 2 rows (Random Forest);
# 4th row. MRMR 1st iteration
# 5th row. MRMR last iteration
res4_fin 






# calculating percentages of Table 2
res1_fin_perc<-matrix(c(res1[3,4],res1[3,3], res1[3,2], 
                        res1[3, 1],res1[3, 5],res1[3, 6]), 1)

res1_fin_perc<-round(res1_fin_perc,2)

res2_fin_perc<-matrix(c(res2[3,4],res2[3,3], res2[3,2], 
                        res2[3, 1],res2[3, 5],res2[3, 6]), 1)
res2_fin_perc<-round(res2_fin_perc,2)

res3_fin_perc<-matrix(c(res3[3,4],res3[3,3], res3[3,2], 
                        res3[3, 1],res3[3, 5],res3[3, 6]), 1)
res3_fin_perc<-round(res3_fin_perc,2)

res4_fin_perc<-matrix(c(res4[3,4],res4[3,3], res4[3,2], 
                        res4[3, 1],res4[3, 5],res4[3, 6]), 1)
res4_fin_perc<- round(res4_fin_perc,3)


# transfer the final output for the latex
res_fin<-rbind(res1_fin, res1_fin_perc,
               res2_fin, res2_fin_perc,
               res3_fin, res3_fin_perc,
               res4_fin, res4_fin_perc)
res_fin


SL_CONC_RES<- xtable(res_fin, type = "latex", file = "filename_self_res.tex",digits=c(3))
SL_CONC_RES

