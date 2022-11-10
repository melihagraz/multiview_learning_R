#' @title Conventional Machine Learning Algorithms
#'
#' @description
#' You can apply conventional machine learning algorithms with the
#' \code{Conventional_ML} function. We can use the following algorithms:
#' Logistic regression (logreg), Naive Bayes (nb), Random Forest (rf),
#'support vector machine (SVM)
#'
#' @param dataL tabulated data
#' @param method Machine Learning methods you can select;
#'   Naive Bayes, Random Forest, Logistic Regression, SVM, Xgboost
#' @param imbalanced imbalanced data problem is fixed or not
#' @param feature_sel feature selection methods:
#'   Lasso, Boruta, MRMR, none
#' @param n_fold number of folds
#' @param seed a positive integer. It is used to control random number
#' generating steps. If not NULL,
#' folds are randomly generated using the user-defined seed value. It might be
#'  usefull to select
#' the same folds
#' and compare multiple models using the same cross-validation folds.
#'  Default is NULL, i.e., unrestricted.
#'
#' @param ... further arguments. Currently, has no effect.
#'
#' @return a matrix including cross-validated performance measures of the fitted
#' model.
#'
#' @examples
#' 1L
#'
#' @export
#'
#'
#---------------------------------------------------------------------------- required R libraries
library(caret)
library(rpart)
library(e1071)
library(dplyr)
library(randomForest)
library(xgboost)
library(ROSE)
library(praznik)
library(gridExtra)
library(pROC)

#----- Internal function ------#
ConventionalMacLearn <- function(dataL, method = c("logreg", "nb", "rf", "svm", "xgb"),
                            imbalanced = TRUE,
                            feature_sel = c("Lasso", "Boruta", "MRMR", "Medical Selected"),
                            n_fold = 5, seed = NULL, ...){

  source("HelperFunctions.R")

  FS_method <- match.arg(feature_sel)

  if (feature_sel == "Lasso") {
    sel_feat_out <- c("fpg_std", "hba1c_mean", "g1diabed_std",
                      "nphl_insulin_mean", "fpg_mean", "g1check_mean",
                      "g1diabed_mean", "g1nutrit_std", "othbol_insulin_mean",
                      "sulfonylurea_mean", "premix_insulin_mean", "out")

    labF <- dataL[sel_feat_out]
    dataL <- labF
  } else if (feature_sel == "Boruta") {
    sel_feat_out <- c("fpg_std", "hba1c_mean", "g1diabed_std",
                      "nphl_insulin_mean",
                      "fpg_mean", "g1check_mean", "g1diabed_mean",
                      "g1nutrit_std",
                      "othbol_insulin_mean", "g1nutrit_mean", "out")
    labF <- dataL[sel_feat_out]
    dataL <- labF
  } else if (feature_sel == "MRMR")  {
    sel_feat_out <- c("fpg_std", "hba1c_mean", "g1diabed_std",
                      "nphl_insulin_mean", "out")
    labF <- dataL[sel_feat_out]
    dataL <- labF
  } else {
   dataL<-dataL
  }
  
  CV.Sensitivity <- CV.Specifity <- numeric()
  CV.PPV <- CV.NPV <- numeric()
  CV.AUC <- CV.Accuracy <- CV.F.score <- numeric()
  
  roc_pool_sens <-c()
  roc_pool_spec <-numeric()
  roc_catg<-c()
  test_dim<-c()
  len_sens<-c()
 
  if (!is.null(seed)){
    set.seed(seed)
  }
  folds <- createFolds(dataL$out, k = n_fold)
  method <- match.arg(method)
  

  for (i_cv in 1:n_fold) {

    testIndex <- folds[[i_cv]]

    if (!length(testIndex) > 1 | !is.numeric(testIndex)){
      stop("There must be at least two observations in the test folds.
           At least one 'cv' fold is empty or has one observation.")
    }
    trainset <- dataL[-testIndex, ]
    testset  <- dataL[ testIndex, ]
    test_dim[i_cv]<-dim(testset)[1]


    if (!is.null(seed)){
      set.seed(seed)
    }

    trainset<-balanced(trainset, seed = 123)

    
    if (method == "logreg") {
      #------------------------------------------------------------------------# Log.Reg.
      modelFit <- glm(out ~ ., data = trainset, family = "binomial")
      predicted <- modelFit %>%
        predict(testset, type = "response")

      par(pty = "m")
      auc_res<-roc(testset$out, predicted, plot=FALSE,
                   print.auc=TRUE)
      auc_res$percent
      auc_res$sensitivities
      auc_res$specificities
      roc_pool_sens<-append(roc_pool_sens, auc_res$sensitivities)
      roc_pool_spec<-append(roc_pool_spec, auc_res$specificities)
      roc_catg     <-append(roc_catg, rep(paste0("ROC", i_cv), 
                                          length(auc_res$sensitivities)))
      
      len_sens[i_cv]<-length(auc_res$sensitivities)
      CV.AUC[i_cv]<-auc_res$auc[1]
      predicted <- ifelse(predicted > 0.5, 1, 0)
    
    }

    

    if (method == "nb") {
      #------------------------------------------------------------------------# Naive Bayes model traing
      modelFit <- naiveBayes(as.factor(out) ~ .,
                             data = trainset,
                             usekernel = T)
      par(pty = "m")
      predicted <- predict(modelFit, testset)
      predicted_pr <- predict(modelFit, testset, type="raw")
 
      auc_res<-roc(testset$out,     predicted_pr[,1], plot=FALSE, 
                   print.auc=TRUE)
      auc_res$specificities
      
      roc_pool_sens<-append(roc_pool_sens, auc_res$sensitivities)
      roc_pool_spec<-append(roc_pool_spec, auc_res$specificities)
      roc_catg     <-append(roc_catg, rep(paste0("ROC", i_cv), 
                                          length(auc_res$sensitivities)))
      len_sens[i_cv]<-length(auc_res$sensitivities)
      CV.AUC[i_cv]<-auc_res$auc[1]
    }
    if (method == "xgb") {
      #------------------------------------------------------------------------# XGBOOST
      x6 <- which(colnames(trainset) == "out")
      xgb_train <- data.matrix(trainset[-x6])
      xgb_trainlabel <- as.matrix(trainset$out)
      modelFit <- xgboost(data = xgb_train,
                          label = xgb_trainlabel,
                          nrounds = 2)
      predicted <- predict(modelFit,
                           newdata =  as.matrix(testset[-x6]),
                           type = "response")
      
      predicted_pr <- predict(modelFit,
                           newdata =  as.matrix(testset[-x6]),
                           type = "raw")
      
      par(pty = "m")
      auc_res<-roc(testset$out, 
                   predicted_pr, plot=FALSE,print.auc=TRUE)
      auc_res
      roc_pool_sens<-append(roc_pool_sens, auc_res$sensitivities)
      roc_pool_spec<-append(roc_pool_spec, auc_res$specificities)
      roc_catg     <-append(roc_catg, rep(paste0("ROC", i_cv), 
                                          length(auc_res$sensitivities)))
      len_sens[i_cv]<-length(auc_res$sensitivities)
      
      CV.AUC[i_cv]<-auc_res$auc[1]
      predicted <- ifelse(predicted > 0.5, 1, 0)
    }
    if (method == "svm") {
      #------------------------------------------------------------------------# SVM
      modelFit <- train( as.factor(out) ~ .,
                         data = trainset,
                         method = "svmLinear",
                         preProcess = c("center", "scale")
      )
      par(pty = "m")

      predicted <- predict(modelFit, testset)
      predict_num<-as.numeric(as.character(predicted))
      auc_res<-roc(testset$out, predict_num, plot=FALSE,print.auc=TRUE)
      
      roc_pool_sens<-append(roc_pool_sens, auc_res$sensitivities)
      roc_pool_spec<-append(roc_pool_spec, auc_res$specificities)
      roc_catg     <-append(roc_catg, rep(paste0("ROC", i_cv), 
                                          length(auc_res$sensitivities)))
      len_sens[i_cv]<-length(auc_res$sensitivities)
      
      CV.AUC[i_cv]<-auc_res$auc[1]
    }
    if (method == "rf") {
      #------------------------------------------------------------------------# RF
      modelFit <- randomForest(
        as.factor(out) ~ .,
        data = trainset,
        method = 'class',
        ntree = 500
      )
      par(pty = "m")
      predicted <- predict(modelFit, testset, type = "class")
      predict_num<-as.numeric(as.character(predicted))
      auc_res<-roc(testset$out, predict_num, plot=FALSE,
                   print.auc=TRUE)
      roc_pool_sens<-append(roc_pool_sens, auc_res$sensitivities)
      roc_pool_spec<-append(roc_pool_spec, auc_res$specificities)
      roc_catg     <-append(roc_catg, rep(paste0("ROC", i_cv), 
                                          length(auc_res$sensitivities)))
      len_sens[i_cv]<-length(auc_res$sensitivities)
      CV.AUC[i_cv]<-auc_res$auc[1]
    }

    actual    <- as.factor(testset$out)
    predicted <- as.factor(predicted)
  
    perfMeas <- confMat(actual, predicted, positive = "1")

    CV.Sensitivity[i_cv] = perfMeas[["sens"]]
    CV.Specifity[i_cv] = perfMeas[["spec"]]
    CV.PPV[i_cv] = perfMeas[["ppv"]]
    CV.NPV[i_cv] = perfMeas[["npv"]]
    CV.Accuracy[i_cv] = perfMeas[["acc"]]
    CV.F.score[i_cv] = perfMeas[["f1"]]
    
  }# end fold

dat<-data.frame(spec=roc_pool_spec,
                sens=roc_pool_sens,
                roc=roc_catg)


f1<-len_sens[1]
f2<-len_sens[2]
f3<-len_sens[3]
f4<-len_sens[4]
f5<-len_sens[5]

dat1<-  dat[1:(f1),]
dat2<-  dat[((f1+1):(f1+f2)),]
dat3<-  dat[(f1+f2+1):(f1+f2+f3),]
dat4<-  dat[(f1+f2+f3+1):(f1+f2+f3+f4),]
dat5<-  dat[(f1+f2+f3+f4+1):(f1+f2+f3+f4+f5),]



gg_ROC<-ggplot()+
  geom_line(data=dat1, aes(1-spec, sens, color="ROC1"),size = 0.5, alpha = 0.5)+
  geom_line(data=dat2, aes(1-spec, sens,color="ROC2"),size = 0.5, alpha = 0.7)+
  geom_line(data=dat3, aes(1-spec, sens,color="ROC3"),size = 0.8, alpha = 0.7)+
  geom_line(data=dat4, aes(1-spec, sens,color="ROC4"),size = 0.8, alpha = 0.7)+
  geom_line(data=dat5, aes(1-spec, sens,color="ROC5"),size = 0.8, alpha = 0.7)+
  scale_color_manual(values = c(
    'ROC1' = "#C4961A",
    'ROC2' = "#D16103",
    'ROC3' = "#52854C",
    'ROC4' = "#4E84C4",
    'ROC5' = "#293352")) +
  labs(color = paste( "mean AUC", round(mean( CV.AUC),4)),
       x = "False Positive Rate (1-Specificity)", 
       y = "True Positive Rate (Sensitivity)"  )+
  theme(legend.position = c(.8, .2))


  res <- matrix(c(
    mean(CV.NPV, na.rm=TRUE ),
    mean(CV.PPV),
    mean(CV.Specifity),
    mean(CV.Sensitivity),
    mean(CV.Accuracy),
    mean(CV.F.score),
    mean(CV.AUC)
  ), nrow = 1)
  colnames(res) <- c("NPV", "PPV", "Spec", "Sens", "Acc", "F1", "AUC")

  return(result = list(res=res, plot=gg_ROC))
}
