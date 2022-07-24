#' @title XX (instead of Self learning we can give ) Machıne Learning Model
#'
#' @description
#' You can apply xx learning algorithms with the \code{self} function.
#'
#' @param dataL tabulated labeled data
#' @param dataU tabulated unlabeled data
#' @param method Machine Learning methods you can select;
#'   Naive Bayes (nb), Random Forest (rf)
#' @param imbalanced imbalanced data problem is fixed or not
#' @param neg_conf_prob confidence probability for negative labels
#' @param pos_conf_prob confidence probability for positive labels
#' @param feature_sel best features are selected from the conventional machine
#' learning
#'
#' @param n_fold number of folds
#' @param seed a positive integer. It is used to control random number
#' generating steps. If not NULL,
#' folds are randomly generated using the user-defined seed value. It might be
#' usefull to select
#' the same folds
#' and compare multiple models using the same cross-validation folds.
#' Default is NULL, i.e., unrestricted.
#' @param ... further arguments. Currently, has no effect.
#'
#' @return a matrix including cross-validated performance measures of the
#' fitted model.
#'
#' @examples
#' self(dataL, dataU, method = c("nb", "rf"), imbalanced = TRUE,
#' neg_conf_prob = 0.9, pos_conf_prob = 0.1, feature_sel = TRUE,
#' n_fold = 5, seed = 123,...)
#'
#' @export

#---------------------------------------------------------------------------required R libraries
library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)
library(ggpubr)
library(ROSE)
library(praznik)
library(e1071)
library(gridExtra)
library(PRROC)

#----- Internal function ------#

self  <- function(dataL, dataU, method = c("nb", "rf"), imbalanced = TRUE,
                  neg_conf_prob = 0.9, pos_conf_prob = 0.1, feature_sel = TRUE,
                  n_fold = 5, seed = 123,...) {

  #--------- you can use below code to install required packages or above code
  # 
  requiredLibs <- c("dplyr", "caret", "randomForest", "ggplot2", "ggpubr",
                    "ROSE", "praznik", "e1071", "gridExtra", "PRROC")

  for (pkg in requiredLibs){
    if (!require(pkg)){
      install.packages(pkg)
      require(pkg)
    }
  }
#------------------------
  
  ###checking data
  if (missing(dataL))
    stop("labeled/unlabeled data is reaquired.\n")
  method <- match.arg(method)

  source("helper_functions.R")
  #-----------------------------------------------------------------------------# FEATURE SEL
  # we have already selected the features before
  if (feature_sel) {
    sel_feat     <- c("g1diabed_std", "hba1c_mean", "fpg_std",
                      "nphl_insulin_mean")
    sel_feat_out <- c("g1diabed_std", "hba1c_mean", "fpg_std",
                      "nphl_insulin_mean", "out")

    dataL <- dataL[sel_feat_out]
    dataU <- dataU[sel_feat]
  }

  Sensitivity_CV <- data.frame()  ; Specificity_CV <- data.frame()
  PPV_CV  <- data.frame()  ; NPV_CV <- data.frame()
  F1_CV   <- data.frame()  ; Accuracy_CV <- data.frame()
  BalancedAccuracy_CV <- data.frame()

  length_ix_pos <- data.frame()
  length_ix_neg <- data.frame()

  TP <- data.frame() ; TN <- data.frame()
  FP <- data.frame() ; FN <- data.frame()

  if (!is.null(seed)){
    set.seed(seed)
  }

  folds <- createFolds(dataL$out, k = n_fold)

  datamainU<-dataU
  for (i_cv in 1:n_fold) {
    testIndex <- folds[[i_cv]]
    train <- dataL[-testIndex,]
    test  <- dataL[testIndex,]

    dataU<-datamainU

    if (imbalanced) {
      imbalanced_result<-balanced(train, seed=seed)
      train<-imbalanced_result
    }

    iterations = 1
    length_negpos_selection <- 1 # : is the length of selected positive and negative
    # pseudo labels.  to start to the while loop, we assign it as 1.
    # while loop will continue till no observation is selected.
    while (length(length_negpos_selection) > 0) {
      if (method == "nb") {
        naiv_bay <- naiveBayes(as.factor(out) ~ ., data = train,
                               method = 'class')
        naiv_pre_unlab <- predict(naiv_bay, dataU, type = "raw")

      } else {
        naiv_bay <- randomForest(
          as.factor(out) ~ .,
          data = train,
          method = 'class',
          ntree = 500
        )
        naiv_pre_unlab <- predict(naiv_bay, dataU, type = "prob")

      }
      confusion_result <- confusionMatrix(predict(naiv_bay, test),
                                          as.factor(test$out),
                                          positive = "1")

      # ACCURACY MEASURES
      Sensitivity_CV[i_cv, iterations] <- round(confusion_result$byClass[[1]], 4)
      Specificity_CV[i_cv, iterations] <- round(confusion_result$byClass[[2]], 4)
      PPV_CV[i_cv, iterations]  <- round(confusion_result$byClass[[3]], 4)
      NPV_CV[i_cv, iterations]  <- round(confusion_result$byClass[[4]], 4)
      Accuracy_CV[i_cv, iterations]  <- round(confusion_result$overall[[1]], 4)
      F1_CV[i_cv, iterations] <- round(confusion_result$byClass[[7]], 4)
      BalancedAccuracy_CV[i_cv, iterations] <- round(confusion_result$byClass[[11]], 4)

      # CONFUSION MATRIX
      TN[i_cv, iterations] <- round(confusion_result$table[1], 4)
      TP[i_cv, iterations] <- round(confusion_result$table[4], 4)
      FN[i_cv, iterations] <- round(confusion_result$table[3], 4)
      FP[i_cv, iterations] <- round(confusion_result$table[2], 4)

      # Create Pseudo labels
      unlabeled_probability <- cbind(dataU, pr = naiv_pre_unlab[, 1])
      unlabeled_probability <- unlabeled_probability[order(-unlabeled_probability$pr), ]

      unlabeled_probability$out <- ifelse(unlabeled_probability$pr > 0.5, 0, 1)
      neg <- which(unlabeled_probability$pr > neg_conf_prob)
      length_ix_neg[i_cv, iterations] <- length(neg)
      pos <- which(unlabeled_probability$pr < pos_conf_prob)
      length_ix_pos[i_cv, iterations] <- length(pos)

      length_negpos_selection <- c(neg, pos)

      unlabeled_probability2 <- unlabeled_probability %>%
        dplyr::select(-"pr")
      unlabeled_probability3 <- unlabeled_probability2[length_negpos_selection,]
      new_labeled_data <-  rbind(train, unlabeled_probability3)
      unlabeled_probability4 <- unlabeled_probability2[-length_negpos_selection, ] %>%
        dplyr::select(-"out")

      train <- new_labeled_data
      dataU <- unlabeled_probability4

      iterations = iterations + 1
    } # end of  while
  } # end of folds

  # definition of missing_vector:
  # every fold generating different number of iterations,We find the iteration
  # number of the one
  # with the lowest iteration among a total of n_fold iterations.
  # for instance, n_fold is 3
  # fold 1 has 15 iterations (fold 1-->15), fold 2--> 28 iterations,
  # fold 3--> 12 iterations,
  # min_NA is 12
  missing_vector <- apply(TN, 1, function(x) {
    first(which(is.na(x) == TRUE))
  })
  min_NA <- min(missing_vector[!is.na(missing_vector)]) - 1
  # accuracy measures
  Sensitivity_CV <- Sensitivity_CV[, 1:min_NA]
  Specificity_CV <- Specificity_CV[, (1:min_NA)]
  PPV_CV <- PPV_CV[, 1:min_NA]
  NPV_CV <- NPV_CV[, 1:min_NA]
  F1_CV <- F1_CV[, 1:min_NA]
  Accuracy_CV <- Accuracy_CV[, 1:min_NA]
  BalancedAccuracy_CV <- BalancedAccuracy_CV[, 1:min_NA]

  colnames(Sensitivity_CV)<-colnames(Specificity_CV)<-colnames(PPV_CV)<-colnames(NPV_CV)<- NULL
  colnames(F1_CV) <- colnames(Accuracy_CV) <- colnames(BalancedAccuracy_CV) <- NULL

  TP <- TP[, 1:min_NA]; TN <- TN[, 1:min_NA]
  FP <- FP[, 1:min_NA]; FN <- FN[, 1:min_NA]

  colnames(FN) <- colnames(FP) <- colnames(TP) <- colnames(TN) <- NULL

  length_ix_neg <- length_ix_neg[, 1:min_NA]
  length_ix_pos <- length_ix_pos[, 1:min_NA]

  #-----------------------------------------------------------------------------
  TPNF <- cbind(TP, TN, FP, FN)
  colnames(TPNF) <- c(
    paste0("it",  paste0(1:min_NA, c("-TP"))),
    paste0("it",  paste0(1:min_NA, c("-TN"))),
    paste0("it",  paste0(1:min_NA, c("-FP"))),
    paste0("it",  paste0(1:min_NA, c("-FN")))
  )
  rownames(TPNF) <- paste0("FOLD", 1:(n_fold))

  saver(TPNF,  name="ConfMat",   format =  ".csv",
        main_method="self",
        method = method, feature_sel = feature_sel)
  #-----------------------------------------------------------------------------
  # function for calculating mean and sd of accuracy measures
  #-----------------------------------------------------------------------------# GRAPH:ACCURACY MEASURES
  # sensitivity graph
  SensCV_fin <- data_summary(Sensitivity_CV)
  Nit <- min_NA

  plot_self( SensCV_fin, name="Sensitivity",
             number_iterations=Nit,
             method = method,
             feature_sel = feature_sel, main_method="Self", format=".pdf")

  # Specifity graph
  SpecCV_fin <- data_summary(Specificity_CV)
  plot_self( SpecCV_fin, name="Specificity",
             number_iterations=Nit,
             method = method,
             feature_sel = feature_sel, main_method="Self", format=".pdf")

  # PPV graph
  PPVCV_fin <- data_summary(PPV_CV)
  plot_self( PPVCV_fin, name="PPV",
             number_iterations=Nit,
             method = method,
             feature_sel = feature_sel, main_method="Self", format=".pdf")

  # NPV graph
  NPVCV_fin <- data_summary(NPV_CV)
  plot_self( NPVCV_fin, name="NPV",
             number_iterations=Nit,
             method = method,
             feature_sel = feature_sel, main_method="Self", format=".pdf")

  #  ACC graph
  AccCV_fin <- data_summary(Accuracy_CV)
  plot_self( AccCV_fin, name="Accuracy",
             number_iterations=Nit,
             method = method,
             feature_sel = feature_sel, main_method="Self", format=".pdf")

  # balanced accuracy graph
  BalAccCV_fin <- data_summary(BalancedAccuracy_CV)
  plot_self( BalAccCV_fin, name="BalancedAccuracy",
             number_iterations=Nit,
             method = method,
             feature_sel = feature_sel, main_method="Self", format=".pdf")

  # F1 graph
  F1CV_fin <- data_summary(F1_CV)

  plot_self(F1CV_fin, name="F1",
             number_iterations=Nit,
             method = method,
             feature_sel = feature_sel, main_method="Self", format=".pdf")
  #-----------------------------------------------------------------------------# GRAPH: CONF MATR
  # TP
  TP_fin <- data_summary(TP)
  plot_self( TP_fin, name="TP",
             number_iterations=Nit,
             method = method,
             feature_sel = feature_sel, main_method="Self", format=".pdf")

  # TN
  TN_fin <- data_summary(TN)
  plot_self( TN_fin, name="TN",
             number_iterations=Nit,
             method = method,
             feature_sel = feature_sel, main_method="Self", format=".pdf")
  #FP
  FP_fin <- data_summary(FP)
  plot_self( FP_fin, name="FP",
             number_iterations=Nit,
             method = method,
             feature_sel = feature_sel, main_method="Self", format=".pdf")

  #FN
  FN_fin <- data_summary(FN)
  plot_self( FN_fin, name="FN",
             number_iterations=Nit,
             method = method,
             feature_sel = feature_sel, main_method="Self", format=".pdf")

  #-----------------------------------------------------------------------------# GRAPH: Pos_Neg Sel
  # how many positive and negative output was selected in each iteration and
  # graph for positive and negative selection in each iteration
  length_ix_neg2 <- data_summary(length_ix_neg)
  length_ix_pos2 <- data_summary(length_ix_pos)

  pos_neg_fin <- data.frame(
    View = c(rep("Positive", (Nit)), rep("Negative", (Nit))),
    Iterations = c(1:(Nit), 1:(Nit)),
    Mean = c(length_ix_pos2[, 2], length_ix_neg2[, 2]),
    Sd = c(length_ix_pos2[, 3], length_ix_neg2[, 3])
  )
  pos_neg_pl <- ggplot(pos_neg_fin, aes(
    x = Iterations,
    y = Mean,
    group = View,
    color = View
  )) +
    geom_line() +
    geom_point() +
    scale_color_brewer(palette = "Paired") +
    theme_minimal() +
    ylab("Selected Positivie Negative Values") +
    theme_bw()

  saver(pos_neg_pl,  name="PosNeg",   format =  ".pdf",
        main_method="self",
        method = method, feature_sel = feature_sel)

  # the final output we want to return
  first_last <- data.frame(
    Sensitivity = c(SensCV_fin[1, 2],
                    SensCV_fin[dim(SensCV_fin)[1], 2],
                    100 * ((SensCV_fin[dim(SensCV_fin)[1], 2] -
                              SensCV_fin[1, 2]) / (SensCV_fin[1, 2]))),
    Specificity = c(SpecCV_fin[1, 2],
                    SpecCV_fin[dim(SpecCV_fin)[1], 2],
                    100 * ((SpecCV_fin[dim(SpecCV_fin)[1], 2] -
                              SpecCV_fin[1, 2]) / (SpecCV_fin[1, 2]))),
    PPV = c(PPVCV_fin[1, 2],
            PPVCV_fin[dim(PPVCV_fin)[1], 2],
            100 * ((PPVCV_fin[dim(PPVCV_fin)[1], 2] -
                      PPVCV_fin[1, 2]) / (PPVCV_fin[1, 2]))),
    NPV = c(NPVCV_fin[1, 2],
            NPVCV_fin[dim(NPVCV_fin)[1], 2],
            100 * ((NPVCV_fin[dim(NPVCV_fin)[1], 2] - NPVCV_fin[1, 2]) /
                     (NPVCV_fin[1, 2]))),
    Acc = c(AccCV_fin[1, 2],
            AccCV_fin[dim(NPVCV_fin)[1], 2],
            100 * ((AccCV_fin[dim(AccCV_fin)[1], 2] - AccCV_fin[1, 2]) /
                     (AccCV_fin[1, 2]))),
    F1 = c(F1CV_fin[1, 2],
           F1CV_fin[dim(F1CV_fin)[1], 2],
           100 * ((F1CV_fin[dim(F1CV_fin)[1], 2] - F1CV_fin[1, 2]) /
                    (F1CV_fin[1, 2]))),
    BalAcc = c(BalAccCV_fin[1, 2],
               BalAccCV_fin[dim(BalAccCV_fin)[1], 2],
               100 * ((BalAccCV_fin[dim(BalAccCV_fin)[1], 2] -
                         BalAccCV_fin[1, 2]) / (BalAccCV_fin[1, 2])))
  )

  rownames(first_last) <- c("First iteration", "Last iteration", "Percentage")

  saver( first_last,  name="FirstLast",   format =  ".csv",
         main_method="self",
         method = method, feature_sel = feature_sel)

  return(first_last = first_last)
}
