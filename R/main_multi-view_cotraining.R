
#' @title Co-Training Multiview  Machine Learning Model
#'
#' @description
#' You can apply Co-Training Multi-view Machine Learning Model with the
#' # \code{CoTrain_CV} function.
#'
#' @param lab labeled tabulated data
#' @param unlabeled tabulated unlabeled data
#' @param method Machine Learning methods you can select;
#'   Naive Bayes (nb)  , Random Forest (rf)
#' @param train_prob train ratio
#' @param n_subPool number of observation of unlabeled pool
#' @param n_iteration number of iterations that we want to run
#' @param imbalanced imbalanced data problem is fixed or not
#' @param feature_sel optimal features are selected from the conventional machine learning
#' @param n_neg how many confidence negative labels to choose
#' @param n_pos  how many confidence positive labels to choose
#' @param n_fold number of folds
#' @param seed a positive integer. It is used to control random number generating steps. If not NULL,
#' folds are randomly generated using the user-defined seed value. It might be usefull to select
#' the same folds  and compare multiple models using the same cross-validation folds.
#'  Default is NULL, i.e., unrestricted.
#'
#' @param ... further arguments. Currently, has no effect.
#'
#' @importFrom  dplyr e1071 caret randomForest ggplot2 ggpubr gridExtra
#'
#' @return a matrix including cross-validated performance measures of the fitted model.
#'
#' @examples
#' CoTrain_CV(lab, unlabeled, method = "nb", train_prob = 0.8,
#' n_subPool=75, n_iteration=30, imbalanced=TRUE,  feature_sel=TRUE,
#' n_neg=7, n_pos=1, n_fold=5, seed = 123, ...)
#'
#' @export
#'
#'
#---------------------------------------------------------------------------- required R libraries
library(dplyr)
library(e1071)
library(caret)
library(randomForest)
library(ggplot2)
library(ggpubr)
library(gridExtra)
source("helper_functions.R")

#----- Internal function ------#
 
CoTrain_CV <-function(lab, unlabeled, method = c("nb", "rf"), train_prob = 0.8,
                      n_subPool=75, n_iteration=30, imbalanced=TRUE,
                      feature_sel=TRUE, n_neg=7, n_pos=1,
                      n_fold=5, seed = 123, ...){

  

  method <- match.arg(method)

  if(feature_sel){

    View1MRMR_col<-c("fpg_std" ,"hba1c_mean", "out" )
    View2MRMR_col<-c("g1diabed_std"  , "nphl_insulin_mean", "out"  )

    View1_lab<-lab[View1MRMR_col]
    View2_lab<-lab[View2MRMR_col]

  }else{
    View1_Medical_Sel_col<-c("fpg_mean", "fpg_std" ,"hba1c_mean","hba1c_std", "out" )
    View2_Medical_Sel_col<-c("g1check_mean","g1check_std","g1diabed_mean","g1diabed_std",
                             "g1nutrit_mean","g1nutrit_std", "out")
    View1_lab<-lab[ View1_Medical_Sel_col]
    View2_lab<-lab[ View2_Medical_Sel_col]

  }
  unlab_pool <- unlabeled
  #------------------------------------------------------------------------------- main function
  SensCV_v1<-data.frame(); SensCV_v2<-data.frame()

  SpecCV_v1<-data.frame(); SpecCV_v2<-data.frame()

  NPVCV_v1<-data.frame();  NPVCV_v2<-data.frame()

  PPVCV_v1<-data.frame(); PPVCV_v2<-data.frame()

  AccCV_v1<-data.frame(); AccCV_v2<-data.frame()

  F1CV_v1<-data.frame(); F1CV_v2<-data.frame()

  BalAccCV_v1<-data.frame(); BalAccCV_v2<-data.frame()

  TP_v1<-data.frame(); TP_v2<-data.frame()

  TN_v1<-data.frame();  TN_v2<-data.frame()

  FP_v1<-data.frame();  FP_v2<-data.frame()

  FN_v1<-data.frame();  FN_v2<-data.frame()

  dim_unlab<-c();   dim_V1<-c();   dim_V2<-c()

  if (!is.null(seed)){
    set.seed(seed)
  }

  folds<-createFolds(View1_lab$out, k=n_fold)
  test_real<-numeric()
  test_fin1 <- numeric(); test_fin2 <-numeric()
  for (i_cv in 1:n_fold) {
    #------------------------------------------------------------------------------- pool
    indices        <- sample(1:nrow(unlabeled), n_subPool*2) # 150 for now I will split them for U1' and U2'
    unlab_subpool  <- unlabeled[indices, ]
    unlabeled      <- unlabeled[-indices,]
    unlab_subpool1 <- unlab_subpool[1:(n_subPool), ] # U1'
    unlab_subpool2 <- unlab_subpool[((n_subPool)+1):(n_subPool*2),] # U2'

    testIndex      <- folds[[i_cv]]

    train_view1    <- View1_lab[-testIndex, ]
    test_view1     <- View1_lab[ testIndex, ]

    train_view2    <- View2_lab[-testIndex, ]
    test_view2     <- View2_lab[ testIndex, ]

    if(! identical(train_view1$out, train_view2$out) )
      stop("  output of view 1 and view 2 are not identical in train data.\n")

    if(! identical(test_view1$out, test_view2$out) )
      stop("  output of view 1 and view 2 are not identical in test data.\n")

    test_real      <- append(test_real,test_view1$out)

    #---------------------------------------------------------------------------- imbalanced
    if(imbalanced){
      balanced_Co(train_view1, train_view2, seed=seed)
    }

    iterations <-  1
    while (iterations <= n_iteration) {
      #----------------------------------------- view1_lab NB
      if (method == "nb") {

        # NB
        naiv_bay_v1 <- naiveBayes(as.factor(out) ~.,
                                  data = train_view1 )


        naiv_pre_v1       <- predict(naiv_bay_v1, unlab_subpool1, type = "raw")
        naiv_pre_test_v1  <- predict(naiv_bay_v1,  test_view1)


        conf_res <- confusionMatrix(predict(naiv_bay_v1,  test_view1),
                                    as.factor(test_view1$out),  positive = "1")

        SensCV_v1[i_cv, iterations]<-round(conf_res$byClass[[1]],4)
        SpecCV_v1[i_cv, iterations]<-round(conf_res$byClass[[2]],4)
        PPVCV_v1[i_cv, iterations]<-round(conf_res$byClass[[3]],4)
        NPVCV_v1[i_cv, iterations]<-round(conf_res$byClass[[4]],4)
        AccCV_v1[i_cv, iterations]<-round(conf_res$overall[[1]],4)
        F1CV_v1[i_cv, iterations]<-round(conf_res$byClass[[7]],4)
        BalAccCV_v1[i_cv, iterations]<-round(conf_res$byClass[[11]],4)

        TN_v1[i_cv, iterations]<-round(conf_res$table[1],4)
        TP_v1[i_cv, iterations]<-round(conf_res$table[4],4)
        FN_v1[i_cv, iterations]<-round(conf_res$table[3],4)
        FP_v1[i_cv, iterations]<-round(conf_res$table[2],4)

      } else {
        naiv_bay_v1 <- randomForest(as.factor(out) ~ .,
                                    data = train_view1,
                                    method = 'class',
                                    ntree=500)
        naiv_pre_v1 <- predict(naiv_bay_v1, unlab_subpool1, type="prob")
        naiv_pre_test_v1  <- predict(naiv_bay_v1,  test_view1)


        conf_res <- confusionMatrix(predict(naiv_bay_v1,  test_view1),
                                    as.factor(test_view1$out),  positive = "1")

        SensCV_v1[i_cv, iterations]<-round(conf_res$byClass[[1]],4)
        SpecCV_v1[i_cv, iterations]<-round(conf_res$byClass[[2]],4)
        PPVCV_v1[i_cv, iterations]<-round(conf_res$byClass[[3]],4)
        NPVCV_v1[i_cv, iterations]<-round(conf_res$byClass[[4]],4)
        AccCV_v1[i_cv, iterations]<-round(conf_res$overall[[1]],4)
        F1CV_v1[i_cv, iterations]<-round(conf_res$byClass[[7]],4)
        BalAccCV_v1[i_cv, iterations]<-round(conf_res$byClass[[11]],4)

        TN_v1[i_cv, iterations]<-round(conf_res$table[1],4)
        TP_v1[i_cv, iterations]<-round(conf_res$table[4],4)
        FN_v1[i_cv, iterations]<-round(conf_res$table[3],4)
        FP_v1[i_cv, iterations]<-round(conf_res$table[2],4)
      }
      # View 2
      if (method == "nb") {
        naiv_bay_v2 <- naiveBayes(as.factor(out) ~.,
                                  data = train_view2)

        naiv_pre_v2  <- predict(naiv_bay_v2, unlab_subpool2, type = "raw")
        naiv_pre_test_v2  <- predict(naiv_bay_v2,  test_view2)

        conf_res <- confusionMatrix(predict(naiv_bay_v2,  test_view2),
                                    as.factor(test_view2$out),  positive = "1")

        SensCV_v2[i_cv, iterations]<-round(conf_res$byClass[[1]],4)
        SpecCV_v2[i_cv, iterations]<-round(conf_res$byClass[[2]],4)
        PPVCV_v2[i_cv, iterations]<-round(conf_res$byClass[[3]],4)
        NPVCV_v2[i_cv, iterations]<-round(conf_res$byClass[[4]],4)
        AccCV_v2[i_cv, iterations]<-round(conf_res$overall[[1]],4)

        F1CV_v2[i_cv, iterations]<-round(conf_res$byClass[[7]],4)
        BalAccCV_v2[i_cv, iterations]<-round(conf_res$byClass[[11]],4)


        TN_v2[i_cv, iterations]<-round(conf_res$table[1],4)
        TP_v2[i_cv, iterations]<-round(conf_res$table[4],4)
        FN_v2[i_cv, iterations]<-round(conf_res$table[3],4)
        FP_v2[i_cv, iterations]<-round(conf_res$table[2],4)

      } else {
        naiv_bay_v2 <- randomForest(as.factor(out) ~ .,
                                    data = train_view2,
                                    method = 'class',
                                    ntree=500)

        naiv_pre_v2 <- predict(naiv_bay_v2, unlab_subpool2, type="prob")
        naiv_pre_test_v2  <- predict(naiv_bay_v2,  test_view2)


        conf_res <- confusionMatrix(predict(naiv_bay_v2,test_view2),
                                    as.factor(test_view2$out),  positive = "1")

        SensCV_v2[i_cv, iterations]<-round(conf_res$byClass[[1]],4)
        SpecCV_v2[i_cv, iterations]<-round(conf_res$byClass[[2]],4)
        PPVCV_v2[i_cv, iterations]<-round(conf_res$byClass[[3]],4)
        NPVCV_v2[i_cv, iterations]<-round(conf_res$byClass[[4]],4)
        AccCV_v2[i_cv, iterations]<-round(conf_res$overall[[1]],4)

        F1CV_v2[i_cv, iterations]<-round(conf_res$byClass[[7]],4)
        BalAccCV_v2[i_cv, iterations]<-round(conf_res$byClass[[11]],4)


        TN_v2[i_cv, iterations]<-round(conf_res$table[1],4)
        TP_v2[i_cv, iterations]<-round(conf_res$table[4],4)
        FN_v2[i_cv, iterations]<-round(conf_res$table[3],4)
        FP_v2[i_cv, iterations]<-round(conf_res$table[2],4)
      }

      #----------------------------------------------------------------------------- UPDATE
      #v1
      unlab_pred_u1 <- cbind(unlab_subpool1 , pr = naiv_pre_v1[,1] )
      head(unlab_pred_u1);tail(unlab_pred_u1)

      unlab_pred_u1$out<- ifelse(unlab_pred_u1$pr>0.5, 0, 1)

      unlab_pred_u1<-unlab_pred_u1[order(-unlab_pred_u1$pr),]

      #v2
      unlab_pred_u2 <- cbind(unlab_subpool2 , pr = naiv_pre_v2[,1] )
      head(unlab_pred_u2);tail(unlab_pred_u2)

      unlab_pred_u2$out<- ifelse(unlab_pred_u2$pr>0.5, 0, 1)

      unlab_pred_u2<-unlab_pred_u2[order(-unlab_pred_u2$pr),]

      #--------------------------------------------------------------------------- update labels
      n_u1_pred <- dim(unlab_pred_u1)[1]
      n_u1_pred

      sel_u1 <- unlab_pred_u1[c(1:n_neg,seq(n_u1_pred,1,-1)[1:n_pos]), ]
      sel_u1

      if( sel_u1$out[(n_neg+n_pos)] != 1){
        sel_u1 <-  sel_u1[-(n_neg+n_pos), ]
      }
      sel_u1

      unlab_subpool1_drop  <- unlab_subpool1[-which(unlab_subpool1$MaskID %in%
                                                      c(sel_u1$MaskID)), ]

      if (!is.null(seed)){
        set.seed(seed)
      }

      ind<-sample(1:nrow(unlabeled), (n_neg + n_pos))

      unlab_subpool1_add<-unlabeled[ind,]

      unlabeled<-unlabeled[-ind,] # U1'_sample

      new_unlab_subpool1<- rbind(unlab_subpool1_drop, unlab_subpool1_add)

      add_view2 <- sel_u1[colnames(train_view2)]
      add_view2

      new_view2_lab<- rbind(train_view2, add_view2)

      n_u2_pred <- dim(unlab_pred_u2)[1]
      n_u2_pred
      #select sıralı oldugu icin ilk n_neg tanesi ve sonuncu
      sel_u2 <- unlab_pred_u2[c(1:n_neg,   seq(n_u2_pred,1,-1)[1:n_pos]), ]
      sel_u2

      if( sel_u2$out[(n_neg+n_pos)] != 1){
        sel_u2 <-  sel_u2[-(n_neg+n_pos), ]
      }
      sel_u2

      # update View2
      unlab_subpool2_drop  <- unlab_subpool2[-which(unlab_subpool2$MaskID %in%
                                                      c(sel_u2$MaskID)), ]
      if (!is.null(seed)){
        set.seed(seed)
      }
      ind2<-sample(1:nrow(unlabeled), (n_neg+n_pos))
      unlab_subpool2_add<-unlabeled[ind2,]
      unlab_pool<-unlab_pool[-ind,]
      new_unlab_subpool2<- rbind(unlab_subpool2_drop, unlab_subpool2_add)

      # update View2
      add_view1 <- sel_u2[colnames(train_view1)]
      add_view1

      new_view1_lab<- rbind(train_view1, add_view1)
      #------------------------------------------------------------------------------- Final Update
      # now I can assign updated label and unlabeled datasets into the iteration
      train_view1 <-new_view1_lab
      train_view2 <-new_view2_lab
      unlab_subpool2<- new_unlab_subpool2
      unlab_subpool1<- new_unlab_subpool1

      iterations = iterations + 1

    } # while
    #concatenate results
    test_fin1<- append(test_fin1,as.numeric( as.character(naiv_pre_test_v1)))
    test_fin2<- append(test_fin2,as.numeric( as.character(naiv_pre_test_v2)))

  } # for fold

  # Sens
  SensCV_fin1 <-    data_summary(SensCV_v1)
  SensCV_fin2 <-    data_summary(SensCV_v2)
  Nit<-dim(SensCV_fin1)[1]

  SensCV_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                          Iterations = c(1:(iterations-1), 1:(iterations-1)),
                          Mean = c(SensCV_fin1[,2], SensCV_fin2[,2]),
                          Sd= c(SensCV_fin1[,3], SensCV_fin2[,3])
  )
  plot_Co(  SensCV_fin, name = "Sensitivity" , method = method, format = ".pdf")
  # PLOT
  SpecCV_fin1 <-    data_summary(SpecCV_v1)
  SpecCV_fin2 <-    data_summary(SpecCV_v2)
  Nit<-dim( SensCV_fin1)[1]

  SpecCV_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                          Iterations = c(1:(iterations-1), 1:(iterations-1)),
                          Mean = c(SpecCV_fin1[,2], SpecCV_fin2[,2]),
                          Sd= c(SpecCV_fin1[,3], SpecCV_fin2[,3])
  )

  plot_Co(SpecCV_fin, name = "Specificity" , method = method,  format = ".pdf")

  # ppv_plot
  PPVCV_fin1 <-    data_summary( PPVCV_v1)
  PPVCV_fin2 <-    data_summary( PPVCV_v2)
  Nit<-dim( SensCV_fin1)[1]

  PPVCV_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                         Iterations = c(1:(iterations-1), 1:(iterations-1)),
                         Mean = c(PPVCV_fin1[,2], PPVCV_fin2[,2]),
                         Sd= c(PPVCV_fin1[,3], PPVCV_fin2[,3])
  )
  plot_Co(  PPVCV_fin, name = "PPV" , method = method, format = ".pdf")
  # npv_plot
  NPVCV_fin1 <-    data_summary( NPVCV_v1)
  NPVCV_fin2 <-    data_summary( NPVCV_v2)
  Nit<-dim( SensCV_fin1)[1]

  NPVCV_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                         Iterations = c(1:(iterations-1), 1:(iterations-1)),
                         Mean = c(NPVCV_fin1[,2], NPVCV_fin2[,2]),
                         Sd= c(NPVCV_fin1[,3], NPVCV_fin2[,3])
  )
  plot_Co(  NPVCV_fin, name = "NPV" , method = method,format = ".pdf")
  # ACC
  ACCCV_fin1 <-    data_summary( AccCV_v1)
  ACCCV_fin2 <-    data_summary( AccCV_v2)
  Nit<-dim( SensCV_fin1)[1]

  AccCV_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                         Iterations = c(1:(iterations-1), 1:(iterations-1)),
                         Mean = c(ACCCV_fin1[,2], ACCCV_fin2[,2]),
                         Sd= c(ACCCV_fin1[,3], ACCCV_fin2[,3])
  )

  plot_Co(  AccCV_fin, name = "Accuracy" , method = method, format = ".pdf")

  # F1
  F1CV_fin1 <-    data_summary(F1CV_v1)
  F1CV_fin2 <-    data_summary(F1CV_v2)
  Nit<-dim( SensCV_fin1)[1]

  F1CV_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                        Iterations = c(1:(iterations-1), 1:(iterations-1)),
                        Mean = c(F1CV_fin1[,2], F1CV_fin2[,2]),
                        Sd= c(F1CV_fin1[,3], F1CV_fin2[,3])
  )

  plot_Co(  F1CV_fin, name = "F1" , method = method, format = ".pdf")
  # Bal Acc
  BalAccCV_fin1 <-    data_summary(BalAccCV_v1)
  BalAccCV_fin2 <-    data_summary(BalAccCV_v2)
  Nit<-dim( SensCV_fin1)[1]

  BalAccCV_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                            Iterations = c(1:(iterations-1), 1:(iterations-1)),
                            Mean = c(BalAccCV_fin1[,2], BalAccCV_fin2[,2]),
                            Sd= c(BalAccCV_fin1[,3], BalAccCV_fin2[,3])
  )

  plot_Co(BalAccCV_fin, name = "BalancedAccuracy" , method = method,  format = ".pdf")
  #-----------------------------------------------------------------------------TP TN
  # TP
  TP_fin1 <-    data_summary( TP_v1)
  TP_fin2 <-    data_summary( TP_v2)
  Nit<-dim(SensCV_fin1)[1]

  TP_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                      Iterations = c(1:(iterations-1), 1:(iterations-1)),
                      Mean = c(TP_fin1[,2], TP_fin2[,2]),
                      Sd= c(TP_fin1[,3], TP_fin2[,3])
  )
  plot_Co(  TP_fin, name = "TP" , method = method,format = ".pdf")
  # TN
  TN_fin1 <-    data_summary( TN_v1)
  TN_fin2 <-    data_summary( TN_v2)
  Nit<-dim(SensCV_fin1)[1]

  TN_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                      Iterations = c(1:(iterations-1), 1:(iterations-1)),
                      Mean = c(TN_fin1[,2], TN_fin2[,2]),
                      Sd= c(TN_fin1[,3], TN_fin2[,3])
  )
  plot_Co(  TN_fin, name = "TN" , method = method, format = ".pdf")
  # FP
  FP_fin1 <-    data_summary( FP_v1)
  FP_fin2 <-    data_summary( FP_v2)
  Nit<-dim(SensCV_fin1)[1]

  FP_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                      Iterations = c(1:(iterations-1), 1:(iterations-1)),
                      Mean = c(FP_fin1[,2], FP_fin2[,2]),
                      Sd= c(FP_fin1[,3], FP_fin2[,3])
  )

  plot_Co(  FP_fin, name = "FP" , method = method, format = ".pdf")

  #FN
  FN_fin1 <-    data_summary( FN_v1)
  FN_fin2 <-    data_summary( FN_v2)
  Nit<-dim(SensCV_fin1)[1]

  FN_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                      Iterations = c(1:(iterations-1), 1:(iterations-1)),
                      Mean = c(FN_fin1[,2], FN_fin2[,2]),
                      Sd= c(FN_fin1[,3], FN_fin2[,3])
  )

  plot_Co(  FN_fin, name = "FN" , method = method, format = ".pdf")
  #-----------------------------------------------------------TP-TN-FP-FN
  TPNF_v1<- rbind(TP_v1, TN_v1, FP_v1, FN_v1)
  rownames(TPNF_v1)<- c(paste0(1:n_fold, c("TP")),paste0(1:n_fold, c("TN")),
                        paste0(1:n_fold, c("FP")),paste0(1:n_fold, c("FN")))

  colnames(TPNF_v1)<- paste0("it", 1:(iterations-1))

  saver( TPNF_v1,  name="ConMatrix",   format =  ".csv",
         main_method="COV1",
         method = method, feature_sel = feature_sel)

  TPNF_v2<- rbind(TP_v2, TN_v2, FP_v2, FN_v2)
  rownames(TPNF_v2)<- c(paste0(1:n_fold, c("TP")),paste0(1:n_fold, c("TN")),
                        paste0(1:n_fold, c("FP")),paste0(1:n_fold, c("FN")))
  colnames(TPNF_v2)<- paste0("it", 1:(iterations-1))

  saver( TPNF_v2,  name="ConMatrix",   format =  ".csv",
         main_method="COV2",
         method = method, feature_sel = feature_sel)

  first_last<-data.frame(Sen_v1 = c(SensCV_fin1[1,2], SensCV_fin1[dim(SensCV_fin1)[1],2],
                                    100*((SensCV_fin1[dim(SensCV_fin1)[1],2]- SensCV_fin1[1,2])/(SensCV_fin1[1,2]))),
                         Sen_v2 = c(SensCV_fin2[1,2], SensCV_fin2[dim(SensCV_fin2)[1],2],
                                    100*((SensCV_fin2[dim(SensCV_fin2)[1],2]- SensCV_fin2[1,2])/(SensCV_fin2[1,2]))),
                         Spec_v1 =c(SpecCV_fin1[1,2], SpecCV_fin1[dim(SpecCV_fin1)[1],2],
                                    100*((SpecCV_fin1[dim(SpecCV_fin1)[1],2]- SpecCV_fin1[1,2])/(SpecCV_fin1[1,2]))),
                         Spec_v2 =c(SpecCV_fin2[1,2], SpecCV_fin2[dim(SpecCV_fin2)[1],2],
                                    100*((SpecCV_fin2[dim(SpecCV_fin2)[1],2]- SpecCV_fin2[1,2])/(SpecCV_fin2[1,2]))),
                         PPV_v1 = c(PPVCV_fin1[1,2], PPVCV_fin1[dim(PPVCV_fin1)[1],2],
                                    100*((PPVCV_fin1[dim(PPVCV_fin1)[1],2]- PPVCV_fin1[1,2])/(PPVCV_fin1[1,2]))),
                         PPV_v2 = c(PPVCV_fin2[1,2], PPVCV_fin2[dim(PPVCV_fin2)[1],2],
                                    100*((PPVCV_fin2[dim(PPVCV_fin2)[1],2]- PPVCV_fin2[1,2])/(PPVCV_fin2[1,2]))),
                         NPV_v1 = c(NPVCV_fin1[1,2], NPVCV_fin1[dim(NPVCV_fin1)[1],2],
                                    100*((NPVCV_fin1[dim(NPVCV_fin1)[1],2]- NPVCV_fin1[1,2])/(NPVCV_fin1[1,2]))),
                         NPV_v2 = c(NPVCV_fin2[1,2], NPVCV_fin2[dim(NPVCV_fin2)[1],2],
                                    100*((NPVCV_fin2[dim(NPVCV_fin2)[1],2]- NPVCV_fin2[1,2])/(NPVCV_fin2[1,2]))),
                         F1_v1=   c(F1CV_fin1[1,2], F1CV_fin1[dim(F1CV_fin1)[1],2],
                                    100*((F1CV_fin1[dim(F1CV_fin1)[1],2]- F1CV_fin1[1,2])/(F1CV_fin1[1,2]))),
                         F1_v2=   c(F1CV_fin2[1,2], F1CV_fin2[dim(F1CV_fin2)[1],2],
                                    100*((F1CV_fin2[dim(F1CV_fin2)[1],2]- F1CV_fin2[1,2])/(F1CV_fin2[1,2]))),
                         Acc_v1=   c(ACCCV_fin1[1,2], ACCCV_fin1[dim(ACCCV_fin1)[1],2],
                                     100*((ACCCV_fin1[dim(ACCCV_fin1)[1],2]- ACCCV_fin1[1,2])/(ACCCV_fin1[1,2]))),
                         Acc_v2=   c(ACCCV_fin2[1,2], ACCCV_fin2[dim(ACCCV_fin2)[1],2],
                                     100*((ACCCV_fin2[dim(ACCCV_fin2)[1],2]- ACCCV_fin2[1,2])/(ACCCV_fin2[1,2]))),
                         BalAcc_v1=   c(BalAccCV_fin1[1,2], BalAccCV_fin1[dim(BalAccCV_fin1)[1],2],
                                        100*((BalAccCV_fin1[dim(BalAccCV_fin1)[1],2]- BalAccCV_fin1[1,2])/(BalAccCV_fin1[1,2]))),
                         BalAcc_v2=   c(BalAccCV_fin2[1,2], BalAccCV_fin2[dim(BalAccCV_fin2)[1],2],
                                        100*((BalAccCV_fin2[dim(BalAccCV_fin2)[1],2]- BalAccCV_fin2[1,2])/(BalAccCV_fin2[1,2]))))
  rownames(first_last)<-c("first_iteration", "second iteration", "percentage")

  test_fin12 <- test_fin1 + test_fin2

  # AND Rule
  test_AND_fin <- ifelse( test_fin12 != 2, 0,1)

  Conc_AND<-data.frame()
  conf_res1<- confusionMatrix( as.factor(test_AND_fin), as.factor(test_real),  positive = "1")
  Conc_AND[1, 1]<-round(conf_res1$byClass[[1]],4)#Sen
  Conc_AND[1, 2]<-round(conf_res1$byClass[[2]],4)#Spec
  Conc_AND[1, 3]<-round(conf_res1$byClass[[3]],4)#PPV
  Conc_AND[1, 4]<-round(conf_res1$byClass[[4]],4)#NPV
  Conc_AND[1, 5]<-round(conf_res1$overall[[1]],4)#Acc
  Conc_AND[1, 6]<-round(conf_res1$byClass[[7]],4)#F1
  Conc_AND[1, 7]<-round(conf_res1$byClass[[11]],4)#Bal
  colnames(Conc_AND)<-c("Sen","Spec",   "PPV","NPV", "Acc", "F1", "BalAcc")
  Conc_AND2<-Conc_AND[,c( "NPV","PPV", "Spec","Sen","Acc","F1")]
  # OR Rule
  test_OR_fin<-ifelse( test_fin12 != 0, 1,0)

  Conc_OR<-data.frame()
  conf_res2<- confusionMatrix(as.factor(test_OR_fin ), as.factor(test_real),  positive = "1")
  Conc_OR[1, 1]<-round(conf_res2$byClass[[1]],4)#Sen
  Conc_OR[1, 2]<-round(conf_res2$byClass[[2]],4)#Spec
  Conc_OR[1, 3]<-round(conf_res2$byClass[[3]],4)#PPV
  Conc_OR[1, 4]<-round(conf_res2$byClass[[4]],4)#NPV
  Conc_OR[1, 5]<-round(conf_res2$overall[[1]],4)#Acc
  Conc_OR[1, 6]<-round(conf_res2$byClass[[7]],4)#F1
  Conc_OR[1, 7]<-round(conf_res2$byClass[[11]],4)#Bal
  colnames( Conc_OR)<-c("Sen","Spec",   "PPV","NPV", "Acc", "F1", "BalAcc")
  Conc_OR2<-Conc_OR[,c( "NPV","PPV", "Spec","Sen","Acc","F1")]

  return(list( first_last= first_last, test_fin1=test_fin1,
               Conc_AND=Conc_AND2, Conc_OR=Conc_OR2 ))
}
