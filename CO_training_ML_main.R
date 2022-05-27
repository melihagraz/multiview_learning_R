
# 1 subpool

CoTrain_cv_errorBar<-function(lab, unlabeled, 
                              method = "Naive Bayes",
                              train_prob = 0.8, 
                              n_subPool=75, 
                              n_iteration=30, 
                              imbalanced=TRUE,
                              feature_sel=TRUE,
                              n_neg=7,
                              n_pos=1,
                              n_fold=5){
  
  
  
  
  
  if(!feature_sel){
    
    View1_Medical_Sel_col<-c("fpg_mean", "fpg_std" ,"hba1c_mean","hba1c_std", "out" )
    View2_Medical_Sel_col<-c("g1check_mean","g1check_std","g1diabed_mean","g1diabed_std",
                             "g1nutrit_mean","g1nutrit_std", "out")
    View1<-lab[ View1_Medical_Sel_col] 
    View2<-lab[ View2_Medical_Sel_col]
  }else{
    View1MRMR_col<-c("fpg_std" ,"hba1c_mean", "out" )
    View2MRMR_col<-c("g1diabed_std"  , "nphl_insulin_mean", "out"  )
    
    View1<-lab[View1MRMR_col] 
    View2<-lab[View2MRMR_col]
    
  }
  
  view1_lab<-View1
  view2_lab<-View2
  
  unlab_pool <- unlabeled
  
  #---------------------------------------------------------------------------- library
  if(!require(dplyr)){install.packages("dplyr");require(dplyr)}
  if(!require(e1071)){install.packages("e1071");require(e1071)}
  if(!require(caret)){install.packages("caret");require(caret)}
  if(!require(randomForest)){install.packages("randomForest");require(randomForest)}
  if(!require(ggplot2)){install.packages("ggplot2");require(ggplot2)}
  if(!require(ggpubr)){install.packages("ggpubr");require(ggpubr)}
  if(!require(gridExtra)){install.packages("gridExtra");require(gridExtra)}
  
  
  
  
  
  
  #------------------------------------------------------------------------------- main function
  
  
  SensCV_v1<-data.frame()
  SensCV_v2<-data.frame()
  
  SpecCV_v1<-data.frame()
  SpecCV_v2<-data.frame()
  
  NPVCV_v1<-data.frame()
  NPVCV_v2<-data.frame()
  
  PPVCV_v1<-data.frame()
  PPVCV_v2<-data.frame()
  
  AccCV_v1<-data.frame()
  AccCV_v2<-data.frame()
  
  F1CV_v1<-data.frame()
  F1CV_v2<-data.frame()
  
  
  BalAccCV_v1<-data.frame()
  BalAccCV_v2<-data.frame()
  
  TP_v1<-data.frame()
  TP_v2<-data.frame()
  
  TN_v1<-data.frame()
  TN_v2<-data.frame()
  
  FP_v1<-data.frame()
  FP_v2<-data.frame()
  
  FN_v1<-data.frame()
  FN_v2<-data.frame()
  
  dim_unlab<-c()
  dim_V1<-c()
  dim_V2<-c()
  
  set.seed(123)  
  folds<-createFolds(view1_lab$out, k=n_fold)
  
  # Blum, 1998 proposed 35 iterations
  
  test_real<-c()
  test_fin1 <- c()
  test_fin2 <-c()
  i_cv<-1
  for (i_cv in 1:n_fold) {
    
    
    print(paste0("############################################ FOLD", i_cv))
    
    #------------------------------------------------------------------------------- pool 
    indices <- sample(1:nrow(unlab_pool), n_subPool*2 ) # 150 for now I will split them for U1' and U2'
    unlab_subpool  <- unlab_pool[indices, ]   
    unlab_pool     <- unlab_pool[-indices,]
    unlab_subpool1 <- unlab_subpool[1:(n_subPool), ] # U1'
    unlab_subpool2 <- unlab_subpool[((n_subPool)+1):(n_subPool*2),] # U2' 
    
    testIndex<-folds[[i_cv]]
    
    
    
    train_view1<-view1_lab[-testIndex, ]
    test_view1 <-view1_lab[ testIndex, ]
    
    train_view2<-view2_lab[-testIndex, ]
    test_view2 <-view2_lab[ testIndex, ]
    
    if(! identical(train_view1$out, train_view2$out) ) 
      stop("  output of view 1 and view 2 are not identical in train data.\n")
    
    if(! identical(test_view1$out, test_view2$out) ) 
      stop("  output of view 1 and view 2 are not identical in test data.\n")
    
    
    test_real<-append(test_real,test_view1$out)
    
    
    
    
    
    
    #---------------------------------------------------------------------------- imbalanced
    if(!imbalanced){
      train_view1<-train_view1
      train_view2<-train_view2
    }else{
      
      imbal_dat1<-train_view1
      imbal_dat2<-train_view2
      
      xx<-table(imbal_dat1$out)
      xx
      maj_cl<-c()
      if(table(imbal_dat1$out)[[1]] > table(imbal_dat1$out)[[2]]){
        maj_cl = as.numeric(names(xx)[1])
      }else{
        maj_cl = as.numeric(names(xx)[2])   
      }
      maj_cl
      # major gruptan minor kadar cekiyorum
      min_val<-c()
      if(table(imbal_dat1$out)[[1]]<table(imbal_dat1$out)[[2]]){
        min_val = table(imbal_dat1$out)[[1]]
      }else{
        min_val = table(imbal_dat1$out)[[2]]  
      }
      set.seed(123)
      maj_imbal_res <-sample(which(imbal_dat1$out==maj_cl), min_val)
      min_imbal_res<-which(imbal_dat1$out!=maj_cl)
      imb_fin<- c(maj_imbal_res, min_imbal_res)
      fin_dat1<-imbal_dat1[imb_fin,]
      
      train_view1<-fin_dat1
      train_view2<-imbal_dat2[imb_fin,]
      
    }
    
    iterations <-  1
    
    while (iterations <= n_iteration) {
      
      
      method <- match.arg(method, choices=c("Naive Bayes", "Random Forest"))
      if( !method%in%c("Naive Bayes", "Random Forest") ) 
        stop("Method must be 'Naive Bayes' or 'Random Forest'.\n")
      
      #----------------------------------------- view1_lab NB  
      
      if (method == "Naive Bayes") {
        
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
      if (method == "Naive Bayes") {
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
      # generating the for unlabeled data
      unlab_pred_u1$out<- ifelse(unlab_pred_u1$pr>0.5, 0, 1) 
      # sorting descending order
      unlab_pred_u1<-unlab_pred_u1[order(-unlab_pred_u1$pr),]
      
      #v2
      unlab_pred_u2 <- cbind(unlab_subpool2 , pr = naiv_pre_v2[,1] )
      head(unlab_pred_u2);tail(unlab_pred_u2)
      # generating the for unlabeled data
      unlab_pred_u2$out<- ifelse(unlab_pred_u2$pr>0.5, 0, 1) 
      # sorting descending order
      unlab_pred_u2<-unlab_pred_u2[order(-unlab_pred_u2$pr),]
      
      
      
      #--------------------------------------------------------------------------- update labels 
      # dimension of pseudo labels of view 1
      n_u1_pred <- dim(unlab_pred_u1)[1]
      n_u1_pred
      
      # selecting the first n_neg (7 negative label for our research) and last n_pos (1 positive label) pseudo
      # labels from the unlab_pred_u1 (predictions of U1')
      which( unlab_pred_u2$out==1)
      sel_u1 <- unlab_pred_u1[c(1:n_neg,seq(n_u1_pred,1,-1)[1:n_pos]), ]
      sel_u1
      
      # sometimes NB or RF predict all the observations as 0, that is why I am checking 
      # if the selected value (n_pos) is whether positive or not, if not I am not going to take it
      if( sel_u1$out[(n_neg+n_pos)] != 1){
        sel_u1 <-  sel_u1[-(n_neg+n_pos), ]
      } 
      sel_u1
      
      # # Drop pseudo-labeled instances from unlabeled data
      # dropping these (sel_u1) selected pseudo labels from U1, let us call it: dropped U1'
      unlab_subpool1_drop  <- unlab_subpool1[-which(unlab_subpool1$MaskID %in% 
                                                      c(sel_u1$MaskID)), ]
      
      set.seed(123)
      #in this part: since we dropped n_neg + n_pos labels fom U1', I am sampling n_neg + n_pos
      # from U and addng it to U1' 
      # sampling indexes in U
      ind<-sample(1:nrow(unlab_pool), (n_neg + n_pos))
      # sampling from U
      unlab_subpool1_add<-unlab_pool[ind,]
      # dropping selected observations above, let us call it U1'_sample
      unlab_pool<-unlab_pool[-ind,] # U1'_sample
      # rowbind between U1'_sample + dropped U1' 
      new_unlab_subpool1<- rbind(unlab_subpool1_drop, unlab_subpool1_add)
      
      # I am updating View2 by using pseudo labels selected from View 1 classifier
      add_view2 <- sel_u1[colnames(train_view2)]
      add_view2
      # this is my new (updated ) labeled view 2
      # Add pseudo-labeled data to view2
      new_view2_lab<- rbind(train_view2, add_view2)
      
      
      
      #------------------------------------------------------------------------------ 
      # we will apply the same steps below,  as we did above between  (x)-(xx)
      #-----view 2 (x)
      # update lab 
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
      set.seed(123)
      ind2<-sample(1:nrow(unlab_pool), (n_neg+n_pos))
      unlab_subpool2_add<-unlab_pool[ind2,]
      unlab_pool<-unlab_pool[-ind,]
      new_unlab_subpool2<- rbind(unlab_subpool2_drop, unlab_subpool2_add)
      
      
      # update View2
      
      # view 2 eklenecekler
      add_view1 <- sel_u2[colnames(train_view1)]
      add_view1
      
      new_view1_lab<- rbind(train_view1, add_view1)
      
      #------------------------------------------------------------------------------- Final Update  
      # now I can assign updated label and unlabeled datasets into the iteration 
      train_view1 <-new_view1_lab
      train_view2 <-new_view2_lab
      unlab_subpool2<- new_unlab_subpool2
      unlab_subpool1<- new_unlab_subpool1
      
      
      
      
      print(paste0("############# iteration ######### ", iterations))
      iterations = iterations + 1
      
    } # while 
    
    #concatenate results
    test_fin1<- append(test_fin1,as.numeric( as.character(naiv_pre_test_v1)))
    test_fin2<- append(test_fin2,as.numeric( as.character(naiv_pre_test_v2)))
    
  } # for fold
  
  
  data_summary<-function(data){
    ncol<-dim(data)[2]
    data_mean_sd<-data.frame(iterations=c(1:ncol),
                             Mean=c(apply(data,2, mean,na.rm=TRUE)), 
                             Sd=c(apply(data,2,sd,na.rm=TRUE)))
    return(data_mean_sd)
    
  }
  
  # Sens
  SensCV_fin1 <-    data_summary(SensCV_v1)
  SensCV_fin2 <-    data_summary(SensCV_v2)
  Nit<-dim( SensCV_fin1)[1]
  
  SensCV_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                          Iterations = c(1:(iterations-1), 1:(iterations-1)),
                          Mean = c(SensCV_fin1[,2], SensCV_fin2[,2]),
                          Sd= c(SensCV_fin1[,3], SensCV_fin2[,3])
  )
  
  Sen_pl<- ggplot(SensCV_fin, aes(x=Iterations, y=Mean, group=View, color=View))+
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=.1) +geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+theme_minimal()+ ylab("Sensitivity")+theme_bw()+ylim(0,1)
  Sen_pl<-Sen_pl +    theme(text = element_text(size = 20)) 
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      ggsave("2CO_NB_Sen_FS.pdf")
    }else{
      ggsave("1CO_NB_Sen_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4CO_RF_Sen_FS.pdf")
    }else{
      ggsave("3CO_RF_Sen_MedSel.pdf")
    }
  }
  
  
  # PLOT
  
  SpecCV_fin1 <-    data_summary( SpecCV_v1)
  SpecCV_fin2 <-    data_summary( SpecCV_v2)
  Nit<-dim( SensCV_fin1)[1]
  
  SpecCV_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                          Iterations = c(1:(iterations-1), 1:(iterations-1)),
                          Mean = c(SpecCV_fin1[,2], SpecCV_fin2[,2]),
                          Sd= c(SpecCV_fin1[,3], SpecCV_fin2[,3])
  )
  
  Spec_pl<- ggplot(SpecCV_fin, aes(x=Iterations, y=Mean, group=View, color=View))+
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=.1) +geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+theme_minimal()+ ylab("Specificity")+theme_bw()+ylim(0,1)
  
  Spec_pl<-Spec_pl +  theme(text = element_text(size = 20))
  
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      ggsave("2CO_NB_Spec_FS.pdf")
    }else{
      ggsave("1CO_NB_Spec_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4CO_RF_Spec_FS.pdf")
    }else{
      ggsave("3CO_RF_Spec_MedSel.pdf")
    }
  }
  
  
  # ppv_plot
  
  PPVCV_fin1 <-    data_summary( PPVCV_v1)
  PPVCV_fin2 <-    data_summary( PPVCV_v2)
  Nit<-dim( SensCV_fin1)[1]
  
  PPVCV_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                         Iterations = c(1:(iterations-1), 1:(iterations-1)),
                         Mean = c(PPVCV_fin1[,2], PPVCV_fin2[,2]),
                         Sd= c(PPVCV_fin1[,3], PPVCV_fin2[,3])
  )
  
  PPV_pl<- ggplot(PPVCV_fin, aes(x=Iterations, y=Mean, group=View, color=View))+
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=.1) +geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+theme_minimal()+ ylab("PPV")+theme_bw()+ylim(0,1)
  PPV_pl<-PPV_pl +  theme(text = element_text(size = 20))
  
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      ggsave("2CO_NB_PPV_FS.pdf")
    }else{
      ggsave("1CO_NB_PPV_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4CO_RF_PPV_FS.pdf")
    }else{
      ggsave("3CO_RF_PPV_MedSel.pdf")
    }
  }
  
  
  
  # npv_plot
  
  NPVCV_fin1 <-    data_summary( NPVCV_v1)
  NPVCV_fin2 <-    data_summary( NPVCV_v2)
  Nit<-dim( SensCV_fin1)[1]
  
  NPVCV_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                         Iterations = c(1:(iterations-1), 1:(iterations-1)),
                         Mean = c(NPVCV_fin1[,2], NPVCV_fin2[,2]),
                         Sd= c(NPVCV_fin1[,3], NPVCV_fin2[,3])
  )
  
  NPV_pl<- ggplot(NPVCV_fin, aes(x=Iterations, y=Mean, group=View, color=View))+
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=.1) +geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+theme_minimal()+ ylab("NPV")+theme_bw()+ylim(0,1)
  NPV_pl<-NPV_pl+ theme(text = element_text(size = 20))
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      ggsave("2CO_NB_NPV_FS.pdf")
    }else{
      ggsave("1CO_NB_NPV_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4CO_RF_NPV_FS.pdf")
    }else{
      ggsave("3CO_RF_NPV_MedSel.pdf")
    }
  }
  
  
  # ACC
  
  ACCCV_fin1 <-    data_summary( AccCV_v1)
  ACCCV_fin2 <-    data_summary( AccCV_v2)
  Nit<-dim( SensCV_fin1)[1]
  
  AccCV_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                         Iterations = c(1:(iterations-1), 1:(iterations-1)),
                         Mean = c(ACCCV_fin1[,2], ACCCV_fin2[,2]),
                         Sd= c(ACCCV_fin1[,3], ACCCV_fin2[,3])
  )
  
  Acc_pl<- ggplot(AccCV_fin, aes(x=Iterations, y=Mean, group=View, color=View))+
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=.1) +geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+theme_minimal()+ ylab("Accuracy")+theme_bw()+ylim(0,1)
  Acc_pl<- Acc_pl + theme(text = element_text(size = 20)) 
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      ggsave("2CO_NB_Acc_FS.pdf")
    }else{
      ggsave("1CO_NB_Acc_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4CO_RF_Acc_FS.pdf")
    }else{
      ggsave("3CO_RF_Acc_MedSel.pdf")
    }
  }
  
  
  
  
  # F1
  F1CV_fin1 <-    data_summary(F1CV_v1)
  F1CV_fin2 <-    data_summary(F1CV_v2)
  Nit<-dim( SensCV_fin1)[1]
  
  F1CV_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                        Iterations = c(1:(iterations-1), 1:(iterations-1)),
                        Mean = c(F1CV_fin1[,2], F1CV_fin2[,2]),
                        Sd= c(F1CV_fin1[,3], F1CV_fin2[,3])
  )
  
  F1_pl<- ggplot(F1CV_fin, aes(x=Iterations, y=Mean, group=View, color=View))+
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=.1) +geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+theme_minimal()+ ylab("F1-Score")+theme_bw()+ylim(0,1)
  F1_pl<-F1_pl +  theme(text = element_text(size = 20))
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      ggsave("2CO_NB_F1_FS.pdf")
    }else{
      ggsave("1CO_NB_F1_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4CO_RF_F1_FS.pdf")
    }else{
      ggsave("3CO_RF_F1_MedSel.pdf")
    }
  }
  
  # Bal Acc
  
  
  BalAccCV_fin1 <-    data_summary(BalAccCV_v1)
  BalAccCV_fin2 <-    data_summary(BalAccCV_v2)
  Nit<-dim( SensCV_fin1)[1]
  
  BalAccCV_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                            Iterations = c(1:(iterations-1), 1:(iterations-1)),
                            Mean = c(BalAccCV_fin1[,2], BalAccCV_fin2[,2]),
                            Sd= c(BalAccCV_fin1[,3], BalAccCV_fin2[,3])
  )
  
  BalAcc_pl<- ggplot( BalAccCV_fin, aes(x=Iterations, y=Mean, group=View, color=View))+
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=.1) +geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+theme_minimal()+ ylab("Balance Accuracy")+theme_bw()+ylim(0,1)
  BalAcc_pl <- BalAcc_pl +  theme(text = element_text(size = 20))
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      ggsave("2CO_NB_BalAcc_FS.pdf")
    }else{
      ggsave("1CO_NB_BalAcc_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4CO_RF_BalAcc_FS.pdf")
    }else{
      ggsave("3CO_RF_BalAcc_MedSel.pdf")
    }
  }
  
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
  
  TP_pl<- ggplot(TP_fin, aes(x=Iterations, y=Mean, group=View, color=View))+
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=.1) +geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+theme_minimal()+ ylab("TP")+theme_bw()
  TP_pl<-TP_pl +  theme(text = element_text(size = 20))
  
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      ggsave("2CO_NB_TP_FS.pdf")
    }else{
      ggsave("1CO_NB_TP_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4CO_RF_TP_FS.pdf")
    }else{
      ggsave("3CO_RF_TP_MedSel.pdf")
    }
  }
  
  # TN 
  TN_fin1 <-    data_summary( TN_v1)
  TN_fin2 <-    data_summary( TN_v2)
  Nit<-dim(SensCV_fin1)[1]
  
  TN_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                      Iterations = c(1:(iterations-1), 1:(iterations-1)),
                      Mean = c(TN_fin1[,2], TN_fin2[,2]),
                      Sd= c(TN_fin1[,3], TN_fin2[,3])
  )
  
  TN_pl<- ggplot(TN_fin, aes(x=Iterations, y=Mean, group=View, color=View))+
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=.1) +geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+theme_minimal()+ ylab("TN")+theme_bw()
  TN_pl<-TN_pl+ theme(text = element_text(size = 20))
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      ggsave("2CO_NB_TN_FS.pdf")
    }else{
      ggsave("1CO_NB_TN_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4CO_RF_TN_FS.pdf")
    }else{
      ggsave("3CO_RF_TN_MedSel.pdf")
    }
  }
  
  
  
  
  # FP 
  FP_fin1 <-    data_summary( FP_v1)
  FP_fin2 <-    data_summary( FP_v2)
  Nit<-dim(SensCV_fin1)[1]
  
  FP_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                      Iterations = c(1:(iterations-1), 1:(iterations-1)),
                      Mean = c(FP_fin1[,2], FP_fin2[,2]),
                      Sd= c(FP_fin1[,3], FP_fin2[,3])
  )
  
  FP_pl<- ggplot(FP_fin, aes(x=Iterations, y=Mean, group=View, color=View))+
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=.1) +geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+theme_minimal()+ ylab("FP")+theme_bw()
  FP_pl<-FP_pl +  theme(text = element_text(size = 20))
  
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      ggsave("2CO_NB_FP_FS.pdf")
    }else{
      ggsave("1CO_NB_FP_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4CO_RF_FP_FS.pdf")
    }else{
      ggsave("3CO_RF_FP_MedSel.pdf")
    }
  }
  
  
  #FN
  FN_fin1 <-    data_summary( FN_v1)
  FN_fin2 <-    data_summary( FN_v2)
  Nit<-dim(SensCV_fin1)[1]
  
  FN_fin<-data.frame( View = c(rep("View1", (iterations-1)),rep("View2", (iterations-1))),
                      Iterations = c(1:(iterations-1), 1:(iterations-1)),
                      Mean = c(FN_fin1[,2], FN_fin2[,2]),
                      Sd= c(FN_fin1[,3], FN_fin2[,3])
  )
  
  FN_pl<- ggplot(FN_fin, aes(x=Iterations, y=Mean, group=View, color=View))+
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=.1) +geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+theme_minimal()+ ylab("FN")+theme_bw()
  FN_pl<-FN_pl+ theme(text = element_text(size = 20))
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      ggsave("2CO_NB_FN_FS.pdf")
    }else{
      ggsave("1CO_NB_FN_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4CO_RF_FP_FS.pdf")
    }else{
      ggsave("3CO_RF_FP_MedSel.pdf")
    }
  }
  
  
  #-----------------------------------------------------------TP-TN-FP-FN
  
  TPNF_v1<- rbind(TP_v1, TN_v1, FP_v1, FN_v1)
  rownames(TPNF_v1)<- c(paste0(1:n_fold, c("TP")),paste0(1:n_fold, c("TN")),
                        paste0(1:n_fold, c("FP")),paste0(1:n_fold, c("FN")))
  colnames(TPNF_v1)<- paste0("it", 1:(iterations-1))
  
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      write.csv(TPNF_v1, "2COV1_ConMat_Data_NB_FS.csv" )
    }else{
      write.csv(TPNF_v1, "1COV1_ConMat_Data_NB_MedSel.csv" )
    }
  }else{
    if(feature_sel==TRUE){
      write.csv(TPNF_v1, "4COV1_ConMat_Data_RF_FS.csv" )
    }else{
      write.csv(TPNF_v1, "3COV1_ConMat_Data_RF_MedSel.csv" )
    }
  }
  
  TPNF_v2<- rbind(TP_v2, TN_v2, FP_v2, FN_v2)
  rownames(TPNF_v2)<- c(paste0(1:n_fold, c("TP")),paste0(1:n_fold, c("TN")),
                        paste0(1:n_fold, c("FP")),paste0(1:n_fold, c("FN")))
  colnames(TPNF_v2)<- paste0("it", 1:(iterations-1))
  
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      write.csv(TPNF_v2, "2COV2_ConMat_Data_NB_FS.csv" )
    }else{
      write.csv(TPNF_v1, "1COV2_ConMat_Data_NB_MedSel.csv" )
    }
  }else{
    if(feature_sel==TRUE){
      write.csv(TPNF_v1, "4COV2_ConMat_Data_RF_FS.csv" )
    }else{
      write.csv(TPNF_v1, "3COV2_ConMat_Data_RF_MedSel.csv" )
    }
  }
  
  
  
  
  # 4 IN 1 tp tn ..
  p.plot1<-ggarrange(TP_pl, TN_pl, FP_pl, FN_pl + 
                       rremove("x.text"), 
                     labels = c("A", "B", "C", "D" ),
                     ncol = 2, nrow = 2)
  
  tit_plt1<- paste(method, "Co-Training Confusion Matrix")
  p.plot<- grid.arrange(p.plot1,  top=tit_plt1) 
  
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      ggsave("2CO_NB_Conf4in1_FS.pdf")
    }else{
      ggsave("1CO_NB_Conf4in1_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4CO_RF_Conf4in1_FS.pdf")
    }else{
      ggsave("3CO_RF_Conf4in1_MedSel.pdf")
    }
  }
  
  
  p.plot<-ggarrange(F1_pl, PPV_pl, Spec_pl, Sen_pl + 
                      rremove("x.text"), 
                    labels = c("A", "B", "C", "D" ),
                    ncol = 2, nrow = 2)
  
  tit_plt<- paste(method, "Test Data Results for Co-Training")
  p.plot<- grid.arrange(p.plot,  top=tit_plt) 
  
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      ggsave("2CO_NB_4in1_FS.pdf")
    }else{
      ggsave("1CO_NB_4in1_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4CO_RF_4in1_FS.pdf")
    }else{
      ggsave("3CO_RF_4in1_MedSel.pdf")
    }
  }
  
  
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
  
  
  #concatanete 
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
  
  
  
  return(list( plot=p.plot,  first_last= first_last, test_fin1=test_fin1,
               Conc_AND=Conc_AND2, Conc_OR=Conc_OR2 ))
}# function

