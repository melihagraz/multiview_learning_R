

self_feb20  <- function(dataL,   dataU, 
                        method        = "Naive Bayes", 
                        imbalanced    = TRUE,
                        neg_conf_prob = 0.9, 
                        pos_conf_prob = 0.1, 
                        feature_sel   = TRUE,
                        n_fold        = 5
){
  ###checking data
  
  if( missing(dataL) ) 
    stop("labeled/unlabeled data is reaquired.\n")
  
  lab<-dataL
  unlab<-dataU  
  
  #---------------------------------------------------------------------------- library
  if(!require(dplyr)){install.packages("dplyr");require(dplyr)}
  if(!require(caret)){install.packages("caret");require(caret)}
  if(!require(randomForest)){install.packages("randomForest");require(randomForest)}
  if(!require(ggplot2)){install.packages("ggplot2");require(ggplot2)}
  if(!require(ggpubr)){install.packages("ggpubr");require(ggpubr)}
  if(!require(ROSE)){install.packages("ROSE");require(ROSE)}
  if(!require(praznik)){install.packages("praznik");require(praznik)}
  if(!require(e1071)){install.packages("e1071");require(e1071)}
  if(!require(gridExtra)){install.packages("gridExtra");require(gridExtra)}  
  if(!require(PRROC)){install.packages("PRROC");require(PRROC)}
  
  
  #----------------------------------------------------------------------------- FEATURE SEL
  # we have already selected the features before
  if(!feature_sel){
    lab <-lab
  }else{
    sel_feat<-c("g1diabed_std", "hba1c_mean", "fpg_std", "nphl_insulin_mean")
    sel_feat_out<-c("g1diabed_std", "hba1c_mean", "fpg_std", "nphl_insulin_mean", "out")
    
    labF<-lab[sel_feat_out]
    unlabF<-unlab[sel_feat]
    
  }
  
  
  
  
  SensCV<-data.frame()
  SpecCV<-data.frame()
  PPVCV<-data.frame()
  NPVCV<-data.frame()
  F1CV<-data.frame()
  AccCV<-data.frame()
  BalAccCV<-data.frame()
  length_ix_pos<-data.frame()
  length_ix_neg<-data.frame()
  
  TP<-data.frame()
  TN<-data.frame()
  FP<-data.frame()
  FN<-data.frame()
  
  
  set.seed(123)  
  folds<-createFolds(lab$out, k=n_fold)
  
  
  for (i_cv in 1:n_fold) {
    
    # feature selection is TRUE or FALSE
    if(!feature_sel){
      lab <-dataL
      unlab <-dataU
    }else{
      
      lab<-labF
      unlab<-unlabF
      
    }
    
    testIndex <-folds[[i_cv]]
    testIndex
    train <- lab[-testIndex, ]
    head(train)
    test  <- lab[ testIndex, ]
    head(test)
    
    # imbalanced is TRUE or FALSE 
    if(!imbalanced){
      train<-train
    }else{
      imbal_dat<-train
      xx<-table(imbal_dat$out)
      xx
      maj_cl<-c()
      if(table(imbal_dat$out)[[1]] > table(imbal_dat$out)[[2]]){
        maj_cl = as.numeric(names(xx)[1])
      }else{
        maj_cl = as.numeric(names(xx)[2])   
      }
      maj_cl
      # major gruptan minor kadar cekiyorum
      min_val<-c()
      if(table(imbal_dat$out)[[1]]<table(imbal_dat$out)[[2]]){
        min_val = table(imbal_dat$out)[[1]]
      }else{
        min_val = table(imbal_dat$out)[[2]]  
      }
      
      set.seed(123)  
      maj_imbal_res <-sample(which(imbal_dat$out==maj_cl), min_val)
      min_imbal_res<-which(imbal_dat$out!=maj_cl)
      imb_fin<- c(maj_imbal_res, min_imbal_res)
      fin_dat<-imbal_dat[imb_fin,]
      
      train<-fin_dat
    }
    
    
    iterations = 1
    ix <- c(1)
    
    while (length(ix) > 0) {
      
      print(paste0("######## ITERATIONS ########### ", iterations))
      method <- match.arg(method, choices=c("Naive Bayes", "Random Forest"))
      if( !method%in%c("Naive Bayes", "Random Forest") ) 
        stop("Method must be 'Naive Bayes' or 'Random Forest'.\n")
      
      if(method == "Naive Bayes"){
        
        naiv_bay <- naiveBayes(as.factor(out)~., data = train,
                               method = 'class')
        
        naiv_pre_unlab <- predict(naiv_bay, unlab, type = "raw")
        
        
      }else {
        
        naiv_bay <- randomForest(as.factor(out) ~ ., 
                                 data = train, 
                                 method = 'class',
                                 ntree=500)
        
        naiv_pre_unlab <- predict(naiv_bay, unlab, type = "prob")
        
      }
      
      conf_res <- confusionMatrix(predict(naiv_bay,test), as.factor(test$out),  positive = "1")
      # ACCURACY MEASURES
      SensCV[i_cv, iterations]<-round(conf_res$byClass[[1]],4)
      SpecCV[i_cv, iterations]<-round(conf_res$byClass[[2]],4)
      PPVCV[i_cv, iterations]<-round(conf_res$byClass[[3]],4)
      NPVCV[i_cv, iterations]<-round(conf_res$byClass[[4]],4)
      AccCV[i_cv, iterations]<-round(conf_res$overall[[1]],4)
      
      F1CV[i_cv, iterations]<-round(conf_res$byClass[[7]],4)
      BalAccCV[i_cv, iterations]<-round(conf_res$byClass[[11]],4)
      
      # CONFUSION MATRIX
      TN[i_cv, iterations]<-round(conf_res$table[1],4)
      TP[i_cv, iterations]<-round(conf_res$table[4],4)
      FN[i_cv, iterations]<-round(conf_res$table[3],4)
      FP[i_cv, iterations]<-round(conf_res$table[2],4)
      
      # Pseudo labels olustur
      
      unlab_pr<- cbind(unlab, pr=naiv_pre_unlab[,1])
      unlab_pr<-unlab_pr[order(-unlab_pr$pr),]
      head(unlab_pr);tail(unlab_pr)
      
      unlab_pr$out<- ifelse(unlab_pr$pr>0.5, 0, 1)
      neg<- which(unlab_pr$pr>neg_conf_prob)
      length_ix_neg[i_cv, iterations]<- length(neg)
      pos<- which(unlab_pr$pr<pos_conf_prob)
      length_ix_pos[i_cv, iterations]<- length(pos)
      
      ix<- c(neg, pos)
      
      unlab_pr2<- unlab_pr %>% 
        dplyr:: select(-"pr") 
      unlab_pr3<- unlab_pr2[ix,]
      new_lab <-  rbind(train, unlab_pr3 ) 
      unlab_pr4<- unlab_pr2[-ix,] %>% 
        dplyr :: select(-"out")
      
      train<-new_lab 
      unlab<-unlab_pr4
      
      
      iterations = iterations + 1
      
      
    } # end of  while 
    
    print(paste0("############## FOLD ######################## ", i_cv))
    
    
    
    
    
    
    
    
  } # end of for loop of folds
  
  # every fold generating different number of iterations,We find the iteration number of the one 
  # with the lowest iteration among a total of n_fold iterations.
  # for instance, n_fold is 3
  # fold 1 has 15 iterations (fold 1-->15), fold 2--> 28 iterations, fold 3--> 12 iterations, 
  # min_NA is 12
  
  mis_vec <-  ( apply(TN, 1, function(x){ first( which( is.na(x)==TRUE))}))
  mis_vec
  min_NA<-  (min( mis_vec[ !is.na(  mis_vec)])-1)
  
  # accuracy measures
  SensCV<-SensCV[,1:min_NA]
  colnames(SensCV)<-NULL
  SpecCV<-SpecCV[,(1:min_NA)]
  colnames(SpecCV)<-NULL
  PPVCV<-PPVCV[,1:min_NA] 
  colnames(PPVCV)<-NULL
  NPVCV<-NPVCV[,1:min_NA] 
  colnames(NPVCV)<-NULL
  F1CV<-F1CV[,1:min_NA] 
  colnames(F1CV)<-NULL
  AccCV<-AccCV[,1:min_NA] 
  colnames(AccCV)<-NULL
  BalAccCV<-BalAccCV[,1:min_NA] 
  colnames(BalAccCV)<-NULL
  
  TP<-TP[,1:min_NA] 
  colnames(TP)<-NULL
  TN<-TN[,1:min_NA]
  colnames(TN)<-NULL
  FP<-FP[,1:min_NA]
  colnames(FP)<-NULL
  FN<-FN[,1:min_NA]
  colnames(FN)<-NULL
  
  length_ix_neg<- length_ix_neg[,1:min_NA]
  length_ix_pos<- length_ix_pos[,1:min_NA]
  
  #------------------------------------------------------------------------
  
  TPNF<- cbind(TP, TN, FP, FN)
  colnames(TPNF)<- c( paste0("it",  paste0(1: min_NA, c("-TP"))),
                      paste0("it",  paste0(1: min_NA, c("-TN"))),
                      paste0("it",  paste0(1: min_NA, c("-FP"))),
                      paste0("it",  paste0(1: min_NA, c("-FN"))))
  rownames(TPNF)<- paste0("FOLD", 1:(n_fold))
  
  # saving the outputs
  if(method == "Naive Bayes"){
    
    if(feature_sel==TRUE){
      write.csv(TPNF, "2ConfMat_NB_FS.csv")
    }else{
      write.csv(TPNF, "1ConfMat_NB_MedSel.csv")
    }
  }else{
    if(feature_sel==TRUE){
      write.csv(TPNF, "4ConfMat_RF_FS.csv")
    }else{
      write.csv(TPNF, "3ConfMat_RF_MEdSel.csv")
    }
  }
  
  #----------------------------------------------------------------------------- 
  # function for calculating mean and sd of accuracy measures
  data_summary<-function(data){
    ncol<-dim(data)[2]
    data_mean_sd<-data.frame(iterations=c(1:ncol),
                             Mean=c(apply(data,2, mean,na.rm=TRUE)), 
                             Sd=c(apply(data,2,sd,na.rm=TRUE)))
    return(data_mean_sd)
    
  }
  
  #----------------------------------------------------------------------------- GRAPH:ACCURACY MEASURES
  # sensitivity graph 
  SensCV_fin <-    data_summary(SensCV)
  Nit<- min_NA
  
  Sen_pl <-ggplot(data = SensCV_fin, aes(x=iterations, y=Mean )) +
    geom_line() + 
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=0.2, position=position_dodge(0.9)) + ylim(0,1)
  
  Sen_pl<- Sen_pl + ylab("Sensitivity")+ theme_bw()
  Sen_pl<- Sen_pl +  theme(text = element_text(size = 20)) 
  #-----------------------------------------------------------------------------  saving graph 
  if(method == "Naive Bayes"){
    Sen_pl<-  Sen_pl + scale_x_continuous(breaks=c(0.9:Nit+0.1), labels=c(0.9:Nit+0.1),limits=c(0.9,Nit+0.1))
    if(feature_sel==TRUE){
      ggsave("2Self_NB_Sen_FS.pdf")
    }else{
      ggsave("1Self_NB_Sen_MedSel.pdf")
    }
  }else{
    if(feature_sel==TRUE){
      ggsave("4Self_RF_Sen_FS.pdf")
    }else{
      ggsave("3Self_RF_Sen_MedSel.pdf")
    }
  }
  
  # Specifity graph
  SpecCV_fin <- data_summary(SpecCV)
  
  Spec_pl <-ggplot(data = SpecCV_fin, aes(x=iterations, y=Mean )) +
    geom_line() + 
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=0.2, position=position_dodge(0.9)) + ylim(0,1.1) 
  
  Spec_pl<- Spec_pl + ylab("Specificity") + theme_bw()
  Spec_pl<-Spec_pl+ theme(text = element_text(size = 20)) 
  
  if(method == "Naive Bayes"){
    Spec_pl<-  Spec_pl + scale_x_continuous(breaks=c(0.9:Nit+0.1), labels=c(0.9:Nit+0.1),limits=c(0.9,Nit+0.1))
    if(feature_sel==TRUE){
      ggsave("2Self_NB_Spec_FS.pdf")
    }else{
      ggsave("1Self_NB_Sepc_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4Self_RF_Spec_FS.pdf")
    }else{
      ggsave("3Self_RF_Spec_MedSel.pdf")
    }
  }
  
  # PPV graph
  PPVCV_fin <-    data_summary(PPVCV)
  
  PPV_pl <-ggplot(data = PPVCV_fin, aes(x=iterations, y=Mean )) +
    geom_line() +  
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=0.2, position=position_dodge(0.9)) + ylim(0,1)
  
  PPV_pl<- PPV_pl + ylab("PPV")+theme_bw()
  PPV_pl<-PPV_pl +  theme(text = element_text(size = 20)) 
  if(method == "Naive Bayes"){
    PPV_pl<-  PPV_pl + scale_x_continuous(breaks=c(0.9:Nit+0.1), labels=c(0.9:Nit+0.1),limits=c(0.9,Nit+0.1))
    if(feature_sel==TRUE){
      ggsave("2Self_NB_PPV_FS.pdf")
    }else{
      ggsave("1Self_NB_PPV_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4Self_RF_PPV_FS.pdf")
    }else{
      ggsave("3Self_RF_PPV_MedSel.pdf")
    }
  }
  
  # NPV graph
  NPVCV_fin <-    data_summary(NPVCV)
  
  NPV_pl <-ggplot(data = NPVCV_fin, aes(x=iterations, y=Mean )) +
    geom_line() + 
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=0.2, position=position_dodge(0.9)) + 
    ylim(0,1)
  
  NPV_pl<- NPV_pl + ylab("NPV")+theme_bw()
  NPV_pl<-NPV_pl +  theme(text = element_text(size = 20)) 
  
  if(method == "Naive Bayes"){
    NPV_pl<-  NPV_pl + scale_x_continuous(breaks=c(0.9:Nit+0.1), labels=c(0.9:Nit+0.1),limits=c(0.9,Nit+0.1))
    if(feature_sel==TRUE){
      ggsave("2Self_NB_NPV_FS.pdf")
    }else{
      ggsave("1Self_NB_NPV_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4Self_RF_NPV_FS.pdf")
    }else{
      ggsave("3Self_RF_NPV_MedSel.pdf")
    }
  }
  
  
  #  ACC graph
  
  AccCV_fin <-    data_summary(AccCV)
  AccCV_pl <-ggplot(data = AccCV_fin, aes(x=iterations, y=Mean )) +
    geom_line() + 
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=0.2, position=position_dodge(0.9)) + ylim(0,1)
  
  AccCV_pl<- AccCV_pl + ylab("Accuracy")+ theme_bw()
  AccCV_pl<-AccCV_pl +  theme(text = element_text(size = 20)) 
  
  if(method == "Naive Bayes"){
    AccCV_pl<-  AccCV_pl + scale_x_continuous(breaks=c(0.9:Nit+0.1), labels=c(0.9:Nit+0.1),limits=c(0.9,Nit+0.1))
    if(feature_sel==TRUE){
      ggsave("2Self_NB_Acc_FS.pdf")
    }else{
      ggsave("1Self_NB_Acc_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4Self_RF_Acc_FS.pdf")
    }else{
      ggsave("3Self_RF_Acc_MedSel.pdf")
    }
  }
  # balanced accuracy graph
  BalAccCV_fin<-data_summary(BalAccCV)
  BalAccCV_pl <-ggplot(data = BalAccCV_fin, aes(x=iterations, y=Mean )) +
    geom_line() + 
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=0.2, position=position_dodge(0.9)) + ylim(0,1)
  
  BalAccCV_pl<- BalAccCV_pl + ylab("Balance Accuracy")+ theme_bw()
  BalAccCV_pl<-BalAccCV_pl +  theme(text = element_text(size = 20)) 
  if(method == "Naive Bayes"){
    BalAcc_pl<-  BalAccCV_pl + scale_x_continuous(breaks=c(0.9:Nit+0.1), labels=c(0.9:Nit+0.1),limits=c(0.9,Nit+0.1))
    if(feature_sel==TRUE){
      ggsave("2Self_NB_BalAcc_FS.pdf")
    }else{
      ggsave("1Self_NB_BalAcc_MedSel.pdf")
    }
  }else{
    if(feature_sel==TRUE){
      ggsave("4Self_RF_BalAcc_FS.pdf")
    }else{
      ggsave("3Self_RF_BalAcc_MedSel.pdf")
    }
  }
  
  # F1 graph
  F1CV_fin<-data_summary(F1CV)
  F1CV_fin_pl <-ggplot(data = F1CV_fin, aes(x=iterations, y=Mean )) +
    geom_line() + 
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=0.2, position=position_dodge(0.9)) + ylim(0,1)
  
  F1CV_fin_pl <- F1CV_fin_pl + ylab("F1")+ theme_bw()
  F1CV_fin_pl<-F1CV_fin_pl +  theme(text = element_text(size = 20)) 
  
  if(method == "Naive Bayes"){
    F1CV_fin_pl<- F1CV_fin_pl + scale_x_continuous(breaks=c(0.9:Nit+0.1), labels=c(0.9:Nit+0.1),limits=c(0.9,Nit+0.1))
    if(feature_sel==TRUE){
      ggsave("2Self_NB_F1_FS.pdf")
    }else{
      ggsave("1Self_NB_F1_MedSel.pdf")
    }
  }else{
    if(feature_sel==TRUE){
      ggsave("4Self_RF_F1_FS.pdf")
    }else{
      ggsave("3Self_RF_F1_MedSel.pdf")
    }
  }
  
  
  #----------------------------------------------------------------------------- GRAPH: CONF MATR
  # TP
  TP_fin<-data_summary(TP)
  TP_fin <-ggplot(data = TP_fin, aes(x=iterations, y=Mean )) +
    geom_line() + 
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=0.2, position=position_dodge(0.9))
  
  TP_fin<- TP_fin+ ylab("TP")+ theme_bw()
  TP_fin<-TP_fin +  theme(text = element_text(size = 20)) 
  if(method == "Naive Bayes"){
    TP_fin<- TP_fin + scale_x_continuous(breaks=c(0.9:Nit+0.1), labels=c(0.9:Nit+0.1),limits=c(0.9,Nit+0.1))
    if(feature_sel==TRUE){
      ggsave("2Self_NB_TP_FS.pdf")
    }else{
      ggsave("1Self_NB_TP_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4Self_RF_TP_FS.pdf")
    }else{
      ggsave("3Self_RF_TP_MedSel.pdf")
    }
  }
  
  # TN
  TN_fin<-data_summary(TN)
  TN_fin <-ggplot(data = TN_fin, aes(x=iterations, y=Mean )) +
    geom_line() + 
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=0.2, position=position_dodge(0.9))  
  
  TN_fin<- TN_fin+ ylab("TN")+ theme_bw()
  TN_fin<-TN_fin +  theme(text = element_text(size = 20)) 
  if(method == "Naive Bayes"){
    TP_fin<- TP_fin + scale_x_continuous(breaks=c(0.9:Nit+0.1), labels=c(0.9:Nit+0.1),limits=c(0.9,Nit+0.1))
    if(feature_sel==TRUE){
      ggsave("2Self_NB_TN_FS.pdf")
    }else{
      ggsave("1Self_NB_TN_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4Self_RF_TN_FS.pdf")
    }else{
      ggsave("3Self_RF_TN_MedSel.pdf")
    }
  }
  
  #FP
  FP_fin<-data_summary(FP)
  FP_fin <-ggplot(data = FP_fin, aes(x=iterations, y=Mean )) +
    geom_line() + 
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=0.2, position=position_dodge(0.9)) 
  
  FP_fin<- FP_fin+ ylab("FP")+ theme_bw()
  FP_fin <-FP_fin +  theme(text = element_text(size = 20)) 
  if(method == "Naive Bayes"){
    FP_fin<- FP_fin + scale_x_continuous(breaks=c(0.9:Nit+0.1), labels=c(0.9:Nit+0.1),limits=c(0.9,Nit+0.1))
    if(feature_sel==TRUE){
      ggsave("2Self_NB_FP_FS.pdf")
    }else{
      ggsave("1Self_NB_FP_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4Self_RF_FP_FS.pdf")
    }else{
      ggsave("3Self_RF_FP_MedSel.pdf")
    }
  }
  
  #FN
  FN_fin<-data_summary(FN)
  FN_fin <-ggplot(data = FN_fin, aes(x=iterations, y=Mean )) +
    geom_line() + 
    geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean+Sd), width=0.2, position=position_dodge(0.9)) 
  
  FN_fin<- FN_fin+ ylab("FN")+ theme_bw()
  FN_fin<-FN_fin +  theme(text = element_text(size = 20)) 
  if(method == "Naive Bayes"){
    FN_fin<- FN_fin + scale_x_continuous(breaks=c(0.9:Nit+0.1), labels=c(0.9:Nit+0.1),limits=c(0.9,Nit+0.1))
    if(feature_sel==TRUE){
      ggsave("2Self_NB_FN_FS.pdf")
    }else{
      ggsave("1Self_NB_FN_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4Self_RF_FN_FS.pdf")
    }else{
      ggsave("3Self_RF_FN_MedSel.pdf")
    }
  }
  
  #----------------------------------------------------------------------------- GRAPH: Pos_Neg Sel
  
  #  graph for positive and negative selection in each iteration
  length_ix_neg2 <-    data_summary(length_ix_neg)
  length_ix_pos2 <-    data_summary(length_ix_pos)
  
  
  pos_neg_fin<-data.frame( View = c(rep("Positive", ( Nit)),rep("Negative", ( Nit))),
                           Iterations = c(1:( Nit), 1:( Nit)),
                           Mean = c(length_ix_pos2[,2], length_ix_neg2[,2]),
                           Sd= c(length_ix_pos2[,3], length_ix_neg2[,3])
  )
  
  pos_neg_pl<-ggplot( pos_neg_fin, aes(x=Iterations, y=Mean, group=View, color=View))+
    geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+theme_minimal()+ 
    ylab("Selected Positivie Negative Values")+theme_bw()
  
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      ggsave("2Self_NB_PosNeg_FS.pdf")
    }else{
      ggsave("1Self_NB_PosNeg_MedSel.pdf")
    }
    
  }else{
    if(feature_sel==TRUE){
      ggsave("4Self_RF_PosNeg_FS.pdf")
    }else{
      ggsave("3Self_RF_PosNeg_MedSel.pdf")
    }
  }
  
  
  
  
  
  # the output we want to return
  first_last<-data.frame(Sensitivity =  c( SensCV_fin[1,2], SensCV_fin[dim(SensCV_fin)[1],2], 
                                           100*((SensCV_fin[dim(SensCV_fin)[1],2]- SensCV_fin[1,2])/(SensCV_fin[1,2])) ),
                         Specificity = c( SpecCV_fin[1,2], SpecCV_fin[dim(SpecCV_fin)[1],2], 
                                          100*((SpecCV_fin[dim(SpecCV_fin)[1],2]- SpecCV_fin[1,2])/(SpecCV_fin[1,2]))),
                         PPV = c( PPVCV_fin[1,2], PPVCV_fin[dim(PPVCV_fin)[1],2], 
                                  100*((PPVCV_fin[dim(PPVCV_fin)[1],2]- PPVCV_fin[1,2])/(PPVCV_fin[1,2]))),
                         NPV = c( NPVCV_fin[1,2], NPVCV_fin[dim(NPVCV_fin)[1],2], 
                                  100*((NPVCV_fin[dim(NPVCV_fin)[1],2]- NPVCV_fin[1,2])/(NPVCV_fin[1,2]))),
                         Acc= c(AccCV_fin[1,2], AccCV_fin[dim(NPVCV_fin)[1],2], 
                                100*((AccCV_fin[dim(AccCV_fin)[1],2]- AccCV_fin[1,2])/(AccCV_fin[1,2]))),
                         F1=   c(F1CV_fin[1,2], F1CV_fin[dim(F1CV_fin)[1],2], 
                                 100*((F1CV_fin[dim(F1CV_fin)[1],2]- F1CV_fin[1,2])/(F1CV_fin[1,2]))),
                         BalAcc=c(BalAccCV_fin[1,2], BalAccCV_fin[dim(BalAccCV_fin)[1],2], 
                                  100*((BalAccCV_fin[dim(BalAccCV_fin)[1],2]- BalAccCV_fin[1,2])/(BalAccCV_fin[1,2])))
  )
  
  rownames(first_last)<-c("First iteration", "Last iteration", "Percentage")
  
  if(method == "Naive Bayes"){
    if(feature_sel==TRUE){
      write.csv( first_last,"2_FirstLast_Self_NB_FS.csv" )
    }else{
      write.csv( first_last,"1_FirstLast_Self_NB_Med_sel.csv" )
    }
    
  }else{
    if(feature_sel==TRUE){
      write.csv( first_last,"4_FirstLast_Self_RF_FS.csv" )
    }else{
      write.csv( first_last,"3_FirstLast_Self_RF_Med_sel.csv" )
    }
  }
  
  # 4 in  1 graph
  p.plot<-ggarrange( F1CV_fin_pl, PPV_pl,  BalAccCV_pl, Sen_pl + 
                       rremove("x.text"), 
                     labels = c("A", "B", "C", "D" ),
                     ncol = 2, nrow = 2)
  
  tit_plt<- paste(method, "Results for Self-Training 5-folds")
  p.plot<- grid.arrange(p.plot,  top=tit_plt)
  
  return( first_last=first_last)
  
}








