

Baseline_Hyp <- function(dataL,  
                         method           =  "Logistic Regression", 
                         imbalanced       =  TRUE,
                         feature_sel      =  "Boruta",
                         n_fold      =  5
){ 
  
  new_data <-  dataL
  
  #---------------------------------------------------------------------------- library
  if(!require(caret)){install.packages("caret");require(caret)}
  if(!require(rpart)){install.packages("rpart");require(rpart)}
  if(!require(e1071)){install.packages("e1071");require(e1071)}
  if(!require(dplyr)){install.packages("dplyr");require(dplyr)}
  if(!require(randomForest)){install.packages("randomForest");require(randomForest)}
  if(!require(xgboost)){install.packages("xgboost");require(xgboost)}
  if(!require(ROSE)){install.packages("ROSE");require(ROSE)}
  if(!require(praznik)){install.packages("praznik");require(praznik)}
  if(!require(e1071)){install.packages("e1071");require(e1071)}
  if(!require(gridExtra)){install.packages("gridExtra");require(gridExtra)}  
  if(!require(adabag)){install.packages("adabag");require(adabag)}  
  
  
  FS_method <- match.arg(feature_sel , choices=c("Lasso","Boruta",  "MRMR", "NO FS"))
  
  if( !FS_method %in% c( "Lasso", "Boruta","MRMR", "NO FS") ) 
    stop("Feature Selection methods are not written properly !!.\n")  
  
  # Medical 
  if(feature_sel== "NO FS"){
    new_data <-new_data
    
  }else if(feature_sel=="Lasso"){
    sel_feat_out<-c( 
      "fpg_std"            ,"hba1c_mean",
      "g1diabed_std"       ,"nphl_insulin_mean",
      "fpg_mean"           ,"g1check_mean",
      "g1diabed_mean"      ,"g1nutrit_std",
      "othbol_insulin_mean","sulfonylurea_mean",    
      "premix_insulin_mean","out")
    
    labF     <- new_data[sel_feat_out]
    new_data <- labF
    
    
  }else if(feature_sel=="Boruta"){
    sel_feat_out<-c( 
      "fpg_std"            ,"hba1c_mean",
      "g1diabed_std"       ,"nphl_insulin_mean",
      "fpg_mean"           ,"g1check_mean",
      "g1diabed_mean"      ,"g1nutrit_std",
      "othbol_insulin_mean","g1nutrit_mean",
      "out")
    
    
    labF     <- new_data[sel_feat_out]
    new_data <- labF
  }else{
    sel_feat_out<-c(
      "fpg_std"            ,"hba1c_mean",
      "g1diabed_std"       ,"nphl_insulin_mean",
      "out")
    labF     <- new_data[sel_feat_out]
    new_data <- labF
  }
  
  CV.Sensitivity  <-c()
  CV.Specifity    <-c()
  CV.PPV          <-c()
  CV.NPV          <-c()
  CV.Accuracy     <-c()
  CV.F.score      <-c()
  
  set.seed(123)  
  folds<-createFolds(new_data$out, k=n_fold)
  
  for (i_cv in 1:n_fold) { #cross validation
    print(paste0("fold ", i_cv))
    
    testIndex <-folds[[i_cv]]
    testIndex
    trainset <- new_data[-testIndex, ]
    head(trainset)
    testset  <- new_data[ testIndex, ]
    
    if(!imbalanced){
      trainset<-trainset
    }else{
      imbal_dat<-trainset
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
      
      trainset<-fin_dat
    }
    
    method <- match.arg(method, choices=c("Naive Bayes", "Random Forest", "Logistic Regression",
                                          "SVM", "Xgboost", "All"))
    
    if( !method%in%c("Naive Bayes", "Random Forest", "Logistic Regression",
                     "SVM", "Xgboost") ) 
      stop("Write in proper way!!.\n")  
    
    
    if(method == "Logistic Regression"){
      # 
      # #https://stats.oarc.ucla.edu/r/dae/logit-regression/
      # 
      trainset$out <- factor(trainset$out)
      mylogit <- glm(out ~ ., data = trainset, family = "binomial")
      
      
      probabilities <- mylogit %>% predict(testset, type = "response")
      log_reg<-ifelse(probabilities>0.5,1,0)
      length(log_reg)
      xx<-table(testset$out, log_reg)
      
      TN<-xx[1]
      FN<-xx[2]
      FP<-xx[3]
      TP<-xx[4]
      
      
      
      CV.Sensitivity[i_cv]=TP / (TP+FN)
      CV.Specifity[i_cv]= TN/(TN+FP)
      CV.PPV[i_cv]= TP/(TP+FP)
      CV.NPV[i_cv]= TN/(TN+FN)
      CV.Accuracy[i_cv] = (TP+ TN)/(TP+TN+FP+FN)
      CV.F.score[i_cv] = 2*(TP)/(2*TP+FP+FN) 
      
      
      
      
      
      
      
    }
    
    if(method == "Naive Bayes"){
      #---------------------------------------------------------------#Naive Bayes model traing
      naiv_bay <- naiveBayes(as.factor(out) ~., data = trainset, usekernel = T)
      
      #summary of model
      summary(naiv_bay) 
      
      #prediction
      naiv_pre <- predict(naiv_bay,testset)
      
      
      #confusion matrix
      xx<-table(testset$out, naiv_pre)
      
      TN<-xx[1]
      FN<-xx[2]
      FP<-xx[3]
      TP<-xx[4]
      
      
      CV.Sensitivity[i_cv]=TP / (TP+FN)
      CV.Specifity[i_cv]= TN/(TN+FP)
      CV.PPV[i_cv]= TP/(TP+FP)
      CV.NPV[i_cv]= TN/(TN+FN)
      CV.Accuracy[i_cv] = (TP+ TN)/(TP+TN+FP+FN)
      CV.F.score[i_cv] = 2*(TP)/(2*TP+FP+FN)
      
      
    }
    
    if(method == "Xgboost"){
      #---------------------------------------------------------------# XGBOOST
      # data preparation
      head(trainset)
      x6<-which(colnames(trainset)=="out")
      
      xgb_train <- as.matrix(trainset[-x6])
      xgb_trainlabel <- as.matrix(trainset$out)
      
      #xgboost model training
      xg_boost <- xgboost(data = xgb_train, label = xgb_trainlabel, nrounds = 2)
      
      
      #prediction
      xgb_pre <- predict(xg_boost, newdata =  as.matrix(testset[-x6]),type="response")
      xgb_pre <- ifelse(xgb_pre > 0.5,1,0)
      
      
      
      #confusion matrix
      xx<-table(testset$out,xgb_pre)
      
      
      
      TN<-xx[1]
      FN<-xx[2]
      FP<-xx[3]
      TP<-xx[4]
      
      CV.Sensitivity[i_cv]=TP / (TP+FN)
      CV.Specifity[i_cv]= TN/(TN+FP)
      CV.PPV[i_cv]= TP/(TP+FP)
      CV.NPV[i_cv]= TN/(TN+FN)
      CV.Accuracy[i_cv] = (TP+ TN)/(TP+TN+FP+FN)
      CV.F.score[i_cv] = 2*(TP)/(2*TP+FP+FN)
      
      
    }
    
    if(method == "SVM"){
      #---------------------------------------------------------------# Bagging
      
      svm1 <- train(as.factor(out) ~., 
                    data = trainset, 
                    method = "svmLinear", 
                    #trControl = train_control,  
                    preProcess = c("center","scale"))
      
      
      
      
      
      #prediction
      svm_pre <- predict(svm1,testset)
      
      #confusion matrix
      xx<-table(testset$out,svm_pre)
      
      
      TN<-xx[1]
      FN<-xx[2]
      FP<-xx[3]
      TP<-xx[4]
      
      
      CV.Sensitivity[i_cv]=TP / (TP+FN)
      CV.Specifity[i_cv]= TN/(TN+FP)
      CV.PPV[i_cv]= TP/(TP+FP)
      CV.NPV[i_cv]= TN/(TN+FN)
      CV.Accuracy[i_cv] = (TP+ TN)/(TP+TN+FP+FN)
      CV.F.score[i_cv] = 2*(TP)/(2*TP+FP+FN)
      
      
      
    }
    
    
    if(method == "Random Forest"){
      
      
      
      #---------------------------------------------------------------# RF
      
      #Random forest model training
      set.seed(123)
      randm_mod <- randomForest(as.factor(out) ~ ., data = trainset, 
                                method = 'class', ntree=500)
      
      #model summary
      summary(randm_mod)
      
      
      #prediction
      randm_pre <- predict(randm_mod,testset, type = "class")
      
      #confusion matrix
      xx<-table(testset$out,randm_pre)
      
      
      
      TN<-xx[1]
      FN<-xx[2]
      FP<-xx[3]
      TP<-xx[4]
      
      
      CV.Sensitivity[i_cv]=TP / (TP+FN)
      CV.Specifity[i_cv]= TN/(TN+FP)
      CV.PPV[i_cv]= TP/(TP+FP)
      CV.NPV[i_cv]= TN/(TN+FN)
      CV.Accuracy[i_cv] = (TP+ TN)/(TP+TN+FP+FN)
      CV.F.score[i_cv] = 2*(TP)/(2*TP+FP+FN)
      
      
      
      
    }
    
  }# end fold
  
  
  CV.Sensitivity[i_cv]=TP / (TP+FN)
  CV.Specifity[i_cv]= TN/(TN+FP)
  CV.PPV[i_cv]= TP/(TP+FP)
  CV.NPV[i_cv]= TN/(TN+FN)
  CV.Accuracy[i_cv] = (TP+ TN)/(TP+TN+FP+FN)
  CV.F.score[i_cv] = 2*(TP)/(2*TP+FP+FN)
  
  mean(CV.NPV)
  res<-matrix(c(mean(CV.NPV),mean(CV.PPV),
                mean(CV.Specifity),mean(CV.Sensitivity),
                mean(CV.Accuracy),mean(CV.F.score)), nrow=1)
  colnames(res)<-c("NPV", "PPV", "Spec", "Sens", "Acc", "F1")
  res 
  
  return(result=res)
}

