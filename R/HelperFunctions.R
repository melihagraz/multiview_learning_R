#' below contains auxiliary functions to the main functions (base.R, self.R, Co.R)
#'  in the
#'                       MULTIVIEW COTRAINING PROJECT




#------------------------------------------------------------------------------- BALANCED CO
#' @title Undersampling Algorithms for Imbalanced Classification for Co training
#'
#' @description
#' This method ensures that the imbalanced data is balanced with the
#' undersampling method with \code{balanced_Co} code.
#'
#' @param data1 View1 we want to balanced
#' @param data2 View2 want to balanced
#' @param seed a positive integer. It is used to control random number
#' generating steps. If not NULL, folds are randomly generated using the
#' user-defined seed value. It might be usefull to select the same folds and
#' compare multiple models using the same cross-validation folds. Default is
#' NULL, i.e., unrestricted.
#' @param ... further arguments. Currently ignored.
#'
#' @return a list with elements \code{view1} and \code{view2} that corresponds
#' to the balanced data sets.
#'
#' @examples
#' 1L
#'
#' @export

balanced_Co <- function(data1, data2, imbalanced = TRUE, seed = NULL, ...){
  
  if(!imbalanced){
    train_view1<-train_view1 ;  train_view2<-train_view2
  }else{
    imbal_dat1 <- data1
    imbal_dat2 <- data2
    
    out_tab <- table(imbal_dat1$out)
    maj_cl <- numeric()
    
    if (table(imbal_dat1$out)[[1]] > table(imbal_dat1$out)[[2]]){
      maj_cl = as.numeric(names(out_tab)[1])
    } else {
      maj_cl = as.numeric(names(out_tab)[2])
    }
    
    min_val <- numeric()
    if (table(imbal_dat1$out)[[1]] < table(imbal_dat1$out)[[2]]){
      min_val = table(imbal_dat1$out)[[1]]
    } else {
      min_val = table(imbal_dat1$out)[[2]]
    }
    
    if (!is.null(seed)){
      set.seed(seed)
    }
    
    maj_imbal_res <- sample(which(imbal_dat1$out == maj_cl), min_val)
    min_imbal_res <- which(imbal_dat1$out != maj_cl)
    imb_fin <- c(maj_imbal_res, min_imbal_res)
    fin_dat1 <- imbal_dat1[imb_fin, ]
    
    train_view1 <- fin_dat1
    train_view2 <- imbal_dat2[imb_fin, ]
  }
  return(list(view1 = train_view1, view2 = train_view2))
}






#------------------------------------------------------------------------------- BALANCED SELF
#' @title Undersampling Algorithms for Imbalanced Classification for self training
#'
#' @description
#' This method ensures that the imbalanced data is balanced with the
#' undersampling method with \code{balanced} code.
#'
#' @param data we want to balanced
#' @param seed a positive integer. It is used to control random number
#' generating steps. If not NULL, #' folds are randomly generated using the
#' user-defined seed value. It might be #' usefull to select the same folds
#' and compare multiple models using the same cross-validation folds. Default
#' is NULL, i.e., unrestricted.
#' @param ... further arguments. Currently ignored.
#'
#' @return balanced data
#'
#' @examples
#' 1L
#'
#' @export
balanced <- function(data, seed = NULL, ...){
  xx <- table(data$out)
  maj_cl <- numeric()
  
  if (table(data$out)[[1]] > table(data$out)[[2]]) {
    maj_cl = as.numeric(names(xx)[1])
  } else{
    maj_cl = as.numeric(names(xx)[2])
  }
  
  min_val <- numeric()
  if (table(data$out)[[1]] < table(data$out)[[2]]) {
    min_val = table(data$out)[[1]]
  } else{
    min_val = table(data$out)[[2]]
  }
  
  if (!is.null(seed)){
    set.seed(seed)
  }
  
  maj_imbal_res <- sample(which(data$out == maj_cl), min_val)
  min_imbal_res <- which(data$out != maj_cl)
  imb_fin <- c(maj_imbal_res, min_imbal_res)
  fin_dat <- data[imb_fin, ]
  train <- fin_dat
  
  return(train)
}

#------------------------------------------------------------------------------- DATA SUMMARY


#' @title Summary of the Data
#'
#' @description
#' This \code{data_summary} calculates the mean and standard deviation of the
#' columns of each data and the index of the observations.
#'
#' @param data dataset, can be data.frame or matrix
#
#' @return tabulated data with number of iterations, mean and standard deviation
#'
#' @examples
#' data_summary(matrix(c(1:12), 3))
#'
#' @export

data_summary<-function(data){
  ncol<-dim(data)[2]
  data_mean_sd<-data.frame(iterations=c(1:ncol),
                           Mean=c(apply(data,2, mean,na.rm=TRUE)),
                           Sd=c(apply(data,2,sd,na.rm=TRUE)))
  return(data_mean_sd)
  
}


#-------------------------------------------------------------------------------  FEATURE SELECTION
# paper: MRMR
# https://arxiv.org/abs/1908.05376

# towardsdata:
# https://towardsdatascience.com/mrmr-explained-exactly-how-you-wished-someone-explained-to-you-9cf4ed27458b
#' @title Feature Selection Algorithms: Lasso & Boruta
#'
#' @description
#' This method select the optimal features with \code{feature_selection} code.
#'
#' @param data dataset
#' @param feature_sel runs Lasso or Boruta feature selection methods
#' @return saved plot
#'
#' @examples
#' feature_selection(data,method = "Lasso" )
#'
#' @export



FeatureSelection <- function(data, method = c("Lasso", "Boruta", "MRMR"), K = NULL, ... ){
  #---------------------------------------------------------------------library
  
  if(!require(dplyr)){install.packages("dplyr");require(dplyr)}
  if(!require(glmnet)){install.packages("glmnet");require(glmnet)}
  if(!require(Boruta)){install.packages("Boruta");require(Boruta)}
  
  #-----------------------------------------------------------------------LASSO FS
  
  if (method == "Lasso") {
    #define response variable
    y <- data$out
    
    x <- data.matrix(data %>% 
                     dplyr::select(-out))
    
    cv_model <- cv.glmnet(x, y, alpha = 1)
    
    best_lambda <- cv_model$lambda.min
    best_lambda
    
    plot(cv_model)
    
    best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
    pos_col <- which(coef(best_model) != 0) - 1
    fin_sel <- colnames(x)[pos_col]
    
  } else if (method == "Boruta"){
    #------------------------------------------------------------------------ BORUTA
    
    
    boruta_output <- Boruta(out ~ ., data = data, doTrace = 2,
                            maxRuns = 200) # to get stable results we need high
    #number of iterations
    
    final.boruta <-
      TentativeRoughFix(boruta_output)
    
    boruta_signif <-
      getSelectedAttributes(final.boruta, withTentative = TRUE)
    
    
    fin_sel<- cat(paste(shQuote(boruta_signif, type = "cmd")))
    
    plot(
      boruta_output,
      cex.axis = .7,
      las = 2,
      xlab = "",
      main = "Variable Importance"
    )
    
 
    
  } else{ # MRMR
    number_features <- K
    data_raw<-data
    #data_F: calculating F score
    data_F<-data.frame(Variables = colnames(data_raw),
                       F_stat  = 0, cor=0.0001, MRMR=0)
    data_F<-data_F[1:(dim(data_raw)[2]-1),] # -1 because dropping the output
    y<-data_raw[,(dim(data_raw)[2])] # output
    # now Calculating the F-statistics
    for (i in 1:(dim(data_raw)[2]-1)) {
      x<-data_raw[,i]
      data<-data.frame(x, y)
      fit<-lm(x ~ y, data = data)
      summary(fit)
      RSS0 <- sum((x - mean(x))^2)
      RSS <- sum(fit$residuals^2) 
      p <-  1 #predictors whos coefficient we are testing
      n <- length(y) #number of observations
      res_F <- ((RSS0-RSS)/p ) / (RSS/(n-p-1))
      options("scipen"=100, "digits"=4)
      data_F[i,2]<-res_F
    }
    X<-data_raw %>% 
      dplyr:: select(-out)
    cor_X<- cor(X)
    selected<-c()
    not_selected<-names(X)
    i<-1
    # the real MRMR part 
    for (i in 1:K) {
      if(i==1){
        # first step is different than the other steps
        #since we have not selected any features yet, we have assigned the corr
        # as cor=0.0001 above between selected features and other variables
        data_F$MRMR     =  data_F$F_stat/data_F$cor
        data_F <- data_F  %>% arrange(desc(MRMR))
        selected   <-  data_F[1,"Variables"] 
        data_F<-data_F[-1,]
        not_selected<- data_F[,1]
      }else{
        data_F$cor<-NULL
        data_F$MRMR<-NULL 
        # correlation between selected features and other variables
        cor_den<-apply(abs(cor(data_raw[,not_selected],data_raw[,selected])),1,mean)
        
        data_cor<-data.frame(Variables=names(cor_den),
                             cor=cor_den)
        data_F<-left_join(data_F, data_cor, "Variables")
        data_F$MRMR<-data_F$F_stat/data_F$cor
        data_F <- data_F  %>% arrange(desc(MRMR)) # sorting in descending order
        selected   <-  append(selected, data_F[ 1,"Variables"] )
        data_F<-data_F[-1,]
        not_selected<- data_F[,1] 
      }
    }
    fin_sel = selected
  }
  return(fin_sel)
}

#-------------------------------------------------------------------------------Separately MRMR

MRMR<-function(data, number_features = 3){
  K<-number_features
  data_raw<-data
  data_F<-data.frame(Variables = colnames(data_raw),
                     F_stat  = 0, cor=0.0001, MRMR=0)
  data_F<-data_F[1:(dim(data_raw)[2]-1),] # -1 because dropping the output
  data_F
  y<-data_raw[,(dim(data_raw)[2])]
  # Calculating the F-statistics
  for (i in 1:(dim(data_raw)[2]-1)) {
    x<-data_raw[,i]
    data<-data.frame(x, y)
    fit<-lm(x ~ y, data = data)
    summary(fit)
    RSS0 <- sum((x - mean(x))^2) 
    RSS <- sum(fit$residuals^2) 
    p <-  1 
    n <- length(y) #number of observations
    res_F <- ((RSS0-RSS)/p ) / (RSS/(n-p-1))
    options("scipen"=100, "digits"=4)
    data_F[i,2]<-res_F
  }
  X<-data_raw %>% 
    dplyr:: select(-out)
  cor_X<- cor(X)
  selected<-c()
  not_selected<-names(X)
  length(not_selected)
  
  for (i in 1:K) {
    if(i==1){
      data_F$MRMR     =  data_F$F_stat/data_F$cor
      data_F <- data_F  %>% arrange(desc(MRMR))
      selected   <-  data_F[ 1,"Variables"] 
      data_F<-data_F[-1,]
      not_selected<- data_F[,1]
    }else{
      data_F$cor<-NULL
      data_F$MRMR<-NULL  
      cor_den<-apply(abs(cor(data_raw[,not_selected],data_raw[,selected])),1,mean)
      cor_den
      data_XX<-data.frame(Variables=names(cor_den),
                          cor=cor_den)
      data_F<-left_join(data_F, data_XX, "Variables")
      data_F$MRMR<-data_F$F_stat/data_F$cor
      data_F <- data_F  %>% arrange(desc(MRMR))
      selected   <-  append(selected, data_F[ 1,"Variables"] )
      data_F<-data_F[-1,]
      not_selected<- data_F[,1] 
    }
  }
  fin_sel = selected
  return(fin_sel)
}


#------------------------------------------------------------------------------- PLOT CO

#' @title Plot and Saver of Multiview Cotraining Algorithm
#'
#' @description you can draw graphs and save the results with the
#'  \code{plot_Co} function.
#'
#' @param data dataset

#' @param name name of the accuracy measures of confusion matrix;
#' "Sensitivity", "Specificity", "PPV", "NPV", "Accuracy","BalancedAccuracy",
#'  "F1", "TP", "TN","FP", "FN"
#' @param  method which ML are we working on, Naive Bayes(nb) or Random Forest(rf).
#' @param format file format ".pdf" or ".csv"
#' @return .pdf or .csv file and graph
#'
#' @examples
#'# basic usage of plot_Co
#'plot_Co(data, name="Sensitivity",method = "nb", format = ".pdf" )
#'
#'-
#'
#' @export



plot_Co <-function(data, name = c( "Sensitivity", "Specificity", "PPV", "NPV",
                                   "Accuracy","BalancedAccuracy"),feature_sel = feature_sel,
                   method = c("nb", "rf"), format = c(".pdf", ".csv")) {
  
  Iterations <- data$Iterations
  Mean <- data$Mean
  View <- data$View
  Sd <- data$Sd
  
  Sen_pl <-
    ggplot(data, aes(
      x = Iterations,
      y = Mean,
      group = View,
      color = View)) +
    geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd), width = .1) +
    geom_line() + geom_point() +
    scale_color_brewer(palette = "Paired") + theme_minimal() + ylab(name) +
    theme_bw() + ylim(0, 1)
  
  Sen_pl <- Sen_pl +  theme(text = element_text(size = 20))
  
  if (method == "nb") {
    if (feature_sel == TRUE) {
      ggsave(
        paste0("2", "MultiView", "_", method, "_", name, "_FS", format))
      
    } else{
      ggsave(
        paste0("1", "MultiView", "_", method, "_", name, "_MedSel", format))
      
    }
  } else{
    if (feature_sel == TRUE) {
      ggsave(
        paste0("4", "MultiView", "_", method, "_", name, "_FS", format))
      
    } else{
      ggsave(
        paste0("3", "MultiView", "_", method, "_", name, "_MedSel", format))
      
    }
  }
  
}


#------------------------------------------------------------------------------- plot_co2
# plot for  "F1", "TP", "TN","FP", "FN"


plot_Co2 <-function(data, name = c( "F1", "TP", "TN",
                                    "FP", "FN"),feature_sel = feature_sel,
                    method = c("nb", "rf"), format = c(".pdf", ".csv")) {
  
  Iterations <- data$Iterations
  Mean <- data$Mean
  min_mean <- min(Mean)
  max_mean <- max(Mean)
  View <- data$View
  Sd <- data$Sd
  
  min_sd <- min(Sd)
  max_sd <- max(Sd)
  
  Sen_pl <-
    ggplot(data, aes(
      x = Iterations,
      y = Mean,
      group = View,
      color = View)) +
    geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd), width = .1) +
    geom_line() + geom_point() +
    scale_color_brewer(palette = "Paired") + theme_minimal() + ylab(name) +
    theme_bw() + ylim((min_mean - 2*max_sd), (max_mean + 2*max_sd))
  
  Sen_pl <- Sen_pl +  theme(text = element_text(size = 20))
  
  if (method == "nb") {
    if (feature_sel == TRUE) {
      ggsave(
        paste0("2", "MultiView", "_", method, "_", name, "_FS", format))
      
    } else{
      ggsave(
        paste0("1", "MultiView", "_", method, "_", name, "_MedSel", format))
      
    }
  } else{
    if (feature_sel == TRUE) {
      ggsave(
        paste0("4", "MultiView", "_", method, "_", name, "_FS", format))
      
    } else{
      ggsave(
        paste0("3", "MultiView", "_", method, "_", name, "_MedSel", format))
      
    }
  }
  
}


#------------------------------------------------------------------------------- SAVER-self
#' @title Saver for the Output of Graphics and csv Files
#'
#' @description
#' This method saves the outputs of plots and confusion matrix through
#'  \code{saver} code.
#'
#' @param name how do you want to define your output? For instance:
#' if you want to save the first and last iterations of result, you can say
#' "Firstlast"
#' @param format file format ".pdf" or ".csv"
#' @param main_method which method are you working on? conventional ("base"),
#' self-learning ( "self"), multi-view-co training ("COV1": Co learning V1 or
#' "COV2" )
#' @param method machine learning methods that we are applying "nb"or "rf"
#' @param feature_sel what features do you want to continuou with?
#' Selected features or medically selected features
#' @return saved plot
#'
#' @examples
#' 1L
#'
#' @export



saver <- function(data,  name="name_of_code",   format = c(".pdf", ".csv"),
                  main_method = c("self","COV1", "COV2"),
                  method = c("nb", "rf"), feature_sel = TRUE){
  
  method <- match.arg(method)
  if(format==".pdf"){
    if (method == "nb") {
      if (feature_sel == TRUE) {
        ggsave(paste0("2", main_method, "_", method, "_", name, "_FS",
                      format))
      } else {
        
        ggsave(paste0("1", main_method, "_", method, "_", name,
                      "_MedicalSelected", format) )
      }
    } else {
      if (feature_sel == TRUE) {
        ggsave(paste0("4", main_method, "_", method, "_", name, "_FS",
                      format))
      } else {
        ggsave(paste0("3", main_method, "_", method, "_", name,
                      "_MedicalSelected", format) )
      }
    }
  }else{
    if (method == "nb") {
      if (feature_sel == TRUE) {
        write.csv(data, paste0("2", main_method, "_", method, "_", name, "_FS",
                               format))
      } else {
        
        write.csv(data, paste0("1", main_method, "_", method, "_", name,
                               "_MedicalSelected", format) )
      }
    } else {
      if (feature_sel == TRUE) {
        write.csv(data,paste0("4", main_method, "_", method, "_", name,
                              "_FS", format))
      } else {
        write.csv(data, paste0("3", main_method, "_", method, "_", name,
                               "_MedicalSelected", format) )
      }
    }
    
  }
  
}



#------------------------------------------------------------------------------- PLOT SELF
#' @title plot for self trainin
#'
#' @description
#' This is a \code{plot_self} function prepared for drawing and saving the figures.
#'
#' @param data Default dataset to use for plot.
#' @param name accuracy measures of confusion matrix:
#'  c("Sensitivity", "Specificity", "PPV",
#'  "NPV", "Accuracy", "BalancedAccuracy","F1","TP","TN","FP","FN")
#' @param number_iterations number of iterations in the self and co training methods
#' @param method Machine Learning models;
#'   Naive Bayes (nb), Random Forest (rf)
#' @param feature_sel optimal features, selected from the conventional machine
#' learning model
#' @param main_method which algorithm we are using ?
#' only working for "self" learning
#' @return plot and saved plot
#'
#' @examples
#'plot_self <- function(data, name = "Sensitivity", number_iterations = Nit,
#' method ="nb", feature_sel = TRUE, main_method = "self",  format = ".pdf")
#'
#' @export

plot_self <- function(data, name = c("Sensitivity", "Specificity", "PPV", "NPV",
                                     "Accuracy", "BalancedAccuracy", "F1","TP",
                                     "TN","FP","FN"),
                      number_iterations = Nit,
                      method = c("nb", "rf"),
                      feature_sel = TRUE,
                      main_method = "self",  format = ".pdf"){
  
  name <- match.arg(name)
  
  Mean<-data$Mean
  iterations<-data$iterations
  Sd<-data$Sd
  
  if (name %in% c("Sensitivity", "Specificity","PPV", "NPV",
                  "Accuracy","BalancedAccuracy", "F1")){
    Sen_pl <- ggplot(data = data,
                     aes(x = iterations, y = Mean)) +
      geom_line() +
      geom_errorbar(aes(ymin = Mean - Sd,
                        ymax = Mean + Sd),
                    width = 0.2,
                    position = position_dodge(0.9)) +
      ylim(-0.2, 1.2) +
      ylab(name) +
      theme_bw() +
      theme(text = element_text(size = 20))
    
    
    if (method == "nb") {
      if (feature_sel == TRUE) {
        ggsave(paste0("2", main_method, "_",  name,
                      "_FS", format))
      } else{
        ggsave(paste0("1", main_method, "_",  name,
                      "_MedSel", format))
        
      }
    } else{
      
      if (feature_sel == TRUE) {
        ggsave(paste0("4", main_method, "_",  name,
                      "_FS", format))
      } else{
        ggsave(paste0("3", main_method, "_",  name,
                      "_MedSel", format))
      }
    }
  } else{
    
    Sen_pl <- ggplot(data = data, aes(x = iterations, y = Mean)) +
      geom_line() +
      geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd),
                    width = 0.2,
                    position = position_dodge(0.9)) +
      ylab(name) +
      theme_bw() +
      theme(text = element_text(size = 20))
    
    if (method == "nb") {
      if (feature_sel == TRUE) {
        ggsave(paste0("2", main_method, "_",  name,
                      "_FS", format))
      } else{
        ggsave(paste0("1", main_method, "_",  name,
                      "_MedSel", format))
        
      }
    } else{
      if (feature_sel == TRUE) {
        ggsave(paste0("4", main_method, "_",  name,
                      "_FS", format))
      } else{
        ggsave(paste0("3", main_method, "_",  name,
                      "_MedSel", format))
      }
    }
  }
  return(list(plot=Sen_pl))
}


#------------------------------------------------------------------------------- CONFUSION MATRIX
#' @title Confusion Matrix from Actual and Predicted Values
#'
#' @description
#' This function is used to return confusion matrix from two vectors
#' (i.e., actual and predicted class labels) and the related performance
#' measures. Currently, it works for binary outcomes, i.e., a confusion
#'  matrix for a 2-by-2 contingency table.
#'
#' @param actual a vector. It contains the actual class labels (or values)
#'  of individuals.
#' @param predicted a vector. It contains the predicted class labels of
#' individuals.
#' @param positive Which level should be considered as positive class?
#' @param ... further arguments. Currently have no effect.
#'
#' @importFrom stats relevel
#'
#'
confMat <- function(actual, predicted, positive = NULL, ...) {
  
  if (length(levels(actual)) != 2 | length(levels(predicted)) != 2){
    stop("Class labels must have exactly two levels. Actual and/or
         predicted values have more than two levels.")
  }
  
  if (!identical(levels(actual), levels(predicted))){
    stop("Class labels for 'actual' and 'predicted' vectors must be identical.")
  }
  
  actual <- as.factor(actual)
  predicted <- as.factor(predicted)
  
  if (is.null(positive)){
    positive <- levels(positive)[1]
  }
  
  if (length(positive) > 1){
    warning("Multiple elements provided for 'positive' class label.
            First element is used.")
    positive <- positive[1]
  }
  
  predicted <- relevel(predicted, positive)
  actual <- relevel(actual, positive)
  
  tbl <- table(predicted, actual)
  
  res <- list(
    sens = tbl[1, 1] / sum(tbl[ ,1]),
    spec = tbl[2, 2] / sum(tbl[ ,2]),
    ppv = tbl[1, 1] / sum(tbl[1, ]),
    npv = tbl[2, 2] / sum(tbl[2, ]),
    acc = sum(diag(tbl)) / sum(tbl),
    f1 = 2 * tbl[1, 1] / (2 * tbl[1, 1] + tbl[1, 2] + tbl[2, 1])
  )
  
  return(res)
}


#-------------------------------------------------------------------------------Selected features

selected_features<-function(lab, feature_sel =TRUE){
  
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
  return(feat_res=list(View1, View1))
}

