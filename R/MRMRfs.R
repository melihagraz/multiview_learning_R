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
