---
title: "Base Learning, Self-Learning and Multi-view Learning on ACCORD Data"
author: "Melih Agraz"
output: R documents
---

# Description

This project provides a structure of folders and files for Multi-view Learning ACCORD Project. You can see 3 different codes structure  like;

```base_ML_.R```; conventional machine learning analysis part, 

```self_ML_.R```; self-training machine learning part and   

```CO_ML_.R``` ; multi-view co-training machine learning parts.

```base_ML_.main.R```, ```self_ML_.main.R``` and ```CO_ML_.main.R``` are the main part of the conventional, self training and co-training machine learning analysis, respectively. 

## Conventional Machine Learning (base_.R)

```base_ML.main.R``` is the main code of the conventional machine learning. You can run Logisctic Regression, Naive Bayes, XGBoost, Support Vector Machine and Random Forest conventional Machine Learning models. You must type ```method="Logisctic Regression"``` or ```method="Naive Bayes"``` or ```method="SVM"``` or ```method="Xgboost"``` or ```method="Random Forest"``` to run conventional machine learning models.  If your data is imbalanced, you can balance your data with ```imbalanced=TRUE```. You can choose the number of folds by ```n_fold```, default is 5.  We have already done the feature selection algorithm for ACCORD dataset and specified the selected features. For this reason, you can select these specified features by typing ```feature_sel =  "Boruta"``` or ```feature_sel =  "Lasso"``` or ```feature_sel =  "MRMR"``` OR if you want to work on only the full ACCORD data, you can type ```feature_sel =  "NO FS"```.

!!Column name of your response variable must be  ```out```.


### Requirements

You must install the related packages below.

```{r setup1, include=FALSE}
install.packages(c("caret","rpart","e1071", "dplyr", "randomForest", "xgboost", 
 "ROSE", "praznik", "gridExtra", "adabag"))
```

### Example

If you want to run Logistic Regression,  on imbalanced data on training set, without feature selection, with 5 fold cross validation, you can run the code below. 

```{r setup1, include=FALSE}
Baseline_Hyp(dataL = dataL, 
                 method           = "Logistic Regression" ,
                 imbalanced       =  TRUE,
                 feature_sel      =  "NO FS",
                 n_fold           =  5 
)
```
So the output will be the confusion matrix like below.


```{r setup1, include=FALSE}
           NPV       PPV      Spec      Sens       Acc        F1
[1,] 0.9130425 0.1835661 0.5912669 0.6118235 0.5943996 0.2803637

```


If you want to run SVM, working on NOT imbalanced data, with LASSO feature selection, with 5-fold cross validation, you can run the code below.  

```{r setup1, include=FALSE}
Baseline_Hyp(dataL = dataL, 
                 method           = "SVM" ,
                 imbalanced       =  FALSE,
                 feature_sel      =  "Lasso",
                 n_fold           =  5 
)
```

If you want to run all the combinations of ```method```, and ```feature_sel```, you can go to the ```base_ML.run.R``` (it is not necessary).  

## Self-training Machine Learning (self_.R)
```self_ML.main.R``` is the main code of the self-learning algorithm. This code gives you an opportunity to run self learning algorithm only for Naive Bayes and Random Forest Model. You must type ```method="Naive Bayes"``` or ```method="Random Forest"``` to run machine learning models. If your data is imbalanced, you must type ```imbalanced=TRUE``` to fix it, otherwise ```imbalanced=FALSE```. ```neg_conf_prob``` is a threshold probability for the negative pseudo-labeled class, i.e. the ones with the higher then ```neg_conf_prob``` will be labeled as confidence "pseudo labels" in negative class. ```pos_conf_prob``` is a threshold probability for the positive pseudo labeled class. Default values are ```neg_conf_prob=0.9``` and ```pos_conf_prob=0.1```, so we are selecting the confidence pseudo classes, if the pseudo-classes are higher than 0.9 for both negative and positive classes. 

You can see the Self-training Machine Learning model steps for cross validation.

<img width="655" alt="b1" src="https://user-images.githubusercontent.com/37498443/171413929-2e52037f-e343-42b3-98da-cf002bfd80a7.png">

Since we think that "MRMR" selected features are the best, we are continuing self-learning algorithm on  MRMR selected features. So if you want to continue with MRMR selected features, you must type ```feature_sel=TRUE```. You can choose the number of folds by ```n_fold```, default is 5.

### Requirements

You must install the related packages below.

```{r setup1, include=FALSE}
install.packages(c("caret", "ggplot2", "rpart", "dplyr",randomForest",  "e1071",  "ggpubr", " "xgboost", 
 "praznik", "gridExtra", "PRROC"))
```



 

### Example

If you want to run self-training algorithm on Naive Bayes,  working on imbalanced data on labeled (dataL) and unlabeled (dataU) data, without feature selection, with 5 fold cross validation, with 0.9 negative negative and positive confident pseudo selection . 

```{r setup1, include=FALSE}
self(dataL, 
    dataU, 
    method = "Naive Bayes", 
    imbalanced=TRUE,
    neg_conf_prob=0.9, 
    pos_conf_prob=0.1, 
    feature_sel=FALSE,
    n_fold=5
)
```

So the output will be the confusion matrix like below. This out gives us, the confusion matrix of the first iteration and the last iteration and the percentage gain or loss. 

```{r setup1, include=FALSE}
                Sensitivity Specificity       PPV      NPV      Acc        F1    BalAcc
First iteration     0.97904     0.03816  0.130600  0.92444  0.15898  0.230200  0.508600
Last iteration      0.86510     0.13326  0.127160  0.90042  0.22794  0.221320  0.499220
Percentage        -11.63793   249.21384 -2.633997 -2.59833 43.37653 -3.857515 -1.844278
```



If you want to run self-training algorithm on Random Forest, working NOT imbalanced data on labeled (dataL) and unlabeled (dataU) data, without feature selection, with 5 fold cross validation, with 0.8 negative negative and 0.7 confident positive pseudo selection . 

```{r setup1, include=FALSE}
self(dataL, 
    dataU, 
    method = "Random Forest", 
    imbalanced=FALSE,
    neg_conf_prob=0.8, 
    pos_conf_prob=0.3, 
    feature_sel=FALSE,
    n_fold=5
)
```

If you want to run all the combinations of ```method```, and ```feature_sel```, you can go to the ```self_ML.run.R``` (it is not necessary).  


## Multi-view Co-training Machine Learning model (CO_.R)
```CO_training_ML.main.R``` is the main code of the multi-view co training learning algorithm. This code is written according to Blum and Mitchell (1998) paper.  This code gives you an opportunity to run multi-view learning algorithm only for Naive Bayes and Random Forest Model. You must type ```method="Naive Bayes"``` or ```method="Random Forest"``` to run machine learning models. You can split the  data as train/test with typing ```train_prob = 0.8```.  Number of pool from the unlabeled data can be assigned typing as```n_subPool=75 ``` and number of iteration is fixed  as 30 as typing ```n_iteration=30```.  If your data is imbalanced, you must type ```imbalanced=TRUE``` to fix it, otherwise ```imbalanced=FALSE```. If you want to continuous with MRMR selected features, you must type ```feature_sel=TRUE```. You can choose the number of folds by ```n_fold```, default is 5. You can specify how many negative and positive confidence pseudo features can be selected by typing ```n_neg``` and ```n_pos```, respectively. 

You can see the Multi-view Co-training Machine Learning model steps for cross validation.
 
<img width="1015" alt="j1" src="https://user-images.githubusercontent.com/37498443/171456092-60a2008f-9404-4317-93a6-a38c13b499ce.png">


Since we think that selected features from "MRMR" is the best for the ACCORD data, we are continuing  MRMR selected features. So if you want to continuous with MRMR selected features, you must type ```feature_sel=TRUE```. You can choose the number of folds by ```n_fold```, default is 5.


### Requirements

You must install the related packages below.

```{r setup1, include=FALSE}
install.packages(c("caret", "ggplot2", "rpart", "dplyr",randomForest",  "e1071",  "ggpubr", " "xgboost", 
 "praznik", "gridExtra", "PRROC"))
```

### Example

If you want to run multi-view learning algorithm on Naive Bayes, working on imbalanced data on labeled (lab) and unlabeled (unlabeled) data, with feature selection (MRMR selected features on ACCORD data), with 5 fold cross validation, with 7 negative confidence features and 1 positive confidence features, with 75 randomly selected unlabeled data for the unlabeled pool for 30 iterations. 

```{r setup1, include=FALSE}
          CoTrain_cv_errorBar(lab, unlabeled, 
                              method = "Naive Bayes",
                              train_prob = 0.8, 
                              n_subPool=75, 
                              n_iteration=30, 
                              imbalanced=TRUE,
                              feature_sel=TRUE,
                              n_neg=7,
                              n_pos=1,
                              n_fold=5)
)
```
It will give the confusion matrix of the first, last iterations and percentage of gain/loss of these iterations. 

```{r setup1, include=FALSE}
first_last
                    Sen_v1    Sen_v2  Spec_v1  Spec_v2   PPV_v1      PPV_v2     NPV_v1    NPV_v2     F1_v1
first_iteration    0.31386   0.60936  0.80424  0.57018 0.191900   0.1723400  0.8874400  0.910180   0.23316
second iteration   0.19850   0.07302  0.88788  0.94098 0.202560   0.1333333  0.8819800  0.873100   0.19658
percentage       -36.75524 -88.01694 10.39988 65.03210 5.554977 -22.6335538 -0.6152529 -4.073919 -15.68880
                       F1_v2   Acc_v1   Acc_v2 BalAcc_v1 BalAcc_v2
first_iteration    0.2671800 0.739840  0.57484  0.559060   0.58976
second iteration   0.1194667 0.798520  0.82826  0.543180   0.50700
percentage       -55.2860743 7.931445 44.08531 -2.840482 -14.03283
)
```

In addition to that, you can receive the AND/OR rule concatenated results of View 1 and View2 and Figures of each confusion matrix.

```{r setup1, include=FALSE}
Conc_AND
  NPV    PPV   Spec    Sen    Acc     F1
1 0.8726 0.2143 0.9893 0.0199 0.8648 0.0364

Conc_OR
     NPV    PPV  Spec    Sen    Acc     F1
1 0.8821 0.1791 0.839 0.2384 0.7619 0.2045

```

<img width="426" alt="c1" src="https://user-images.githubusercontent.com/37498443/171415493-45da71f9-a608-48d6-b9d7-210df389d8dc.png">

If you want to run multi-view learning algorithm on Random Forest, working on imbalanced data on labeled (lab) and unlabeled (unlabeled) data, with feature selection (MRMR selected features on ACCORD data), with 5 fold cross validation, with 5 negative confidence features and 1 positive confidence features, with 75 randomly selected unlabeled data for the unlabeled pool for 30 iterations. 

```{r setup1, include=FALSE}
          CoTrain_cv_errorBar(lab, unlabeled, 
                              method = "Random Forest",
                              train_prob = 0.8, 
                              n_subPool=75, 
                              n_iteration=30, 
                              imbalanced=TRUE,
                              feature_sel=TRUE,
                              n_neg=5,
                              n_pos=1,
                              n_fold=5)
)
```
----------------------------------------------------------------- Reference--------------------------------------------------------------

[1] Blum, A and Mitchel, T. Combining Labeled and Unlabeled Data with Co-Training, Proceedings of the eleventh annual conference on Computational learning theory (1998).
