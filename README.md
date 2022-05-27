---
title: "Base Learning, Self-Learning and Multi-view Learning on ACCORD Data"
author: "Melih Agraz"
output: R documents
---

# Description

This project provides a structure of folders and files for Multi-view Learning ACCORD Project. You can see 3 different codes structure  like ```base_ML_.R```, ```self_ML_.R``` and ```CO_ML_.R``` in this project. 

```base_ML_.main.R```, ```self_ML_.main.R``` and ```CO_ML_.main.R``` are the main part of the conventional, self training and co-training of code, respectively. 

## Conventional Machine Learning (base_.R)

```base_ML.main.R``` is the main code of the conventional machine learning. You can run Logisctic Regression, Naive Bayes, XGBoost, Support Vector Machine and Random Forest conventional Machine Learning models. You must type ```method="Logisctic Regression"``` or ```method="Naive Bayes"``` or ```method="SVM"``` or ```method="Xgboost"``` or ```method="Random Forest"``` to run conventional machine learning models.  If your data is imbalanced, you can balance your data with ```imbalanced=TRUE```. You can choose the number of folds by ```n_fold```, default is 5.  We have already done the feature selection algorithm for ACCORD dataset and specified the selected features. For this reason, you can select these specified features by typing ```feature_sel =  "Boruta"``` or ```feature_sel =  "Lasso"``` or ```feature_sel =  "MRMR"``` OR if you want to work on only the full ACCORD data, you can type ```feature_sel =  "NO FS"```.

!!Column name of your response variable must be  ```out```.

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

## Self Learning (self_.R)
```self_ML.main.R``` is the main code of the self-learning algorithm. This code gives you an opportunity to run self learning algorithm only for Naive Bayes and Random Forest Model. You must type ```method="Naive Bayes"``` or ```method="Random Forest"``` to run machine learning models. If your data is imbalanced, you must type ```imbalanced=TRUE``` to fix it, otherwise ```imbalanced=FALSE```. ```neg_conf_prob``` is a threshold probability for the negative pseudo-labeled class, i.e. the ones with the higher then ```neg_conf_prob``` will be labeled as confidence "pseudo labels" in negative class. ```pos_conf_prob``` is a threshold probability for the positive pseudo labeled class. Default values are ```neg_conf_prob=0.9``` and ```pos_conf_prob=0.1```, so we are selecting the confidence pseudo classes, if the pseudo-classes are higher than 0.9 for both negative and positive classes. 

Since we think that "MRMR" selected features are the best, we are continuing self-learning algorithm on  MRMR selected features. So if you want to continue with MRMR selected features, you must type ```feature_sel=TRUE```. You can choose the number of folds by ```n_fold```, default is 5.


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


## Multi-view Learning (CO_.R)
```CO_training_ML.main.R``` is the main code of the multi-view co training learning algorithm. This code gives you an opportunity to run multi-view learning algorithm only for Naive Bayes and Random Forest Model. You must type ```method="Naive Bayes"``` or ```method="Random Forest"``` to run machine learning models. You can split the  data as train/test with typing ```train_prob = 0.8```.  Number of pool from the unlabeled data can be assigned typing as```n_subPool=75 ``` and number of iteration is fixed  as 30 as typing ```n_iteration=30```.  If your data is imbalanced, you must type ```imbalanced=TRUE``` to fix it, otherwise ```imbalanced=FALSE```. If you want to continuous with MRMR selected features, you must type ```feature_sel=TRUE```. You can choose the number of folds by ```n_fold```, default is 5. You can specify how many negative and positive confidence pseudo features can be selected by typing ```n_neg``` and ```n_pos```, respectively. 


If your data is imbalanced, you must type ```imbalanced=TRUE``` to fix it, otherwise ```imbalanced=FALSE```. ```neg_conf_prob``` is a threshold probability for the negative confidence pseudo-labeled class, i.e. the ones with the higher then ```neg_conf_prob``` will be labeled as confidence "pseudo labels" in negative class. ```pos_conf_prob``` is a threshold probability for the positive pseudo labeled class. Default values are ```neg_conf_prob=0.9``` and ```pos_conf_prob=0.1```, so we are selecting the pseudo classes which is higher than 0.9 for both negative and positive classes. 

Since we think that selected features from "MRMR" is the best, we are continuing  MRMR selected features. So if you want to continuous with MRMR selected features, you must type ```feature_sel=TRUE```. You can choose the number of folds by ```n_fold```, default is 5.



### Example

If you want to run multi-view learning algorithm on Naive Bayes, working on imbalanced data on for labeled (lab) and unlabeled (unlabeled) data, with feature selection (MRMR selected features on ACCORD data), with 5 fold cross validation, with 7 negative confidence features and 1 positive confidence features, with 75 randomly selected unlabeled data for the unlabeled pool for 30 iterations. 

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



If you want to run multi-view learning algorithm on Random Forest, working on imbalanced data on for labeled (lab) and unlabeled (unlabeled) data, with feature selection (MRMR selected features on ACCORD data), with 5 fold cross validation, with 5 negative confidence features and 1 positive confidence features, with 75 randomly selected unlabeled data for the unlabeled pool for 30 iterations. 

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

## R Template Project

This project provides a template structure of folders and files for an R project.

## Folder Structure

The project comprises of three main folders in which you would store certain files.

### data/

This folder should contain data files like .csv, .xls, .spss, etc.

In this folder:

- An example data set of bird information taken from the [UCLA statistics teaching material](http://www.stat.ucla.edu/projects/datasets/).

### R/

This folder should contain R script files.

In this folder:

- Template script with the key section headers described by the [Google's R Style Guide](https://google.github.io/styleguide/Rguide.xml).

### Rmd/

This folder should contain all R markdown and associated files.

In this folder:

- **reference_docx/** which should contain any reference documents to be used by R Markdown file. This currently includes:
  - A default APA-style word doc, "APA-style-01.docx"
- **bib/** which should contain bibliographies to be used by R Markdown file. This currently includes:
  - An example bibliography file, "example-bib.bib"
- **img/** which should contain images to be inserted into R Markdown file. This currently imcludes:
  - An image showing the output formats generated by R Markdown, "RMarkdownOutputFormats.png"
- A template R Markdown for producing APA-style word documents. For styling, this references "reference_docx/APA-style-01.docx"