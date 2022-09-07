#  Description
This project provides a structure of folders and files for Multi-view Learning ACCORD Project. You can see 3 different codes structure like;

```baseline.R```; conventional machine learning analysis part,

```cotraining.R```; self-training machine learning part and

```MultiViewCotraining.R``` ; multi-view co-training machine learning parts.

```MainBaseline.R```, ```MainCotraining.R``` and ```MainMultiView.R``` are the main part of the conventional, self training and multi-view cotraining machine learning analysis, respectively.

## Feature selection

FeatureSelection(data, method = "Lasso" )

[1] "fpg_mean"            "fpg_std"             "hba1c_mean"                   
[4] "g1check_mean"        "g1diabed_mean"       "g1diabed_std"        "g1nutrit_std"       
[8] "sulfonylurea_mean"   "nphl_insulin_mean"   "othbol_insulin_mean" "premix_insulin_mean"

![Rplot](https://user-images.githubusercontent.com/37498443/188721984-6aaffe1f-85c3-494c-86d6-72fd26c020bc.png)


FeatureSelection(data, method = "Boruta" )

FeatureSelection(data, method = "MRMR", K = 4)

## Baseline 

You can run conventional machine learning algorithms (logistic regression, naive bayes, random forest, support vector machine, xgboost) with ```MainBaseline.R``` code. ```baseline.R``` is a code that shows how to run the main ```MainBaseline.R``` code.


## Cotraining 

```MainCotraining.R``` is the main code of the self-learning algorithm.
This code gives you an opportunity to run self learning algorithm only for 
Naive Bayes and Random Forest Model. You must type method="Naive Bayes" or 
method="Random Forest" to select machine learning models. If your data is imbalanced, 
you must type imbalanced=TRUE to fix it, otherwise imbalanced=FALSE. neg_conf_prob 
is a threshold probability for the negative pseudo-labeled class, i.e. the ones 
with the higher then neg_conf_prob will be labeled as confidence "pseudo labels" in 
negative class. pos_conf_prob is a threshold probability for the positive pseudo labeled class.
Default values are neg_conf_prob=0.9 and pos_conf_prob=0.1, so we are selecting the confidence 
pseudo classes, if the pseudo-classes are higher than 0.9 for both negative and positive classes.

You can run the main code with ```cotraining.R``` code. 


## Multi-view Cotraining

```MultiViewCotraining.R``` is the main code of the multi-view co training algorithm. This code is written according to Blum and Mitchell (1998) paper. This code gives you an opportunity to run multi-view learning algorithm only for Naive Bayes and Random Forest Model. You must type method="Naive Bayes" or method="Random Forest" to run machine learning models. You can split the data as train/test with typing train_prob = 0.8. Number of pool from the unlabeled data can be assigned typing asn_subPool=75 and number of iteration is fixed as 30 as typing n_iteration=30. If your data is imbalanced, you must type imbalanced=TRUE to fix it, otherwise imbalanced=FALSE. If you want to continuous with MRMR selected features, you must type feature_sel=TRUE. You can choose the number of folds by n_fold, default is 5. You can specify how many negative and positive confidence pseudo features can be selected by typing n_neg and n_pos, respectively.

You can run the main multi-view code with ```MultiViewCotraining.R```. 


## ```HelperFunctions.R```

```HelperFunctions.R``` is a code that contains auxiliary functions to the main functions.

The functions in the ```HelperFunctions.R```  are;

```balanced_Co``` is created to fix imbalanced problem in multi-view cotraining algorithm.

```balanced``` is created to fix imbalanced problem in cotraining algorithm.

```data_summary``` calculates the mean and standard deviation of the tabulated data.

```FeatureSelection``` includes "Lasso", "Boruta" and "MRMR"  feature selection algorithms.

```plot_Co``` ```plot Co``` plots the figures of multi-view co training algorithm.

```saver``` saves the results as ".pdf" and ".csv" format.

```plot_self``` plots the figures of cotraining algorithm.
 
 ```confMat``` calculates the accuracy measures of confusion matrix.

