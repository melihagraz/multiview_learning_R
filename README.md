---
title: "Base Learning, Self-Learning and Multi-view Learning"
author: "Melih Agraz"
output: R documents
---

## Description

This project provides a template structure of folders and files for Multi-view Learning ACCORD Project. You are going to see 3 different code structures like ```base_ML_.R```, ```self_ML_.R``` and ```CO_ML_.R```. 

```base_ML_.main.R```, ```self_ML_.main.R``` and ```CO_ML_.main.R``` are the main part of the conventional, self training and co-training of code,respectively and you have to run the ```base_ML_.run.R```, ```self_ML_.run.R``` and ```CO_ML_.run.R``` to run these codes.

## Conventional MAchine Learning (base_.R)

```base_ML.main.R``` is the main code of the conventional machine learning. You can run Logisctic Regression, Naive Bayes, XGBoost, Support Vector Machine and Random Forest conventional Machine Learning models.


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
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