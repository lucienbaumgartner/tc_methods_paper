## ---------------------------
##
## Script name: xx-polarity-classification.R
##
## Project: Tracing Thick Concepts
##
## Purpose of script: Classifier for sentiment polarity
##
## Author: Lucien Baumgartner
##
## Date created: 16.10.2020
##
## Email: lucienbaumgartner@philos.uzh.ch
##
## ---------------------------
##
## Notes:
##    These models were not
##    used in the final paper
##
## ---------------------------

## ---------------------------
######## 1 Libraries #########
## ---------------------------
library(caret)
library(dplyr)
library(utc)
rm(list=ls())

## ---------------------------
## 2 Set working directory ###
## ---------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## ---------------------------
######## 3 Load data #########
## ---------------------------
load('/Volumes/INTENSO/methods_paper/output/02-finalized-corpora/baseline/reddit/ML_corpus.RDS')

## ---------------------------
###### 4 Prepare Data ########
## ---------------------------
## Add ML-variables for full corpus analysis
dfx <- mutate(dfx, 
              CLASS = gsub('_POS|_NEG', '', cat),
              TARGET_really_intensifier = ifelse(TARGET_mod == 'really', 1, 0),
              TARGET_truly_intensifier = ifelse(TARGET_mod == 'truly', 1, 0),
              ADV_really_intensifier = ifelse(ADV == 'really', 1, 0),
              ADV_truly_intensifier = ifelse(ADV == 'truly', 1, 0))
fullMvars <- c('TARGET_really_intensifier', 'TARGET_truly_intensifier', 'ADV_really_intensifier', 'ADV_truly_intensifier')
dfx <- mutate_at(dfx, fullMvars,
                 .funs = function(x) ifelse(is.na(x), 0, x))
## Have a look at their distribution
table(dfx$TARGET_really_intensifier)
table(dfx$TARGET_truly_intensifier)
table(dfx$ADV_really_intensifier)
table(dfx$ADV_truly_intensifier)
## They will probably have to be dropped when we do not use full data

## Drop variables
dfx <- select(dfx, -txt, -corpus, -match_first, 
              -TARGET, -context, -cat, -CLASS, 
              -ADV_pol, -TARGET_mod_pol, -TARGET_mod, 
              -ADV, -match, -ADJ)
## Change variable classes accordingly
dfx <- mutate_at(dfx, vars(-sentiWords), as.factor)
## Rename data
df <- dfx
rm(dfx)

## ---------------------------
##### 5 Classification #######
## ---------------------------
## Specify a 10-fold cross-validation process and the dominant metric to select best model
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
## Make sample to test models
set.seed(7826)
df <- sample_n(df, 10000)
## Make test train split
set.seed(7826)
validation_index <- createDataPartition(df$TARGET_pol, p=0.80, list=FALSE)
## Select 20% of the data for validation
validation <- df[-validation_index,]
## Use the remaining 80% of data to training and testing the models
trainSet <- df[validation_index,]

## ---------------------------
## Models
## a) Linear algorithms
set.seed(7826)
fit.lda <- train(TARGET_pol~., data=trainSet, method="lda", metric=metric, trControl=control)
## b) Nonlinear algorithms
## CART
set.seed(7826)
fit.cart <- train(TARGET_pol~., data=trainSet, method="rpart", metric=metric, trControl=control)
## kNN
set.seed(7826)
fit.knn <- train(TARGET_pol~., data=trainSet, method="knn", metric=metric, trControl=control)
## c) Advanced algorithms
## SVM
set.seed(7826)
fit.svm <- train(TARGET_pol~., data=trainSet, method="svmRadial", metric=metric, trControl=control)
## Random Forest
set.seed(7826)
fit.rf <- train(TARGET_pol~., data=trainSet, method="rf", metric=metric, trControl=control)

## ---------------------------
## Compute model comparison
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
## Plot kappa and accuracy comparisons for all models
dotplot(results)

## Retrieve variable importance for the best model
rfImp <- varImp(fit.rf)

## Estimate skill of the best model on the validation dataset
predictions <- predict(fit.rf, validation)
confusionMatrix(predictions, validation$TARGET_pol)
## Different performance measures
confusionMatrix(predictions, validation$TARGET_pol, mode = "prec_recall")

## ---------------------------
## Save models
time_index <- as.numeric(toUTC(Sys.time()))
time_index <- as.character(time_index)
save(fit.cart, fit.knn, fit.lda, fit.rf, fit.svm, trainSet, validation, predictions, 
     file = paste0('../../output/04-classification-models/target_pol_class_', time_index, '.Rdata'))
