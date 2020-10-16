library(caret)
library(dplyr)
library(utc)
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

load('/Volumes/INTENSO/methods_paper/output/02-finalized-corpora/baseline/reddit/ML_corpus.RDS')

# specify a 10-fold cross-validation process and the dominant metric to select best model
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# add ML-variables for full corpus analysis
dfx <- mutate(dfx, 
              CLASS = gsub('_POS|_NEG', '', cat),
              TARGET_really_intensifier = ifelse(TARGET_mod == 'really', 1, 0),
              TARGET_truly_intensifier = ifelse(TARGET_mod == 'truly', 1, 0),
              ADV_really_intensifier = ifelse(ADV == 'really', 1, 0),
              ADV_truly_intensifier = ifelse(ADV == 'truly', 1, 0))
fullMvars <- c('TARGET_really_intensifier', 'TARGET_truly_intensifier', 'ADV_really_intensifier', 'ADV_truly_intensifier')
dfx <- mutate_at(dfx, fullMvars,
                 .funs = function(x) ifelse(is.na(x), 0, x))
# have a look at their distribution
table(dfx$TARGET_really_intensifier)
table(dfx$TARGET_truly_intensifier)
table(dfx$ADV_really_intensifier)
table(dfx$ADV_truly_intensifier)
# they will probably have to be dropped when we do not use full data

# drop variables
dfx <- select(dfx, -txt, -corpus, -match_first, 
              -TARGET, -context, -cat, -CLASS, 
              -ADV_pol, -TARGET_mod_pol, -TARGET_mod, 
              -ADV, -match, -ADJ)
# change variable classes accordingly
dfx <- mutate_at(dfx, vars(-sentiWords), as.factor)

# rename data
df <- dfx
rm(dfx)

# make sample to test models
set.seed(7826)
df <- sample_n(df, 10000)

# make test train split
set.seed(7826)
validation_index <- createDataPartition(df$TARGET_pol, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- df[-validation_index,]
# use the remaining 80% of data to training and testing the models
trainSet <- df[validation_index,]

# a) linear algorithms
set.seed(7826)
fit.lda <- train(TARGET_pol~., data=trainSet, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7826)
fit.cart <- train(TARGET_pol~., data=trainSet, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7826)
fit.knn <- train(TARGET_pol~., data=trainSet, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7826)
fit.svm <- train(TARGET_pol~., data=trainSet, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7826)
fit.rf <- train(TARGET_pol~., data=trainSet, method="rf", metric=metric, trControl=control)

# compute model comparison
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
# plot kappa and accuracy comparisons for all models
dotplot(results)

# retrieve variable importance for the best model
rfImp <- varImp(fit.rf)

# estimate skill of the best model on the validation dataset
predictions <- predict(fit.rf, validation)
confusionMatrix(predictions, validation$TARGET_pol)
# different performance measures
confusionMatrix(predictions, validation$TARGET_pol, mode = "prec_recall")

# save models
time_index <- as.numeric(toUTC(Sys.time()))
time_index <- as.character(time_index)
save(fit.cart, fit.knn, fit.lda, fit.rf, fit.svm, trainSet, validation, predictions, 
     file = paste0('../output/04-classification-models/target_pol_class_', time_index, '.Rdata'))
