library(ggplot2)
library(stringr)
library(readr)
library(tm)
library(data.table)
library(lightgbm)
library(caret)

setwd("C:/R-Studio/Mercari")

train <- fread('train.tsv', header=TRUE)
test <- fread('test.tsv', header=TRUE)
sample_submission <- fread('sample_submission.csv', header=TRUE)

# Convert int and chr to integer
colnames(train)[1] <- "id"
colnames(test)[1] <- "id"
test$price <- -1
train_test <- rbind(train,test)
train_test[,category_name:=as.integer(as.factor(train_test$category_name))]
train_test[,brand_name:=as.integer(as.factor(train_test$brand_name))]
train <- head(train_test,nrow(train))
test <- tail(train_test,nrow(test))
rm(train_test)

# Create the folds
no_folds <- 5
folds <- createFolds(1:nrow(train),no_folds)
# quick check
sum(unlist(lapply(s, function(x) length(x))))
nrow(train)



