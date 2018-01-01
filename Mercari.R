library(ggplot2)
library(stringr)
library(readr)
library(tm)
library(data.table)
library(lightgbm)
library(caret)
library(tidytext)

setwd("C:/R-Studio/Mercari")
source("Mercari_Functions.R")

train <- fread('train.tsv', header=TRUE)
test <- fread('test.tsv', header=TRUE)
sample_submission <- fread('sample_submission.csv', header=TRUE)

# Convert int and chr to integer
colnames(train)[1] <- "id"
colnames(test)[1] <- "id"
test$price <- -1
train_test <- rbind(train,test)
train_test$id <- 1:nrow(train_test)

train_test[,category_name:=as.integer(as.factor(train_test$category_name))]
train_test[,brand_name:=as.integer(as.factor(train_test$brand_name))]
train <- head(train_test,nrow(train))
test <- tail(train_test,nrow(test))
rm(train_test)

# Create the folds
no_folds <- 5
folds <- createFolds(1:nrow(train),no_folds)
# quick check
sum(unlist(lapply(folds, function(x) length(x))))
nrow(train)

train$price <- log(train$price+1)

cat_vector_list <- list(c("category_name","item_condition_id"),
                        c("brand_name"),c("shipping"),c("item_condition_id"),
                        c("brand_name","item_condition_id"),
                        c("brand_name","shipping"))

feature_names <- as.vector(NULL)
for (cats in 1:length(cat_vector_list)) {
  print(paste0("Now work on feature ",paste0(cat_vector_list[[cats]],collapse = ",")))
  train_add <- get_nway_interaction(train=train,
                                    y_name="price",category_vector=cat_vector_list[[cats]],
                                    trainOrApply='T',folds=folds,eliminateOrigColumns=TRUE)

  feature_names <- c(feature_names,colnames(train_add))
  train <- cbind(train,train_add)
}

name_words <- train_test[,c("id","name","price"),with=FALSE] %>% 
  unnest_tokens(word,name)
agg <- name_words[!name_words$price == (-1),j=list(mean_price=mean(price,na.rm=TRUE),
                                                   Count=length(price)),by=c("word")]


mean(name_words$price[name_words$word == 'girl' & !name_words$price== (-1)])


# Now light gbm
params = list(num_leaves=31,min_data_in_leaf=50,
              learning_rate=0.1,feature_fraction=0.8,bagging_fraction=0.8,
              bagging_freq=2,num_threads=4)

dtrain = lgb.Dataset(as.matrix(train[,feature_names,with=FALSE]), label=as.matrix(train[,"price",with=FALSE]))
#dvalid = lgb.Dataset(as.matrix(valid), label=y_valid[,day],reference=dtrain)
lgb_fit <- lgb.cv(params=params, data=dtrain,nrounds=50,folds=folds,
                  early_stopping_rounds =50, verbose = 1,eval_freq=10,
                  objective="regression_l2", metric="l2_root")

feature_importance <- lgb.importance(lgb_fit$boosters[[1]]$booster)
feature_importance


