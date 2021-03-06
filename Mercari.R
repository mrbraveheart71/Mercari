library(ggplot2)
library(stringr)
library(readr)
library(tm)
library(data.table)
library(lightgbm)
library(caret)
library(tidytext)
library(text2vec)

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

prep_fun = tolower
tok_fun = word_tokenizer
it_train = itoken(train_test$name, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = train_test$id, 
                  progressbar = FALSE)
vocab = create_vocabulary(it_train, ngram=c(1,3))
vocab <- vocab[vocab$term_count>10,]
vectorizer = vocab_vectorizer(vocab)
t1 = Sys.time()
dtm_train = create_dtm(it_train, vectorizer)
print(difftime(Sys.time(), t1, units = 'sec'))

train_test$price <- log(train_test$price+1)

train <- head(train_test,nrow(train))
test <- tail(train_test,nrow(test))
rm(train_test)

# Create the folds
# no_folds <- 5
# folds <- createFolds(1:nrow(train),no_folds)
# # quick check
# sum(unlist(lapply(folds, function(x) length(x))))
# nrow(train)
# 
# 
# cat_vector_list <- list(c("category_name","item_condition_id"),
#                         c("brand_name"),c("shipping"),c("item_condition_id"),
#                         c("brand_name","item_condition_id"),
#                         c("brand_name","shipping"))
# 
# feature_names <- as.vector(NULL)
# for (cats in 1:length(cat_vector_list)) {
#   print(paste0("Now work on feature ",paste0(cat_vector_list[[cats]],collapse = ",")))
#   train_add <- get_nway_interaction(train=train,
#                                     y_name="price",category_vector=cat_vector_list[[cats]],
#                                     trainOrApply='T',folds=folds,eliminateOrigColumns=TRUE)
# 
#   feature_names <- c(feature_names,colnames(train_add))
#   train <- cbind(train,train_add)
# }
# 
# train_add <- get_text_feature(train=train,test=0,trainOrApply = 'T',folds=folds,
#                               eliminateOrigColumns = TRUE)
# train <- cbind(train, train_add)
# 
# feature_names <- union(feature_names, c("weighted_avg_word_price"))
                       
# Now light gbm
params = list(num_leaves=99,
              learning_rate=0.75,feature_fraction=0.8,bagging_fraction=0.8,
              bagging_freq=2,num_threads=4)

t <- sample(1:nrow(train),0.8*nrow(train))
v <- setdiff(1:nrow(train),t)
dtrain = lgb.Dataset(dtm_train[t,], label=as.matrix(train[t,"price",with=FALSE]),max_bin=8192)
dvalid = lgb.Dataset(dtm_train[v,], label=as.matrix(train[v,"price",with=FALSE]),max_bin=8192)
watchlist = list(dtrain=dtrain, dvalid=dvalid)

#dvalid = lgb.Dataset(as.matrix(valid), label=y_valid[,day],reference=dtrain)
lgb_fit <- lgb.train(params=params, data=dtrain,nrounds=1000,
                  early_stopping_rounds =50, verbose = 1,eval_freq=10,
                  objective="regression_l2", metric="l2_root",valids =watchlist)

feature_importance <- lgb.importance(lgb_fit$boosters[[1]]$booster)
feature_importance


