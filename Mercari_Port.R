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

start_time <- proc.time()[3]

NUM_BRANDS = 4000
NUM_CATEGORIES = 1000
NAME_MIN_DF = 10
MAX_FEATURES_ITEM_DESCRIPTION = 40000

train <- fread('train.tsv', header=TRUE)
test <- fread('test.tsv', header=TRUE)
sample_submission <- fread('sample_submission.csv', header=TRUE)

print(paste0('Finished to load data ',format(proc.time()[3] - start_time)))
print(paste0('Train shape: ', paste0(dim(train),collapse=" ,")))
print(paste0('Test shape: ', paste0(dim(test),collapse=" ,")))

nrow_train <- nrow(train)
y <- log(train$price+1)
train[,price:=NULL]
colnames(train)[1] <- "id"
colnames(test)[1] <- "id"

train_test <- rbind(train,test)
submission <- test[,"id",with=FALSE] 

rm("train")
rm("test")
gc()

# eliminate NA's, none there, but python code has this as well just in case
for (j in c("category_name","brand_name","item_description"))
  set(train_test,which(is.na(train_test[[j]])),j,0)
print(paste0('Finished to handle NAs ',format(proc.time()[3] - start_time)))

# cutting
mercari_cut <- function() {
  #agg_pop_brand_t <- train_test[,j=list(count=length(id)),by=c("brand_name")][order(count, decreasing = TRUE)]
  agg_pop_brand <- train_test[,j=list(count=length(id)),by=c("brand_name")][order(count, decreasing = TRUE)][1:NUM_BRANDS,c("brand_name")]
  train_test[!train_test$brand_name %in% agg_pop_brand$brand_name, brand_name:="missing"]
  agg_pop_cats <- train_test[,j=list(count=length(id)),by=c("category_name")][order(count, decreasing = TRUE)][1:NUM_CATEGORIES,c("category_name")]
  train_test[!train_test$category_name %in% agg_pop_cats$category_name, category_name:="missing"]
}
mercari_cut()
print(paste0('Finished to cut ',format(proc.time()[3] - start_time)))

to_categorical(merge)
print('[{}] Finished to convert categorical'.format(time.time() - start_time))

cv = CountVectorizer(min_df=NAME_MIN_DF)
X_name = cv.fit_transform(merge['name'])
print('[{}] Finished count vectorize `name`'.format(time.time() - start_time))

cv = CountVectorizer()
X_category = cv.fit_transform(merge['category_name'])
print('[{}] Finished count vectorize `category_name`'.format(time.time() - start_time))

tv = TfidfVectorizer(max_features=MAX_FEATURES_ITEM_DESCRIPTION,
                     ngram_range=(1, 3),
                     stop_words='english')
X_description = tv.fit_transform(merge['item_description'])
print('[{}] Finished TFIDF vectorize `item_description`'.format(time.time() - start_time))

lb = LabelBinarizer(sparse_output=True)
X_brand = lb.fit_transform(merge['brand_name'])
print('[{}] Finished label binarize `brand_name`'.format(time.time() - start_time))

X_dummies = csr_matrix(pd.get_dummies(merge[['item_condition_id', 'shipping']],
                                      sparse=True).values)
print('[{}] Finished to get dummies on `item_condition_id` and `shipping`'.format(time.time() - start_time))

sparse_merge = hstack((X_dummies, X_description, X_brand, X_category, X_name)).tocsr()
print('[{}] Finished to create sparse merge'.format(time.time() - start_time))

X = sparse_merge[:nrow_train]
X_test = sparse_merge[nrow_train:]

#train_X, valid_X, train_y, valid_y = train_test_split(X, y, test_size = 0.1, random_state = 144) 
d_train = lgb.Dataset(X, label=y, max_bin=8192)
#d_valid = lgb.Dataset(valid_X, label=valid_y, max_bin=8192)
#watchlist = [d_train, d_valid]

params = {
  'learning_rate': 0.75,
  'application': 'regression',
  'max_depth': 3,
  'num_leaves': 100,
  'verbosity': -1,
  'metric': 'RMSE',
}


model = lgb.train(params, train_set=d_train, num_boost_round=3335,  \
                  verbose_eval=100) 
preds = 0.6*model.predict(X_test)


model = Ridge(solver="sag", fit_intercept=True, random_state=205)
model.fit(X, y)
print('[{}] Finished to train ridge'.format(time.time() - start_time))
preds += 0.4*model.predict(X=X_test)
print('[{}] Finished to predict ridge'.format(time.time() - start_time))


submission['price'] = np.expm1(preds)
submission.to_csv("submission_lgbm_ridge_5.csv", index=False)



def to_categorical(dataset):
  dataset['category_name'] = dataset['category_name'].astype('category')
dataset['brand_name'] = dataset['brand_name'].astype('category')
dataset['item_condition_id'] = dataset['item_condition_id'].astype('category')
