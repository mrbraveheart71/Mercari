library(ggplot2)
library(stringr)
library(readr)
library(tm)
library(SnowballC)

# Calculates n-way interactions for categorical variables
# If train is true we use folds to get out of sample results
get_nway_interaction <- function(train, y_name,category_vector,test=NULL,
                                       trainOrApply = 'T', folds, 
                                      eliminateOrigColumns=FALSE) {
  no_rows <- nrow(train)
  # if (trainOrApply=='A') folds=1
  # sample_folds <- sample(1:folds,no_rows,replace=TRUE)
  
  # Set names of columns
  colMeanResponseName <- paste0(paste0(category_vector,collapse="_"),"_Mean_Response")
  colMedianResponseName <- paste0(paste0(category_vector,collapse="_"),"_Median_Response")
  colCountResponseName <- paste0(paste0(category_vector,collapse="_"),"_Count_Response")
  colMaxResponseName <- paste0(paste0(category_vector,collapse="_"),"_Max_Response")
  colMinResponseName <- paste0(paste0(category_vector,collapse="_"),"_Min_Response")
  colSDResponseName <- paste0(paste0(category_vector,collapse="_"),"_SD_Response")

  if (trainOrApply=='T') no_folds=length(names(folds))
  if (trainOrApply=='A') no_folds=1

  # Go through all folds
  for (f in 1:no_folds) {
    
    if (trainOrApply=='T') {
      idx_train <- setdiff(1:nrow(train),folds[[f]])
      idx_out_of_sample <- folds[[f]]
    } else {
      idx_train <- 1:nrow(train)
    }
    n_Way_Results <- train[idx_train,j=list(Mean.Response=mean(get(y_name),na.rm=TRUE),Median.Response=median(get(y_name),na.rm=TRUE),
                                            Max.Response=max(get(y_name),na.rm=TRUE),Min.Response=min(get(y_name),na.rm=TRUE),
                                            SD.Response=sd(get(y_name),na.rm=TRUE),
                                            Count=length(get(y_name))),by=category_vector]
    setkeyv(n_Way_Results,category_vector)
    mean_y <- mean(train[idx_train][[y_name]])
    
    if (trainOrApply=='T')  {
      train[idx_out_of_sample,colMeanResponseName] <- n_Way_Results[train[idx_out_of_sample,category_vector,with=FALSE], list(Mean.Response)]
      train[idx_out_of_sample,colMedianResponseName] <- n_Way_Results[train[idx_out_of_sample,category_vector,with=FALSE], list(Median.Response)]
      train[idx_out_of_sample,colCountResponseName] <- n_Way_Results[train[idx_out_of_sample,category_vector,with=FALSE], list(Count)]
      train[idx_out_of_sample,colMaxResponseName] <- n_Way_Results[train[idx_out_of_sample,category_vector,with=FALSE], list(Max.Response)]
      train[idx_out_of_sample,colMinResponseName] <- n_Way_Results[train[idx_out_of_sample,category_vector,with=FALSE], list(Min.Response)]
      train[idx_out_of_sample,colSDResponseName] <- n_Way_Results[train[idx_out_of_sample,category_vector,with=FALSE], list(SD.Response)]
    } else {
      test[,colMeanResponseName] <- n_Way_Results[test[,category_vector,with=FALSE], list(Mean.Response)]
      test[,colMedianResponseName] <- n_Way_Results[test[,category_vector,with=FALSE], list(Median.Response)]
      test[,colCountResponseName] <- n_Way_Results[test[,category_vector,with=FALSE], list(Count)]
      test[,colMaxResponseName] <- n_Way_Results[test[,category_vector,with=FALSE], list(Max.Response)]
      test[,colMinResponseName] <- n_Way_Results[test[,category_vector,with=FALSE], list(Min.Response)]
      test[,colSDResponseName] <- n_Way_Results[test[,category_vector,with=FALSE], list(SD.Response)]
    }
    
  } # end of For Loop with Folds
  
  returnCols <- c(colMeanResponseName,colMedianResponseName,colCountResponseName, 
                  colMaxResponseName,colMinResponseName,colSDResponseName)
  
  if (trainOrApply=='T') {
    return <- train[,returnCols,with=FALSE]
    # This is apply
  } else {
    return <- test[,returnCols,with=FALSE]
  }

  if (eliminateOrigColumns==FALSE) category_vector <- NULL
  returnCols <- setdiff(colnames(return),c(category_vector))
  #return[[colMeanResponseName]] <- ifelse(is.na(return[[colMeanResponseName]]),mean_y, return[[colMeanResponseName]])
  return <- return[,returnCols,with=FALSE]
  return
} # end of get_nway_interaction

# get text feature
get_text_feature <- function(train, test=NULL,
                             trainOrApply = 'T', folds, 
                             eliminateOrigColumns=FALSE) {
  
  no_rows <- nrow(train)
  
  # Set names of columns
  if (trainOrApply=='T') no_folds=length(names(folds))
  if (trainOrApply=='A') no_folds=1
  
  # Go through all folds
  for (f in 1:no_folds) {
    
    print(paste0("We are inside get_text_feature and at fold number :",f))
    if (trainOrApply=='T') {
      idx_train <- setdiff(1:nrow(train),folds[[f]])
      idx_out_of_sample <- folds[[f]]
    } else {
      idx_train <- 1:nrow(train)
    }
    
    name_words <- train[idx_train,c("id","name","price"),with=FALSE] %>% 
      unnest_tokens(word,name) #%>%  count(word,sort=TRUE) %>% ungroup()
    name_words_out_of_sample <- train[idx_out_of_sample,c("id","name","price"),with=FALSE] %>%
      unnest_tokens(word,name)
    words_agg <- name_words[,j=list(mean_price=mean(price,na.rm=TRUE),
                                    Count=length(price)),by=c("word")]
    setkeyv(name_words_out_of_sample,"word")
    setkeyv(words_agg,"word")
    
    if (trainOrApply=='T')  {
      name_words_out_of_sample <- words_agg[name_words_out_of_sample,list(id,word,mean_price,Count)]
      name_words_out_of_sample$weight <- exp(name_words_out_of_sample$Count * -0.0001)
      name_words_out_of_sample$weight <- ifelse(name_words_out_of_sample$Count < 5, 0, name_words_out_of_sample$weight)
      name_words_agg <- name_words_out_of_sample[,j=list(weighted_avg_price=weighted.mean(mean_price,weight,na.rm=TRUE)),
                                                         by=c("id")]

      no_rows <- nrow(train[idx_train])
      # Formula to merge word mean prices
      setkeyv(name_words_agg,"id")
      train[idx_out_of_sample,c("weighted_avg_word_price")] <- name_words_agg[train[idx_out_of_sample,"id",with=FALSE], 
                                                                    list(weighted_avg_price)]
    } else {
      #test[,colMeanResponseName] <- n_Way_Results[test[,category_vector,with=FALSE], list(Mean.Response)]
    }
    
  } # end of For Loop with Folds
  
  returnCols <- c("weighted_avg_word_price")
  
  if (trainOrApply=='T') {
    #return <- train[,returnCols,with=FALSE]
    return <- train
    # This is apply
  } else {
    #return <- test[,returnCols,with=FALSE]
    return <- test
  }
  
  if (eliminateOrigColumns==FALSE)
    returnCols <- colnames(train)
  #returnCols <- setdiff(colnames(return),c(category_vector))
  #return[[colMeanResponseName]] <- ifelse(is.na(return[[colMeanResponseName]]),mean_y, return[[colMeanResponseName]])
  return <- return[,returnCols,with=FALSE]
  return
} # end of get_nway_interaction


