library(ggplot2)
library(stringr)
library(readr)
library(tm)
library(data.table)
library(lightgbm)

setwd("C:/R-Studio/Mercari")

train <- fread('train.tsv', header=TRUE)
test <- fread('test.tsv', header=TRUE)
sample_submission <- fread('sample_submission.csv', header=TRUE)

