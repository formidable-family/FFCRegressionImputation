#Data cleaning for logical and regression imputation

library(zoo)

message('Importing data...')
#this directory and data needs to be created
background <- read.csv('data/background.csv', stringsAsFactors=F)

source('imputation_logical.R', echo=F)

background_imputed <- logical_imputation(background)

message('drop missing')
background_imputed[background_imputed < 0] <- NA

background_numeric <- data.frame(sapply(background_imputed, as.numeric))

message('impute means')
background_mean <- na.aggregate(background_numeric)

source('imputation_regression.R')