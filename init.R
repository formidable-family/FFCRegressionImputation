#Data cleaning for logical and regression imputation

library(zoo)

fcc_imputation_init <- function(dropna=1, ageimpute=1, meanimpute=1) {

	message('Importing data...')
	#this directory and data needs to be created
	background <- read.csv('data/background.csv', stringsAsFactors=F)

	message('Convert to numeric...')
	background_numeric <- data.frame(sapply(background, as.numeric))

	if(ageimpute == 1) {
		source('imputation_logical.R', echo=F)
		background_numeric <- logical_imputation(background_numeric)
	}

	if(dropna == 1) {
		message('Drop missing data...')
		background_numeric[background_numeric < 0] <- NA	
	}

	if(meanimpute ==1) {
		message('Impute means...')
		background_numeric <- na.aggregate(background_numeric)
	}

	source('imputation_regression.R')
	message('Ready!')

	return(background_numeric)

}