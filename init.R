#Data cleaning for logical and regression imputation

library(zoo)

imputation_ffc_init <- function(data='',dropna=1, ageimpute=1, meanimpute=1) {

	message('Importing data...')
	#this directory and data needs to be created
	result = tryCatch({
		background <- read.csv(data, stringsAsFactors=F)

	}, error = function(e) {
	    stop(message("Please enter the location of your background.csv data file!"))
	})


	message('Convert to numeric...')
	background_numeric <- data.frame(sapply(background, as.numeric))

	if(ageimpute == 1) {
		message('Run logical imputation...')
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

	return(background_numeric)

	message('Ready!')

}