#' Data cleaning for logical and regression imputation
#'
#' Creates a dataframe with imputed values from 
#'
#' @param data location of data file
#' @param dropna whether to convert missing values to NA
#' @param ageimpute perform logical imputation of mothers and fathers age across waves
#' @param meanimpute perform a mean imputation to help get better results from regression imputation
#'
#' @return Dataframe with same dimensions as input, entirely numeric
#'
#' @examples
#' \dontrun{imputation_ffc_init('background.csv', dropna=1, ageimpute=1, meanimpute=1)}
#'
#' @export

imputation_ffc_init <- function(data='',dropna=1, ageimpute=1, meanimpute=1) {

	message('Importing data...')
	#this directory and data needs to be created
	result = tryCatch({
		background <- utils::read.csv(data, stringsAsFactors=F)

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
		background_numeric <- zoo::na.aggregate(background_numeric)
	}

	source('imputation_regression.R')

	return(background_numeric)

	message('Ready!')

}