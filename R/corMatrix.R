#' Correlation matrix to be used for regression based imputatoin
#'
#' Creates a dataframe with imputed values using either linear regression or lasso based models. For each variable in given data frame, the function finds the best correlated predictors (number of which is set by top_predictors), and uses these to construct models for predicting missing values. 
#'
#' @param varpattern A regular expression for subsetting variable names from input dataframe 
#' @param parallel whether to use parallel processes (MacOS only, cores autodetected)
#' @param debug debug mode; shows which models are running, the quality of predictions relative to original data, and any model errors. 1=progress notifications, errors and warnings.
#' @param test test mode; runs on only the first 4 variables of a given dataframe; helpful for trying out the function options before running full correlation matrix.  
#'
#' @return a list of two objects: $corMatrix == the correlation matrix between all variables given in the input dataframe, $df ==  a filtered version of the dataframe, that removes columns with no variance and applies any regex filters given.
#'
#' @examples
#' \dontrun{corMatrix(dataframe, varpattern="^c[mfhpktfvino]{1,2}[12345]", parallel=1, debug=1, test=1)}
#'
#' @export

# Todo: better case logic instead of multiple if statements. 


## Main imputation script

corMatrix <- function(dataframe, varpattern="",debug=0) {

	##
	## Two helper functions by @ccgilroy. Todo: depend on the rest of his code. 
	## 
	## drop variables with too many NAs
	get_na_vars <- function(data, non_na_responses = 0) {
	  na_info <- vapply(data, function(x) length(which(is.na(x))), numeric(1))
	  names(na_info[which(na_info >= max(na_info) - non_na_responses)])
	}

	## remove zero-variance variables
	get_no_variance_vars <- function(data, variance_threshold = 0) {
	  variance_info <- vapply(data, function(x) stats::var(as.numeric(x), na.rm = TRUE), 
	                          numeric(1))
	  names(variance_info[which(variance_info <= variance_threshold)])
	}

	#make sure input is a data frame
	if ("data.frame" %in% class(dataframe)) {

		#check if only constructed variables are requested (default: yes)
		#varpattern <- "^c[mfhpktfvino]{1,2}[12345]" #for testing

		if (stringr::str_length(varpattern) > 0) {

			variables <- colnames(dataframe)
			reduced_df <- data.frame(dataframe[which(stringr::str_detect(variables, varpattern))])

			#stop script if no matching columns found 
			if (ncol(reduced_df) < 1) {
				stop(paste("corMatrix error: you asked for a selection of variables, but no matching variables were found.",
					"Try changing the regex used, or supply a different dataframe."))
				
			}

		} else {

			reduced_df <- dataframe
		}

		if ("challengeID" %in% colnames(reduced_df)) {
			reduced_df <- dplyr::select(reduced_df, -challengeID)
		}

		#get rid of columns with all NAs
		vars_nas <- get_na_vars(reduced_df)
		no_nas <- dplyr::select(reduced_df , -dplyr::one_of(vars_nas))

		#get rid of columns with absolutely no variance
		vars_no_variance <- get_no_variance_vars(no_nas,variance_threshold = 0)

		out_novar <- dplyr::select(no_nas, -dplyr::one_of(vars_no_variance))
		out_numeric <- data.frame(sapply(out_novar, as.numeric))
		out_imputed <- zoo::na.aggregate(out_numeric)	#same shape as out_numeric, but with means imputed	

		deviations <-  data.frame(sapply(out_imputed, function(x) scale(x, scale = FALSE)))
		deviations <- as.matrix(deviations)
		product <- Matrix::crossprod(deviations)

		x2 <- deviations**2
		x2sum <- as.matrix(colSums(x2))
		y2sum <- t(x2sum)

		product2 <- x2sum%*%y2sum

		sqrt <- sqrt(product2)
		output <- product/sqrt

		return(output)


	#if we don't find a data frame, throw an error and quit. Boo! 
	} else {
		stop(paste("Error: function in file", this.file, "expects a data frame"))
	}

} # end regression_imputation function 
