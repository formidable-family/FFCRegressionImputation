#' Correlation matrix to be used for regression based imputatoin
#'
#' Creates a dataframe with imputed values using either linear regression or lasso based models. For each variable in given data frame, the function finds the best correlated predictors (number of which is set by top_predictors), and uses these to construct models for predicting missing values. 
#'
#' @param dataframe (Required) An input dataframe you would like a correlation matrix for.
#' @param continuous (Optional) A character vector containing variable names that should be treated as continuous.
#' @param categorical (Optional) A character vector containing variable names that should be treated as categorical.
#' @param varpattern A regular expression for subsetting variable names from input dataframe.
#' @param parallel (deprecated)
#' @param test (deprecated)
#' @param debug Debug mode; shows which models are running, the quality of predictions relative to original data, and any model errors. 1=progress notifications, errors and warnings.
#'
#' @return A correlation matrix between all variables given in the input dataframe after removing columns with no variance and applying any regex filters given.
#'
#' @examples
#' \dontrun{corMatrix(dataframe, varpattern="^c[mfhpktfvino]{1,2}[12345]", debug=1)}
#'
#' @export


## Main imputation script

corMatrix <- function(dataframe='', continuous='', categorical='', varpattern="",debug=0, test=0, parallel=0) {

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
	  variance_info <- vapply(data, function(x) stats::var(suppressWarnings(as.numeric(x)), na.rm = TRUE), 
	                          numeric(1))
	  names(variance_info[which(variance_info <= variance_threshold)])
	}


	if (test ==1) {
		message('Test mode is now deprecated because of significantly improved run-time. Operations will be performed on entire input dataframe.')
	}

	if (parallel ==1) {
		message('Parallel mode is now deprecated because the code now performs matrix multiplications, significantly improving runtime.')
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

		if(debug>=1){message('Preparing dataframe for computation...')}

		if (length(continuous) > 1 & length(categorical) > 1) {

			message("Found categorical and continuous lists of variables! Attempting to use this information... ")

			out_numeric <- data.frame(suppressWarnings(sapply(out_novar, as.numeric)))

			categoricalvars <- suppressWarnings(dplyr::select(out_numeric, dplyr::one_of(categorical)))
			continuousvars <- suppressWarnings(dplyr::select(out_numeric, dplyr::one_of(continuous)))

			getmode <- function(input) { #from tutorialspoint :D 
			   uniqv <- unique(input)
			   uniqv[which.max(tabulate(match(input, uniqv)))]
			}

			categorical_imputed <- zoo::na.aggregate(categoricalvars, FUN=getmode)
			continuous_imputed <- zoo::na.aggregate(continuousvars, FUN=mean)


			modes <- apply(categorical_imputed, 2, function(x) getmode(x))
			categorical_deviations <- sweep(categorical_imputed,2, modes, FUN="-")

			continuous_deviations <- data.frame(sapply(continuous_imputed, function(x) scale(x, scale = FALSE)))

			deviations <- cbind(continuous_deviations, categorical_deviations)

		} else {

			message("No continuous and categorical lists found. Defaulting to treating all input variables as continuous... ")

			out_numeric <- data.frame(suppressWarnings(sapply(out_novar, as.numeric)))
			out_imputed <- zoo::na.aggregate(out_numeric)	#same shape as out_numeric, but with means imputed	

			deviations <-  data.frame(sapply(out_imputed, function(x) scale(x, scale = FALSE)))

		}

		deviations <- as.matrix(deviations)

		if(debug>=1){message('Calculating correlation matrix...')}

		product <- Matrix::crossprod(deviations)

		x2 <- deviations**2
		x2sum <- as.matrix(colSums(x2))
		y2sum <- t(x2sum)


		product2 <- x2sum%*%y2sum

		sqrt <- sqrt(product2)
		output <- product/sqrt

		if(debug>=1){message('Done!')}

		return(round(output,3))


	#if we don't find a data frame, throw an error and quit. Boo! 
	} else {
		stop(paste("Error: function in file", this.file, "expects a data frame"))
	}

} # end regression_imputation function 
