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

corMatrix <- function(dataframe, varpattern="",debug=0, test=0, parallel = 0) {

	if (parallel == 1) {
		message('Enabling parallelization')
		if (!requireNamespace("parallel", quietly = TRUE)) {
		  stop("Parallel package not found. Please install it.",
		    call. = FALSE)
		}
	}


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
		
		#get rid of columns with all NAs
		vars_nas <- get_na_vars(reduced_df)
		no_nas <- dplyr::select(reduced_df , -dplyr::one_of(vars_nas))

		#get rid of columns with absolutely no variance
		vars_no_variance <- get_no_variance_vars(no_nas,variance_threshold = 0)
		out_noID <- dplyr::select(no_nas, -challengeID)

		out_numeric <- dplyr::select(out_noID, -dplyr::one_of(vars_no_variance))
		#out_imputed <- zoo::na.aggregate(out_numeric)	#same shape as out_numeric, but with means imputed	
		#out_scaled <- data.frame(lapply(out_imputed, function(x) scale(x)))
		#print(head(out_scaled,20))



		if(test == 1) {
			message('Running in test mode')
			columnstorun <- subset(out_numeric, select=c('cm4hhinc', 'cf4hhinc', 'cm5adult', 'cm1bsex'))
			#columnstorun <- out_numeric[,1:4]
		} else {
			columnstorun <- out_numeric
		}


		getCorMatrix <- function(column, input_df) {
			correlations <- matrix(NA, nrow=ncol(input_df), ncol=1)
			rownames(correlations) <- colnames(input_df)
			#find column index of column names passed into function 
			col <- as.numeric(which(colnames(input_df)==column))

			if(debug>=1) { message(paste("running variable", col, column, "...")) }

			#for each column in input_df (processed in ways described above, not original input input_df)
			for (i in 1:ncol(input_df)) {

				#we will try to impute this 
				var1 <- input_df[,col]
				#the variable we will correlate with variable we are trying to predict
				var2 <- input_df[,i]
				name <- colnames(input_df)[i]
				#for each remaining variable, make sure its not the same as the column we are predicting 
				if (!i == col) {

					#print(column, name)

					#error handling -- make sure correlation test can run, otherwise spit out names of offending variables
					result = tryCatch({
					    cor <- psych::corFiml(cbind(var1, var2))[1,2]
					    correlations[name,1] <- abs(cor)


					}, error = function(e) {
					    #complain only if debug is set to 1, otherwise silently move on to next pair 
					    if(debug>=1) {
					    message(paste(column, "and", name, "don't seem to play well together"))
						}
						correlations[name,1] <- NA
					})
			
				} else {
					correlations[name,1] <- 1
				}
				#end if
			} #end getCorMatrix 	
		
			return(correlations)
		}

		#if parallelization option is set 
		if (parallel == 1) {
				final <- parallel::mclapply(colnames(columnstorun), function(x) getCorMatrix(x, out_numeric), mc.cores=parallel::detectCores(logical=FALSE))
				final <- as.matrix(do.call(cbind, final))
				colnames(final) <- colnames(columnstorun)
				rownames(final) <- colnames(columnstorun)
				object <- list(corMatrix = final, df = columnstorun)
				return(object)
		#run in sequence = off
		} else {
				final <- sapply(colnames(columnstorun), function(x) getCorMatrix(x, out_numeric))
				final <- as.matrix(final)
				object <- list(corMatrix = final, df = columnstorun)
				return(object)
		}



	#if we don't find a data frame, throw an error and quit. Boo! 
	} else {
		stop(paste("Error: function in file", this.file, "expects a data frame"))
	}

} # end regression_imputation function 
