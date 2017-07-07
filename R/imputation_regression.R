#' Linear regression and lasso based imputation
#'
#' Creates a dataframe with imputed values using either linear regression or lasso based models. For each variable in given data frame, the function finds the best correlated predictors (number of which is set by top_predictors), and uses these to construct models for predicting missing values. 
#'
#' @param method method for imputation (\code{"lm"} or \code{"polywog"})
#' @param parallel whether to use parallel processes
#' @param threshold for selection of predictors based on correlation; values between 0 and 1.
#' @param top_predictors how many predictors to use in imputation prediction; more values can lead to better quality but more sparsely available predictions.
#' @param constructed whether to return only constructed variables, or any variables passed to function; warning: slow if running on full dataset.
#' @param debug debug mode; shows which models are running, the quality of predictions relative to original data, and any model errors.
#' @param test test mode; runs on only the first 4 variables; helpful for trying out the function options before running full imputation. 
#'
#' @return Dataframe containing imputed variables, with imputations performed only on missing values and retaining original data where available.
#'
#' @examples
#' \dontrun{regression_imputation(dataframe, method='polywog', parallel=1, debug=1, test=1)}
#'
#' @export

# Todo: better case logic instead of multiple if statements. 


## Main imputation script

regression_imputation <- function(dataframe, method='lm', parallel=0, threshold=0.4,top_predictors=3, constructed=1,debug=0, test=0) {

	#avoid loading everything under the sun if we don't need it
	#if (method == 'lavaan') {
	#	library(lavaan)
	#} else 
	if (method == 'polywog') {
		message('Using polywog...')
		if (!requireNamespace("polywog", quietly = TRUE)) {
		  stop("Polywog package not found. Please install it.",
		    call. = FALSE)
		}
	} else {
		message('Using lm...')
	}

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

	#just grabs name of this file to be able to display in error messages
	this.file <- (function() utils::getSrcFilename(sys.call(sys.nframe())))()

	#make sure input is a data frame
	if ("data.frame" %in% class(dataframe)) {

		#check if only constructed variables are requested (default: yes)
		if (constructed == 1) {

			variables <- colnames(dataframe)
			reduced_df <- data.frame(dataframe[which(stringr::str_detect(variables, "^c[mfhpktfvino]{1,2}[12345]"))])

			#stop script if no matching columns found 
			if (ncol(reduced_df) < 1) {
				stop(paste("Error: you asked for constructed variables, but no matching constructed variables found.",
					"Try setting constructed flag to 0, or feed a different data frame.",
					" Error in file:", this.file))
				
			}

		} else {

			reduced_df <- dataframe
		}
		
		#get rid of columns with all NAs
		vars_nas <- get_na_vars(reduced_df)
		no_nas <- dplyr::select(reduced_df , -one_of(vars_nas))

		#get rid of columns with absolutely no variance
		vars_no_variance <- get_no_variance_vars(no_nas,variance_threshold = 0)
		out_numeric <- dplyr::select(no_nas, -one_of(vars_no_variance))		

		impute <- function(column, input_df) {

			#set initial empty data frame for use in loop
			bestpredictors <- data.frame()
			#find column index of column names passed into function 
			col <- as.numeric(which(colnames(input_df)==column))

			if(debug==1) { message(paste("running variable", col, column, "...")) }

			#for each column in input_df (processed in ways described above, not original input input_df)
			for (i in 1:ncol(input_df)) {

				#we will try to impute this 
				var1 <- input_df[,col]

				#for each remaining variable, make sure its not the same as the column we are predicting 
				if (!i == col) {
					#the variable we will correlate with variable we are trying to predict
					var2 <- input_df[,i]
					name <- colnames(input_df)[i]
					#print(column, name)

					#error handling -- make sure correlation test can run, otherwise spit out names of offending variables
					result = tryCatch({
					    cor <- psych::corFiml(cbind(var1, var2))[1,2]
					    if(abs(cor) >= threshold) {
					    	#write to outcome data frame if correlation is above set threshold (default = 0.4)
					    	bestpredictors <- rbind(bestpredictors, data.frame(names=name, correlation=abs(cor)))
					    }

					}, error = function(e) {
					    #complain only if debug is set to 1, otherwise silently move on to next pair 
					    if(debug==1) {
					    message(paste(column, "and", name, "don't seem to play well together"))
						}
					})
			
				} #end if  
				#print(head(bestpredictors))
			} #end for 

			#if predictors data frame is not empty (ie we actually have correlations matching threshold)
			#pick the top as set in preferences (default = 3)

			if (nrow(bestpredictors) > 0) {
				arranged <- suppressMessages(dplyr::arrange(bestpredictors,desc(correlation)))
				top <- suppressMessages(top_n(arranged,top_predictors))
					

				#construct formula with best predictors as independent and our variable of interest (column) as dependent
				formula <- paste(column, ' ~ ', paste(top$names, collapse=" + "))
				model <- stats::as.formula(formula)
				#print(formula)
				
				#wrap in error handling 
				result = tryCatch({

					#if polywog flag is set
					if (method == 'polywog') {
						model_fit <- polywog::polywog(model, data=input_df, degree = 1)
						prediction <- stats::predict(model_fit, input_df, type='response')
						imputed <- ifelse(is.na(input_df[,col]), prediction, input_df[,col])
						#try to return some prediction 
						#return(prediction)
						return(imputed)

					#if lavaan flag is set -- not working for now, pending this feature being integrated in lavaan
					#} else if (method == 'lavaan') {

						#lavaan_fit <- cfa(formula, data=input_df, missing='fiml')
						#print(lavaan_fit)
						#prediction <- lavPredict(lavaan_fit, type='lv',newdata=input_df, type='yhat')
						#print(head(prediction))
						#try to return some prediction 
						#return(prediction)

					#default to lm - fastest
					} else {
						lm_fit <- stats::lm(model, data=input_df)	
						#print(summary(model_fit))	
						#new[,column] <- data.frame(rep(0, nrow(input_df)))
						prediction <- stats::predict(lm_fit, input_df, type='response')
						imputed <- ifelse(is.na(input_df[,col]), prediction, input_df[,col])

						#try to return some prediction 
						#return(prediction)
						return(imputed)
					}


					if(debug==1) { 
						#print information about our model quality and the model itself 
						message(paste("Prediction quality for model:"), formula)
						message(stats::cor.test(input_df[,col],prediction))
					}



				#if we can't get the model we want to run, throw an error and return column of NAs
				}, error = function(e) {
				    if(debug==1) { message(paste("Error in model", formula)) } 
				    #prediction <- as.vector(rep(NA,nrow(input_df)))
				    unimputed <- as.vector(input_df[,col]) #return original unmodified variable column
				    return(unimputed)
				})

			#if we have no good predictors, also return a column of NAs and throw an error if debug is on			
			} else {
				if(debug==1) { message(paste("No predictors for variable", column)) } 
				#prediction <- as.vector(rep(NA,nrow(input_df)))
				unimputed <- as.vector(input_df[,col]) #return original unmodified variable column
				return(unimputed)
			}

		} # end of impute function

		if(test == 1) {
			message('Running in test mode')
			#columnstorun <- subset(out_numeric, select=c('cm4hhinc', 'cf4hhinc', 'cm5adult', 'cm1bsex'))
			columnstorun <- out_numeric[,1:4]
		} else {
			columnstorun <- out_numeric
		}

		#if parallelization option is set 
		if (parallel == 1) {
				final <- parallel::mclapply(colnames(columnstorun), function(x) impute(x, out_numeric), mc.cores=parallel::detectCores())
				final <- data.frame(do.call(cbind, final))
				colnames(final) <- colnames(columnstorun)
				merged <- cbind(challengeID=input_df$challengeID, final)
				return(merged)
		#run in sequence = off
		} else {
				final <- sapply(colnames(columnstorun), function(x) impute(x, out_numeric))
				final <- data.frame(final)
				merged <- cbind(challengeID=input_df$challengeID, final)
				return(merged)
		}

	#if we don't find a data frame, throw an error and quit. Boo! 
	} else {
		stop(paste("Error: function in file", this.file, "expects a data frame"))
	}

} # end regression_imputation function 
