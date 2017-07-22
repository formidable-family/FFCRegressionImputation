#' Linear regression and lasso based imputation
#'
#' Creates a dataframe with imputed values using either linear regression or lasso based models. For each variable in given data frame, the function finds the best correlated predictors (number of which is set by top_predictors), and uses these to construct models for predicting missing values. 
#'
#' @param method method for imputation (\code{"lm"} for ordinary least squares linear regression or \code{"lasso"} for lasso regularization)
#' @param degree the degree of polynomial effects to estimate (1=main effects only, 2=quadratic, 3=cubic, etc.)
#' @param parallel whether to use parallel processes (for MacOSX only at the moment)
#' @param threshold for selection of predictors based on correlation; values between 0 and 1.
#' @param top_predictors how many predictors to use in imputation prediction; more values can lead to better quality but more sparsely available predictions.
#' @param debug debug mode; shows which models are running, the quality of predictions relative to original data, and any model errors. 1=progress, errors and warnings, 2=progress,errors, warnings and prediction quality.
#' @param test test mode; runs on only the first 4 variables; helpful for trying out the function options before running full imputation. 
#'
#' @return Dataframe containing imputed variables, with imputations performed only on missing values and retaining original data where available.
#'
#' @examples
#' \dontrun{regImputation(dataframe, method='polywog', varpattern="^c[mfhpktfvino]{1,2}[12345]", parallel=1, debug=1, test=1)}
#'
#' @export

# Todo: better case logic instead of multiple if statements. 


## Main imputation script

regImputation <- function(dataframe, matrix, method='lm', parallel=0, threshold=0.4,top_predictors=3, debug=0, degree=1, test=1) {

	#avoid loading everything under the sun if we don't need it
	if (method == 'polywog' | method=='lasso') {
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

	
	#just grabs name of this file to be able to display in error messages
	this.file <- (function() utils::getSrcFilename(sys.call(sys.nframe())))()

	#make sure input is a data frame
	if ("data.frame" %in% class(dataframe) & 'matrix' %in% class(matrix)) {

		result = tryCatch({
			if(debug>=1){message('Subsetting dataframe to match correlation matrix...')}
			reduced_df <- dplyr::select(dataframe, dplyr::one_of(colnames(matrix)))
		}, error = function(e) {
			stop("Failed to match correlation matrix variables to input data frame.\nCheck that input data frame has (at least) all the columns specified in the input matrix.\nYou can give this function a dataframe with more variables than specified in the correlation matrix, but not less.")
		})


		if(debug>=1){message('Preparing dataframe for computation [1]...')}
		out_numeric <- data.frame(suppressWarnings(sapply(reduced_df, as.numeric))) #converts to numeric
		if(debug>=1){message('Preparing dataframe for computation [2]...')}
		out_imputed <- zoo::na.aggregate(out_numeric)	#same shape as dataframe, but with means imputed
		if(debug>=1){message('Preparing dataframe for computation [3]...')}
		out_scaled <- data.frame(lapply(out_imputed, function(x) scale(x)))
		#print(head(out_scaled,20))

		impute <- function(column, cors, imputed_df, original_df) {
			if(debug>=1){message(paste('Running variable', column))}

			col <- as.numeric(which(colnames(imputed_df)==column))

			bestpredictors <- data.frame(correlation=cors[,column])
			bestpredictors$names <- rownames(bestpredictors)

			filtered <- suppressMessages(dplyr::filter(bestpredictors, correlation >= threshold))
			filtered <- suppressMessages(dplyr::filter(filtered, correlation < 1))

			if (nrow(filtered) > 0) {

				arranged <- suppressMessages(dplyr::arrange(filtered,desc(correlation)))

				if (nrow(arranged) > top_predictors) {
					top_values <- dplyr::top_n(arranged,top_predictors, correlation)
				} else {
					top_values<- arranged
				}

				#construct formula with best predictors as independent and our variable of interest (column) as dependent
				formula <- paste(column, ' ~ ', paste(top_values$names, collapse=" + "))
				model <- stats::as.formula(formula)
				#print(formula)
				
				#wrap in error handling 
				result = tryCatch({

					#if polywog flag is set
					if (method == 'polywog' | method=='lasso') {
						#print("aa")
						model_fit <- polywog::polywog(model, data=imputed_df, degree = degree)
						if(debug>1) { 
							message(paste("Polywog model fit:", formula))
							print(summary(model_fit))
						}

						prediction <- stats::predict(model_fit, imputed_df , type='response')
						prediction_unscaled <- prediction * sd(original_df[,col], na.rm=TRUE) + mean(original_df[,col], na.rm=TRUE)
						#print(head(prediction_unscaled, 20))
						imputed <- ifelse(is.na(original_df[,col]), prediction_unscaled, original_df[,col])

						if(debug>1) { 
							#print information about our model quality and the model itself 
							message(paste("Prediction quality for model:", formula))
							print(stats::cor.test(original_df[,col],prediction)$estimate)
						}
						#try to return some prediction 
						#return(prediction)
						return(imputed)

					#if lavaan flag is set -- not working for now, pending this feature being integrated in lavaan
					#} else if (method == 'lavaan') {

						#lavaan_fit <- cfa(formula, data=imputed_df, missing='fiml')
						#print(lavaan_fit)
						#prediction <- lavPredict(lavaan_fit, type='lv',newdata=imputed_df, type='yhat')
						#print(head(prediction))
						#try to return some prediction 
						#return(prediction)

					#default to lm - fastest

					} else {
						lm_fit <- stats::lm(model, data=imputed_df)	

						if(debug>1) { 
							message(paste("Lm model fit:", formula))
							print(summary(lm_fit)$r.squared)
						}
						#print(summary(model_fit))	
						#new[,column] <- data.frame(rep(0, nrow(imputed_df)))
						prediction <- stats::predict(lm_fit, imputed_df, type='response')
						prediction_unscaled <- prediction * sd(original_df[,col], na.rm=TRUE) + mean(original_df[,col], na.rm=TRUE)
						imputed <- ifelse(is.na(original_df[,col]), prediction_unscaled, original_df[,col])

						if(debug>1) { 
							#print information about our model quality and the model itself 
							message(paste("Prediction quality for model:", formula))
							print(stats::cor.test(original_df[,col],prediction)$estimate)
						}

						#try to return some prediction 
						#return(prediction)
						return(imputed)
					}


				#if we can't get the model we want to run, throw an error and return column of NAs
				}, error = function(e) {
				    if(debug>=1) { message(paste("Error in model", formula)) } 
				    #prediction <- as.vector(rep(NA,nrow(imputed_df)))
				    unimputed <- as.vector(original_df[,col]) #return original unmodified variable column
				    return(unimputed)
				})

			#if we have no good predictors, also return a column of NAs and throw an error if debug is on			
			} else {
				if(debug>=1) { message(paste("No predictors for variable", column)) } 
				#prediction <- as.vector(rep(NA,nrow(imputed_df)))
				unimputed <- as.vector(original_df[,col]) #return original unmodified variable column
				return(unimputed)
			}

		} # end of impute function

		if(test==1) {
			colstorun <- subset(out_scaled, select=c('cm4hhinc', 'cf4hhinc', 'cm1kids', 'cf3adult'))
		} else {
			colstorun <- out_scaled
		}
		#if parallelization option is set 
		if (parallel == 1) {
				final <- parallel::mclapply(colnames(colstorun), function(x) impute(x, matrix, out_scaled, out_numeric), mc.cores=parallel::detectCores(logical=FALSE))
				if(debug>=1) { message("Assembling predictions...") } 
				final <- data.frame(do.call(cbind, final))
				colnames(final) <- colnames(colstorun)
				return(final)
		#run in sequence = off
		} else {
				final <- sapply(colnames(colstorun), function(x) impute(x, matrix, out_scaled, out_numeric))
				final <- data.frame(final)
				return(final)
		}

	#if we don't find a data frame, throw an error and quit. Boo! 
	} else {
		stop(paste("Error: function in file", this.file, "expects a data frame as first input, and matrix of correlatons of first df variables as second input"))
	}

} # end regression_imputation function 
