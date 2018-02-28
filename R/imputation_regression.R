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
#' @param failmode what to do if prediction fails for any reason. Defaults to returning the original variable vector (failmode='skip'), but can be told to impute central tendency instead with option (failmode='impute')
#' @param ncores number of cores for when running in parallel. Defaults to all available cores.
#'
#' @return Dataframe containing imputed variables, with imputations performed only on missing values and retaining original data where available.
#'
#' @examples
#' \dontrun{regImputation(dataframe, matrix, method='polywog', parallel=1, debug=1, test=1)}
#'
#' @export


## Main imputation script

regImputation <- function(dataframe, matrix, continuous='', categorical='', method='lm', parallel=0, threshold=0.4,top_predictors=3, debug=0, degree=1, test=0, failmode='skip', ncores=parallel::detectCores(logical=FALSE)) {

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


	if (length(continuous) > 1 & length(categorical) > 1) {
		if (!requireNamespace("nnet", quietly = TRUE)) {
		  stop("Nnet package not found. Please install it if you wish to treat continuous and categorical variables differently.",
		    call. = FALSE)
		}

		if (method == 'polywog' | method=='lasso') {
			message('Warning: You selected the lasso option and gave the function a list of continous and categorical variables, which is currently only supported by lm: the lists will not be used.')
		}
	}


	if (test == 1) {
		message('Running in test mode with only 4 variables!')
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



		if (length(continuous) > 1 & length(categorical) > 1) {

			message("Found categorical and continuous lists of variables! Attempting to use this information... ")
			categoricalvars <- suppressWarnings(dplyr::select(reduced_df, dplyr::one_of(categorical)))
			continuousvars <- suppressWarnings(dplyr::select(reduced_df, dplyr::one_of(continuous)))

			if(debug>=1){message('Preparing dataframe for computation [1]...')}
			categorical_numeric <- data.frame(suppressWarnings(sapply(categoricalvars, as.numeric)))
			continuous_numeric <- data.frame(suppressWarnings(sapply(continuousvars, as.numeric)))

			out_numeric <- cbind(categorical_numeric , continuous_numeric)

			if(debug>=1){message('Preparing dataframe for computation [2]...')}

			getmode <- function(input) { #from tutorialspoint :D
			   uniqv <- unique(input)
			   uniqv[which.max(tabulate(match(input, uniqv)))]
			}

			#turn categorical into factor levels
			categorical_imputed <- zoo::na.aggregate(categorical_numeric, FUN=getmode)
			categorical_imputed <- Map(as.factor, categorical_imputed)

			#print(sapply(categorical_imputed,class))

			continuous_imputed <- zoo::na.aggregate(continuous_numeric)

			out_imputed <- cbind(categorical_imputed, continuous_imputed)
			#saveRDS(out_imputed, 'out_imputed.rds')

			if(debug>=1){message('Preparing dataframe for computation [3]...')}
			#we scale only continuous variables
			continuous_scaled <- data.frame(sapply(continuous_imputed, function(x) scale(x)))

			out_scaled <- cbind(categorical_imputed, continuous_scaled)

		} else {

			if(debug>=1){message('Preparing dataframe for computation [1]...')}
			out_numeric <- data.frame(suppressWarnings(sapply(reduced_df, as.numeric))) #converts to numeric
			if(debug>=1){message('Preparing dataframe for computation [2]...')}

			out_imputed <- zoo::na.aggregate(out_numeric)	#same shape as dataframe, but with means imputed

			if(debug>=1){message('Preparing dataframe for computation [3]...')}
			out_scaled <- data.frame(sapply(out_imputed, function(x) scale(x)))

		}


		#print(head(out_scaled,20))

		impute <- function(column, cors, imputed_df, original_df) {

			if(debug>=1){message(paste('Running variable', column))}

			col <- as.numeric(which(colnames(imputed_df)==column))

			bestpredictors <- data.frame(correlation=cors[,column])
			bestpredictors$names <- rownames(bestpredictors)

			filtered <- suppressMessages(dplyr::filter(bestpredictors, correlation >= threshold))
			filtered <- suppressMessages(dplyr::filter(filtered, correlation < 1))
			filtered2 <- suppressMessages(dplyr::filter(filtered, names != column))

			if (nrow(filtered2) > 0) {

				arranged <- suppressMessages(dplyr::arrange(filtered2,desc(correlation)))

				if (nrow(arranged) >= top_predictors) {
					top_values <- dplyr::top_n(arranged,top_predictors, correlation)
				} else {
					top_values<- arranged
				}

				#construct formula with best predictors as independent and our variable of interest (column) as dependent
				formula <- paste(column, ' ~ ', paste(top_values$names, collapse=" + "))
				model <- stats::as.formula(formula)
				#print(formula)

				#print(top_values$names)

				#wrap in error handling
				result = tryCatch({

					#if polywog flag is set
					if (method == 'polywog' | method=='lasso') {
						#print("aa")

						if (column %in% categorical) {

							#print('selecting...')

							temp_df <- imputed_df
							temp_df[,col] <- ifelse(is.na(original_df[,col]), NA, imputed_df[,col])

							df.train<-temp_df[!is.na(temp_df[,col]),]

							x <- dplyr::select(df.train, dplyr::one_of(top_values$names))
							#print(class(x))
							x <- dummies::dummy.data.frame(x,sep=".")
							print("Running multinomial regression")

							x <- as.matrix(x)
							y <- df.train[,col]

							#print("fiting...")

							model_fit <- glmnet::cv.glmnet(x, y, family="multinomial", alpha = 1, parallel=F)
							newx <- dplyr::select(imputed_df, dplyr::one_of(top_values$names))
							newx <- dummies::dummy.data.frame(newx,sep=".")
							newx <- as.matrix(newx)

							if(debug>1) {
								message(paste("Glmnet lambda value used", formula))
								print(model_fit$lambda.min)
							}

							#print("predicting...")
							#print(head(newx))
							#print(head(x))
							prediction <- predict(model_fit, newx=newx, s='lambda.min',type="class")
							#print("predicting...")
							#print(head(prediction, 100))

							imputed <- ifelse(is.na(original_df[,col]), prediction, original_df[,col])
							return(imputed)

						} else {
							print("Using polywog")


							model_fit <- polywog::polywog(model, data=imputed_df, degree = degree)
							if(debug>1) {
								message(paste("Polywog lambda value used", formula))
								print(model_fit$lambda)
							}

							print("test")
							prediction <- stats::predict(model_fit, imputed_df , type='response')
							prediction_unscaled <- prediction * stats::sd(original_df[,col], na.rm=TRUE) + mean(original_df[,col], na.rm=TRUE)
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

						}

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

						temp_df <- imputed_df
						temp_df[,col] <- ifelse(is.na(original_df[,col]), NA, imputed_df[,col])

						df.train<-temp_df[!is.na(temp_df[,col]),]
						#df.test<-temp_df[is.na(temp_df[,col]),]
						#df.test <- df.test[-col]
						#print(nrow(df.train))
						#print(nrow(df.test))

						#if (column %in% colnames(df.test)){
						#	print("exists!")
						#}

						if (column %in% categorical) {
							#print("Running multinomial regression")
							lm_fit <- nnet::multinom(model, data=df.train)
						} else {
							#print("Running linear regression")
							lm_fit <- stats::lm(model, data=df.train)
						}

						if(debug>1) {
							if (column %in% categorical) {
								message(paste("Model fit (AIC):", formula))
								print(lm_fit$AIC)
							} else {
								message(paste("Model fit (R^2):", formula))
								print(summary(lm_fit)$r.squared)
							}
						}

						if (column %in% categorical) {
							prediction <- predict(lm_fit, temp_df, type='c')

							if(debug>1) {
								#print information about our model quality and the model itself
								message(paste("Prediction quality for model:", formula))
								print(stats::cor.test(original_df[,col],as.numeric(prediction))$estimate)
							}

							imputed <- ifelse(is.na(original_df[,col]), prediction, original_df[,col])
							#print(head(imputed))
							#try to return some prediction
							return(imputed)

						} else {
							prediction <- suppressWarnings(stats::predict(lm_fit, temp_df, type='response'))
							prediction_unscaled <- prediction * stats::sd(original_df[,col], na.rm=TRUE) + mean(original_df[,col], na.rm=TRUE)
							imputed <- ifelse(is.na(original_df[,col]), prediction_unscaled, original_df[,col])
							#print(head(imputed))

							if(debug>1) {
								#print information about our model quality and the model itself
								message(paste("Prediction quality for model:", formula))
								print(stats::cor.test(original_df[,col],prediction_unscaled)$estimate)
							}

							#try to return some prediction
							return(imputed)
						}

					}


				#if we can't get the model we want to run, throw an error and return column of NAs
				}, error = function(e) {
				    if(debug>=1) {
				    	message(paste("Error in model", formula))
					}

					if (failmode == "impute") {

						if (column %in% categorical) {
							prediction_unscaled <- imputed_df[,col]

						} else {

							prediction <- imputed_df[,col]
							prediction_unscaled <- prediction * stats::sd(original_df[,col], na.rm=TRUE) + mean(original_df[,col], na.rm=TRUE)

						}

						imputed <- ifelse(is.na(original_df[,col]), prediction_unscaled, original_df[,col]) #return imputation with median or mode
						#print(imputed)
						return(imputed)

					} else {

						unimputed <- original_df[,col]
						#print(unimputed)
						return(unimputed)

					}
				})



			#if we have no good predictors, also return a column of NAs and throw an error if debug is on
			} else {
				if(debug>=1) { message(paste("No predictors for variable", column)) }

				if (failmode == "impute") {

					if (column %in% categorical) {
						prediction_unscaled <- imputed_df[,col]

					} else {

						prediction <- imputed_df[,col]
						prediction_unscaled <- prediction * stats::sd(original_df[,col], na.rm=TRUE) + mean(original_df[,col], na.rm=TRUE)

					}

					imputed <- ifelse(is.na(original_df[,col]), prediction_unscaled, original_df[,col]) #return imputation with median or mode
					#print(imputed)

					return(imputed)

				} else {

					unimputed <- original_df[,col]
					#print(imputed)
					return(unimputed)

				}


			}
		} # end of impute function

		if(test==1) {
			colstorun <- subset(out_scaled, select=c('cm4hhinc', 'cf4hhimp', 'cm5md_case_con', 'cf3adult'))
		} else {
			colstorun <- out_scaled
		}
		#if parallelization option is set
		if (parallel == 1) {
				final <- parallel::mclapply(colnames(colstorun), function(x) impute(x, matrix, out_scaled, out_numeric), mc.cores=ncores)
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
