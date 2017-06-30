# Code by Anna Filippova 
# Awesome stats by Antje Kirchner 

# Note: if you want to use lm and polywog, it helps to mean impute on the data set first. 
# There are other functions in this repo that handle this, 
# since this is slow it is not done here automagically. 

# Todo: better case logic instead of multiple if statements. 

#common dependencies
library(psych)
library(dplyr)
require(stringr)
require(tidyverse)

##
## Two helper functions by @ccgilroy. Todo: repend on the rest of his code. 
## 
## drop variables with too many NAs
get_na_vars <- function(data, non_na_responses = 0) {
  na_info <- vapply(data, function(x) length(which(is.na(x))), numeric(1))
  names(na_info[which(na_info >= max(na_info) - non_na_responses)])
}

## remove zero-variance variables
get_no_variance_vars <- function(data, variance_threshold = 0) {
  variance_info <- vapply(data, function(x) var(as.numeric(x), na.rm = TRUE), 
                          numeric(1))
  names(variance_info[which(variance_info <= variance_threshold)])
}

## Main imputation script

regression_imputation <- function(dataframe, method='lm', parallel=0, threshold=0.4,top_predictors=3, constructed=1,debug=0, test=0) {

	#depends on Connor's highly modularized helper code 
	source('imputation_helpers.R')

	#avoid loading everything under the sun if we don't need it
	if (method == 'lavaan') {
		library(lavaan)
	} else if (method == 'polywog') {
		library(polywog)
	}

	if (parallel == 1) {
		library(parallel)
		library(doMC)
	}

	#just grabs name of this file to be able to display in error messages
	this.file <- (function() getSrcFilename(sys.call(sys.nframe())))()

	#make sure input is a data frame
	if ("data.frame" %in% class(dataframe)) {

		#check if only constructed variables are requested (default: yes)
		if (constructed == 1) {

			variables <- colnames(dataframe)
			reduced_df <- data.frame(dataframe[which(str_detect(variables, "^c[mfhpktfvino]{1,2}[12345]"))])

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
		no_nas <- reduced_df %>% 
					select(-one_of(vars_nas))

		#get rid of columns with absolutely no variance
		vars_no_variance <- get_no_variance_vars(no_nas,variance_threshold = 0)
		out_numeric <- no_nas %>%
					select(-one_of(vars_no_variance))
		

		impute <- function(column, df) {

			#set initial empty data frame for use in loop
			bestpredictors <- data.frame()
			#find column index of column names passed into function 
			col <- as.numeric(which(colnames(df)==column))

			message(paste("running variable", col, column, "..."))

			#for each column in df (processed in ways described above, not original input df)
			for (i in 1:ncol(df)) {

				#we will try to impute this 
				var1 <- df[,col]

				#for each remaining variable, make sure its not the same as the column we are predicting 
				if (!i == col) {
					#the variable we will correlate with variable we are trying to predict
					var2 <- df[,i]
					name <- colnames(df)[i]
					#print(column, name)

					#error handling -- make sure correlation test can run, otherwise spit out names of offending variables
					result = tryCatch({
					    cor <- corFiml(cbind(var1, var2))[1,2]
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
				top <- bestpredictors %>%
					arrange(desc(correlation)) %>%
					top_n(top_predictors)

				#construct formula with best predictors as independent and our variable of interest (column) as dependent
				formula <- paste(column, ' ~ ', paste(top$names, collapse=" + "))
				model <- as.formula(formula)
				#print(formula)
				
				#wrap in error handling 
				result = tryCatch({

					#if polywog flag is set
					if (method == 'polywog') {
						print('using polywog...')
						model_fit <- polywog(model, data=df, degree = 1)
						prediction <- predict(model_fit, df, type='response')
						#try to return some prediction 
						return(prediction)

					#if lavaan flag is set -- not working for now, pending this feature being integrated in lavaan
					#} else if (method == 'lavaan') {

						#lavaan_fit <- cfa(formula, data=df, missing='fiml')
						#print(lavaan_fit)
						#prediction <- lavPredict(lavaan_fit, type='lv',newdata=df, type='yhat')
						#print(head(prediction))
						#try to return some prediction 
						#return(prediction)

					#default to lm - fastest
					} else {
						print('using lm...')
						lm_fit <- lm(model, data=df)	
						#print(summary(model_fit))	
						#new[,column] <- data.frame(rep(0, nrow(df)))
						prediction <- predict(lm_fit, df, type='response')
						#try to return some prediction 
						return(prediction)
					}


					if(debug==1) { 
						#print information about our model quality and the model itself 
						message(paste("Prediction quality for model:"), formula)
						message(cor.test(df[,col],prediction))
					} 

				#if we can't get the model we want to run, throw an error and return column of NAs
				}, error = function(e) {
				    if(debug==1) { message(paste("Error in model", formula)) } 
				    prediction <- as.vector(rep(NA,nrow(df)))
				    return(prediction)
				})

			#if we have no good predictors, also return a column of NAs and throw an error if debug is on			
			} else {
				if(debug==1) { message(paste("No predictors for variable", column)) } 
				prediction <- as.vector(rep(NA,nrow(df)))
				return(prediction)
			}

		} # end of impute function

		if(test == 1) {
			columnstorun <- out_numeric[,1:4]
		} else {
			columnstorun <- out_numeric
		}

		#if parallelization option is set 
		if (parallel == 1) {
				final <- mclapply(colnames(columnstorun), function(x) impute(x, out_numeric), mc.cores=parallel::detectCores())
				final <- data.frame(do.call(cbind, final))
				colnames(final) <- colnames(columnstorun)
				return(final)
		#run in sequence = off
		} else {
				final <- sapply(colnames(columnstorun), function(x) impute(x, out_numeric))
				final <- data.frame(do.call(cbind, final))
				return(final)
		}

	#if we don't find a data frame, throw an error and quit. Boo! 
	} else {
		stop(paste("Error: function in file", this.file, "expects a data frame"))
	}

} # end regression_imputation function 
