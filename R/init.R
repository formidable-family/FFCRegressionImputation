#' Data cleaning for logical and regression imputation
#'
#' Creates a dataframe with imputed values from 
#'
#' @param data location of data file
#' @param dropna whether to convert missing values to NA
#' @param ageimpute perform logical imputation of mothers and fathers age across waves
#' @param meanimpute convert all input columns to numeric and perform a mean imputation to help get better results from regression imputation; default off because regression imputation will perform this automatically. 
#'
#' @return Dataframe with same dimensions as input, entirely numeric
#'
#' @examples
#' \dontrun{initImputation('background.csv', dropna=1, ageimpute=1, meanimpute=1)}
#'
#' @export

initImputation <- function(data='',dropna=1, ageimpute=1, meanimpute=0) {

	message('Importing data...')
	#this directory and data needs to be created
	result = tryCatch({
		background <- utils::read.csv(data, stringsAsFactors=F)

	}, error = function(e) {
	    stop(message("Please enter the location of your background.csv data file!"))
	})


	logical_imputation <- function(input_df) {

		#just grabs name of this file to be able to display in error messages
		this.file <- (function() utils::getSrcFilename(sys.call(sys.nframe())))()

		#check that input is a input_df
		if ("data.frame" %in% class(input_df)) {
			
			variables <- colnames(input_df)

			#these are actually constructed, but don't start with c, so renaming them
			input_df$co5oint <- input_df$o5oint
			input_df$ct5int <- input_df$t5tint
			input_df$cn5d2_age <- input_df$n5d2_age

			print("Generating refusalcount, dontknowcount, nacount...")

			input_df$refusalcount <- rowSums(input_df==-1, na.rm=TRUE)
			input_df$dontknowcount <- rowSums(input_df==-2, na.rm=TRUE)
			input_df$nacount <- input_df$refusalcount + input_df$dontknowcount

			#find just constructed age variables
			reduced_df <- data.frame(input_df[which(stringr::str_detect(variables, "^c[mf]{1}[12345]age$"))])

			if (ncol(reduced_df) < 1) {
				#break script if no matching columns found 
				stop(paste("Error: no matching constructed variables found. Are you feeding this a full dataset from background file?", this.file))
				
			}

			reduced_df$challengeID <- input_df$challengeID
			#reduced_df$ID <- seq.int(nrow(reduced_df))
			print("Running logical age imputation ... ")

			#transform all values below 0 to NA, only in reduced data frame 
			reduced_df[reduced_df < 0] <- NA 

			#print(class(reduced_df$cf2age))

			reduced_df$cf1age <- ifelse(reduced_df$cf1age < 10, NA, reduced_df$cf1age)
			reduced_df$cf2age <- ifelse(reduced_df$cf2age < 10, NA, reduced_df$cf2age)
			reduced_df$cf3age <- ifelse(reduced_df$cf3age < 10, NA, reduced_df$cf3age)
			reduced_df$cf4age <- ifelse(reduced_df$cf4age < 10, NA, reduced_df$cf4age)
			reduced_df$cf5age <- ifelse(reduced_df$cf5age < 10, NA, reduced_df$cf5age)

			reduced_df$cm1age <- ifelse(reduced_df$cm1age < 10, NA, reduced_df$cm1age)
			reduced_df$cm2age <- ifelse(reduced_df$cm2age < 10, NA, reduced_df$cm2age)
			reduced_df$cm3age <- ifelse(reduced_df$cm3age < 10, NA, reduced_df$cm3age)
			reduced_df$cm4age <- ifelse(reduced_df$cm4age < 10, NA, reduced_df$cm4age)
			reduced_df$cm5age <- ifelse(reduced_df$cm5age < 10, NA, reduced_df$cm5age)

			#calculate mean age difference between mum and dad, and use this predict information where none at all is available for dad
			agedifference <- mean(reduced_df$cf1age - reduced_df$cm1age, na.rm=TRUE)

			#function to impute fathers age if we know any age of mother at any time but don't know anything about father's age across any time 
			fixage <- function(flag, cf1age, cf2age, cf3age, cf4age, cf5age, cm1age, cm2age,cm3age,cm4age,cm5age) {
				if (is.na(cf1age) & is.na(cf2age) & is.na(cf3age) & is.na(cf4age) & is.na(cf5age)) {
					if (!is.na(cm1age)) {
						cf1age <- cm1age + agedifference
						cf2age <- cm1age + agedifference + 1
						cf3age <- cm1age + agedifference + 3
						cf4age <- cm1age + agedifference + 5
						cf5age <- cm1age + agedifference + 9
					} else if (!is.na(cm2age)) {
						cf1age <- cm2age + agedifference - 1
						cf2age <- cm2age + agedifference
						cf3age <- cm2age + agedifference + 2
						cf4age <- cm2age + agedifference + 4
						cf5age <- cm2age + agedifference + 8
					} else if (!is.na(cm3age)) {
						cf1age <- cm3age + agedifference - 3
						cf2age <- cm3age + agedifference - 2
						cf3age <- cm3age + agedifference
						cf4age <- cm3age + agedifference + 2
						cf5age <- cm3age + agedifference + 6
					} else if (!is.na(cm4age)) {
						cf1age <- cm4age + agedifference - 5
						cf2age <- cm4age + agedifference - 4
						cf3age <- cm4age + agedifference - 2
						cf4age <- cm4age + agedifference
						cf5age <- cm4age + agedifference + 4
					} else if (!is.na(cm5age)) {
						cf1age <- cm5age + agedifference - 9
						cf2age <- cm5age + agedifference - 8
						cf3age <- cm5age + agedifference - 6
						cf4age <- cm5age + agedifference - 4
						cf5age <- cm5age + agedifference 
					}
				}

				if (flag == 1) {
					return(cf1age)
				} else if (flag == 2) {
					return(cf2age) 
				} else if (flag == 3) {
					return(cf3age)
				} else if (flag == 4) {
					return(cf4age)
				} else if (flag == 5) {
					return (cf5age)
				}
			}

			#function to impute values of either fathers or mothers age across periods 2,3,4 and 5, assumng we know as much as we can about wave 1
			#(note: this is a reasonable assumption because most of mother's data from wave 1 is complete, and therefore so is father's after above step)
			fixage_m <- function(flag, c1age, c2age, c3age, c4age, c5age) {

				if (is.na(c2age)) {
					c2age <- c1age + 1
				}
				if (is.na(c3age)) {
					c3age <- c1age + 3
				}
				if (is.na(c4age)) {
					c4age <- c1age + 5
				}

				if (is.na(c5age)) {
					c5age <- c1age + 9
				}

				if (flag == 1) {
					return(c1age)
				} else if (flag == 2) {
					return(c2age) 
				} else if (flag == 3) {
					return(c3age)
				} else if (flag == 4) {
					return(c4age)
				} else if (flag == 5) {
					return (c5age)
				}

			}

			#one particular record for mum is all NAs, but we have information for dad. use similar process to impute 
			reduced_df[reduced_df$challengeID == 4236,]$cm1age <- reduced_df[reduced_df$challengeID == 4236,]$cf1age - agedifference
			reduced_df[reduced_df$challengeID == 4236,]$cm2age <- reduced_df[reduced_df$challengeID == 4236,]$cf1age - agedifference + 1
			reduced_df[reduced_df$challengeID == 4236,]$cm3age <- reduced_df[reduced_df$challengeID == 4236,]$cf1age - agedifference + 3
			reduced_df[reduced_df$challengeID == 4236,]$cm4age <- reduced_df[reduced_df$challengeID == 4236,]$cf1age - agedifference + 5
			reduced_df[reduced_df$challengeID == 4236,]$cm5age <- reduced_df[reduced_df$challengeID == 4236,]$cf1age - agedifference + 9

			#actually perform the age imputations
			rowwise <- dplyr::rowwise(reduced_df) 
			out <- dplyr::mutate(rowwise, cf1age_m=fixage(1,cf1age, cf2age, cf3age, cf4age, cf5age, cm1age, cm2age,cm3age,cm4age,cm5age),
					cf2age_m=fixage(2,cf1age, cf2age, cf3age, cf4age, cf5age, cm1age, cm2age,cm3age,cm4age,cm5age),
					cf3age_m=fixage(3,cf1age, cf2age, cf3age, cf4age, cf5age, cm1age, cm2age,cm3age,cm4age,cm5age),
					cf4age_m=fixage(4,cf1age, cf2age, cf3age, cf4age, cf5age, cm1age, cm2age,cm3age,cm4age,cm5age),
					cf5age_m=fixage(5,cf1age, cf2age, cf3age, cf4age, cf5age, cm1age, cm2age,cm3age,cm4age,cm5age),
					cm2age_m=fixage_m(2,cm1age, cm2age, cm3age, cm4age, cm5age),
					cm3age_m=fixage_m(3,cm1age, cm2age, cm3age, cm4age, cm5age),
					cm4age_m=fixage_m(4,cm1age, cm2age, cm3age, cm4age, cm5age),
					cm5age_m=fixage_m(5,cm1age, cm2age, cm3age, cm4age, cm5age),
					cf2age_m=fixage_m(2,cf1age_m, cf2age_m, cf3age_m, cf4age_m, cf5age_m),
					cf3age_m=fixage_m(3,cf1age_m, cf2age_m, cf3age_m, cf4age_m, cf5age_m),
					cf4age_m=fixage_m(4,cf1age_m, cf2age_m, cf3age_m, cf4age_m, cf5age_m),
					cf5age_m=fixage_m(5,cf1age_m, cf2age_m, cf3age_m, cf4age_m, cf5age_m),
					)

			#overwrite original input_df columns with our logically imputed values  
			input_df$cf1age <- out$cf1age_m
			input_df$cf2age <- out$cf2age_m
			input_df$cf3age <- out$cf3age_m
			input_df$cf4age <- out$cf4age_m
			input_df$cf5age <- out$cf5age_m
			input_df$cm2age <- out$cm2age_m
			input_df$cm3age <- out$cm3age_m
			input_df$cm4age <- out$cm4age_m
			input_df$cm5age <- out$cm5age_m

			print("Done with logical age imputation!")

			#return the entire background data frame 
			return(input_df)
			

		} else {

			stop(paste("Error: function in file", this.file, "expects a data frame"))

		}

	}

	if(ageimpute == 1 | ageimpute==TRUE) {
		message('Run logical imputation...')
		#source('imputation_logical.R', echo=F)
		background <- logical_imputation(background)
	}


	if(meanimpute ==1 | meanimpute==TRUE) {
		dropna = 1
		message('Convert to numeric...')
		background <- data.frame(sapply(background, as.numeric))
	}

	if(dropna == 1 | dropna==TRUE) {
		message('Drop missing data...')
		background[background < 0] <- NA	
	}

	if(meanimpute ==1 | meanimpute==TRUE) {
		message('Impute means...')
		background <- zoo::na.aggregate(background)
	}

	return(background)

	message('Ready!')

}