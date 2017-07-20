# Regression-based Imputation

Several predictive approaches to imputation using linear regressions and lasso based approach for data in the Fragile Families Challenge. For more information on the challenge: fragilefamilieschallenge.org.

## Tl;dr

To get started, in R:
1. Make sure "devtools" is installed and loaded:
  - `library(devtools)`
2. Install and load the package: 
  - `devtools::install_github("annafil/FFCRegressionImputation")`
  - `library(FFCRegressionImputation)`
3. Run initial data cleaning and basic imputation (necessary for step 4 but can be used standalone):
  - `yourDF <- initImpute(data='location/ofyour.file')`
4. Create a correlation matrix containing strengths of associations between all variables of interest. This also produces a cleaned up dataframe for use in the next function. 
  - `output <- corMatrix(data=yourDF)`
5. Run regression imputation using output from 4. 
  - `yourImputedDF <- regImputation(output$df, output$corMatrix)`

See below for options to customize each function. 

Final output is a dataframe of imputed values (either constructed only or the full data frame, depending on the options you specify), where an original value is missing, and original values where they exist. For example, if the dataset is missing data in the first case in househost income from mom's survey in wave 4 (cm4hhinc), but not cases 2 and 3, the function will only impute the first case, and return the original values for cases 2 and 3. 

*Note: This software is still under development, so it's possible things may not work exactly as they should. If you encounter a problem, please help by submitting an issue on this project page.*

## Available options

### Initialization

This is the initialization function that imports data from the available background.csv file (see Fragile Families website for how to obtain the data), and performs a couple of basic processing functions. By default, the function converts all values below 0 to NAs, and imputes age for mom and dad using available information across waves. This can be extended to other kinds of logical imputations (PRs welcome!). 

`initImpute(data='', dropna = 1, ageimpute=1, meanimpute=0)`

- `data=''` location of your background.csv data file (required)
- `dropna=1` whether to convert all coded missing values to NA (e.g. -9 Not in wave, -6 Skip) (default:yes)
- `ageimpute=1` whether to impute age using extrapolation based on average age difference between Mom and Dad (only for constructed age variables) (default:yes). This is pretty safe to leave on: data in the survey is pretty spotty across waves, but where one piece of information on either Mom or Dad is available, we can reasonably assume and fill in age for all other waves, as this will change in predictable way. If information about only Mom, or only Dad is available across all waves, this option will make a reasonable guess about missing parter's age based on the mean age difference between Mom and Dad in the sample. 
- `meanimpute=1` whether to (optionally) convert input dataframe into numeric and impute NA values with column means (default:no). This is off by default because it is performed automatically by regression imputation function, regImpute, but provided here as a helper option. 

### Correlation Matrix
The bulk of the resource intensive portion of this imputation is in computing a correlation Matrix. This is done (painfully) by testing the correlation of every variable with every other variable, and is necessary in order to do error handling for issues that normally trip up other functions. For this reason, this portion of the process is abstracted into a separate function that can be run only once on the data set of interest, and the output can be easily stored. 

`corMatrix(data='', varpattern='',debug=0, test=0, parallel = 0)`

- `varpattern='[regularexpression]'` apply a regular expression to filter the data columns, and reduce size of matrix processed. Filtering the variables is optional but HIGHLY RECOMMENDED if you are working with the entire background.csv file. Example query for selecting only constructed variables (about 300 variables, runtime of 1 hour on a modern 4-core machine): `^c[mfhpktfvino]{1,2}[12345]`. 
- `debug=0` shows which models are running, the quality of predictions relative to original data, and any model errors. Any value >= 1 will show progress notifications, errors and warnings.
- `test=0` runs on only the first 4 variables of a given dataframe; helpful for trying out the function options before running full correlation matrix.  
- `parallel=0` whether to attempt to parallelize for faster processing time (currently, for MacOSX only). 

#### Saving and restoring output from CorMatrix

CorMatrix produces a list of two objects: $df and $corMatrix. $df is the given dataframe, but with columns dropped that have no variance, and therefore cannot be used to make predictions. $corMatrix is the correlation matrix of all useable columns in given dataframe. They are designed to be used together in regImpute, and should be stored together. Here is an example:

To save the resulting object for reuse:
`saveRDS([output variable from corMatrix], "output.rds")`

To restore the object:
`restored <- readRDS("output.rds")`


### Regression Imputation 

`regImpute(dataframe='', matrix='', method='lm', degree=1, parallel=0, threshold=0.4,top_predictors=3, debug=0, test=0)`

- `method='lm'` Options: 'lm' (default) for OLS regression or 'lasso' for lasso-based prediction (uses the 'polywog' package).
- `degree=1` Lasso only: The degree of polynomial effects to estimate: 1=main effects only, 2=quadratic, 3=cubic, etc.
- `parallel=0` Parallelization option (using parallel package). Only for Mac OSX at the moment. Auto-detects number of cores. (default: off)
- `threshold=0.4` Quality cut-off for building prediction models. Must be > 0 and < 1 (runs a correlation in the background). (default:0.4)
- `top_predictors=3` How many predictors to use in each model. 3 is a sane default, you may want more if you want to try for more accuracy, or less if you find a lot of your variables do not have predictions. 
- `debug=0` Turn on if you would like to see models, warnings about lack of model convergence and performance of your models. (default:off)
- `test=0` Turn on if you would like to run a trial with the first 4 variables instead of the full dataframe. (default:off) Note: If you give regImpute very few variables, such as a test output of corMatrix, this may not work as expected and generate no predictions because there is not enough data. 

## Todo

- Implement full information maximum likelihood. Lavaan has a good implementation, and scaffolding code is already in this repo, but lavaan does not currently support regression-type predictions, only for latent models. There is existing conversation on implementing this in Lavaan. See: https://github.com/yrosseel/lavaan/issues/44 for updates...
