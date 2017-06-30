# Regression-based Imputation

Several predictive approaches to imputation using linear regressions, lasso and full imputation maximum likelihood for data in the Fragile Families Challenge. For more information on the challenge: fragilefamilieschallenge.org.

## Tl;dr

To get started, in R:
1. Load the initialization script: `source('init.R')`
3. Run `yourDF <- fcc_imputation_init(data='location/ofyour.file')` (assigns output to new variable name of your choice)
4. Run `yourImputedDF <- regression_imputation(yourDF)`

## Available options

### Initialization

`fcc_imputation_init(data='', dropna = 1, ageimpute=1, meanimpute=1)`

- `data=''` location of your background.csv data file (required)
- `dropna=1` whether to convert all coded missing values to NA (e.g. -9 Not in wave, -6 Skip) (default:yes)
- `ageimpute=1` whether to impute age using extrapolation based on average age difference between Mom and Dad (only for constructed variables) (default:yes) 
- `meanimpute=1` whether to impute NA values with column means (necessary for regression imputation) (default:yes)

### Regression Imputation 

`regression_imputation(yourDF, method='lm', parallel=0, threshold=0.4,top_predictors=3, constructed=1,debug=0, test=0)`

- `method='lm'` Options: 'lm' (default) for OLS regression or 'polywog' for lasso-based prediction.
- `parallel=0` Parallelization option (using parallel package). Only for Mac OSX at the moment. Auto-detects number of cores. (default: off)
- `threshold=0.4` Quality cut-off for building prediction models. Must be > 0 and < 1 (runs a correlation in the background). (default:0.4)
- `top_predictors=3` How many predictors to use in each model. 3 is a sane default, you may want more if you want to try for more accuracy, or less if you find a lot of your variables do not have predictions. 
- `constructed=1` Work only with about 320 valid constructed variables. Default is yes. Warning: setting this to 0 will cause very long runtime as will run on all columns of your data frame. 
- `debug=0` Turn on if you would like to see models, warnings about lack of model convergence and performance of your models. (default:off)
- `test=0` Turn on if you would like to run a trial with the first 4 variables (hard coded for now) instead of the full dataframe. Highly recommended (default:off)

