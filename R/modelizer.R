#' Generate a classification model
#'
#' Generate a nested cross validated classification model based on a dfm with class labels as docvars
#' Currently only supports Naïve Bayes using quanteda's textmodel_nb
#' Hyperparemeter optimization is enabled through the grid parameter
#' A grid should be generated from vectors with the labels as described for each model, using the crossing() command
#' For Naïve Bayes, the following parameters can be used:
#' - percentiles (cutoff point for tf-idf feature selection)
#' - measures (what measure to use for determining feature importance, see textstat_keyness for options)
#' @param dfm A quanteda dfm used to train and evaluate the model, should contain the vector with class labels in docvars
#' @param outer_k Number of outer cross-validation folds (for performance estimation)
#' @param inner_k Number of inner cross-validation folds (for hyperparameter optimization and feature selection)
#' @param class_type Type of classification to model ("junk", "aggregate", or "codes")
#' @param opt_measure Label of measure in confusion matrix to use as performance indicator
#' @param country Two-letter country abbreviation of the country the model is estimated for (used for filename)
#' @param grid Data frame providing all possible combinations of hyperparameters and feature selection parameters for a given model (grid search)
#' @param seed Integer to use as seed for random number generation, ensures replicability
#' @param model Classification algorithm to use (currently only "nb" for Naïve Bayes using textmodel_nb)
#' @param cores Number of threads used for parallel processing using future_lapply, defaults to 1
#' @return An .Rds file in the current working directory (getwd()) with a list containing all relevant output
#' @export
#' @examples
#' modelizer(dfm, outer_k, inner_k, class_type, opt_measure, country, grid, seed, model, cores = 1)
#################################################################################################
#################################### Function to generate classification models #################
#################################################################################################

modelizer <- function(dfm, outer_k, inner_k, class_type, opt_measure, country, grid, seed, model, cores = 1) {
  ## Generate list containing outer folds row numbers, inner folds row numbers, and grid for model building
  folds <- cv_generator(outer_k,inner_k = inner_k, dfm = dfm, class_type = class_type, grid = grid, seed = seed)
  inner_grid <- folds$grid
  outer_folds <- folds$outer_folds
  inner_folds <- folds$inner_folds

  ## Create multithread work pool for future_lapply
  plan(strategy = multiprocess, workers = cores)

  ## Use estimator function to build models for every parameter combination in grid
  inner_cv_output <- future_lapply(1:nrow(inner_grid), estimator,
                                   grid = inner_grid,
                                   outer_folds = outer_folds,
                                   inner_folds = inner_folds,
                                   dfm = dfm,
                                   class_type = class_type,
                                   model = model) %>%
    future_lapply(.,metric_gen) %>% # Generate model performance metrics for each grid row
    bind_rows(.)


  outer_grid <- inner_cv_output %>%
    # Group by outer folds, and by parameters used for model tuning
    group_by_at(c("outer_fold", colnames(grid))) %>%
    # Get mean values for all numeric (performance indicator) variables
    summarise_if(is.numeric, mean, na.rm = F) %>%
    # Group the mean performance indicators by outer_fold
    group_by(outer_fold) %>%
    # Get for each outer_fold the row with the highest value of opt_measure
    slice(which.max((!!as.name(opt_measure)))) %>%
    # Select only the columns outer_fold, and the columns that are in the original parameter grid
    select(outer_fold, colnames(grid))

  # Use the estimator function to build optimum models for each outer_fold
  outer_cv_output <- future_lapply(1:nrow(outer_grid), estimator,
                                   grid = outer_grid,
                                   outer_folds = outer_folds,
                                   inner_folds = NULL,
                                   dfm = dfm,
                                   class_type = class_type,
                                   model = model) %>%
    future_lapply(., metric_gen) %>% # Generate performance metrics for each row in outer_grid
    bind_rows(.)

  # Create (inner) folds for parameter optimization on the entire dataset
  final_folds <- cv_generator(NULL,inner_k = inner_k, dfm = dfm, class_type = class_type, grid = grid, seed = seed)
  final_grid <- final_folds$grid
  final_inner <- final_folds$inner_folds

  # Use the estimator function to estimate the performance of each row in final_grid
  final_cv_output <- future_lapply(1:nrow(final_grid), estimator,
                                   grid = final_grid,
                                   outer_folds = NULL,
                                   inner_folds = final_inner,
                                   dfm = dfm,
                                   class_type = class_type,
                                   model = model) %>%
    future_lapply(.,metric_gen) %>% # Generate performance metrics for each row in final_grid
    bind_rows(.)


  final_params <- final_cv_output %>%
    # Group final parameter optimization cv results by parameters used for optimization
    group_by_at(colnames(grid)) %>%
    # Get mean performance metrics for each fold
    summarise_if(is.numeric, mean, na.rm = F) %>%
    # Ungroup to allow for slicing
    ungroup() %>%
    # Select row with highest value of opt_measure
    slice(which.max((!!as.name(opt_measure)))) %>%
    # Keep only the columns that are present in the original parameter grid
    select(colnames(grid))
  # Use the estimator function to estimate the final model, using the optimum parameters provided in final_params
  model_final <- estimator(1,
                           grid = final_params,
                           outer_folds = NULL,
                           inner_folds = NULL,
                           dfm = dfm,
                           class_type = class_type,
                           model = model)
  # Create list with output variables
  output <- list(final_cv_output = final_cv_output,
                 final_params = final_params,
                 outer_cv_output = outer_cv_output,
                 model_final = model_final,
                 grid = grid,
                 seed = seed,
                 opt_measure = opt_measure,
                 model = model,
                 country = country,
                 class_type = class_type)

  # Set name for output .Rds file
  filename <- paste0(getwd(),'/',country,'_',model,'_',class_type,'_',opt_measure,'_',Sys.time(),'.Rds')

  # Save RDS file with output
  saveRDS(output, file = filename)

  # Return ouput RDS filename
  return(filename)
}
