#' Generate models and get classifications on test sets
#'
#' Creates a grid of models to be estimated for each outer fold, inner fold and parameter combination
#'
#' @param row Row number of current item in grid
#' @param grid Grid with model parameters and CV folds
#' @param outer_folds List with row numbers for outer folds
#' @param dfm DFM containing labeled documents
#' @param class_type Name of column in docvars() containing the classes
#' @param model Model to use (currently only nb)
#' @return Dependent on mode, if folds are included, returns true and predicted classes of test set, with parameters, model and model idf. When no folds, returns final model and idf values.
#' @export
#' @examples
#' estimator(row, grid, outer_folds, dfm, class_type, model)
#################################################################################################
#################################### Generate models ############################################
#################################################################################################

### Classification function
estimator <- function (row, grid, outer_folds, inner_folds, dfm, class_type, model) {
  # Get parameters for current iteration
  params <- grid[row,]

  # If both inner and outer folds, subset dfm to outer_fold training set, then create train and test sets according to inner fold. Evaluate performance
  if ("inner_fold" %in% colnames(params) && "outer_fold" %in% colnames(params))  {
    dfm_train <- dfm[-outer_folds[[params$outer_fold]],] %>%
      .[-inner_folds[[params$outer_fold]][[params$inner_fold]],]
    dfm_test <- dfm[-outer_folds[[params$outer_fold]],] %>%
      .[inner_folds[[params$outer_fold]][[params$inner_fold]],]
    # If only outer folds, but no inner folds, validate performance of outer fold training data on outer fold test data
  } else if ("outer_fold" %in% colnames(params)) {
    dfm_train <- dfm[-outer_folds[[params$outer_fold]],]
    dfm_test <- dfm[outer_folds[[params$outer_fold]],]
    # If only inner folds, validate performance directly on inner folds
  } else if ("inner_fold" %in% colnames(params)) {
    dfm_train <- dfm[-inner_folds[[params$inner_fold]],]
    dfm_test <- dfm[inner_folds[[params$inner_fold]],]
    # If both inner and outer folds are NULL, set training set to whole dataset, estimate model and return final model
  } else {
    final <- T ### Indicate final modeling run on whole dataset
    dfm_train <- dfm
  }

  if (exists("final")) {
    preproc_dfm <- preproc(dfm_train, NULL, params)
    dfm_train <- preproc_dfm$dfm_train
  } else {
    preproc_dfm <- preproc(dfm_train, dfm_test, params)
    dfm_train <- preproc_dfm$dfm_train
    dfm_test <- preproc_dfm$dfm_test
  }
  idf <- preproc_dfm$idf

  if (model == "nb") {
    text_model <- textmodel_nb(dfm_train, y = docvars(dfm_train, class_type), smooth = .001, prior = "uniform", distribution = "multinomial")
  }
  if (model == "svm") {
    text_model <- svm(x=dfm_train, y=as.factor(docvars(dfm_train, class_type)), type = "C-classification", kernel = params$kernel, gamma = params$gamma, cost = params$cost, epsilon = params$epsilon)
  }
  if (model == 'nnet') {
    idC <- class.ind(as.factor(docvars(dfm_train, class_type)))
    text_model <- nnet(dfm_train, idC, decay = params$decay, size=params$size, maxit=params$maxit, softmax=T, reltol = params$reltol, MaxNWts = params$size*(length(dfm_train@Dimnames$features)+1)+(params$size*2)+2)
  }
  ### Add more if statements for different models

  # If training on whole dataset, return final model, and idf values from dataset
  if (exists("final")) {
    return(list(text_model=text_model, idf=idf))
  } else { # Create a test set, and classify test items
    # Use force=T to keep only features present in both training and test set
    pred <- predict(text_model, newdata = dfm_test, type = 'class', force = T)

    return(data.frame(
      tv = I(list(docvars(dfm_test, class_type))), # True values from test set
      pred = I(list(pred)), # Predictions of test set
      params, # Parameters used to generate classification model
      text_model = I(list(text_model)), # The classification model
      idf = I(list(idf)), # IDF of the training dataset used for model creation
      stringsAsFactors = F
    ))
  }
}
