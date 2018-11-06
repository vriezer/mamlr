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
#' @param cores_outer Number of cores to use for outer CV (cannot be more than the number of outer folds)
#' @param cores_grid Number of cores to use for grid search (cannot be more than the number of grid rows (i.e. possible parameter combinations), multiplies with cores_outer)
#' @param cores_inner Number of cores to use for inner CV loop (cannot be more than number of inner CV folds, multiplies with cores_outer and cores_grid)
#' @param cores_feats Number of cores to use for feature selection (multiplies with cores outer, cores_grid and cores_inner)
#' @param seed Integer to use as seed for random number generation, ensures replicability
#' @param outer_k Number of outer cross-validation folds (for performance estimation)
#' @param inner_k Number of inner cross-validation folds (for hyperparameter optimization and feature selection)
#' @param model Classification algorithm to use (currently only "nb" for Naïve Bayes using textmodel_nb)
#' @param class_type Type of classification to model ("junk", "aggregate", or "codes")
#' @param opt_measure Label of measure in confusion matrix to use as performance indicator
#' @param country Two-letter country abbreviation of the country the model is estimated for (used for filename)
#' @param grid Data frame providing all possible combinations of hyperparameters and feature selection parameters for a given model (grid search)
#' @return An .RData file in the current working directory (getwd()) containing the final model, performance estimates and the parameters used for grid search and cross-validation
#' @export
#' @examples
#' modelizer(dfm, cores_outer = 1, cores_grid = 1, cores_inner = 1, cores_feats = 1, seed = 42, outer_k = 3, inner_k = 5, model = model, class_type = class_type, opt_measure = opt_measure, country = country, grid = grid)
#################################################################################################
#################################### Function to generate classification models #################
#################################################################################################

modelizer <- function(dfm, cores_outer, cores_grid, cores_inner, cores_feats, seed, outer_k, inner_k, model, class_type, opt_measure, country, grid) {
  ### Functions ###
  feat_select <- function (topic, dfm, class_type, percentile,measure) {
    keyness <- textstat_keyness(dfm, measure = measure, docvars(dfm, class_type) == as.numeric(topic)) %>%
      na.omit()
    keyness <- filter(keyness, keyness[,2] > quantile(as.matrix(keyness[,2]),percentile))$feature
    return(keyness)
  }
  ### Generate inner folds for nested cv
  inner_loop <- function(fold, dfm, inner_k, class_type) {
    # RNG needs to be set explicitly for each fold
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
    ## Either createDataPartition for simple holdout parameter optimization
    ## Or createFolds for proper inner CV for nested CV
    # if (inner_k <= 1) {
    #   inner_folds <- createDataPartition(as.factor(docvars(dfm[-fold], class_type)), times = 1, p = 1-0.8)
    # } else {
    inner_folds <- createFolds(as.factor(docvars(dfm[-fold], class_type)), k= inner_k)
    # }
    return(c(outer_fold = list(fold),inner_folds))
  }

  ### Generate outer folds for nested cv
  generate_folds <- function(outer_k, inner_k, dfm, class_type){
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
    folds <- createFolds(as.factor(docvars(dfm, class_type)), k= outer_k)
    return(lapply(folds,inner_loop, dfm = dfm, inner_k = inner_k, class_type = class_type))
  }

  ### Gets called for every parameter combination, and calls classifier for every inner cv fold
  inner_cv <- function(row,grid,outer_fold, inner_folds, dfm, class_type, model, cores_inner, cores_feats) {
    params <- grid[row,]
    # For each inner fold, cross validate the specified parameters
    res <-
      bind_rows(mclapply(inner_folds,
                         classifier,
                         outer_fold = outer_fold,
                         params = params,
                         dfm = dfm,
                         class_type = class_type,
                         model = model,
                         cores_feats = cores_feats,
                         mc.cores = cores_inner
      )
      )

    return(cbind(as.data.frame(t(colMeans(select(res, 1:`Balanced Accuracy`)))),params))
  }

  ### Gets called for every outer cv fold, and calls inner_cv for all parameter combinations in grid
  outer_cv <- function(fold, grid, dfm, class_type, model, cores_grid, cores_inner, cores_feats) {
    # If fold contains both inner folds and outer fold
    if (length(fold) == inner_k + 1) {
      inner_folds <- fold[-1]
      outer_fold <- fold$outer_fold
      # For each row in grid, cross-validate results
      res <- bind_rows(mclapply(seq(1,length(grid[[1]]),1),
                                inner_cv,
                                cores_feats= cores_feats,
                                grid = grid,
                                dfm = dfm,
                                class_type = class_type,
                                model = model,
                                outer_fold = outer_fold,
                                inner_folds = inner_folds,
                                cores_inner = cores_inner,
                                mc.cores = cores_grid)
      )
      # Determine optimum hyperparameters within outer fold training set
      optimum <- res[which.max(res[,opt_measure]),] %>%
        select(percentiles: ncol(.))
      # Validate performance of optimum hyperparameters on outer fold test set
      return(classifier(NULL, outer_fold = outer_fold, params = optimum, dfm = dfm, class_type = class_type, model = model, cores_feats = cores_feats))
    } else {
      # If no outer fold, go directly to parameter optimization using inner folds, and return performance of hyperparameters
      inner_folds <- fold
      outer_fold <- NULL
      res <- bind_rows(mclapply(seq(1,length(grid[[1]]),1),
                                inner_cv,
                                cores_feats= cores_feats,
                                grid = grid,
                                dfm = dfm,
                                class_type = class_type,
                                model = model,
                                outer_fold = outer_fold,
                                inner_folds = inner_folds,
                                cores_inner = cores_inner,
                                mc.cores = cores_grid)
      )
      return(res)
    }
  }

  ### Custom tfidf function to allow same idf for different dfm's
  custom_tfidf <- function(x, scheme_tf = "count", scheme_df = "inverse", base = 10, dfreq = dfreq) {
    if (!nfeat(x) || !ndoc(x)) return(x)
    tfreq <- dfm_weight(x, scheme = scheme_tf, base = base)
    if (nfeat(x) != length(dfreq))
      stop("missing some values in idf calculation")
    # get the document indexes
    j <- as(tfreq, "dgTMatrix")@j + 1
    # replace just the non-zero values by product with idf
    x@x <- tfreq@x * dfreq[j]
    # record attributes
    x@weightTf <- tfreq@weightTf
    x@weightDf <- c(list(scheme = scheme_df, base = base), args)
    return(x)
  }

  ### Classification function
  classifier <- function (inner_fold, outer_fold, params, dfm, class_type, model, cores_feats) {
    # If both inner and outer folds, subset dfm to outer_fold training set, then create train and test sets according to inner fold. Evaluate performance
    if (length(inner_fold) > 0 && length(outer_fold) > 0)  {
      dfm_train <- dfm[-outer_fold] %>%
        .[-inner_fold]
      dfm_test <- dfm[-outer_fold] %>%
        .[inner_fold]
      # If only outer folds, but no inner folds, validate performance of outer fold training data on outer fold test data
    } else if (length(outer_fold) > 0 ) {
      dfm_train <- dfm[-outer_fold]
      dfm_test <- dfm[outer_fold]
      # If only inner folds, validate performance directly on inner folds (is the same as above?)
    } else if (length(inner_fold) > 0 ) {
      dfm_train <- dfm[-inner_fold]
      dfm_test <- dfm[inner_fold]
      # If both inner and outer folds are NULL, set training set to whole dataset, estimate model and return final model
    } else {
      final <- T ### Indicate final modeling run on whole dataset
      dfm_train <- dfm
    }
    ### Getting features from training dataset
    # Getting idf from training data, and using it to normalize both training and testing feature occurence
    dfm_train <- dfm_trim(dfm_train, min_termfreq = 1, min_docfreq = 0)
    dfreq <- docfreq(dfm_train, scheme = "inverse", base = 10, smoothing = 0, k = 0, threshold = 0, use.names=T)
    dfm_train <- custom_tfidf(dfm_train, scheme_tf = "count", scheme_df = "inverse", base = 10, dfreq = dfreq)
    words <- unlist(mclapply(unique(docvars(dfm_train, class_type)),
                             feat_select,
                             dfm = dfm_train,
                             class_type = class_type,
                             percentile = params$percentiles,
                             measure = params$measures,
                             mc.cores = cores_feats
    ))
    dfm_train <- dfm_keep(dfm_train, words, valuetype="fixed", verbose=T)


    if (model == "nb") {
      text_model <- textmodel_nb(dfm_train, y = docvars(dfm_train, class_type), smooth = .001, prior = "uniform", distribution = "multinomial")
    }
    if (model == "svm") {
      text_model <- svm(x=dfm_train, y=as.factor(docvars(dfm_train, class_type)), type = "C-classification", kernel = "linear", gamma = params$gamma, cost = params$cost, epsilon = params$epsilon)
    }
    ### Add more if statements for different models
    if (exists("final") == T) {
      return(text_model)
    } else {
      ### Removing all features not in training set from test set and weighting the remaining features according to training idf
      dfm_test <- dfm_keep(dfm_test, pattern = dfm_train, valuetype = "fixed", verbose = T)
      dfm_test <- custom_tfidf(dfm_test, scheme_tf = "count", scheme_df = "inverse", base = 10, dfreq = dfreq[words])
      pred <- predict(text_model, newdata = dfm_test)
      class_table <- table(prediction = pred, trueValues = docvars(dfm_test, class_type))
      conf_mat <- confusionMatrix(class_table, mode = "everything")
      if (is.matrix(conf_mat$byClass) == T) {
        return(cbind(as.data.frame(t(conf_mat$overall)),as.data.frame(t(colMeans(conf_mat$byClass))),params))
      } else {
        return(cbind(as.data.frame(t(conf_mat$overall)),as.data.frame(t(conf_mat$byClass)),params, pos_cat = conf_mat$positive))
      }
    }
  }

  ## Generate nested CV folds, based on number of inner and outer folds defined (see start of script)
  folds <- generate_folds(outer_k,inner_k = inner_k, dfm = dfm, class_type = class_type)

  ## Get performance of each outer fold validation, and add row with mean scores (This is the final performance indicator)
  performance <- bind_rows(mclapply(folds, outer_cv, grid=grid, dfm=dfm, class_type=class_type, model=model, cores_grid=cores_grid, cores_inner=cores_inner, cores_feats=cores_feats, mc.cores = cores_outer)) %>%
    bind_rows(., colMeans(select(., 1:`Balanced Accuracy`)))

  ## Set seed and generate folds for final hyperparameter optimization search (using CV)
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
  folds_final <- list(createFolds(as.factor(docvars(dfm, class_type)), k= inner_k))

  ## Get the final hyperparameter performance for all value combinations in grid
  params_final <- bind_rows(mclapply(folds_final, outer_cv, grid=grid, dfm=dfm, class_type=class_type, model=model, cores_grid=cores_grid, cores_inner=cores_inner, cores_feats=cores_feats, mc.cores = cores_outer))

  ## Select optimum final hyperparameters
  optimum_final <- params_final[which.max(params_final[,opt_measure]),] %>%
    select(percentiles: ncol(.))

  ## Estimate final model on whole dataset, using optimum final hyperparameters determined above
  model_final <- classifier(NULL, outer_fold = NULL, params = optimum_final, dfm = dfm, class_type = class_type, model = model, cores_feats = detectCores())
  rm(list=setdiff(ls(), c("model_final", "optimum_final","params_final","performance","grid","folds","folds_final","country","model","class_type","opt_measure")), envir = environment())
  save(list = ls(all.names = TRUE), file = paste0(getwd(),'/',country,'_',model,'_',class_type,'_',opt_measure,'_',Sys.time(),'.RData'), envir = environment())
  return(paste0(getwd(),'/',country,'_',model,'_',class_type,'_',opt_measure,'_',Sys.time(),'.RData'))
}
