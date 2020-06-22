#' Preprocess dfm data for use in modeling procedure
#'
#' Process dfm according to parameters provided in params
#'
#' @param dfm_train Training dfm
#' @param dfm_test Testing dfm if applicable, otherwise NULL
#' @param params Row from grid with parameter optimization
#' @param we_vectors Matrix with word embedding vectors
#' @return List with dfm_train and dfm_test, processed according to parameters in params
#' @export
#' @examples
#' preproc(dfm_train, dfm_test = NULL, params)
#################################################################################################
#################################### Preprocess data ############################################
#################################################################################################


### CURRENTLY UNUSED!!!###

preproc <- function(dfm_train, dfm_test = NULL, params, we_vectors) {
  # Remove non-existing features from training dfm
  dfm_train <- dfm_trim(dfm_train, min_termfreq = 1, min_docfreq = 0)
  if (params$tfidf) {
    idf <- docfreq(dfm_train, scheme = "inverse", base = 10, smoothing = 0, k = 0, threshold = 0)
    dfm_train <- dfm_weight(dfm_train, weights = idf)
    if (!is.null(dfm_test)) {
      dfm_test <- dfm_weight(dfm_test, weights = idf)
    }
  } else {
    idf <- NULL
  }

  if (!is.null(params$feat_percentiles) && !is.null(params$feat_measures)) {

    # Keeping unique words that are important to one or more categories (see textstat_keyness and feat_select)
    words <- unique(unlist(lapply(unique(docvars(dfm_train, params$class_type)),
                                  feat_select,
                                  dfm = dfm_train,
                                  class_type = params$class_type,
                                  percentile = params$feat_percentiles,
                                  measure = params$feat_measures
    )))
    dfm_train <- dfm_keep(dfm_train, words, valuetype="fixed", verbose=F)
  }

  if (!is.null(we_vectors)) {
    shared_dict <- sort(intersect(dfm_train@Dimnames$features,we_vectors$V1))
    if (!is.null(dfm_test)) {
      shared_dict <- sort(intersect(dfm_test@Dimnames$features,shared_dict))
      dfm_test <- dfm_keep(dfm_test, pattern = shared_dict, valuetype = "fixed", case_insensitive=F) %>%
        .[, sort(colnames(.))]
    }
    dfm_train <- dfm_keep(dfm_train, pattern = shared_dict, valuetype = "fixed", case_insensitive=F) %>%
      .[, sort(colnames(.))]
    we_matrix <- filter(we_vectors, V1 %in% shared_dict) %>%
      arrange(V1) %>%
      as.data.table(.) %>%
      .[,2:ncol(.), with = F] %>%
      as.matrix(.)

    dfm_train_we_sum <- dfm_train %*% we_matrix
    # dfm_train_we_mean <- dfm_train_we_sum / as.vector(rowSums(dfm_train))

    if (!is.null(dfm_test)) {
      dfm_test_we_sum <- dfm_test %*% we_matrix
      # dfm_test_we_mean <- dfm_test_we_sum / as.vector(rowSums(dfm_test))
    }
    return(list(dfm_train = dfm_train, dfm_test = dfm_test, idf = idf, dfm_train_we = dfm_train_we_sum, dfm_test_we = dfm_test_we_sum))
  }
  return(list(dfm_train = dfm_train, dfm_test = dfm_test, idf = idf))
}
