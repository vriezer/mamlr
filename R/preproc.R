#' Preprocess dfm data for use in modeling procedure
#'
#' Process dfm according to parameters provided in params
#'
#' @param dfm_train Training dfm
#' @param dfm_test Testing dfm if applicable, otherwise NULL
#' @param params Row from grid with parameter optimization
#' @return List with dfm_train and dfm_test, processed according to parameters in params
#' @export
#' @examples
#' preproc(dfm_train, dfm_test = NULL, params)
#################################################################################################
#################################### Preprocess data ############################################
#################################################################################################
preproc <- function(dfm_train, dfm_test = NULL, params) {
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

  if ("feat_percentiles" %in% colnames(params) && "feat_measures" %in% colnames(params)) {

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

  return(list(dfm_train = dfm_train, dfm_test = dfm_test, idf = idf))
}
