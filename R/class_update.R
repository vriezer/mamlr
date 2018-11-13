#' Classifier function for use in combination with the elasticizer function as 'update' parameter (without brackets), see elasticizer documentation for more information
#'
#' Classifier function for use in combination with the elasticizer function as 'update' parameter (without brackets), see elasticizer documentation for more information
#' @param out Does not need to be defined explicitly! (is already parsed in the elasticizer function)
#' @param localhost Defaults to false. When true, connect to a local Elasticsearch instance on the default port (9200)
#' @param model_final The classification model (e.g. output from textstat_nb(), svm() or others)
#' @param dfm_words A dfm containing all the words and only the words used to generate the model (is used for subsetting)
#' @param varname String containing the variable name to use for the classification result, usually has the format computerCodes.varname
#' @param es_super Password for write access to ElasticSearch
#' @return As this is a nested function used within elasticizer, there is no return output
#' @export
#' @examples
#' class_update(out, localhost = T, model_final, dfm_words, varname, es_super)
#################################################################################################
#################################### Update any kind of classification ##########################
#################################################################################################
class_update <- function(out, localhost = T, model_final, dfm_words, varname, es_super) {
  print('updating')
  dfm <- dfm_gen(out, text = 'lemmas') %>%
    dfm_keep(dfm_words, valuetype="fixed", verbose=T)
  pred <- data.frame(id = out$`_id`, pred = predict(model_final, newdata = dfm))
  bulk <- apply(pred, 1, bulk_writer, varname = varname, type = 'set')
  res <- elastic_update(bulk, es_super = es_super, localhost = localhost)
  stop_for_status(res)
  appData <- content(res)
  if (appData$errors == T){
    print(appData)
    stop("Aborting, errors found during updating")
  }
  print("updated")
}
