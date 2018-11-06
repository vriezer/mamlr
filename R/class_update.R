#' Classifier function for use in combination with the elasticizer function as 'update' parameter (without brackets), see elasticizer documentation for more information
#'
#' Classifier function for use in combination with the elasticizer function as 'update' parameter (without brackets), see elasticizer documentation for more information
#' @param out Does not need to be defined explicitly! (is already parsed in the elasticizer function)
#' @param model_final The classification model (e.g. output from textstat_nb(), svm() or others)
#' @param dfm_words A dfm containing all the words and only the words used to generate the model (is used for subsetting)
#' @param varname String containing the variable name to use for the classification result, usually has the format computerCodes.varname
#' @param es_super Password for write access to ElasticSearch
#' @return As this is a nested function used within elasticizer, there is no return output
#' @export
#' @examples
#' elasticizer(query, src = T, es_pwd = es_pwd, update = class_update, model_final = model_final, dfm_words = dfm_words, varname = computerCodes.varname, es_super = es_super)
#################################################################################################
#################################### Update any kind of classification ##########################
#################################################################################################
class_update <- function(out, model_final, dfm_words, varname, es_super) {
  print('updating')
  dfm <- dfm_gen(out, text = 'lemmas') %>%
    dfm_keep(dfm_words, valuetype="fixed", verbose=T)
  pred <- data.frame(id = out$`_id`, pred = predict(model_final, newdata = dfm))
  bulk <- apply(pred, 1, bulk_writer, varname = varname)
  res <- elastic_update(bulk, es_super = es_super)
  stop_for_status(res)
  content(res, "parsed", "application/json")
  appData <- content(res)
  if (appData$errors == T){
    print(appData)
    stop("Aborting, errors found during updating")
  }
  print("updated")
  Sys.sleep(1)
}
