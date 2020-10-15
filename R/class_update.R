#' Classifier function for use in combination with the elasticizer function as 'update' parameter (without brackets), see elasticizer documentation for more information
#'
#' Classifier function for use in combination with the elasticizer function as 'update' parameter (without brackets), see elasticizer documentation for more information
#' @param out Does not need to be defined explicitly! (is already parsed in the elasticizer function)
#' @param localhost Defaults to false. When true, connect to a local Elasticsearch instance on the default port (9200)
#' @param model_final The classification model (e.g. output from textstat_nb(), svm() or others)
#' @param dfm_words A dfm containing all the words and only the words used to generate the model (is used for subsetting)
#' @param varname String containing the variable name to use for the classification result, usually has the format computerCodes.varname
#' @param words String indicating the number of words to keep from each document (maximum document length), 999 indicates the whole document
#' @param text String indicating whether the "merged" field will contain the "full" text, old-style "lemmas" (will be deprecated), new-style "ud", or ud_upos combining lemmas with upos tags
#' @param clean Boolean indicating whether the results should be cleaned by removing words matching regex (see code).
#' @param ver Short string (preferably a single word/sequence) indicating the version of the updated document (i.e. for a udpipe update this string might be 'udV2')
#' @param es_super Password for write access to ElasticSearch
#' @return As this is a nested function used within elasticizer, there is no return output
#' @export
#' @examples
#' class_update(out, localhost = T, model_final, dfm_words, varname, es_super = .rs.askForPassword('ElasticSearch WRITE'))
#################################################################################################
#################################### Update any kind of classification ##########################
#################################################################################################
class_update <- function(out, localhost = T, model_final, varname, text, words, clean, ver, es_super = .rs.askForPassword('ElasticSearch WRITE')) {
  print('updating')
  dfm <- dfm_gen(out, text = text, words = words, clean = clean)
  if (!is.null(model_final$idf)) {
    dfm <- dfm_weight(dfm, weights = model_final$idf)
  }
  pred <- data.frame(id = out$`_id`, pred = predict(model_final$text_model, newdata = dfm, type = "class", force = T))
  bulk <- apply(pred, 1, bulk_writer, varname = varname, type = 'set', ver = ver)
  res <- elastic_update(bulk, es_super = es_super, localhost = localhost)
}
