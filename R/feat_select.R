#' Select features using quanteda textstat_keyness
#'
#' Select features based on the textstat_keyness function and a percentile cutoff
#' Percentiles are based on absolute values i.e. both on words that are key and *not* key to the topic
#'
#' @param topic The topic to determine keywords for
#' @param dfm The input dfm
#' @param class_type Name of the column in docvars containing the classification
#' @param percentile Cutoff for the list of words that should be returned
#' @param measure Measure to use in determining keyness, default = chi2; see textstat_keyness for other options
#' @return A vector of words that are key to the topic
#' @export
#' @examples
#' feat_select(topic, dfm, class_type, percentile, measure="chi2")
#################################################################################################
#################################### Feature selection ##########################################
#################################################################################################

feat_select <- function (topic, dfm, class_type, percentile, measure="chi2") {
  # Use quanteda textstat_keyness to determine feature importance
  keyness <- textstat_keyness(dfm, measure = measure, target = docvars(dfm, class_type) == as.numeric(topic)) %>%
    na.omit()
  # Convert keyness values to absolute values, to take into account both positive and negative extremes
  keyness[,2] <- abs(keyness[,2])
  # Keep only the words with an absolute keyness value falling in the top [percentile] percentile
  keyness <- filter(keyness, keyness[,2] > quantile(as.matrix(keyness[,2]),percentile))$feature
  return(keyness)
}
