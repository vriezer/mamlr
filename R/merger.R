#' Merges list of lemmas back into a pseudo-document
#'
#' Merges list of lemmas back into a pseudo-document
#' @param row A row number form the Elasticizer-generated data frame
#' @param words String indicating the number of words to keep from each document (maximum document length), 999 indicates the whole document
#' @param out The elasticizer-generated data frame
#' @return A documentified string of lemmas, one document at a time
#' @export
#' @examples
#' merger(1, words = '999', out = out)
#################################################################################################
#################################### Reconstructing documents from lemmas########################
#################################################################################################
## Only merging lemmas for now, feature selection has no impact on junk classification
merger <- function(row, out = out) {
  df <- out[row,]
  # Mergin lemmas into single string
  lemmas <- paste(str_split(df$`_source.tokens.lemmas`, "\\|")[[1]],collapse = ' ')
  # Replacing $-marked punctuation with their regular forms
  lemmas <- str_replace_all(lemmas," \\$(.+?)", "\\1") %>%
    ### Removing numbers and non-words containing numbers
    str_replace_all("\\S*?[0-9@#]+(\\S*?)([:;.,?!\\s])+?", "\\2") %>%
    # Adding extra . at end of string to allow for strings that contain less than 150 words and do not end on ". "
    paste0(.,". ")
  return(lemmas)
}
