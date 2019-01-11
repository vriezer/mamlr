#' Merges list of lemmas back into a pseudo-document
#'
#' Merges list of lemmas back into a pseudo-document
#' @param row A row number form the Elasticizer-generated data frame
#' @param words String indicating the number of words to keep from each document (maximum document length), 999 indicates the whole document
#' @param out The elasticizer-generated data frame
#' @param text String indicating whether the "merged" field will contain the "full" text, old-style "lemmas" (will be deprecated), new-style "ud"
#' @return A documentified string of lemmas, one document at a time
#' @export
#' @examples
#' merger(1, words = '999', out, text)
#################################################################################################
#################################### Reconstructing documents from lemmas########################
#################################################################################################
## Only merging lemmas for now, feature selection has no impact on junk classification
merger <- function(row, out, text) {
  df <- out[row,]
  # Mergin lemmas into single string
  if (text == 'lemmas') {
    lemmas <- paste(str_split(df$`_source.tokens.lemmas`, "\\|")[[1]],collapse = ' ')
  }
  if (text == 'ud') {
    lemmas <- paste0(df$`_source.ud`[[1]]$lemma[[1]], collapse = ' ')
  }
  # Replacing $-marked punctuation with their regular forms
  lemmas <- str_replace_all(lemmas," \\$(.+?)", "\\1") %>%
    ### Removing numbers and non-words containing numbers
    str_replace_all("\\S*?[0-9@#$%]+[^\\s!?.,;:]*", "") %>%
    # Adding extra . at end of string to allow for strings that contain less than 150 words and do not end on ". "
    paste0(.,". ")
  return(lemmas)
}
