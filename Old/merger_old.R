#' Merges list of lemmas back into a pseudo-document
#'
#' Merges list of lemmas back into a pseudo-document
#' @param row A row number form the Elasticizer-generated data frame
#' @param words String indicating the number of words to keep from each document (maximum document length), 999 indicates the whole document
#' @param out The elasticizer-generated data frame
#' @param text String indicating whether the "merged" field will contain the "full" text, old-style "lemmas" (will be deprecated), new-style "ud"
#' @param clean Boolean indicating whether the results should be cleaned by removing words matching regex (see code).
#' @return A documentified string of lemmas, one document at a time
#' @export
#' @examples
#' merger(1, words = '999', out, text)
#################################################################################################
#################################### Reconstructing documents from lemmas########################
#################################################################################################
## Only merging lemmas for now, feature selection has no impact on junk classification
merger_old <- function(row, out, text, clean) {
  df <- out[row,]
  # Mergin lemmas into single string
  if (text == 'lemmas') {
    lemmas <- paste(str_split(df$`_source.tokens.lemmas`, "\\|")[[1]],collapse = ' ')
  }
  if (text == 'ud') {
    lemmas <- paste0(df$`_source.ud`[[1]]$lemma[[1]], collapse = ' ')
  }
  if (text == 'ud_upos') {
    df <- unnest(df,`_source.ud`)
    lemmas <- str_c(unlist(df$lemma)[which(unlist(df$upos) != 'PUNCT')], unlist(df$upos)[which(unlist(df$upos) != 'PUNCT')], sep = '_', collapse = ' ') %>%
      # Regex removes all words consisting of or containing numbers, @#$%
      # Punctuation is not taken into account, as it is already filtered out, see above
      {if(clean == T) str_replace_all(.,"\\S*?[0-9@#$%]+[^\\s]*", "")  else . }
    # In the very rare but obviously occuring (CxqrOmMB4Bzg6Uhtzw0P) case that a document consists only of punctuation, return an empty string
    if (length(lemmas) == 0 ){
      lemmas <- ''
    }
    return(lemmas)
  }
  # Replacing $-marked punctuation with their regular forms
  lemmas <- str_replace_all(lemmas," \\$(.+?)", "\\1") %>%
    # Regex removes all words consisting of or containing numbers, @#$%
    # Punctuation is only filtered out when not followed by a whitespace character, and when the word contains any of the characters above
    # Regex also used in out_parser
    {if(clean == T) str_replace_all(.,"\\S*?[0-9@#$%]+([^\\s!?.,;:]|[!?.,:;]\\S)*", "")  else . } %>%
    # Adding extra . at end of string to allow for strings that contain less than 150 words and do not end on ". "
    paste0(.,". ")
  return(lemmas)
}
