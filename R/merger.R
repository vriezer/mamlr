#' Merges list of lemmas back into a pseudo-document
#'
#' Merges list of lemmas back into a pseudo-document
#' @param out The elasticizer-generated data frame
#' @param text String indicating whether the "merged" field will contain the "full" text, old-style "lemmas" (will be deprecated), new-style "ud"
#' @param clean Boolean indicating whether the results should be cleaned by removing words matching regex (see code).
#' @return A documentified string of lemmas, one document at a time
#' @export
#' @examples
#' merger(out, text, clean)
#################################################################################################
#################################### Reconstructing documents from lemmas########################
#################################################################################################
## Only merging lemmas for now, feature selection has no impact on junk classification
merger <- function(out, text, clean) {
  df <- unnest(out, cols = '_source.ud') %>%
    select(`_id`,lemma,upos) %>%
    unnest(cols = c('lemma','upos')) %>%
    # This line is added in the new merger function, in the old merger function this would result in the following:
    # 1: when using ud, it would result in the string "NA" being present in place of the faulty lemma
    # 2: when using ud_upos, it would result in the entire article becoming NA, because of str_c() returning NA when any value is NA
    filter(!is.na(lemma)) %>%
    group_by(`_id`)
  if (text == 'ud_upos') {
    df <- df %>%
      filter(upos != 'PUNCT') %>%
      mutate(
        lem_u = str_c(lemma,upos,sep="_")
      ) %>%
      summarise(
                merged = str_c(c(lem_u), collapse= ' ')
      ) %>%
      # Regex removes all words consisting of or containing numbers, @#$%
      # Punctuation is not taken into account, as it is already filtered out, see above
      {if(clean == T) mutate(.,
                             merged = str_replace_all(merged,"\\S*?[0-9@#$%]+[^\\s]*", "")
      )
        else . }
  }
  if (text == 'ud') {
    df <- df %>%
      summarise(
                merged = str_c(c(lemma), collapse= ' ')
      ) %>%
      mutate(
        merged = str_replace_all(merged," \\$(.+?)", "\\1")
      ) %>%
      # Regex removes all words consisting of or containing numbers, @#$%
      # Punctuation is only filtered out when not followed by a whitespace character, and when the word contains any of the characters above
      # Regex also used in out_parser
      # Adding extra . at end of string to allow for strings that contain less than 150 words and do not end on ". "
      {if(clean == T) mutate(.,
                             merged = str_replace_all(merged,"\\S*?[0-9@#$%]+([^\\s!?.,;:]|[!?.,:;]\\S)*", "")
      )
        else . } %>%
      mutate(.,
             merged = paste0(merged,'. '))
  }
  return(df)
}
