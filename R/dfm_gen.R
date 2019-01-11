#' Generates dfm from ElasticSearch output
#'
#' Generates dfm from ElasticSearch output
#' @param out The elasticizer-generated data frame
#' @param words String indicating the number of words to keep from each document (maximum document length), 999 indicates the whole document
#' @param text String indicating whether the "merged" field will contain the "full" text, old-style "lemmas" (will be deprecated), new-style "ud"
#' @param clean Boolean indicating whether the results should be cleaned by removing words matching regex (see code). Lemmatized output is always cleaned!
#' @return A Quanteda dfm
#' @export
#' @examples
#' dfm_gen(out, words = '999')


#################################################################################################
#################################### DFM generator #############################
#################################################################################################

# filter(`_source.codes.timeSpent` != -1) %>% ### Exclude Norwegian summer sample hack

dfm_gen <- function(out, words = '999', text = "lemmas", clean) {
  # Create subset with just ids, codes and text
  out <- out %>%
    select(`_id`, matches("_source.*")) ### Keep only the id and anything belonging to the source field
  fields <- length(names(out))
  if (text == "lemmas" || text == 'ud') {
    out$merged <- unlist(mclapply(seq(1,length(out[[1]]),1),merger, out = out, text = text, mc.cores = detectCores()))

  }
  if (text == "full") {
    out <- out_parser(out, field = '_source' , clean = clean)
  }
  if ('_source.codes.majorTopic' %in% colnames(out)) {
    out <- out %>%
      mutate(codes = case_when(
        .$`_source.codes.timeSpent` == -1 ~ NA_character_,
        TRUE ~ .$`_source.codes.majorTopic`
      )
      ) %>%
      mutate(junk = case_when(
        .$codes == 2301 ~ 1,
        .$codes == 3101 ~ 1,
        .$codes == 34 ~ 1,
        .$`_source.codes.timeSpent` == -1 ~ NA_real_,
        TRUE ~ 0
      )
      ) %>%
      mutate(aggregate = .$codes %>%
               str_pad(4, side="right", pad="a") %>%
               str_match("([0-9]{1,2})?[0|a][1-9|a]") %>%
               .[,2] %>%
               as.numeric()
      )
   vardoc <- out[,-seq(1,(length(names(out))-3),1)]
  } else {
    vardoc <- NULL
  }
  if (words != "999") {
    ### Former word count regex, includes words up until the next sentence boundary, instead of cutting to the last sentence boundary
    # out$merged2 <- str_extract(lemmas, str_c("^(([\\s\\S]*? ){0,",words,"}[\\s\\S]*?[.!?])\\s+?"))
    out <- out %>% rowwise() %>% mutate(merged = paste0(str_split(merged, '\\s')[[1]][1:words], collapse = ' ') %>%
                                          str_extract('.*[.?!]'))
  }
  dfm <- corpus(out$merged, docnames = out$`_id`, docvars = vardoc) %>%
    dfm(tolower = T, stem = F, remove_punct = T, valuetype = "regex", ngrams = 1)
  return(dfm)
}
