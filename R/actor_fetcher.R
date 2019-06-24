#' Generate actor data frames (with sentiment) from database
#'
#' Generate actor data frames (with sentiment) from database
#' @param out Data frame produced by elasticizer
#' @param sent_dict Optional dataframe containing the sentiment dictionary (see sentiment paper scripts for details on format)
#' @param cores Number of threads to use for parallel processing
#' @param validation Boolean indicating whether human validation should be performed on sentiment scoring
#' @return No return value, data per batch is saved in an RDS file
#' @export
#' @examples
#' actor_fetcher(out, sent_dict = NULL, cores = 1)
#################################################################################################
#################################### Aggregate actor results ################################
#################################################################################################
actor_fetcher <- function(out, sent_dict = NULL, cores = 1, localhost = NULL, validation = F) {
  plan(multiprocess, workers = cores)
  ### Functions
  ### Calculate sentiment scores for each actor-document
  sent_scorer <- function(row, out_row, ud_sent) {
    ### Contains sentiment per sentence for actor
    actor_tone <- filter(ud_sent, sentence_id %in% unlist(out_row[row,]$sentence_id))

    ### Aggregated sentiment per actor (over all sentences containing actor)
    actor <- summarise(actor_tone,
                       sent = sum(sent_sum)/sum(words),
                       sent_sum = sum(sent_sum),
                       sent_words = sum(sent_words),
                       words = sum(words),
                       arousal = sum(sent_words)/sum(words)
    )
    return(cbind(out_row[row,],data.frame(actor = actor)))
  }

  par_sent <- function(row, out, sent_dict = NULL) {
    out_row <- out[row,]
    ### Generating actor dataframe, unnest by actorsDetail, then by actor ids. Filter out non-relevant actor ids.
    if (is.null(sent_dict) == F) {
      ud_sent <- out_row$`_source.ud`[[1]] %>%
        select(-one_of('exists')) %>%
        unnest() %>%
        filter(upos != 'PUNCT') %>% # For getting proper word counts
        mutate(V1 = str_c(lemma,'_',upos)) %>%
        left_join(sent_dict, by = 'V1') %>%
        # ### Setting binary sentiment as unit of analysis
        # mutate(V2 = V3) %>%
        group_by(sentence_id) %>%
        mutate(
          V2 = case_when(
            is.na(V2) == T ~ 0,
            TRUE ~ V2
          )
        ) %>%
        summarise(sent_sum = sum(V2),
                  words = length(lemma),
                  sent_words = length(na.omit(V3))) %>%
        mutate(
          sent = sent_sum/words,
          arousal = sent_words/words
        )
      out_row <- select(out_row, -`_source.ud`) %>%
        unnest(`_source.computerCodes.actorsDetail`, .preserve = colnames(.))
      ### Aggregated sentiment per article (over all sentences in article)
      text_sent <- summarise(ud_sent,
                             sent = sum(sent_sum)/sum(words),
                             sent_sum = sum(sent_sum),
                             sent_words = sum(sent_words),
                             words = sum(words),
                             arousal = sum(sent_words)/sum(words)
      )
      out_row <- bind_rows(lapply(seq(1,nrow(out_row),1),sent_scorer, out_row = out_row, ud_sent = ud_sent)) %>%
        cbind(., text = text_sent)
      if (validation == T) {
        codes_sent <- filter(ud_sent, sentence_id == out_row$`_source.codes.sentence.id`[1]) %>%
          select(-sentence_id)
        out_row <- cbind(out_row, codes = codes_sent)
      }
    } else {
      out_row <- unnest(out_row, `_source.computerCodes.actorsDetail`, .preserve = colnames(out_row))
    }
    out_row <- out_row %>%
      mutate(
        year = strftime(`_source.publication_date`, format = '%Y'),
        yearmonth = strftime(`_source.publication_date`, format = '%Y%m'),
        yearmonthday = strftime(`_source.publication_date`, format = '%Y%m%d'),
        yearweek = strftime(`_source.publication_date`, format = "%Y%V")
      ) %>%
      select(-`_source.computerCodes.actorsDetail`,
             -`_score`,
             -`_index`,
             -`_type`)
    return(out_row)
  }
  saveRDS(bind_rows(future_lapply(1:nrow(out), par_sent, out = out, sent_dict = sent_dict)), file = paste0('df_out',as.numeric(as.POSIXct(Sys.time())),'.Rds'))
  return()
}
