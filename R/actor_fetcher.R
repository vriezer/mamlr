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

  aggregator <- function (pid, dupe_df) {
    ### Party ids excluding actors
    p_ids <- c(str_c(pid,'_f'),str_c(pid,'_s'))
    ### Party ids including actors
    p_ids_a <- c(p_ids,str_c(pid,'_a'))
    summarizer <- function (p_ids, dupe_df, merged_id) {
        id <- dupe_df$`_id`[[1]]
        dupe_df <- dupe_df %>%
          filter(ids %in% p_ids)
        if (nrow(dupe_df) > 0) {
          return(
            dupe_df %>% summarise(
              `_id` = first(`_id`),
              `_source.doctype` = first(`_source.doctype`),
              `_source.publication_date` = first(`_source.publication_date`),
              prom = list(length(unique(unlist(sentence_id)))/round(occ[[1]]/prom[[1]])),
              sentence_id = list(sort(unique(unlist(sentence_id)))),
              rel_first = list(max(unlist(rel_first))),
              ids = merged_id,
              occ = list(length(unique(unlist(sentence_id)))),
              first = list(min(unlist(sentence_id))),
              actor_start = list(sort(unique(unlist(actor_start)))),
              actor_end = list(sort(unique(unlist(actor_end)))),
              sentence_start = list(sort(unique(unlist(sentence_start)))),
              sentence_end = list(sort(unique(unlist(sentence_end))))
            )
          )
        } else {
          print(paste0('id:',id))
          return(NULL)
        }
    }
    party <- summarizer(p_ids, dupe_df, str_c(pid,'_mfs'))
    party_actor <- summarizer(p_ids_a, dupe_df, str_c(pid,'_mfsa'))
    return(bind_rows(party,party_actor))
  }

  par_sent <- function(row, out, sent_dict = NULL) {
    out_row <- out[row,]
    ### Generating sentence-level sentiment scores from ud
    if (is.null(sent_dict) == F) {
      ud_sent <- out_row$`_source.ud`[[1]] %>%
        select(-one_of('exists')) %>%
        unnest() %>%
        filter(upos != 'PUNCT') %>% # For getting proper word counts
        mutate(lem_u = str_c(lemma,'_',upos)) %>%
        left_join(sent_dict, by = 'lem_u') %>%
        # ### Setting binary sentiment as unit of analysis
        # mutate(prox = V3) %>%
        group_by(sentence_id) %>%
        mutate(
          prox = case_when(
            is.na(prox) == T ~ 0,
            TRUE ~ prox
          )
        ) %>%
        summarise(sent_sum = sum(prox),
                  words = length(lemma),
                  sent_words = sum(prox != 0),
                  sent_lemmas = list(lem_u[prox != 0])) %>%
        mutate(
          sent = sent_sum/words,
          arousal = sent_words/words
        )
      out_row <- select(out_row, -`_source.ud`)
    }

    if (validation == T) {
      codes_sent <- filter(ud_sent, sentence_id == out_row$`_source.codes.sentence.id`[1])
      return(cbind(out_row, codes = codes_sent))
    }

    ### Unnest out_row to individual actor ids
    out_row <- out_row %>%
      unnest(`_source.computerCodes.actorsDetail`) %>%
      mutate(ids_list = ids) %>%
      unnest(ids) %>%
      mutate(
        pids = str_sub(ids, start = 1, end = -3)
      )

    ### Get list of party ids occuring more than once in the document
    pids_table <- table(out_row$pids)
    dupe_pids <- names(pids_table[pids_table > 1])%>%
      str_subset(pattern = fixed('P_'))
    single_pids <- names(pids_table[pids_table <= 1]) %>%
      str_subset(pattern = fixed('P_'))
    ### Data frame containing only duplicate party ids
    dupe_df <- out_row %>%
      filter(pids %in% dupe_pids)
    ### Data frame containing only single party ids
    single_df <- out_row %>%
      filter(pids %in% single_pids)

    ### Data frame for single occurrence mfsa
    single_party_actor <- single_df %>%
      mutate(
        ids = str_c(pids,'_mfsa')
      )
    ### Data frame for single occurence mfs
    single_party <- single_df %>%
      filter(!endsWith(ids, '_a')) %>%
      mutate(
        ids = str_c(pids,'_mfs')
      )
    out_row <- out_row %>%
      filter(startsWith(ids,'A_')) %>%
      bind_rows(., single_party, single_party_actor)
    ### For each of the party ids in the list above, aggregate to _mfs and _mfsa
    if (length(dupe_pids) > 0) {
      aggregate <- bind_rows(lapply(dupe_pids, aggregator, dupe_df = dupe_df))
      out_row <- bind_rows(out_row, aggregate)
    }

    ### Generating sentiment scores for article and actors
    if (is.null(sent_dict) == F) {
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
    }
    out_row <- out_row %>%
      mutate(
        year = strftime(`_source.publication_date`, format = '%Y'),
        yearmonth = strftime(`_source.publication_date`, format = '%Y%m'),
        yearmonthday = strftime(`_source.publication_date`, format = '%Y%m%d'),
        yearweek = strftime(`_source.publication_date`, format = "%Y%V")
      ) %>%
      select(#-`_source.computerCodes.actorsDetail`,
             -`_score`,
             -`_index`,
             -`_type`,
             -pids)
    return(out_row)
  }
  saveRDS(bind_rows(future_lapply(1:nrow(out), par_sent, out = out, sent_dict = sent_dict)), file = paste0('df_out',as.numeric(as.POSIXct(Sys.time())),'.Rds'))
  return()
}
