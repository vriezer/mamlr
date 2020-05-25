#' Generate actor data frames (with sentiment) from database
#'
#' Generate actor data frames (with sentiment) from database
#' @param out Data frame produced by elasticizer
#' @param sent_dict Optional dataframe containing the sentiment dictionary and values. Words should be either in the "lem_u" column when they consist of lemma_upos pairs, or in the "lemma" column when they are just lemmas. The "prox" column should either contain word values, or 0s if not applicable.
#' @param validation Boolean indicating whether human validation should be performed on sentiment scoring
#' @return No return value, data per batch is saved in an RDS file
#' @export
#' @examples
#' sentencizer(out, sent_dict = NULL, validation = F)
#################################################################################################
#################################### Aggregate actor results ################################
#################################################################################################
sentencizer <- function(out, sent_dict = NULL, localhost = NULL, validation = F) {
  par_sent <- function(row, out, sent_dict = NULL) {
    out <- out[row,]
    metadata <- out %>%
      select(`_id`,contains("_source"),-contains("computerCodes.actors"),-contains("ud"))
    ud_sent <- out %>% select(`_id`,`_source.ud`) %>%
      unnest(cols = colnames(.)) %>%
      select(-one_of('exists')) %>%
      unnest(cols = colnames(.)) %>%
      filter(upos != 'PUNCT')

    if (!is.null(sent_dict)) {
      if ("lem_u" %in% colnames(sent_dict)) {
        ud_sent <- ud_sent %>%
          mutate(lem_u = str_c(lemma,'_',upos)) %>%
          left_join(sent_dict, by = 'lem_u')
      } else if ("lemma" %in% colnames(sent_dict)) {
        ud_sent <- ud_sent %>%
          left_join(sent_dict, by = 'lemma') %>%
          mutate(lem_u = lemma)
      }
      ud_sent <- ud_sent %>%
        group_by(`_id`,sentence_id) %>%
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
    } else {
      ud_sent <- ud_sent %>% group_by(`_id`,sentence_id) %>% summarise()
    }
    out <- select(out, -`_source.ud`)

    if (validation == T) {
      codes_sent <- ud_sent %>%
        left_join(.,out, by='_id') %>%
        rowwise() %>%
        filter(sentence_id == `_source.codes.sentence.id`)
      return(codes_sent)
    }

    ### Unnest out_row to individual actor ids

    if("_source.computerCodes.actorsDetail" %in% colnames(out)) {
      out <- out %>%
        unnest(`_source.computerCodes.actorsDetail`) %>%
        # mutate(ids_list = ids) %>%
        unnest(ids) %>%
        unnest(sentence_id) %>%
        group_by(`_id`,sentence_id) %>%
        summarise(
          ids = list(ids)
        )
    } else {
      out <- out %>%
        group_by(`_id`) %>%
        summarise() %>%
        mutate(sentence_id = 1)
    }


      out <- out %>%
        left_join(ud_sent,.,by = c('_id','sentence_id')) %>%
        group_by(`_id`)
    if(!is.null(sent_dict)) {
      text_sent <- out %>%
        summarise(
          text.sent_sum = sum(sent_sum),
          text.words = sum(words),
          text.sent_words = sum(sent_words),
          text.sent_lemmas = I(list(unlist(sent_lemmas))),
          text.sentences = n()
        ) %>%
        mutate(
          text.sent = text.sent_sum/text.words,
          text.arousal = text.sent_words/text.words
        )
      out <- out %>%
        summarise_all(list) %>%
        left_join(.,text_sent,by='_id') %>%
        left_join(.,metadata,by='_id')
    } else {
      out <- out %>%
        summarise_all(list) %>%
        left_join(.,metadata,by='_id')
    }
    return(out)
  }
  saveRDS(par_sent(1:nrow(out),out = out, sent_dict=sent_dict), file = paste0('df_out',as.numeric(as.POSIXct(Sys.time())),'.Rds'))
  return()
  ### Keeping the option for parallel computation
  # microbenchmark::microbenchmark(out_normal <- par_sent(1:nrow(out),out = out, sent_dict=sent_dict), times = 1)
  # plan(multiprocess, workers = cores)
  # chunks <- split(1:nrow(out), sort(1:nrow(out)%%cores))
  # microbenchmark::microbenchmark(out_par <- bind_rows(future_lapply(chunks,par_sent, out=out, sent_dict=sent_dict)), times = 1)
}
