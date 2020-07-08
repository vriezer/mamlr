#' Generate sentence-level dataset with sentiment and actor presence
#'
#' Generate sentence-level dataset with sentiment and actor presence
#' @param out Data frame produced by elasticizer
#' @param sent_dict Optional dataframe containing the sentiment dictionary and values. Words should be either in the "lem_u" column when they consist of lemma_upos pairs, or in the "lemma" column when they are just lemmas. The "prox" column should either contain word values, or 1 for all words if there are no values.
#' @param validation Boolean indicating whether human validation should be performed on sentiment scoring
#' @return No return value, data per batch is saved in an RDS file
#' @export
#' @examples
#' sentencizer(out, sent_dict = NULL, validation = F)
#################################################################################################
#################################### Generate sentence-level dataset#############################
#################################################################################################
sentencizer <- function(out, sent_dict = NULL, localhost = NULL, validation = F) {
  ## Despite the function name, parallel processing is not used, because it is slower
  par_sent <- function(row, out, sent_dict = NULL) {
    out <- out[row,]
    ## Create df with article metadata (fields that are included in the elasticizer function)
    metadata <- out %>%
      select(`_id`,contains("_source"),-contains("computerCodes.actors"),-contains("ud"))

    ## Unnest documents into individual words
    ud_sent <- out %>% select(`_id`,`_source.ud`) %>%
      unnest(cols = colnames(.)) %>%
      select(-one_of('exists')) %>%
      unnest(cols = colnames(.)) %>%
      filter(upos != 'PUNCT')

    ## If there is a dictionary, apply it
    if (!is.null(sent_dict)) {
      ## If the dictionary contains the column lem_u, assume lemma_upos format
      if ("lem_u" %in% colnames(sent_dict)) {
        ud_sent <- ud_sent %>%
          mutate(lem_u = str_c(lemma,'_',upos)) %>%
          left_join(sent_dict, by = 'lem_u')
        ## If the dictionary contains the column lemma, assume simple lemma format
      } else if ("lemma" %in% colnames(sent_dict)) {
        ud_sent <- ud_sent %>%
          left_join(sent_dict, by = 'lemma') %>%
          mutate(lem_u = lemma)
      }

      ## Group by sentences, and generate dictionary scores per sentence
      ud_sent <- ud_sent %>%
        group_by(`_id`,sentence_id) %>%
        mutate(
          prox = case_when(
            is.na(prox) == T ~ 0,
            TRUE ~ prox
          )
        ) %>%
        summarise(sent_sum = sum(prox),
                  sent_sum_pos = sum(prox[prox>0]),
                  sent_sum_neg = sum(prox[prox<0]),
                  words = length(lemma),
                  sent_words = sum(prox != 0),
                  # sent_lemmas = list(lem_u[prox != 0])
                  )
      ## If there is no dictionary, create a ud_sent, with just sentence ids and word counts per sentence
    } else {
      ud_sent <- ud_sent %>%
        group_by(`_id`,sentence_id) %>%
        summarise(words = length(lemma))
    }

    ## Remove ud ouptut from source before further processing
    out <- select(out, -`_source.ud`)

    ## If dictionary validation, return just the sentences that have been hand-coded
    if (validation == T) {
      codes_sent <- ud_sent %>%
        left_join(.,out, by='_id') %>%
        rowwise() %>%
        filter(sentence_id == `_source.codes.sentence.id`)
      return(codes_sent)
    }

    if("_source.computerCodes.actorsDetail" %in% colnames(out)) {

      ## If actor details in source, create vector of actor ids for each sentence
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
      ## If no actor details, keep one row per article and add a bogus sentence_id
      out <- out %>%
        group_by(`_id`) %>%
        summarise() %>%
        mutate(sentence_id = 1)
    }

    ## Combine ud_sent with the source dataset
      out <- out %>%
        left_join(ud_sent,.,by = c('_id','sentence_id')) %>%
        group_by(`_id`)

    ## If there is a sent_dict, generate sentiment scores on article level
    if(!is.null(sent_dict)) {
      text_sent <- out %>%
        summarise(
          text.sent_sum = sum(sent_sum),
          text.words = sum(words),
          text.sent_words = sum(sent_words),
          text.sentences = n()
        ) %>%
        mutate(
          text.sent = text.sent_sum/text.words,
          text.arousal = text.sent_words/text.words
        )

    } else {
      text_sent <- out %>%
        summarise(
          text.words = sum(words),
          text.sentences = n()
        )
    }
      out <- out %>%
        summarise_all(list) %>%
        left_join(.,text_sent,by='_id') %>%
        left_join(.,metadata,by='_id') %>%
        ungroup()
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
