#' Aggregate sentence-level dataset containing actors (from sentencizer())
#'
#' Aggregate sentence-level dataset containing actors (from sentencizer())
#' @param df Data frame with actor ids, produced by sentencizer
#' @param actors_meta Data frame containing actor metadata obtained using elasticizer(index="actors")
#' @param actor_groups Optional list of vectors, where each vector contains actor ids to be merged (e.g. merge all left-wing parties)
#' @return When no ids, returns actor-article dataset with individual actors, party aggregations, party-actor aggregations and overall actor sentiment (regardless of specific actors). When ids, returns aggregations for each vector in list
#' @export
#' @examples
#' actor_merger(df, actors_meta, ids = NULL)
#################################################################################################
#################################### Generate actor-article dataset #############################
#################################################################################################

### NOTE: The exceptions for various partyId_a ids has been implemented because of an error with
### some individual actors, where the partyId of an individual actor doesn't match an actual
### partyId in the actor dataset

actor_merger <- function(df, actors_meta, actor_groups = NULL) {
  grouper <- function(id2, df) {
    if ('P_1206_a' %in% id2) {
      id2 <- c('P_212_a','P_1771_a',id2)
    }
    if ('P_1605_a' %in% id2) {
      id2 <- c('P_1606_a', id2)
    }
    if ('P_1629_a' %in% id2) {
      id2 <- c(str_c('P_',as.character(1630:1647),'_a'), id2)
    }
    return(df[ids %in% id2,] %>%
             .[!duplicated(.,by = c('id','sentence_id')),.(
               actor.sent = sum(sent_sum)/sum(words),
               actor.sent_sum = sum(sent_sum),
               actor.sent_words = sum(sent_words),
               actor.words = sum(words),
               actor.arousal = sum(sent_words)/sum(words),
               actor.first = first(sentence_id),
               actor.occ = .N,
               publication_date = first(publication_date),
               ids = str_c(id2, collapse = '-')
             ), by = c('id')]
           )
  }

  ## Remove some of the metadata from the source df
  df <- data.table(df)[,.(
    (.SD),
    doctype = as.factor(`_source.doctype`),
    publication_date = as.Date(`_source.publication_date`),
    id = as.factor(`_id`)
  ), .SDcols = !c('_source.doctype','_source.publication_date','_id')]

  text_sent <- df[,.SD, .SDcols = c('id', 'doctype',grep('text\\.',names(df), value = T))]

  ## Create bogus variables if sentiment is not scored
  if(!"sent_sum" %in% colnames(df)) {
    df <- df[,.(
      (.SD),
      sent_words = 0,
      sent_sum = 0
    )]
  }

  ## Unnest to sentence level
  df <- df[,lapply(.SD, unlist, recursive=F),
           .SDcols = c('sentence_id', 'sent_sum', 'words', 'sent_words','ids'),
           by = list(id,publication_date)]



  text_noactors <- df[lengths(ids) == 0L,
                      .(noactor.sent = sum(sent_sum)/sum(words),
                        noactor.sent_sum = sum(sent_sum),
                        noactor.sent_words = sum(sent_words),
                        noactor.words = sum(words),
                        noactor.arousal = sum(sent_words)/sum(words),
                        noactor.first = first(sentence_id),
                        noactor.occ = .N), by = list(id)]




  all <- df[lengths(ids) > 0L,
            .(actor.sent = sum(sent_sum)/sum(words),
              actor.sent_sum = sum(sent_sum),
              actor.sent_words = sum(sent_words),
              actor.words = sum(words),
              actor.arousal = sum(sent_words)/sum(words),
              actor.first = first(sentence_id),
              actor.occ = .N,
              ids = 'all'), by = list(id)]

  ## Unnest to actor level
  df <- df[,.(ids = as.character(unlist(ids))),
                  by = list(id,publication_date,sentence_id, sent_sum, words, sent_words)]

  ## Create aggregations according to list of actorId vectors in ids
  if(!is.null(actor_groups)) {
    output <- lapply(actor_groups,grouper, df = df) %>%
      rbindlist(.) %>%
      left_join(text_sent, by="id") %>%
      mutate(
        actor.prom = actor.occ/text.sentences,
        actor.rel_first = 1-(actor.first/text.sentences),
        year = strftime(publication_date, format = '%Y'),
        yearmonth = strftime(publication_date, format = '%Y%m'),
        yearmonthday = strftime(publication_date, format = '%Y%m%d'),
        yearweek = strftime(publication_date, format = "%Y%V")
      )
    return(output)
  } else {
    ## Create aggregate measures for individual actors
    actors <- df[str_starts(ids, 'A_'),
                 .(actor.sent = sum(sent_sum)/sum(words),
                   actor.sent_sum = sum(sent_sum),
                   actor.sent_words = sum(sent_words),
                   actor.words = sum(words),
                   actor.arousal = sum(sent_words)/sum(words),
                   actor.first = first(sentence_id),
                   actor.occ = .N,
                   publication_date = first(publication_date)), by = list(id, ids)]
    ## Create actor metadata dataframe per active date (one row per day per actor)
    colnames(actors_meta) <- str_replace(colnames(actors_meta),'_source.','')
    actors_meta <- actors_meta[,
                               .((.SD),
                                 startDate = as.Date(startDate),
                                 endDate = as.Date(endDate),
                                 ids = ifelse(actorId != '', actorId, partyId)
                               ), .SDcols = -c('_id','startDate','endDate','_index','_type','_score')
                               ]
    actors <- actors_meta[actors,
                           c('x.startDate','x.endDate',colnames(actors), 'lastName','firstName','function.','gender','yearOfBirth','parlPeriod','partyId','ministerName','ministryId','actorId','startDate','endDate'),
                           on =.(ids = ids, startDate <= publication_date, endDate >= publication_date),
                           allow.cartesian = T,
                           mult = 'all',
                           with = F][,.(
                             startDate = x.startDate,
                             endDate = x.endDate,
                             (.SD)
                           ), .SDcols = -c('x.startDate', 'x.endDate','startDate','endDate')]
    ## Generate party-actor aggregations (mfsa)
    # identical(as.data.frame(setcolorder(setorderv(parties_actors,c('id','ids')), colnames(parties_actors_dp))),as.data.frame(parties_actors_dp))

    parties_actors <- df[str_starts(ids,'P_'),.(
      ids = str_sub(ids, start = 1, end = -3),
      (.SD)
    ),.SDcols = -c('ids')][, .(
      ids = case_when(ids == 'P_212' ~ 'P_1206',
                      ids == 'P_1771' ~ 'P_1206',
                      ids == 'P_1606' ~ 'P_1605',
                      ids %in% str_c('P_',as.character(1630:1647)) ~ 'P_1629',
                      TRUE ~ ids),
      (.SD)
    ), .SDcols = -c('ids')][,.(
      actor.sent = sum(sent_sum)/sum(words),
      actor.sent_sum = sum(sent_sum),
      actor.sent_words = sum(sent_words),
      actor.words = sum(words),
      actor.arousal = sum(sent_words)/sum(words),
      actor.first = first(sentence_id),
      actor.occ = .N
    ), by = c('id','ids')]
    parties_actors <- actors_meta[parties_actors, on = c('ids'), mult = 'first'][!is.na(id),.(ids = str_c(ids,"_mfsa"), (.SD)), .SDcols = -c('ids')]

    ## Generate party aggregations (mfs)
    parties <- df[str_ends(ids,'_f') | str_ends(ids,'_s'),.(
      ids = str_sub(ids, start = 1, end = -3),
      (.SD)
    ),.SDcols = -c('ids')][,.(
      actor.sent = sum(sent_sum)/sum(words),
      actor.sent_sum = sum(sent_sum),
      actor.sent_words = sum(sent_words),
      actor.words = sum(words),
      actor.arousal = sum(sent_words)/sum(words),
      actor.first = first(sentence_id),
      actor.occ = .N
    ), by = c('id','ids')]
    parties <- actors_meta[parties, on = c('ids')][!is.na(id),.(ids = str_c(ids,"_mfs"), (.SD)), .SDcols = -c('ids')]

    ## Join all aggregations into a single data frame, compute derived actor-level measures, and add date dummies
    df <- bind_rows(actors, parties, parties_actors, all) %>%
      left_join(.,text_sent, by="id") %>%
      left_join(.,text_noactors, by="id") %>%
      mutate(
        actor.prom = actor.occ/text.sentences,
        actor.rel_first = 1-(actor.first/text.sentences),
        year = strftime(publication_date, format = '%Y'),
        yearmonth = strftime(publication_date, format = '%Y%m'),
        yearmonthday = strftime(publication_date, format = '%Y%m%d'),
        yearweek = strftime(publication_date, format = "%Y%V")
      ) %>%
      ungroup() %>%
      select(-contains('Search'),-starts_with('not'))
    return(df)
  }
}

