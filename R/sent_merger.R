#' Aggregate sentence-level dataset containing sentiment (from sentencizer())
#'
#' Aggregate sentence-level dataset containing sentiment (from sentencizer())
#' @param df Data frame with actor ids, produced by sentencizer
#' @param actors_meta Optional data frame containing actor metadata obtained using elasticizer(index="actors")
#' @param actor_groups Optional list of vectors, where each vector contains actor ids to be merged (e.g. merge all left-wing parties)
#' @param pos_cutoff Optional value above which sentence-level sentiment scores should be considered "positive"
#' @param neg_cutoff Optional value below which sentence-level sentiment scores should be considered "negative"
#' @param single_party Boolean to generate data only from sentences in which a single party is mentioned, defaults to FALSE
#' @return When no ids, returns actor-article dataset with individual actors, party aggregations, party-actor aggregations and overall actor sentiment (regardless of specific actors). When ids, returns aggregations for each vector in list
#' @export
#' @examples
#' sent_merger(df, actors_meta, ids = NULL)
#################################################################################################
#################################### Generate actor-article dataset #############################
#################################################################################################

### NOTE: The exceptions for various partyId_a ids has been implemented because of an error with
### some individual actors, where the partyId of an individual actor doesn't match an actual
### partyId in the actor dataset

sent_merger <- function(df, actors_meta = NULL, actor_groups = NULL, pos_cutoff = NULL, neg_cutoff = NULL, single_party = F) {
  grouper <- function(id2, df) {
    # Prevent usage of deprecated partyId_a ids, which are incorrect and should no longer be used
    if (any(str_ends(id2, '_a'))) {
      return("You're seemingly using a deprecated [partyId]_a id in your aggregations")
    }
    return(df[ids %in% id2,] %>%
             .[!duplicated(.,by = c('id','sentence_id')),.(
               actor.sent = sum(sent_binary_weighted)/sum(words),
               actor.sent_words = sum(sent_words),
               actor.words = sum(words),
               # actor.arousal = sum(abs(sent_binary_weighted))/sum(words),
               actor.first = min(sentence_id),
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

  ## Create bogus variables if sentiment is not scored
  if(!"sent_sum" %in% colnames(df)) {
    df <- df[,.(
      (.SD),
      sent_words = 0,
      sent_sum = 0
    )]
  }

  ## Unnest to sentence level

  ## Check if raw sentiment data contains actor ids
  if ('ids' %in% colnames(df)) {
    df <- df[,lapply(.SD, unlist, recursive=F),
             .SDcols = c('sentence_id', 'sent_sum', 'words', 'sent_words','ids'),
             by = list(id,publication_date,doctype)]
  } else {
    df <- df[,lapply(.SD, unlist, recursive=F),
             .SDcols = c('sentence_id', 'sent_sum', 'words', 'sent_words'),
             by = list(id,publication_date,doctype)]
  }

  df <- df[,.(
    (.SD),
    sent = sent_sum/words
  )][,.(
    (.SD),
    sent_binary = case_when(
      sent > pos_cutoff ~ 1,
      sent == 0 ~ 0,
      sent >= neg_cutoff & sent <= pos_cutoff ~ 0,
      TRUE ~ -1
    )
  )][,.(
    (.SD),
    sent_binary_weighted = sent_binary*words
  )]

  text_sent <- df[,
                      .(text.sent = sum(sent_binary_weighted)/sum(words),
                        text.sent_words = sum(sent_words),
                        text.words = sum(words),
                        text.arousal = sum(sent_words)/sum(words),
                        text.sentences = .N,
                        doctype = first(doctype),
                        publication_date = first(publication_date)
                      ), by = list(id)]

    ## Create aggregations according to list of actorId vectors in ids
  if(!is.null(actor_groups)) {
    output <- lapply(actor_groups,grouper, df = df) %>%
      rbindlist(.) %>%
      left_join(text_sent, by=c("id","publication_date")) %>%
      mutate(
        actor.prom = actor.occ/text.sentences,
        actor.rel_first = 1-(actor.first/text.sentences),
        year = strftime(publication_date, format = '%Y'),
        yearmonth = strftime(publication_date, format = '%Y%m'),
        yearmonthday = strftime(publication_date, format = '%Y%m%d'),
        yearweek = strftime(publication_date, format = "%Y%V")
      ) %>%
      mutate(across(where(is.character), as.factor)) %>%
      mutate(across(where(is.Date), as.factor))
    return(output)
  } else if(!is.null(actors_meta)) {
    text_noactors <- df[lengths(ids) == 0L,
                        .(noactor.sent = sum(sent_binary_weighted)/sum(words),
                          noactor.sent_words = sum(sent_words),
                          noactor.words = sum(words),
                          noactor.arousal = sum(sent_words)/sum(words),
                          noactor.first = min(sentence_id),
                          noactor.occ = .N
                        ), by = list(id)]

    all <- df[lengths(ids) > 0L,
              .(actor.sent = sum(sent_binary_weighted)/sum(words),
                actor.sent_words = sum(sent_words),
                actor.words = sum(words),
                actor.arousal = sum(sent_words)/sum(words),
                actor.first = min(sentence_id),
                actor.occ = .N,
                publication_date = first(publication_date),
                ids = 'all'), by = list(id)]

    ## Unnest to actor level
    df <- df[,.(ids = as.character(unlist(ids))),
             by = list(id,publication_date,sentence_id, sent_sum, words, sent_words,sent_binary_weighted)
            ][ # Remove deprecated actor_partyids from ES database
             !str_ends(ids, '_a')]

    ## Prepare actor metadata
    colnames(actors_meta) <- str_replace(colnames(actors_meta),'_source.','')
    actors_meta <- data.table(actors_meta)[,
                               .((.SD),
                                 startDate = as.Date(startDate),
                                 endDate = as.Date(endDate),
                                 ids = ifelse(!is.na(actorId), actorId, partyId)
                               ), .SDcols = -c('_id','startDate','endDate','_index','_type','_score')
    ]

    ## Create table with partyIds by date and actorId to join by
    actors_party <- actors_meta %>%
      group_by(ids,partyId,startDate,endDate) %>%
      summarise() %>%
      na.omit() %>%
      ungroup() %>%
      data.table(.)

    ## Add partyId to each actorId without filtering parties out
    df <- df %>%
      # Fill partyId column for actor mentions
      actors_party[., c(colnames(.),'partyId'), # Join by actorId, within active period (start/endDate)
                   on = .(ids == ids, startDate <= publication_date, endDate >= publication_date),
                   with = F] %>%
      # Fill partyId column for party mentions
      .[is.na(partyId), partyId:=str_sub(ids, start = 1, end = -3)] %>%
      # Some actors seemingly belong to different parties on the same day, hence basing unique rows on both (actor)ids and partyId
      .[!duplicated(.,by = c('id','ids','sentence_id','partyId')),] # Keep all unique rows

    ## Removing sentences containing more than one party
    if(single_party) {
      # Create variable indicating number of unique party ids per sentence, and keep only sentences where unique parties == 1
      df <- df %>%
        .[, upid := length(unique(partyId)), by = c('id','sentence_id')] %>%
        .[upid == 1,]
    }

    ## Create aggregate measures for individual actors
    actors_merged <- df[str_starts(ids, 'A_')] %>% .[!duplicated(.,by = c('id','ids','sentence_id')),] %>% # Removing duplicate rows when actor is counted multiple times in the same sentence, because of multiple functions or parties.
      .[,
        .(actor.sent = sum(sent_binary_weighted)/sum(words),
          actor.sent_words = sum(sent_words),
          actor.words = sum(words),
          actor.arousal = sum(sent_words)/sum(words),
          actor.first = min(sentence_id),
          actor.occ = .N,
          publication_date = first(publication_date)), by = list(id, ids)]

    ## Create actor metadata dataframe per active date (one row per day per actor)
    actors_merged <- actors_meta[actors_merged,
                           c('x.startDate','x.endDate',colnames(actors_merged), 'lastName','firstName','function.','gender','yearOfBirth','parlPeriod','partyId','ministerName','ministryId','actorId','startDate','endDate'),
                           on =.(ids = ids, startDate <= publication_date, endDate >= publication_date),
                           mult = 'all',
                           with = F][,.(
                             startDate = x.startDate,
                             endDate = x.endDate,
                             (.SD)
                           ), .SDcols = -c('x.startDate', 'x.endDate','startDate','endDate')]
    ## Generate party-actor aggregations (mfsa)

    # Create party data table
    parties_actors <- df %>%
    .[!duplicated(.,by = c('id','partyId','sentence_id')),.( # Remove rows (sentences) where a party is counted multiple times
      actor.sent = sum(sent_binary_weighted)/sum(words),
      actor.sent_words = sum(sent_words),
      actor.words = sum(words),
      actor.arousal = sum(sent_words)/sum(words),
      actor.first = min(sentence_id),
      actor.occ = .N,
      publication_date = first(publication_date)
    ), by = c('id','partyId')] # Summarize by article and partyId
    # Add party metadata
    parties_actors <- actors_meta[str_starts(ids, 'P_')][parties_actors, on = c('partyId'), mult = 'first'][!is.na(id),.(ids = str_c(partyId,"_mfsa"), (.SD)), .SDcols = -c('ids')]

    ## Generate party aggregations (mfs)
    parties <- df[str_ends(ids,'_f') | str_ends(ids,'_s'),.(
      ids = str_sub(ids, start = 1, end = -3),
      (.SD)
    ),.SDcols = -c('ids')] %>% .[!duplicated(.,by = c('id','ids','sentence_id')),.(
      actor.sent = sum(sent_binary_weighted)/sum(words),
      actor.sent_words = sum(sent_words),
      actor.words = sum(words),
      actor.arousal = sum(sent_words)/sum(words),
      actor.first = min(sentence_id),
      actor.occ = .N,
      publication_date = first(publication_date)
    ), by = c('id','ids')]
    parties <- actors_meta[parties, on = c('ids'), mult = 'first'][!is.na(id),.(ids = str_c(ids,"_mfs"), (.SD)), .SDcols = -c('ids')]

    ## Join all aggregations into a single data frame, compute derived actor-level measures, and add date dummies
    df <- bind_rows(actors_merged, parties, parties_actors, all) %>%
      left_join(.,text_sent, by=c("id","publication_date")) %>%
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
      select(-contains('Search'),-starts_with('not')) %>%
      mutate(across(where(is.character), as.factor)) %>%
      mutate(across(where(is.Date), as.factor))
    return(df)
  } else {
    df <- text_sent %>%
      mutate(
        year = strftime(publication_date, format = '%Y'),
        yearmonth = strftime(publication_date, format = '%Y%m'),
        yearmonthday = strftime(publication_date, format = '%Y%m%d'),
        yearweek = strftime(publication_date, format = "%Y%V")
      ) %>%
      ungroup() %>%
      mutate(across(where(is.character), as.factor)) %>%
      mutate(across(where(is.Date), as.factor))
    return(df)
  }
}

