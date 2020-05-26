#' Aggregate sentence-level dataset containing actors (from sentencizer())
#'
#' Aggregate sentence-level dataset containing actors (from sentencizer())
#' @param df Data frame with actor ids, produced by sentencizer
#' @param actors_meta Data frame containing actor metadata obtained using elasticizer(index="actors")
#' @param ids Optional list of vectors, where each vector contains actor ids to be merged (e.g. merge all left-wing parties)
#' @return When no ids, returns actor-article dataset with individual actors, party aggregations, party-actor aggregations and overall actor sentiment (regardless of specific actors). When ids, returns aggregations for each vector in list
#' @export
#' @examples
#' actor_merger(df, actors_meta, ids = NULL)
#################################################################################################
#################################### Generate actor-article dataset #############################
#################################################################################################

actor_merger <- function(df, actors_meta, ids = NULL) {
  grouper <- function(id, df) {
    return(df %>%
      rowwise() %>%
      filter(length(intersect(id,ids)) > 0) %>%
      group_by(`_id`) %>%
      summarise(actor.sent = sum(sent_sum)/sum(words),
                actor.sent_sum = sum(sent_sum),
                actor.sent_words = sum(sent_words),
                actor.words = sum(words),
                actor.arousal = sum(sent_words)/sum(words),
                actor.first = first(sentence_id),
                actor.occ = n(),
                publication_date = as.Date(first(`_source.publication_date`)),
                doctype = first(`_source.doctype`)) %>%
      mutate(
        ids = str_c(id, collapse = '-')
      )
    )
  }

  ## Remove some of the metadata from the source df
  text_sent <- df %>%
    select(`_id`,starts_with("text."),-ends_with("sent_lemmas"))
  df <- df %>%
    select(-ends_with("sent_lemmas"),-starts_with("text.")) %>%
    unnest(cols = colnames(.)) ## Unnest to sentence level

  ## Create bogus variables if sentiment is not scored
  if(!"sent_sum" %in% colnames(df)) {
    df <- df %>%
      mutate(
        sent_words = 0,
        sent_sum = 0,
      )
  }
  ## Create aggregations according to list of actorId vectors in ids
  if(!is.null(ids)) {
    output <- lapply(ids,grouper, df = df) %>%
      bind_rows(.) %>%
      left_join(text_sent, by="_id") %>%
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
    all <- df %>%
      rowwise() %>%
      filter(!is.null(unlist(ids))) %>%
      group_by(`_id`) %>%
      summarise(actor.sent = sum(sent_sum)/sum(words),
                actor.sent_sum = sum(sent_sum),
                actor.sent_words = sum(sent_words),
                actor.words = sum(words),
                actor.arousal = sum(sent_words)/sum(words),
                actor.first = first(sentence_id),
                actor.occ = n(),
                publication_date = as.Date(first(`_source.publication_date`)),
                doctype = first(`_source.doctype`)) %>%
      mutate(
        ids = "all"
      )

    df <- df %>%
      unnest(cols = ids) %>% ## Unnest to actor level
      mutate(
        `_source.publication_date` = as.Date(`_source.publication_date`)
      )
    ## Create aggregate measures for individual actors
    actors <- df %>%
      filter(str_starts(ids,"A_")) %>%
      group_by(`_id`,ids) %>%
      summarise(actor.sent = sum(sent_sum)/sum(words),
                actor.sent_sum = sum(sent_sum),
                actor.sent_words = sum(sent_words),
                actor.words = sum(words),
                actor.arousal = sum(sent_words)/sum(words),
                actor.first = first(sentence_id),
                actor.occ = n(),
                publication_date = first(`_source.publication_date`),
                doctype = first(`_source.doctype`)
      )
    ## Create actor metadata dataframe per active date (one row per day per actor)
    colnames(actors_meta) <- str_replace(colnames(actors_meta),'_source.','')
    actors_meta_bydate <- actors_meta %>%
      mutate(
        startDate = as.Date(startDate),
        endDate = as.Date(endDate)
      ) %>%
      select(
        lastName,firstName,`function`,gender,yearOfBirth,parlPeriod,partyId,ministerName,ministryId,actorId,startDate,endDate
      ) %>%
      rowwise() %>%
      mutate(
        publication_date = list(seq(from=startDate, to=endDate,by="day")),
        ids = actorId
      ) %>%
      unnest(cols=publication_date)

    ## Join the actor metadata with the article data by actor id and date
    actors <- actors %>%
      left_join(.,actors_meta_bydate, by=c("ids","publication_date"))

    ## Generate party-actor aggregations (mfsa)
    parties_actors <- df %>%
      filter(str_starts(ids,"P_")) %>%
      mutate(
        ids = str_sub(ids, start = 1, end = -3)
      ) %>%
      group_by(`_id`,ids) %>%
      summarise(actor.sent = sum(sent_sum)/sum(words),
                actor.sent_sum = sum(sent_sum),
                actor.sent_words = sum(sent_words),
                actor.words = sum(words),
                actor.arousal = sum(sent_words)/sum(words),
                actor.first = first(sentence_id),
                actor.occ = n(),
                publication_date = first(`_source.publication_date`),
                doctype = first(`_source.doctype`)) %>%
      mutate(
        ids = str_c(ids,"_mfsa")
      )

    ## Generate party aggregations (mfs)
    parties <- df %>%
      filter(str_ends(ids,"_f") | str_ends(ids,"_s")) %>%
      mutate(
        ids = str_sub(ids, start = 1, end = -3)
      ) %>%
      group_by(`_id`,ids) %>%
      summarise(actor.sent = sum(sent_sum)/sum(words),
                actor.sent_sum = sum(sent_sum),
                actor.sent_words = sum(sent_words),
                actor.words = sum(words),
                actor.arousal = sum(sent_words)/sum(words),
                actor.first = first(sentence_id),
                actor.occ = n(),
                publication_date = first(`_source.publication_date`),
                doctype = first(`_source.doctype`)) %>%
      mutate(
        ids = str_c(ids,"_mfs")
      )

    ## Join all aggregations into a single data frame, compute derived actor-level measures, and add date dummies
    df <- bind_rows(actors, parties, parties_actors, all) %>%
      left_join(text_sent, by="_id") %>%
      mutate(
        actor.prom = actor.occ/text.sentences,
        actor.rel_first = 1-(actor.first/text.sentences),
        year = strftime(publication_date, format = '%Y'),
        yearmonth = strftime(publication_date, format = '%Y%m'),
        yearmonthday = strftime(publication_date, format = '%Y%m%d'),
        yearweek = strftime(publication_date, format = "%Y%V")
      )
    return(df)
  }
}

