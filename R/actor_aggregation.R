### Notes:
# Do you want to search for either one OR other actorid, or both occuring in the same document?
# Do you want to keep only the occurences of the actorids you are searching for, or all actor occurences in the hits?
# Search by actorId, then aggregate by month
# When actorId starts with P_, define what hits you want to get (short, full, actor), if more than one, aggregate properly
# Develop query generator for specific actors (ie combine actorId with start and end dates)



#' Generate aggregated actor measures from raw data
#'
#' Generate aggregated actor measures from raw data
#' @param row The row of the actors data frame used for aggregation
#' @param actors The data frame containing actor data
#' @param es_pwd The password for read access to ES
#' @param localhost Boolean indicating if the script is running locally or not
#' @param default_operator String indicating whether actor aggregations should be made by searching for the presence of any of the actor ids (OR), or all of them (AND). Defaults to OR
#' @return No return value, data per actor is saved in an RDS file
#' @export
#' @examples
#' actor_aggregation(row, actors, es_pwd, localhost, default_operator = 'OR')
#################################################################################################
#################################### Aggregate actor results ################################
#################################################################################################
actor_aggregation <- function(row, actors, es_pwd, localhost, default_operator = 'OR', sent_dict = NULL, cores = detectCores()) {
  ### Functions
  aggregator <- function (id, duplicates) {
    df <- duplicates %>%
      filter(`_id` == id) %>%
      group_by(`_id`) %>%
      summarise(
        `_source.doctype` = first(`_source.doctype`),
        `_source.publication_date` = first(`_source.publication_date`),
        actor_end = list(sort(unique(unlist(actor_end)))),
        prom = list(length(unique(unlist(sentence_id)))/round(occ[[1]]/prom[[1]])),
        sentence_id = list(sort(unique(unlist(sentence_id)))),
        rel_first = list(max(unlist(rel_first))),
        sentence_end = list(sort(unique(unlist(sentence_end)))),
        actor_start = list(sort(unique(unlist(actor_start)))),
        ids = list(sort(unique(unlist(ids)))),
        sentence_start = list(sort(unique(unlist(sentence_start)))),
        occ = list(length(unique(unlist(sentence_id)))),
        first = list(min(unlist(sentence_id)))
      )
    return(df)
  }
  ### Calculate sentiment scores for each actor-document
  par_sent <- function(row, out, sent_dict) {
    out_row <- out[row,]
    ### Contains sentiment per sentence for whole article
    sentiment_ud <- out_row$`_source.ud`[[1]] %>%
      select(-one_of('exists')) %>%
      unnest() %>%
      filter(upos != 'PUNCT') %>% # For getting proper word counts
      mutate(V1 = str_c(lemma,'_',upos)) %>%
      left_join(sent_dict, by = 'V1') %>%
      ### Setting binary sentiment as unit of analysis
      mutate(V2 = V3) %>%
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
    out_row <- select(out_row, -`_source.ud`)
    ### Contains sentiment per sentence for actor
    actor_tone <- filter(sentiment_ud, sentence_id %in% unlist(out_row$sentence_id))

    ### Aggregated sentiment per actor (over all sentences containing actor)
    actor <- summarise(actor_tone,
                       sent = sum(sent_sum)/sum(words),
                       sent_sum = sum(sent_sum),
                       sent_words = sum(sent_words),
                       words = sum(words),
                       arousal = sum(sent_words)/sum(words)
    )

    ### Aggregated sentiment per article (over all sentences in article)
    text <- summarise(sentiment_ud,
                      sent = sum(sent_sum)/sum(words),
                      sent_sum = sum(sent_sum),
                      sent_words = sum(sent_words),
                      words = sum(words),
                      arousal = sum(sent_words)/sum(words)
    )
    return(cbind(out_row,data.frame(actor = actor, text = text)))
  }


  ### Creating aggregate measuers at daily, weekly, monthly and yearly level
  grouper <- function(level, out, actorids, sent = F) {
    by_newspaper <- out %>%
      mutate(
        sentence_count = round(unlist(occ)/unlist(prom))
      ) %>%
      group_by_at(vars(level, `_source.doctype`)) %>%
      summarise(
        occ = sum(unlist(occ)),
        prom_art = mean(unlist(prom)),
        rel_first_art = mean(unlist(rel_first)),
        first = mean(unlist(first)),
        sentence_count = sum(sentence_count),
        articles = length(`_id`),
        level = level
      ) %>%
      ungroup()
    aggregate <- out %>%
      mutate(
        sentence_count = round(unlist(occ)/unlist(prom))
      ) %>%
      group_by_at(vars(level)) %>%
      summarise(
        occ = sum(unlist(occ)),
        prom_art = mean(unlist(prom)),
        rel_first_art = mean(unlist(rel_first)),
        first = mean(unlist(first)),
        sentence_count = sum(sentence_count),
        articles = length(`_id`),
        `_source.doctype` = 'agg',
        level = level
      ) %>%
      ungroup()

    if(sent == T) {
      by_newspaper_sent <- out %>%
        group_by_at(vars(level, `_source.doctype`)) %>%
        summarise(
          actor.sent = mean(actor.sent),
          actor.sent_sum = sum(actor.sent_sum),
          actor.sent_words = sum(actor.sent_words),
          actor.words = sum(actor.words),
          actor.arousal = mean(actor.arousal),
          text.sent = mean(text.sent),
          text.sent_sum = sum(text.sent_sum),
          text.sent_words = sum(text.sent_words),
          text.words = sum(text.words),
          text.arousal = mean(text.arousal)
        ) %>%
        ungroup() %>%
        select(-level,-`_source.doctype`)
      aggregate_sent <- out %>%
        group_by_at(vars(level)) %>%
        summarise(
          actor.sent = mean(actor.sent),
          actor.sent_sum = sum(actor.sent_sum),
          actor.sent_words = sum(actor.sent_words),
          actor.words = sum(actor.words),
          actor.arousal = mean(actor.arousal),
          text.sent = mean(text.sent),
          text.sent_sum = sum(text.sent_sum),
          text.sent_words = sum(text.sent_words),
          text.words = sum(text.words),
          text.arousal = mean(text.arousal)
        ) %>%
        ungroup() %>%
        select(-level)
      aggregate <- bind_cols(aggregate,aggregate_sent)
      by_newspaper <- bind_cols(by_newspaper, by_newspaper_sent)
    }
    output <- bind_rows(by_newspaper, aggregate) %>%
      bind_cols(.,bind_rows(actor)[rep(seq_len(nrow(bind_rows(actor))), each=nrow(.)),]) %>%
      select(
        -`_index`,
        -`_type`,
        -`_score`,
        -`_id`,
        -contains('Search'),
        -contains('not')
      )
    colnames(output) <- gsub("_source.",'', colnames(output))
    return(output)
  }
  ###########################################################################################
  if (is.null(sent_dict) == F) {
    fields <- c('ud','computerCodes.actorsDetail', 'doctype', 'publication_date')
  } else {
    fields <- c('computerCodes.actorsDetail', 'doctype', 'publication_date')
  }
  actor <- actors[row,]
  if (actor$`_source.function` == "Party"){
    years = seq(2000,2019,1)
    startDate <- '2000'
    endDate <- '2019'
  } else {
    years = c(0)
    startDate <- actor$`_source.startDate`
    endDate <- actor$`_source.endDate`
  }
  if (actor$`_source.function` == 'Party' && actor$party_only == T) {
    actorids <- c(paste0(actor$`_source.partyId`,'_s'), paste0(actor$`_source.partyId`,'_f'))
  } else if (actor$`_source.function` == 'Party') {
    actorids <- c(paste0(actor$`_source.partyId`,'_s'), paste0(actor$`_source.partyId`,'_f'), paste0(actor$`_source.partyId`,'_a'))
    actor$party_only <- F
  } else {
    actorids <- actor$`_source.actorId`
    actor$party_only <- NULL
  }

  actor_aggregator <- function(year, query, actor, actorids, default_operator, localhost = F, es_pwd) {
    if (year > 0) {
      query <- paste0('computerCodes.actors:(',paste(actorids, collapse = ' '),') && publication_date:[',year,'-01-01 TO ',year,'-12-31]')
    } else {
      query <- paste0('computerCodes.actors:(',paste(actorids, collapse = ' '),') && publication_date:[',actor$`_source.startDate`,' TO ',actor$`_source.endDate`,']')
    }

    ### Temporary exception for UK missing junk coding
    if (actor$`_source.country` != 'uk') {
      query <- paste0(query,' && computerCodes.junk:0')
    }

    out <- elasticizer(query_string(paste0('country:',actor$`_source.country`,' && ',query),
                                    fields = fields, default_operator = default_operator),
                       localhost = localhost,
                       es_pwd = es_pwd)
    if (length(out$`_id`) > 0 ) {
      if (is.null(sent_dict) == F) {
        out_ud <- out %>% select(`_id`,`_source.ud`)
        out <- out %>% select(-`_source.ud`)
      }
      ### Generating actor dataframe, unnest by actorsDetail, then by actor ids. Filter out non-relevant actor ids.
      out <- out %>%
        unnest(`_source.computerCodes.actorsDetail`, .preserve = colnames(.)) %>%
        unnest(ids, .preserve = colnames(.)) %>%
        filter(ids1 %in% actorids) # %>%
      # select(-ends_with('start')) %>%
      # select(-ends_with('end')) %>%
      # select(-starts_with('ids'))

      ### Only if there are more rows than articles, recalculate
      if (length(unique(out$`_id`)) != length(out$`_id`)) {
        duplicates <- out[(duplicated(out$`_id`) | duplicated(out$`_id`, fromLast = T)),]
        actor_single <- out[!(duplicated(out$`_id`) | duplicated(out$`_id`, fromLast = T)),]
        art_id <- unique(duplicates$`_id`)
        dupe_merged <- bind_rows(mclapply(art_id, aggregator, duplicates = duplicates, mc.cores = cores))
        out <- bind_rows(dupe_merged, actor_single)
      }

      if (is.null(sent_dict) == F) {
        out <- left_join(out, out_ud, by = '_id')
        out <- bind_rows(mclapply(seq(1,nrow(out),1),par_sent, out = out, sent_dict = sent_dict, mc.cores = cores))
      }

      ### Creating date grouping variables
      out <- out %>%
        mutate(
          year = strftime(`_source.publication_date`, format = '%Y'),
          yearmonth = strftime(out$`_source.publication_date`, format = '%Y%m'),
          yearmonthday = strftime(out$`_source.publication_date`, format = '%Y%m%d'),
          yearweek = strftime(out$`_source.publication_date`, format = "%Y%V")
        ) %>%
        select(
          -`_score`,
          -`_index`,
          -`_type`,
          -`_score`,
          -`_source.computerCodes.actorsDetail`,
          -ids1
        )
      levels <- c('year','yearmonth','yearmonthday','yearweek')
      aggregate_data <- bind_rows(lapply(levels, grouper, out = out, actorids = actorids, sent = !is.null(sent_dict)))
      return(aggregate_data)
    } else {
      return()
    }
  }
  saveRDS(bind_rows(lapply(years, actor_aggregator, query, actor, actorids, default_operator, localhost, es_pwd)), file = paste0(actor$`_source.country`,'_',paste0(actorids,collapse = ''),actor$`_source.function`,startDate,endDate,'.Rds'))
  print(paste0('Done with ',row,'/',nrow(actors),' actors'))
  return()
}
