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
actor_aggregation <- function(row, actors, es_pwd, localhost, default_operator = 'OR') {
  actor <- actors[row,]
  if (actor$`_source.function` == "Party"){
    years = seq(2000,2019,1)
  } else {
    years = c(0)
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
    ### Functions
    aggregator <- function (id, duplicates) {
      article <- filter(duplicates, `_id` == id) %>%
        unnest(sentence_id, .preserve = colnames(.))

      occ <- length(unlist(unique(article$sentence_id1)))
      sentence_count <- round(article$occ[[1]]/article$prom[[1]])
      prom <- occ/sentence_count
      rel_first <- 1-(min(article$sentence_id1)/sentence_count)
      return(bind_cols(as.list(article[1,1:6]), # Sentence id, start and end position for actor sentences
                       data.frame(occ = I(list(occ)), # Number of sentences in which actor occurs
                                  prom = I(list(prom)), # Relative prominence of actor in article (number of occurences/total # sentences)
                                  rel_first = I(list(rel_first)), # Relative position of first occurence at sentence level
                                  first = I(list(min(article$sentence_id1))) # First sentence in which actor is mentioned
                       )
      )
      )
    }
    if (year > 0) {
      query <- paste0('computerCodes.actors:(',paste(actorids, collapse = ' '),') && publication_date:[',year,'-01-01 TO ',year,'-12-31]')
    } else {
      query <- paste0('computerCodes.actors:(',paste(actorids, collapse = ' '),') && publication_date:[',actor$`_source.startDate`,' TO ',actor$`_source.endDate`,']')
    }
    Sys.sleep(runif(1,0.1,0.3))
    out <- elasticizer(query_string(paste0('country:',actor$`_source.country`,' && ',query),
                                    fields = c('computerCodes.actorsDetail', 'doctype', 'publication_date'), default_operator = default_operator),
                       localhost = localhost,
                       es_pwd = es_pwd)
    if (length(out$`_id`) > 0 ) {
      ### Generating actor dataframe, unnest by actorsDetail, then by actor ids. Filter out non-relevant actor ids.
      actor_df <- out %>%
        unnest() %>%
        unnest(ids, .preserve = colnames(.)) %>%
        filter(ids1 %in% actorids) %>%
        select(-ends_with('start')) %>%
        select(-ends_with('end')) %>%
        select(-starts_with('ids'))

      ### Only if there are more rows than articles, recalculate
      if (length(unique(actor_df$`_id`)) != length(actor_df$`_id`)) {
        duplicates <- actor_df[(duplicated(actor_df$`_id`) | duplicated(actor_df$`_id`, fromLast = T)),]
        actor_single <- actor_df[!(duplicated(actor_df$`_id`) | duplicated(actor_df$`_id`, fromLast = T)),]
        art_id <- unique(duplicates$`_id`)
        dupe_merged <- bind_rows(lapply(art_id, aggregator, duplicates = duplicates))
        actor_df <- bind_rows(dupe_merged, actor_single)
      }

      ### Creating date grouping variables
      actor_df <- actor_df %>%
        mutate(
          year = strftime(`_source.publication_date`, format = '%Y'),
          yearmonth = strftime(actor_df$`_source.publication_date`, format = '%Y%m'),
          yearmonthday = strftime(actor_df$`_source.publication_date`, format = '%Y%m%d'),
          yearweek = strftime(actor_df$`_source.publication_date`, format = "%Y%V")
        )
      ### Creating aggregate measuers at daily, weekly, monthly and yearly level
      grouper <- function(level) {
        by_newspaper <- actor_df %>% group_by_at(vars(level, `_source.doctype`)) %>%
          summarise(
            occ = mean(unlist(occ)),
            prom = mean(unlist(prom)),
            rel_first = mean(unlist(rel_first)),
            first = mean(unlist(first)),
            articles = length(`_id`),
            level = level
          )

        aggregate <- actor_df %>% group_by_at(vars(level)) %>%
          summarise(
            occ = mean(unlist(occ)),
            prom = mean(unlist(prom)),
            rel_first = mean(unlist(rel_first)),
            first = mean(unlist(first)),
            articles = length(`_id`),
            `_source.doctype` = 'agg',
            level = level
          )
        output <- bind_rows(by_newspaper, aggregate) %>%
          bind_cols(.,bind_rows(actor)[rep(seq_len(nrow(bind_rows(actor))), each=nrow(.)),])
        return(output)
      }
      levels <- c('year','yearmonth','yearmonthday','yearweek')
      aggregate_data <- bind_rows(lapply(levels, grouper))
      return(aggregate_data)
    } else {
      return()
    }
  }

  saveRDS(bind_rows(lapply(years, actor_aggregator, query, actor, actorids, default_operator, localhost, es_pwd)), file = paste0(actor$`_source.country`,'_',paste0(actorids,collapse = ''),'.Rds'))
  print(paste0('Done with ',row,'/',nrow(actors),' actors'))
  return()
}

