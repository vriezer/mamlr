### Notes:
# Do you want to search for either one OR other actorid, or both occuring in the same document?
# Do you want to keep only the occurences of the actorids you are searching for, or all actor occurences in the hits?
# Search by actorId, then aggregate by month
# When actorId starts with P_, define what hits you want to get (short, full, actor), if more than one, aggregate properly
# Develop query generator for specific actors (ie combine actorId with start and end dates)



#' Generate and store aggregate actor measures to elasticsearch
#'
#' Generate and store aggregate actor measures to elasticsearch
#' @param out The output provided by elasticizer()
#' @param localhost Boolean indicating if the script should run locally, or remote
#' @param es_super Write password for ES
#' @param actorids List of actorids used in the search, should be the same as the actorids used for elasticizer()
#' @param ver String indicating the version of the update
#' @return Return value is based on output of elastic_update()
#' @export
#' @examples
#' aggregator_elastic(out, localhost = F, actorids, ver, es_super)
#################################################################################################
#################################### Aggregate actor results ################################
#################################################################################################
aggregator_elastic <- function(out, localhost = F, actorids, ver, es_super) {
  ### Generating actor dataframe, unnest by actorsDetail, then by actor ids. Filter out non-relevant actor ids.
  partyid <- str_sub(actorids[1], end=-3)
  actor_df <- out %>%
    unnest() %>%
    unnest(ids, .preserve = colnames(.)) %>%
    filter(ids1 %in% actorids)

  agg_party_actors <- bind_rows(lapply(unique(actor_df$`_id`),
                                         mamlr:::aggregator,
                                         actor_df = actor_df,
                                         merge_id = paste0(partyid,'_mfsa')))

  party <- actor_df %>%
    filter(!endsWith(ids1, '_a'))
  agg_party <- bind_rows(lapply(unique(party$`_id`),
                                         mamlr:::aggregator,
                                         actor_df = party,
                                         merge_id = paste0(partyid,'_mfs')))

  actors_only <- actor_df %>%
    filter(endsWith(ids1, '_a'))
  agg_actors <- bind_rows(lapply(unique(actors_only$`_id`),
                                  mamlr:::aggregator,
                                  actor_df = actors_only,
                                  merge_id = paste0(partyid,'_ma')))
  df_out <- bind_rows(agg_party_actors, agg_party, agg_actors)
  doc_ids <- df_out$doc_id
  df_out <- df_out %>%
    select(-1) %>%
    split(as.factor(doc_ids))
  df_out <- data.frame(doc_id = names(df_out), list = I(df_out))
  bulk <- apply(df_out, 1, bulk_writer, varname ='actorsDetail', type = 'add', ver = ver)
  return(elastic_update(bulk, es_super = es_super, localhost = localhost))
}


