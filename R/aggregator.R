#' Aggregator function, to aggregate actor results
#'
#' Aggregator function, to aggregate actor results
#' @param id Article id of the article for which actor aggregation should be done
#' @param actor_df The dataframe containing the actor data
#' @param merge_id The actorid that should be assigned to the merged result
#' @return A dataframe with the merged results
#' @export
#' @examples
#' aggregator(id, actor_df, merge_id)

aggregator <- function (id, actor_df, merge_id) {
article <- filter(actor_df, `_id` == id) %>%
  unnest(sentence_id, .preserve = colnames(.))
occ <- length(unlist(unique(article$sentence_id1)))
sentence_count <- round(article$occ[[1]]/article$prom[[1]])
prom <- occ/sentence_count
rel_first <- 1-(min(article$sentence_id1)/sentence_count)
actor_start <- sort(unique(unlist(article$actor_start)))
actor_end <- sort(unique(unlist(article$actor_end)))
sentence_start <- sort(unique(unlist(article$sentence_start)))
sentence_end <- sort(unique(unlist(article$sentence_end)))
sentence_id <- sort(unique(unlist(article$sentence_id)))

return(data.frame(doc_id = first(article$`_id`),
                  sentence_id = I(list(as.integer(sentence_id))),
                  sentence_start = I(list(sentence_start)),
                  sentence_end = I(list(sentence_end)),
                  actor_start = I(list(actor_start)), # List of actor ud token start positions
                  actor_end = I(list(actor_end)), # List of actor ud token end positions
                  occ = occ, # Number of sentences in which actor occurs
                  prom = prom, # Relative prominence of actor in article (number of occurences/total # sentences)
                  rel_first = rel_first, # Relative position of first occurence at sentence level
                  first = min(article$sentence_id1), # First sentence in which actor is mentioned
                  ids = merge_id, # List of actor ids
                  stringsAsFactors = F
)
)
}
