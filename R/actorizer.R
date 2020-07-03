#' Updater function for elasticizer: Conduct actor searches
#'
#' Updater function for elasticizer: Conduct actor searches
#' @param out Does not need to be defined explicitly! (is already parsed in the elasticizer function)
#' @param localhost Defaults to false. When true, connect to a local Elasticsearch instance on the default port (9200)
#' @param ids List of actor ids
#' @param prefix Regex containing prefixes that should be excluded from hits
#' @param postfix Regex containing postfixes that should be excluded from hits
#' @param identifier String used to mark highlights. Should be a lowercase string
#' @param ver Short string (preferably a single word/sequence) indicating the version of the updated document (i.e. for a udpipe update this string might be 'udV2')
#' @param es_super Password for write access to ElasticSearch
#' @return As this is a nested function used within elasticizer, there is no return output
#' @export
#' @examples
#' actorizer(out, localhost = F, ids, prefix, postfix, identifier, es_super)
actorizer <- function(out, localhost = F, ids, prefix, postfix, pre_tags, post_tags, es_super, ver) {
  offsetter <- function(x, pre_tags, post_tags) {
    return(as.list(as.data.frame(x-((row(x)-1)*(nchar(pre_tags)+nchar(post_tags))))))
  }

  out <- mamlr:::out_parser(out, field = 'highlight', clean = F)

  prefix[prefix==''] <- NA
  postfix[postfix==''] <- NA
  pre_tags_regex <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", pre_tags)
  post_tags_regex <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", post_tags)
  out$markers <- lapply(str_locate_all(out$merged,coll(pre_tags)), offsetter, pre_tags = pre_tags, post_tags = post_tags)
  markers <- out %>%
    select(`_id`,markers) %>%
    unnest_wider(markers) %>%
    rename(marker_start = start, marker_end = end) %>%
    unnest(colnames(.))

  if (sum(nchar(out$merged) > 990000) > 0) {
    stop("One or more documents in this batch exceed 990000 characters")
  }
  # Extracting ud output from document
  ud <- out %>%
    select(`_id`,`_source.ud`, merged) %>%
    unnest(cols = c("_source.ud")) %>%
    select(`_id`,lemma,start,end, sentence_id,merged) %>%
    unnest(cols = colnames(.))

  sentences <- ud %>%
    group_by(`_id`, sentence_id) %>%
    summarise(
      sentence_start = min(start),
      sentence_end = max(end)
    ) %>%
    mutate(
      sentence_count = n()
    )
  hits <- as.data.table(ud)[as.data.table(markers), .(`_id`, lemma,x.start, start, end, x.end, sentence_id, merged), on =.(`_id` = `_id`, start <= marker_start, end >= marker_start)] %>%
    mutate(end = x.end,
           start = x.start) %>%
    select(`_id`, sentence_id, start, end,merged) %>%
    group_by(`_id`,sentence_id) %>%
    summarise(
      actor_start = I(list(start)),
      actor_end = I(list(end)),
      n_markers = length(start),
      merged = first(merged)
    ) %>%
    left_join(.,sentences, by=c('_id','sentence_id')) %>%
    ungroup %>%
    arrange(`_id`,sentence_id) %>%
    group_by(`_id`) %>%
    mutate(n_markers = cumsum(n_markers)) %>%
    mutate(
      sentence_start_tags = sentence_start+((nchar(pre_tags)+nchar(post_tags))*(lag(n_markers, default = 0))),
      sentence_end_tags = sentence_end+((nchar(pre_tags)+nchar(post_tags))*(n_markers))
    ) %>%
    mutate(
      sentence = paste0(' ',str_sub(merged, sentence_start_tags, sentence_end_tags),' ')
    ) %>%
    select(-merged) %>%
    ungroup()
  # Conducting regex filtering on matches only when there is a prefix and/or postfix to apply
  if (!is.na(prefix) || !is.na(postfix)) {
    ### If no pre or postfixes, match *not nothing* i.e. anything
    if (is.na(prefix)) {
      prefix = '$^'
    }
    if (is.na(postfix)) {
      postfix = '$^'
    }
    hits <- hits %>%
      filter(
        !str_detect(sentence, paste0(post_tags_regex,'(',postfix,')')) & !str_detect(sentence, paste0('(',prefix,')',pre_tags_regex))
      )
  }

  hits <- hits %>%
    group_by(`_id`) %>%
    summarise(
      sentence_id = list(as.integer(sentence_id)),
      sentence_start = list(sentence_start),
      sentence_end = list(sentence_end),
      actor_start = I(list(unlist(actor_start))), # List of actor ud token start positions
      actor_end = I(list(unlist(actor_end))), # List of actor ud token end positions
      occ = length(unique(unlist(sentence_id))), # Number of sentences in which actor occurs
      first = min(unlist(sentence_id)), # First sentence in which actor is mentioned
      ids = I(list(ids)),
      sentence_count = first(sentence_count)# List of actor ids
    ) %>%
    mutate(
      prom = occ/sentence_count, # Relative prominence of actor in article (number of occurrences/total # sentences)
      rel_first = 1-(first/sentence_count), # Relative position of first occurrence at sentence level
    ) %>%
    select(`_id`:occ, prom,rel_first,first,ids)

  if (nrow(hits) == 0) {
    print("Nothing to update for this batch")
    return(NULL)
  } else {
    bulk <- apply(hits, 1, bulk_writer, varname ='actorsDetail', type = 'add', ver = ver)
    bulk <- c(bulk,apply(hits[c(1,11)], 1, bulk_writer, varname='actors', type = 'add', ver = ver))
    return(elastic_update(bulk, es_super = es_super, localhost = localhost))
  }

}
