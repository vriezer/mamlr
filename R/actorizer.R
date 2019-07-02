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
#' @param cores Number of cores to use for parallel processing, defaults to cores (all cores available)
#' @return As this is a nested function used within elasticizer, there is no return output
#' @export
#' @examples
#' actorizer(out, localhost = F, ids, prefix, postfix, identifier, es_super)
actorizer <- function(out, localhost = F, ids, prefix, postfix, pre_tags, post_tags, es_super, ver, cores = 1) {
  plan(multiprocess, workers = cores)
  ### Function to filter out false positives using regex
  exceptionizer <- function(id, ud, doc, markers, pre_tags_regex, post_tags_regex,pre_tags,post_tags, prefix, postfix) {
    min <- min(ud$start[ud$sentence_id == id]) # Get start position of sentence
    max <- max(ud$end[ud$sentence_id == id]) # Get end position of sentence
    split <- markers[markers %in% seq(min, max, 1)] # Get markers in sentence
    min <- min+((nchar(pre_tags)+nchar(post_tags))*((match(split,markers))-1))
    max <- max+((nchar(pre_tags)+nchar(post_tags))*match(split,markers)) # Set end position to include markers (e.g if there are two markers of three characters in the sentence, the end position needs to be shifted by +6)
    sentence <- paste0(' ',str_sub(doc$merged, min, max),' ') # Extract sentence from text, adding whitespaces before and after for double negation (i.e. Con only when preceded by "("))

    # Check if none of the regexes match, if so, return sentence id, otherwise (if one of the regexes match) return nothing
    if (!str_detect(sentence, paste0(post_tags_regex,'(',postfix,')')) && !str_detect(sentence, paste0('(',prefix,')',pre_tags_regex))) {
      return(id)
    } else {
      return(NULL)
    }
  }
  ranger <- function(x, ud) {
    return(which((ud$start <= x) & (ud$end >= x)))
  }
  sentencizer <- function(row, out, ids, prefix, postfix, pre_tags, post_tags, pre_tags_regex, post_tags_regex) {
    doc <- out[row,]
    if (nchar(doc$merged) > 990000) {
      return(
        data.frame(
          err = T,
          errorMessage = "Merged document exceeded 990000 characters, highlighting possibly incorrect"
        )
      )
    }
    # Extracting ud output from document
    ud <- doc$`_source.ud`[[1]] %>%
      select(-one_of('exists')) %>% # Removing ud.exists variable
      unnest() %>%
      mutate(doc_id = doc$`_id`)
    markers <- doc$markers[[1]][,'start'] # Extract list of markers
    # Convert markers to udpipe rows (in some cases the start position doesn't align with the udpipe token start position (e.g. when anti-|||EU is treated as a single word))
    rows <- unlist(lapply(markers, ranger, ud = ud))

    # Setting up an actor variable
    ud$actor <- F
    ud$actor[rows] <- T

    sentence_count <- max(ud$sentence_id) # Number of sentences in article
    actor_sentences <- unique(ud$sentence_id[ud$actor]) # Sentence ids of sentences mentioning actor

    # Conducting regex filtering on matches only when there is a prefix and/or postfix to apply
    if (!is.na(prefix) || !is.na(postfix)) {
      ### If no pre or postfixes, match *not nothing* i.e. anything
      if (is.na(prefix)) {
        prefix = '$^'
      }
      if (is.na(postfix)) {
        postfix = '$^'
      }
      sentence_ids <- unlist(lapply(actor_sentences,
                             exceptionizer,
                             ud = ud,
                             doc = doc,
                             markers = markers,
                             pre_tags_regex = pre_tags_regex,
                             pre_tags = pre_tags,
                             post_tags_regex = post_tags_regex,
                             post_tags = post_tags,
                             prefix = prefix,
                             postfix = postfix))
    } else {
      sentence_ids <- actor_sentences
    }
    if (length(sentence_ids > 0)) {
      # Generating nested sentence start and end positions for actor sentences
      ud <- ud %>%
        filter(sentence_id %in% sentence_ids)
      actor_start <- ud$start[ud$actor == T] # Udpipe token start positions for actor
      actor_end <- ud$end[ud$actor == T] # Udpipe token end positions for actor
      ud <- ud %>%
        group_by(sentence_id) %>%
        summarise (
          sentence_start = as.integer(min(start)),
          sentence_end = as.integer(max(end)),
          doc_id = first(doc_id)
        ) %>%
        group_by(doc_id) %>%
        summarise(
          sentence_id = list(as.integer(sentence_id)),
          sentence_start = list(sentence_start),
          sentence_end = list(sentence_end)
        )

      return(
        data.frame(ud, # Sentence id, start and end position for actor sentences
                   actor_start = I(list(actor_start)), # List of actor ud token start positions
                   actor_end = I(list(actor_end)), # List of actor ud token end positions
                   occ = length(unique(sentence_ids)), # Number of sentences in which actor occurs
                   prom = length(unique(sentence_ids))/sentence_count, # Relative prominence of actor in article (number of occurences/total # sentences)
                   rel_first = 1-(min(sentence_ids)/sentence_count), # Relative position of first occurence at sentence level
                   first = min(sentence_ids), # First sentence in which actor is mentioned
                   ids = I(list(ids)) # List of actor ids
        )
      )
    } else {
      return(NULL)
    }

  }
  out <- mamlr:::out_parser(out, field = 'highlight', clean = F, cores = cores)
  offsetter <- function(x, pre_tags, post_tags) {
    return(x-((row(x)-1)*(nchar(pre_tags)+nchar(post_tags))))
  }
  prefix[prefix==''] <- NA
  postfix[postfix==''] <- NA
  pre_tags_regex <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", pre_tags)
  post_tags_regex <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", post_tags)
  out$markers <- future_lapply(str_locate_all(out$merged,coll(pre_tags)), offsetter, pre_tags = pre_tags, post_tags = post_tags)

  # ids <- fromJSON(ids)
  updates <- bind_rows(future_lapply(seq(1,length(out[[1]]),1), sentencizer,
                                out = out,
                                ids = ids,
                                postfix = postfix,
                                prefix=prefix,
                                pre_tags_regex = pre_tags_regex,
                                pre_tags = pre_tags,
                                post_tags_regex = post_tags_regex,
                                post_tags = post_tags))
  if (nrow(updates) == 0) {
    print("Nothing to update for this batch")
    return(NULL)
  } else {
    bulk <- apply(updates, 1, bulk_writer, varname ='actorsDetail', type = 'add', ver = ver)
    bulk <- c(bulk,apply(updates[c(1,11)], 1, bulk_writer, varname='actors', type = 'add', ver = ver))
    return(elastic_update(bulk, es_super = es_super, localhost = localhost))
  }

}
