#' Updater function for elasticizer: Conduct actor searches
#'
#' Updater function for elasticizer: Conduct actor searches
#' @param out Does not need to be defined explicitly! (is already parsed in the elasticizer function)
#' @param localhost Defaults to false. When true, connect to a local Elasticsearch instance on the default port (9200)
#' @param ids List of actor ids
#' @param prefix Regex containing prefixes that should be excluded from hits
#' @param postfix Regex containing postfixes that should be excluded from hits
#' @param identifier String used to mark highlights. Should be a lowercase string
#' @param udmodel The udpipe model used for parsing every hit
#' @param ver Short string (preferably a single word/sequence) indicating the version of the updated document (i.e. for a udpipe update this string might be 'udV2')
#' @param es_super Password for write access to ElasticSearch
#' @return As this is a nested function used within elasticizer, there is no return output
#' @export
#' @examples
#' actorizer(out, localhost = F, ids, type, prefix, postfix, identifier, udmodel, es_super)
actorizer <- function(out, localhost = F, ids, type, prefix, postfix, identifier, udmodel, es_super, ver) {
  exceptionizer <- function(id, ud, doc, markers, regex_identifier, prefix, postfix) {
    min <- min(ud$start[ud$sentence_id == id])
    max <- max(ud$end[ud$sentence_id == id])
    split <- markers[markers %in% seq(min, max, 1)]
    max <- max+(length(split)*nchar(identifier))
    sentence <- str_sub(doc$highlight, min, max)
    if (!str_detect(sentence, paste0(regex_identifier,postfix)) && !str_detect(sentence, paste0(prefix,regex_identifier))) {
      return(id)
    } else {
      return(NULL)
    }
  }
  sentencizer <- function(row, out, udmodel, ids, prefix, postfix, identifier, type) {
    print(row)
    ### If no pre or postfixes, match *not nothing* i.e. anything
    if (is.na(prefix) || prefix == '') {
      prefix = '$^'
    }
    if (is.na(postfix) || postfix == '') {
      postfix = '$^'
    }
    ### Also needs fix for empty strings (non-NA)
    err <- F
    doc <- out[row,]
    ud <- doc$`_source.ud`[[1]] %>%
      select(-one_of('exists')) %>% # Removing ud.exists variable
      unnest() %>%
      mutate(doc_id = doc$`_id`)
    # ud <- as.data.frame(udpipe(udmodel, x = doc$merged, parser = "none", doc_id = doc$`_id`))
    markers <- doc$markers[[1]][,'start']
    if (length(setdiff(markers,ud$start)) > 0) {
      err <- T
      ud <- ud %>%
        group_by(doc_id) %>%
        summarise(
          sentence_id = list(list(as.integer(0))),
          sentence_start = list(list(0)),
          sentence_end = list(list(0))
        )
      occurences <- 0
      prominence <- 0
      rel_first <- 0

      return(data.frame(ud,actor_start = I(list(list(markers))), occ = occurences,prom = prominence,rel_first = rel_first, ids = I(list(list(ids))), err = err))
    }
    ud$actor[ud$start %in% markers] <- T
    sentence_count <- length(unique(ud$sentence_id))
    actor_sentences <- unique(na.omit(ud$sentence_id[ud$actor == T]))
    if (type == "Party") {
      sentence_ids <- lapply(actor_sentences, exceptionizer, ud = ud, doc = doc, markers = markers, regex_identifier = regex_identifier, prefix = prefix, postfix = postfix)
    } else {
      sentence_ids <- actor_sentences
    }

    ud <- ud %>%
      filter(sentence_id %in% sentence_ids) %>%
      group_by(sentence_id) %>%
      summarise (
        sentence_start = as.integer(min(start)),
        sentence_end = as.integer(max(end)),
        doc_id = first(doc_id)
      ) %>%
      group_by(doc_id) %>%
      summarise(
        sentence_id = list(list(as.integer(sentence_id))),
        sentence_start = list(list(sentence_start)),
        sentence_end = list(list(sentence_end))
      )
    occurences <- length(unique(ud$sentence_id[[1]][[1]])) # Number of sentences in which actor occurs
    prominence <- occurences/sentence_count # Relative prominence of actor in article (number of occurences/total # sentences)
    rel_first <- 1-(ud$sentence_id[[1]][[1]][1]/sentence_count) # Relative position of first occurence at sentence level

    return(data.frame(ud,actor_start = I(list(list(markers))), occ = occurences,prom = prominence,rel_first = rel_first, ids = I(list(list(ids))), err = err))


    # ## The exception below is only valid for the UK, where the original UDPipe output misses a dot at the end of the article, but the actor output does not
    # ## (UK output is older than actor output, should be updated)
    # if (length(ud_org$sentence_id) == length(ud$sentence_id)-1) {
    #   ud <- ud[-length(ud$sentence_id),]
    # }
    # if (length(ud_org$sentence_id) == length(ud$sentence_id)) {
    #   ud <- bind_cols(ud_org, sentence = ud$sentence, token = ud$token, doc_id = ud$doc_id, actor = ud$actor)
    # } else {
    #   err = T
    #   print(paste0('ud_org and ud_actor not the same length for id ', doc$`_id`))
    #   print(length(ud_org$sentence_id))
    #   print(length(ud$sentence_id))
    # }

  }
  out <- mamlr:::out_parser(out, field = 'highlight', clean = F)
  # out$highlight <- out$merged
  # out <- mamlr:::out_parser(out, field = '_source', clean = F)
  offsetter <- function(x) {
    return(x-((row(x)-1)*nchar(identifier)))
  }
  regex_identifier <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", identifier)
  out$markers <- lapply(str_locate_all(out$merged,coll(identifier)), offsetter)

  ids <- fromJSON(ids)
  updates <- bind_rows(mclapply(seq(1,length(out[[1]]),1), sentencizer, out = out, ids = ids, postfix = postfix, prefix=prefix, identifier=identifier, udmodel = udmodel, type = type, mc.cores = detectCores()))
  bulk <- apply(updates, 1, bulk_writer, varname ='actorsDetail2', type = 'add', ver = ver)
  bulk <- c(bulk,apply(updates[c(1,8)], 1, bulk_writer, varname='actors2', type = 'add', ver = ver))
  return(elastic_update(bulk, es_super = es_super, localhost = localhost))
}
