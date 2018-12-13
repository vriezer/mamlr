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
#' @param es_super Password for write access to ElasticSearch
#' @return As this is a nested function used within elasticizer, there is no return output
#' @export
#' @examples
#' actorizer(out, localhost = F, ids, type, prefix, postfix, identifier, udmodel, es_super)
actorizer <- function(out, localhost = F, ids, type, prefix, postfix, identifier, udmodel, es_super) {
  fncols <- function(data, cname) {
    add <-cname[!cname%in%names(data)]

    if(length(add)!=0) data[add] <- NA
    data
  }

  sentencizer <- function(row, out, udmodel, ids, prefix, postfix, identifier) {

    ### If no pre or postfixes, match *not nothing* i.e. anything
    if (is.na(prefix) || prefix == '') {
      prefix = '$^'
    }
    if (is.na(postfix) || postfix == '') {
      postfix = '$^'
    }
    ### Also needs fix for empty strings (non-NA)
    doc <- out[row,]
    print(doc$merged)
    print(row)
    ud <- as.data.frame(udpipe_annotate(udmodel, x = doc$merged, parser = "none", doc_id = doc$`_id`)) %>%
      filter(upos != "PUNCT") # Removing punctuation to get accurate word counts
    sentence_count <- length(unique(ud$sentence))
    ud <- ud %>%
      filter(grepl(paste0(identifier), sentence)) %>% # Only select sentences that contain the identifier
      filter(!str_detect(sentence, postfix)) %>% # Filter out sentences with matching postfixes (false positives)
      filter(!str_detect(sentence, prefix)) %>% # Filter out sentences with matching prefixes (false positives)
      filter(grepl(paste0(identifier,'.*'), token)) %>% # Only select tokens that start with the identifier
      group_by(doc_id) %>%
      summarise(
        sentence_id = list(list(as.integer(sentence_id))),
        token_id = list(list(as.integer(token_id))),
        text = list(list(unique(as.character(sentence))))
      )
    occurences <- length(unique(ud$sentence_id)) # Number of sentences in which actor occurs
    prominence <- occurences/sentence_count # Relative prominence of actor in article (number of occurences/total # sentences)
    rel_first <- 1-(ud$sentence_id[[1]][[1]][1]/sentence_count) # Relative position of first occurence at sentence level

    return(data.frame(ud,occ = occurences,prom = prominence,rel_first = rel_first, ids = I(list(list(ids)))))
  }

  out <- fncols(out, c("highlight.text","highlight.title","highlight.teaser", "highlight.subtitle", "highlight.preteaser", '_source.text', '_source.title','_source.teaser','_source.subtitle','_source.preteaser'))
  out <- replace(out, out=="NULL", NA)

  ### Replacing empty highlights with source text (to have the exact same text for udpipe to process)
  out$highlight.title[is.na(out$highlight.title)] <- out$`_source.title`[is.na(out$highlight.title)]
  out$highlight.text[is.na(out$highlight.text)] <- out$`_source.text`[is.na(out$highlight.text)]
  out$highlight.teaser[is.na(out$highlight.teaser)] <- out$`_source.teaser`[is.na(out$highlight.teaser)]
  out$highlight.subtitle[is.na(out$highlight.subtitle)] <- out$`_source.subtitle`[is.na(out$highlight.subtitle)]
  out$highlight.preteaser[is.na(out$highlight.preteaser)] <- out$`_source.preteaser`[is.na(out$highlight.preteaser)]

  out$merged <- str_c(str_replace_na(unlist(out$highlight.title), replacement = " "),
                      str_replace_na(unlist(out$highlight.subtitle), replacement = " "),
                      str_replace_na(unlist(out$highlight.preteaser), replacement = " "),
                      str_replace_na(unlist(out$highlight.teaser), replacement = " "),
                      str_replace_na(unlist(out$highlight.text), replacement = " "),
                      sep = " ") %>%
    # Replacing html tags with whitespaces
    str_replace_all("<.*?>", " ") %>%
    str_replace_all("\\s+"," ")

  ids <- fromJSON(ids)
  updates <- bind_rows(mclapply(seq(1,length(out[[1]]),1), sentencizer, out = out, ids = ids, postfix = postfix, prefix=prefix, identifier=identifier, udmodel = udmodel, mc.cores = detectCores()))
  bulk <- apply(updates, 1, bulk_writer, varname ='actorsDetail', type = 'add')
  bulk <- c(bulk,apply(updates[c(1,8)], 1, bulk_writer, varname='actors', type = 'add'))
  return(elastic_update(bulk, es_super = es_super, localhost = localhost))
}
