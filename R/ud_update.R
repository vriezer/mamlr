#' Elasticizer update function: generate UDpipe output from base text
#'
#' Elasticizer update function: generate UDpipe output from base text
#' @param out Does not need to be defined explicitly! (is already parsed in the elasticizer function)
#' @param localhost Defaults to false. When true, connect to a local Elasticsearch instance on the default port (9200)
#' @param udmodel UDpipe model to use
#' @param es_super Password for write access to ElasticSearch
#' @param cores Number of cores to use for parallel processing, defaults to detectCores() (all cores available)
#' @return A vector of 1's indicating the success of each update call
#' @export
#' @examples
#' ud_update(out, localhost = T, udmodel, es_super = .rs.askForPassword("ElasticSearch WRITE"), cores = detectCores())
ud_update <- function(out, localhost = T, udmodel, es_super = .rs.askForPassword("ElasticSearch WRITE"), cores = detectCores()) {
  out$merged <- str_c(str_replace_na(out$`_source.title`, replacement = " "),
                      str_replace_na(out$`_source.subtitle`, replacement = " "),
                      str_replace_na(out$`_source.preteaser`, replacement = " "),
                      str_replace_na(out$`_source.teaser`, replacement = " "),
                      str_replace_na(out$`_source.text`, replacement = " "),
                      sep = " ") %>%
    # Remove html tags, and multiple consequent whitespaces
    str_replace_all("<.*?>", " ") %>%
    str_replace_all("\\s+"," ")
  par_proc <- function(row, out, udmodel) {
    doc <- out[row,]
    ud <- as.data.frame(udpipe_annotate(udmodel, x = doc$merged, parser = "default", doc_id = doc$`_id`)) %>%
      group_by(doc_id) %>%
      summarise(
        paragraph_id = list(list(paragraph_id)),
        sentence_id = list(list(sentence_id)),
        token_id = list(list(as.numeric(token_id))),
        lemma = list(list(lemma)),
        upos = list(list(upos)),
        feats = list(list(feats)),
        head_token_id = list(list(as.numeric(head_token_id))),
        dep_rel = list(list(dep_rel)),
        exists = list(list(TRUE))
     )
    return(ud)
  }
  ud <- bind_rows(mclapply(seq(1,length(out[[1]]),1), par_proc, out = out, udmodel=udmodel, mc.cores = cores))
  bulk <- apply(ud, 1, bulk_writer, varname = 'tokens', type = 'set')
  res <- elastic_update(bulk, es_super = es_super, localhost = localhost)
  return(res)
}

#### Old code ####
# Use | as separator (this is not done anymore, as all data is stored as actual lists, instead of strings. Code kept for future reference)
# str_replace_all("\\|", "") %>%
# Remove VERY annoying single backslashes and replace them by whitespaces
# str_replace_all("\\\\", " ") %>%
# Replace any occurence of (double) whitespace characters by a single regular whitespace
# t_id <- paste(ud[,5], collapse = '|')
# lemmatized <- paste(ud[,7], collapse = '|') %>%
#   # Replacing double quotes with single quotes in text
#   str_replace_all("\"","\'")
# upos_tags <- paste(ud[,8], collapse = '|')
# head_t_id <- paste(ud[,11], collapse = '|')
# dep_rel <- paste(ud[,12], collapse = '|')
