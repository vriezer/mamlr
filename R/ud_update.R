#' Elasticizer update function: generate UDpipe output from base text
#'
#' Elasticizer update function: generate UDpipe output from base text
#' @param out Does not need to be defined explicitly! (is already parsed in the elasticizer function)
#' @param localhost Defaults to false. When true, connect to a local Elasticsearch instance on the default port (9200)
#' @param udmodel UDpipe model to use
#' @param es_super Password for write access to ElasticSearch
#' @param cores Number of cores to use for parallel processing, defaults to detectCores() (all cores available)
#' @param ver Short string (preferably a single word/sequence) indicating the version of the updated document (i.e. for a udpipe update this string might be 'udV2')
#' @return A vector of 1's indicating the success of each update call
#' @export
#' @examples
#' ud_update(out, localhost = T, udmodel, es_super = .rs.askForPassword("ElasticSearch WRITE"), cores = detectCores())
#'

# punct_check <- function(str) {
#   if (!(stri_sub(str, from = -1)) %in% c('.','!','?')) {
#     return(str_c(str, '.'))
#   }
# }

ud_update <- function(out, localhost = T, udmodel, es_super = .rs.askForPassword("ElasticSearch WRITE"), cores = detectCores(), ver) {
  out <- mamlr:::out_parser(out, field = '_source', clean = F)
  par_proc <- function(row, out, udmodel) {
    doc <- out[row,]
    ud <- as.data.frame(udpipe(udmodel, x = doc$merged, parser = "default", doc_id = doc$`_id`)) %>%
      group_by(doc_id) %>%
      summarise(
        sentence_id = list(list(as.integer(sentence_id))),
        token_id = list(list(as.integer(token_id))),
        lemma = list(list(as.character(lemma))),
        upos = list(list(as.character(upos))),
        feats = list(list(as.character(feats))),
        head_token_id = list(list(as.integer(head_token_id))),
        dep_rel = list(list(as.character(dep_rel))),
        start = list(list(as.integer(start))),
        end = list(list(as.integer(end))),
        exists = list(list(TRUE))
     )
    return(ud)
  }
  ud <- bind_rows(mclapply(seq(1,length(out[[1]]),1), par_proc, out = out, udmodel=udmodel, mc.cores = cores))
  bulk <- apply(ud, 1, bulk_writer, varname = 'ud', type = 'set', ver = ver)
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
