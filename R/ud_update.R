#' Generate UDpipe output from base text
#'
#' Generate UDpipe output from base text
#' @param file Filename of file to read in, also used for generating output file name
#' @param wd Working directory where *file*s can be found
#' @param ud_file Filename of udpipe model to use, should be in *wd*
#' @param ver Short string (preferably a single word/sequence) indicating the version of the updated document (i.e. for a udpipe update this string might be 'udV2')
#' @return A vector of 1's indicating the success of each update call
#' @export
#' @examples
#' ud_update(out, udmodel, ver, file)
#'

# punct_check <- function(str) {
#   if (!(stri_sub(str, from = -1)) %in% c('.','!','?')) {
#     return(str_c(str, '.'))
#   }
# }

ud_update <- function(file, wd, ud_file, ver) {
  out <- readRDS(str_c(wd,'/',file)) %>%
    out_parser(., field = '_source', clean = F)
  ud_model <- udpipe_load_model(file = str_c(wd,'/',ud_file))
  ud <- as.data.frame(udpipe(ud_model, x = out$merged, parser = "default", doc_id = out$`_id`)) %>%
    group_by(doc_id) %>%
    summarise(
      sentence_id = list(as.integer(sentence_id)),
      token_id = list(as.integer(token_id)),
      lemma = list(as.character(lemma)),
      upos = list(as.character(upos)),
      feats = list(as.character(feats)),
      head_token_id = list(as.integer(head_token_id)),
      dep_rel = list(as.character(dep_rel)),
      start = list(as.integer(start)),
      end = list(as.integer(end)),
      exists = list(TRUE)
   )
  bulk <- apply(ud, 1, bulk_writer, varname = 'ud', type = 'set', ver = ver)
  saveRDS(bulk, file = str_c(wd,'/ud_',file))
  # res <- elastic_update(bulk, es_super = es_super, localhost = localhost)
  return()
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
