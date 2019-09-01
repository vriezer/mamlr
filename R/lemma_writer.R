#' Generates text output files (without punctuation) for external applications, such as GloVe embeddings
#'
#' Generates text output files (without punctuation) for external applications, such as GloVe embeddings
#' @param out The elasticizer-generated data frame
#' @param file The file to write the output to (including path, when required). When documents = T, provide path including trailing /
#' @param documents Indicate whether the writer should output to a single file, or individual documents
#' @param lemma Indicate whether document output should be lemmas or original document
#' @param cores Indicate the number of cores to use for parallel processing
#' @param localhost Unused, but defaults to FALSE
#' @return A Quanteda dfm
#' @export
#' @examples
#' dfm_gen(out, words = '999')


#################################################################################################
#################################### Lemma text file generator #############################
#################################################################################################

lemma_writer <- function(out, file, localhost = F, documents = F, lemma = F, cores = 1) {
  plan(multiprocess, workers = cores)
  par_writer <- function(row, out, lemma) {
    if (lemma == T) {
      cat(iconv(unlist(unnest(out[row,],`_source.ud`)$lemma), to = "UTF-8"), file = paste0(file,out[row,]$`_id`,'.txt'), append = F)
    } else {
      cat(iconv(out[row,]$merged, to = "UTF-8"), file = paste0(file,out[row,]$`_id`,'.txt'), append = F)
    }
  }
  if (documents == F) {
    out <- unnest(out,`_source.ud`)
    lemma <- str_c(unlist(out$lemma)[-which(unlist(out$upos) == 'PUNCT')], unlist(out$upos)[-which(unlist(out$upos) == 'PUNCT')], sep = '_')
    cat(lemma, file = file, append = T)
  }
  if (documents == T) {
    out <- out_parser(out, field = '_source', clean = F, cores = cores)
    future_lapply(1:nrow(out), par_writer, out = out, lemma = lemma)
  }
}
