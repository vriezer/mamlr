#' Generates text output files (without punctuation) for external applications, such as GloVe embeddings
#'
#' Generates text output files (without punctuation) for external applications, such as GloVe embeddings
#' @param out The elasticizer-generated data frame
#' @param file The file to write the output to (including path, when required)
#' @param localhost Unused, but defaults to FALSE
#' @return A Quanteda dfm
#' @export
#' @examples
#' dfm_gen(out, words = '999')


#################################################################################################
#################################### Lemma text file generator #############################
#################################################################################################

lemma_writer <- function(out, file, localhost = F) {
  out <- unnest(out,`_source.ud`)
  lemma <- str_c(unlist(out$lemma)[-which(unlist(out$upos) == 'PUNCT')], unlist(out$upos)[-which(unlist(out$upos) == 'PUNCT')], sep = '_')
  cat(lemma, file = file, append = T)
}
