#' Get ids of duplicate documents that have a cosine similarity score higher than [threshold]
#'
#' Get ids of duplicate documents that have a cosine similarity score higher than [threshold]
#' @param row Row of grid to parse
#' @param grid A cross-table of all possible combinations of doctypes and dates
#' @param cutoff_lower Cutoff value for minimum cosine similarity above which documents are considered duplicates (inclusive)
#' @param cutoff_upper Cutoff value for maximum cosine similarity, above which documents are not considered duplicates (for debugging and manual parameter tuning, inclusive)
#' @param es_pwd Password for Elasticsearch read access
#' @param words Document cutoff point in number of words. Documents are cut off at the last [.?!] before the cutoff (so document will be a little shorter than [words])
#' @return dupe_objects.json and data frame containing each id and all its duplicates. remove_ids.txt and character vector with list of ids to be removed. Files are in current working directory
#' @export
#' @examples
#' dupe_detect(1,grid,cutoff_lower, cutoff_upper = 1, es_pwd, words)

#################################################################################################
#################################### Duplicate detector ################################
#################################################################################################

dupe_detect <- function(row, grid, cutoff_lower, cutoff_upper = 1, es_pwd, words) {
  params <- grid[row,]
  print(paste0('Parsing ',params$doctypes,' on ',params$dates ))
  query <- paste0('{"query":
                  {"bool": {"filter":[{"term":{"doctype": "',params$doctypes,'"}},
                  {"range" : {
                  "publication_date" : {
                  "gte" : "',params$dates,'T00:00:00Z",
                  "lt" :  "',params$dates+1,'T00:00:00Z"
                  }
                  }}]

                  } } }')


  out <- elasticizer(query, es_pwd = es_pwd)
  dfm <- dfm_gen(out, text = "full", words = words)
  simil <- as.matrix(textstat_simil(dfm, margin="documents", method="cosine"))
  diag(simil) <- NA
  df <- as.data.frame(which(simil >= cutoff_lower & simil <= cutoff_upper, arr.ind = TRUE))

  if (length(rownames(df)) > 0) {
    df <- df %>%
      rownames_to_column("rowid") %>%
      mutate(colid = colnames(simil)[col]) %>%
      .[,c(1,4)] %>%
      group_by(colid) %>% summarise(rowid=list(rowid))
    text <- capture.output(stream_out(df))
    write(text[-length(text)], file = paste0(getwd(),'/dupe_objects.json'), append=T)
    simil[upper.tri(simil)] <- NA
    write(unique(rownames(which(simil >= cutoff_lower & simil <= cutoff_upper, arr.ind = TRUE))),
          file = paste0(getwd(),'/remove_ids.txt'),
          append=T)
    return(list(df,unique(rownames(which(simil >= cutoff_lower & simil <= cutoff_upper, arr.ind = TRUE)))))
  } else {
    return(NULL)
  }
  ### Dummy code to verify that filtering out unique ids using the bottom half of the matrix actually works
  # id_filter <- unique(rownames(which(simil >= cutoff, arr.ind = TRUE)))
  # dfm_nodupes <- dfm_subset(dfm, !(docnames(dfm) %in% id_filter))
  # simil_nodupes <- as.matrix(textstat_simil(dfm_nodupes, margin="documents", method="cosine"))
  # diag(simil_nodupes) <- NA
  # which(simil_nodupes >= cutoff, arr.ind = TRUE)
}
