#' Get ids of duplicate documents that have a cosine similarity score higher than [threshold]
#'
#' Get ids of duplicate documents that have a cosine similarity score higher than [threshold]
#' @param row Row of grid to parse
#' @param grid A cross-table of all possible combinations of doctypes and dates
#' @param cutoff Cutoff value for cosine similarity above which documents are considered duplicates
#' @param es_pwd Password for Elasticsearch read access
#' @return dupe_objects.json (containing each id and all its duplicates) and remove_ids.txt (list of ids to be removed) in current working directory
#' @export
#' @examples
#' dupe_detect(1,grid,es_pwd)

#################################################################################################
#################################### Duplicate detector ################################
#################################################################################################

dupe_detect <- function(row, grid, cutoff, es_pwd) {
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
  dfm <- dfm_gen(out, text = "full")
  simil <- as.matrix(textstat_simil(dfm, margin="documents", method="cosine"))
  diag(simil) <- NA
  simil_og <- simil
  df <- as.data.frame(which(simil >= cutoff, arr.ind = TRUE)) %>%
    rownames_to_column("rowid") %>%
    mutate(colid = colnames(simil)[col]) %>%
    .[,c(1,4)] %>%
    group_by(colid) %>% summarise(rowid=list(rowid))
  text <- capture.output(stream_out(df))
  write(text[-length(text)], file = paste0(getwd(),'/dupe_objects.json'), append=T)
  simil[upper.tri(simil)] <- NA
  write(unique(rownames(which(simil >= cutoff, arr.ind = TRUE))),
        file = paste0(getwd(),'/remove_ids.txt'),
        append=T)
  ### Dummy code to verify that filtering out unique ids using the bottom half of the matrix actually works
  # id_filter <- unique(rownames(which(simil >= cutoff, arr.ind = TRUE)))
  # dfm_nodupes <- dfm_subset(dfm, !(docnames(dfm) %in% id_filter))
  # simil_nodupes <- as.matrix(textstat_simil(dfm_nodupes, margin="documents", method="cosine"))
  # diag(simil_nodupes) <- NA
  # which(simil_nodupes >= cutoff, arr.ind = TRUE)
}
