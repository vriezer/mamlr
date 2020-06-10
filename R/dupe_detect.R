#' Get ids of duplicate documents that have a cosine similarity score higher than [threshold]
#'
#' Get ids of duplicate documents that have a cosine similarity score higher than [threshold]
#' @param row Row of grid to parse
#' @param grid A cross-table of all possible combinations of doctypes and dates
#' @param cutoff_lower Cutoff value for minimum cosine similarity above which documents are considered duplicates (inclusive)
#' @param cutoff_upper Cutoff value for maximum cosine similarity, above which documents are not considered duplicates (for debugging and manual parameter tuning, inclusive)
#' @param es_pwd Password for Elasticsearch read access
#' @param es_super Password for write access to ElasticSearch
#' @param words Document cutoff point in number of words. Documents are cut off at the last [.?!] before the cutoff (so document will be a little shorter than [words])
#' @param localhost Defaults to true. When true, connect to a local Elasticsearch instance on the default port (9200)
#' @param ver Short string (preferably a single word/sequence) indicating the version of the updated document (i.e. for a udpipe update this string might be 'udV2')
#' @return dupe_objects.json and data frame containing each id and all its duplicates. remove_ids.txt and character vector with list of ids to be removed. Files are in current working directory
#' @export
#' @examples
#' dupe_detect(1,grid,cutoff_lower, cutoff_upper = 1, es_pwd, es_super, words, localhost = T)

#################################################################################################
#################################### Duplicate detector ################################
#################################################################################################
dupe_detect <- function(row, grid, cutoff_lower, cutoff_upper = 1, es_pwd, es_super, words, localhost = T, ver) {
  params <- grid[row,]
  print(paste0('Parsing ',params$doctypes,' on ',params$dates ))
  query <- paste0('doctype:\\"',params$doctypes,'\\" && publication_date:',params$dates,' && !computerCodes._delete:1')
  out <- elasticizer(query_string(query, fields = c('country','text','title','subtitle','teaser','preteaser')), es_pwd = es_pwd, localhost= localhost)
  if (class(out$hits$hits) != 'list') {
    dfm <- dfm_gen(out, text = "full", words = words, clean = T)
    if (sum(dfm[1,]) > 0) {
      simil <- as.matrix(textstat_simil(dfm, margin="documents", method="cosine"))
      diag(simil) <- NA
      duplicates <- which(simil >= cutoff_lower & simil <= cutoff_upper, arr.ind = TRUE)
      duplicates <- cbind(duplicates, rowid= rownames(duplicates))
      if (length(duplicates) > 0) {
        rownames(duplicates) <- seq(1:length(rownames(duplicates)))
        df <- as.data.frame(duplicates, make.names = NA, stringsAsFactors = F) %>%
          # bind_cols(colid = colnames(simil)[.['col']]) %>%
          mutate(colid = colnames(simil)[as.numeric(col)]) %>%
          .[,c(3,4)] %>%
          group_by(colid) %>% summarise(rowid=list(rowid))
        text <- capture.output(stream_out(df))
        # write(text[-length(text)], file = paste0(getwd(),'/dupe_objects.json'), append=T)
        simil[upper.tri(simil)] <- NA
        # write(unique(rownames(which(simil >= cutoff_lower & simil <= cutoff_upper, arr.ind = TRUE))),
        #       file = paste0(getwd(),'/remove_ids.txt'),
        #       append=T)
        dupe_delete <- data.frame(id=unique(rownames(which(simil >= cutoff_lower & simil <= cutoff_upper, arr.ind = TRUE))),
                                  dupe_delete = rep(1,length(unique(rownames(which(simil >= cutoff_lower & simil <= cutoff_upper, arr.ind = TRUE))))))
        bulk <- c(apply(df, 1, bulk_writer, varname='duplicates', type = 'set', ver = ver),
                  apply(dupe_delete, 1, bulk_writer, varname='_delete', type = 'set', ver = ver))
        res <- elastic_update(bulk, es_super = es_super, localhost = localhost)
        return(paste0('Checked ',params$doctypes,' on ',params$dates ))
      } else {
        return(paste0('No duplicates for ',params$doctypes,' on ',params$dates ))
      }
    } else {
      return(paste0('No results for ',params$doctypes,' on ',params$dates ))
    }
  } else {
    return(paste0('No results for ',params$doctypes,' on ',params$dates ))
  }
  ### Dummy code to verify that filtering out unique ids using the bottom half of the matrix actually works
  # id_filter <- unique(rownames(which(simil >= cutoff, arr.ind = TRUE)))
  # dfm_nodupes <- dfm_subset(dfm, !(docnames(dfm) %in% id_filter))
  # simil_nodupes <- as.matrix(textstat_simil(dfm_nodupes, margin="documents", method="cosine"))
  # diag(simil_nodupes) <- NA
  # which(simil_nodupes >= cutoff, arr.ind = TRUE)
}
