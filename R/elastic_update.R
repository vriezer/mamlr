#' Push a line-delimited JSON string to Elasticsearch as bulk update
#'
#' Push a line-delimited JSON string to Elasticsearch as bulk update
#' @param x Line-delimited JSON suitable for use as Elasticsearch bulk update
#' @param es_super The even-more-secret (do not store this anywhere!!!) password for updating (or messing up!) the entire database
#' @param localhost Defaults to true. When true, connect to a local Elasticsearch instance on the default port (9200)
#' @return An html response object indicating the status of the update
#' @export
#' @examples
#' elastic_update(x, es_super = 'secret', local = F)

#################################################################################################
#################################### Elasticsearch Updater ################################
#################################################################################################
elastic_update <- function(x, es_super = 'secret', localhost = T) {
  bulk <- paste0(paste0(x, collapse = '\n'),'\n')
  if (localhost == F) {
    url <- paste0('https://super:',es_super,'@linux01.uis.no/es/_bulk?pretty&refresh=wait_for')
  }
  if (localhost == T) {
    url <- 'http://localhost:9200/_bulk?pretty'
  }
  res <- RETRY("POST", url = url
               , body = bulk
               , encode = "raw"
               , add_headers("Content-Type" = "application/json")
               , times = 10
               , pause_min = 10
  )
  return(res)
}
