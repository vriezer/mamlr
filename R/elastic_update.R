#' Push a line-delimited JSON string to Elasticsearch as bulk update
#'
#' Push a line-delimited JSON string to Elasticsearch as bulk update
#' @param x Line-delimited JSON suitable for use as Elasticsearch bulk update
#' @param es_super The even-more-secret (do not store this anywhere!!!) password for updating (or messing up!) the entire database
#' @return An html response object indicating the status of the update
#' @export
#' @examples
#' elastic_update(x, es_super = 'secret')

#################################################################################################
#################################### Elasticsearch Updater ################################
#################################################################################################
elastic_update <- function(x, es_super = 'secret') {
  bulk <- paste0(x,'\n')
  url <- paste0('https://super:',es_super,'@linux01.uis.no/es/_bulk?pretty')
  res <- RETRY("POST", url = url
               , body = bulk
               , encode = "raw"
               , add_headers("Content-Type" = "application/json")
               , times = 10
               , pause_min = 10
  )
  # stop_for_status(res)
  # content(res, "parsed", "application/json")
  # appData <- content(res)
  return(res)
}
