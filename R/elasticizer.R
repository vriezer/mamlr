#' Generate a data frame out of unparsed Elasticsearch JSON
#'
#' Generate a data frame out of unparsed Elasticsearch JSON
#' @param query A JSON-formatted query in the Elasticsearch query DSL
#' @param src Logical (true/false) indicating whether or not the source of each document should be retrieved
#' @param index The name of the Elasticsearch index to search through
#' @param update When set, indicates an update function to use on each batch of 1000 articles
#' @param local Defaults to false. When true, connect to a local Elasticsearch instance on the default port (9200)
#' @param ... Parameters passed on to the update function

#' @return A data frame containing all the search results
#' @export
#' @examples
#' elasticizer(query, src = TRUE, index = "maml", update = NULL, localhost = F)
#################################################################################################
#################################### Get data from ElasticSearch ################################
#################################################################################################
elasticizer <- function(query, src = T, index = 'maml', es_pwd = .rs.askForPassword("Elasticsearch READ"), update = NULL, localhost = F, ...){
  httr::set_config(httr::config(http_version = 0))
  if (localhost == F) {
    connect(es_port = 443,
            es_transport = 'https',
            es_host = 'linux01.uis.no',
            es_path = 'es',
            es_user = 'es',
            es_pwd = es_pwd,
            errors = 'complete')
  }
  if (localhost == T){
    connect(es_port = 9200,
            es_transport = 'http',
            es_host = 'localhost',
            es_path = '',
            es_user = '',
            es_pwd = '',
            errors = 'complete')
  }
  # Get all results - one approach is to use a while loop
  if (src == T) {
    res <- Search(index = index, time_scroll="20m",body = query, size = 1000, raw=T)
  }
  if (src == F) {
    res <- Search(index = index, time_scroll="20m",body = query, size = 1000, raw=T, source = F)
  }
  json <- fromJSON(res)
  if (json$hits$total == 0) {
    return(json)
  } else {
    out <-  jsonlite:::flatten(json$hits$hits)
    total <- json$hits$total
    hits <- 1
    batch <- 1
    print(paste0('Processing documents ',batch*1000-1000,' through ',batch*1000,' out of ',total,' documents.'))
    if (length(update) > 0){
      update(out, localhost = localhost, ...)
    }
    while(hits != 0){
      res <- scroll(json$`_scroll_id`, time_scroll="20m", raw=T)
      json <- fromJSON(res)
      hits <- length(json$hits$hits)
      if(hits > 0) {
        batch <- batch+1
        print(paste0('Processing documents ',batch*1000-1000,' through ',batch*1000,' out of ',total,' documents.'))
        if (length(update) > 0){
          out <-  jsonlite:::flatten(json$hits$hits)
          update(out, localhost = localhost, ...)
          if (batch%%200 == 0) {
            Sys.sleep(900)
          }
        } else {
          out <- bind_rows(out, jsonlite:::flatten(json$hits$hits))
        }
      }
    }
    if (length(update) > 0) {
      return("Done updating")
    } else {
      return(out)
    }
  }
}
