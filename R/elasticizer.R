#' Generate a data frame out of unparsed Elasticsearch JSON
#'
#' Generate a data frame out of unparsed Elasticsearch JSON
#' @param query A JSON-formatted query in the Elasticsearch query DSL
#' @param src Logical (true/false) indicating whether or not the source of each document should be retrieved
#' @param index The name of the Elasticsearch index to search through
#' @param es_pwd The password for Elasticsearch read access
#' @param batch_size Batch size
#' @param max_batch Maximum number batches to retrieve
#' @param time_scroll Time to keep the scroll instance open (defaults to 5m, with a maximum of 500 allowed instances, so a maximum of 100 per minute)
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
### Just a bogus comment
elasticizer <- function(query, src = T, index = 'maml', es_pwd = .rs.askForPassword("Elasticsearch READ"), batch_size = 1024, max_batch = Inf, time_scroll = "5m", update = NULL, localhost = F, ...){
  retries <- 10 ### Number of retries on error
  sleep <- 30 ### Number of seconds between retries
  httr::set_config(httr::config(http_version = 0))
  ## Transitional code for syntax change in elastic package
  if (packageVersion("elastic") < 1) {
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
    conn <- NULL
  } else {
    if (localhost == F) {
      conn <- connect(port = 443,
              transport = 'https',
              host = 'linux01.uis.no',
              path = 'es',
              user = 'es',
              pwd = es_pwd,
              errors = 'complete')
    }
    if (localhost == T){
      conn <- connect(port = 9200,
              transport = 'http',
              host = 'localhost',
              path = '',
              user = '',
              pwd = '',
              errors = 'complete')
    }
  }
  # Get all results - one approach is to use a while loop
  if (src == T) {
    res <- NULL
    attempt <- 0
    while( is.null(res) && attempt <= retries ) {
      if (attempt > 0) {
        Sys.sleep(sleep)
      }
      attempt <- attempt + 1
      try(
        res <- Search(conn = conn, index = index, time_scroll=time_scroll,body = query, size = batch_size, raw=T)
      )
    }
  }
  if (src == F) {
    res <- NULL
    attempt <- 0
    while( is.null(res) && attempt <= retries ) {
      if (attempt > 0) {
        Sys.sleep(sleep)
      }
      attempt <- attempt + 1
      try(
        res <- Search(conn = conn, index = index, time_scroll=time_scroll,body = query, size = batch_size, raw=T, source = F)
      )
    }
  }
  json <- fromJSON(res)
  if (json$hits$total$value == 0) {
    return(json)
  } else {
    out <-  jsonlite:::flatten(json$hits$hits)
    total <- json$hits$total$value
    hits <- length(json$hits$hits)
    batch <- 1
    print(paste0('Processing documents ',batch*batch_size-batch_size,' through ',batch*batch_size,' out of ',total,' documents.'))
    if (length(update) > 0){
      update(out, localhost = localhost, ...)
    }
    while(hits > 0 && batch < max_batch ){
      res <- NULL
      attempt <- 0
      while( is.null(res) && attempt <= retries ) {
        if (attempt > 0) {
          Sys.sleep(sleep)
        }
        attempt <- attempt + 1
        try(
          res <- scroll(conn = conn, json$`_scroll_id`, time_scroll=time_scroll, raw=T)
        )
      }
      json <- fromJSON(res)
      hits <- length(json$hits$hits)
      if(hits > 0) {
        batch <- batch+1
        print(paste0('Processing documents ',batch*batch_size-batch_size,' through ',batch*batch_size,' out of ',total,' documents.'))
        if (length(update) > 0){
          out <-  jsonlite:::flatten(json$hits$hits)
          update(out, localhost = localhost, ...)
          if (batch%%500 == 0) {
            Sys.sleep(900)
          }
        } else {
          out <- bind_rows(out, jsonlite:::flatten(json$hits$hits))
        }
      }
    }
    if (length(update) > 0) {
      scroll_clear(x = json$`_scroll_id`)
      return("Done updating")
    } else {
      scroll_clear(x = json$`_scroll_id`)
      return(out)
    }
  }
}
