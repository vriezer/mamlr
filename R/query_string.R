#' Generate a query string query for ElasticSearch
#'
#' Generate a query string query for ElasticSearch
#' @param x Query string in ElasticSearch query string format
#' @return A formatted ElasticSearch query string query
#' @export
#' @examples
#' query_string(x)
#################################################################################################
#################################### Get data from ElasticSearch ################################
#################################################################################################

query_string <- function(x) {
  return(paste0(
    '{
    "query": {
        "query_string" : {
            "default_field" : "text",
            "query" : "',x,'",
            "default_operator": "AND",
            "allow_leading_wildcard" : false
        }
    }
}'
  ))
}
