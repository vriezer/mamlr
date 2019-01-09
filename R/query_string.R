#' Generate a query string query for ElasticSearch
#'
#' Generate a query string query for ElasticSearch
#' @param query Query string in ElasticSearch query string format
#' @param fields List of field names to return, defaults to all
#' @param random Return randomized results. Boolean, defaults to FALSE
#' @return A formatted ElasticSearch query string query
#' @export
#' @examples
#' query_string(query)
#################################################################################################
#################################### Get data from ElasticSearch ################################
#################################################################################################

query_string <- function(query, fields = F, random = F) {
  if (fields == F) {
    fields <- '*'
  }
  if (random == T) {
    return(paste0(
      '{
      "_source": ',toJSON(fields),',
        "query": {
          "function_score": {
            "query": {
              "bool":{
                "filter": [{
                  "query_string" : {
                      "default_field" : "text",
                      "query" : "',query,'",
                      "default_operator": "AND",
                      "allow_leading_wildcard" : false
                  }
                }]
              }
            },
            "random_score": {},
            "boost_mode": "sum"
          }
        }
      }'
    ))
  } else {
  return(paste0(
    '{
      "_source": ',toJSON(fields),',
      "query": {
        "bool":{
          "filter": [{
            "query_string" : {
                "default_field" : "text",
                "query" : "',query,'",
                "default_operator": "AND",
                "allow_leading_wildcard" : false
            }
          }]
        }
      }
    }'
  ))
  }
}
