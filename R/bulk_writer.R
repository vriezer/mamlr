#' Generate a line-delimited JSON string for use in Elasticsearch bulk updates
#'
#' Generate a line-delimited JSON string for use in Elasticsearch bulk updates
#' @param x A single-row data frame, or a string containing the variables and/or values that should be updated (a data frame is converted to a JSON object, strings are stored as-is)
#' @param index The name of the Elasticsearch index to update
#' @param varname String indicating the parent variable that should be updated (when it does not exist, it will be created)
#' @return A string usable as Elasticsearch bulk update command, in line-delimited JSON
#' @export
#' @examples
#' bulk_writer(x, index = 'maml', varname = 'updated_variable')
#################################################################################################
#################################### Bulk update writer ################################
#################################################################################################
bulk_writer <- function(x, index = 'maml', varname = 'updated_variable') {
  return(
    paste0('{"update": {"_index": "',index,'", "_type": "doc", "_id": "',x[1],'"}}
  { "script" : { "source": "ctx._source.',varname,' = params.code", "lang" : "painless","params" : {"code":',toJSON(x[-1], collapse = F),'}}}')
  )
}