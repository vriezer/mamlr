#' Generate a line-delimited JSON string for use in Elasticsearch bulk updates
#'
#' Generate a line-delimited JSON string for use in Elasticsearch bulk updates
#' Type can be either one of three values:
#' set: set the value of [varname] to x
#' add: add x to the values of [varname]
#' varname: When using tokens, the token field will be updated instead of a computerCodes field
#' @param x A single-row data frame, or a string containing the variables and/or values that should be updated (a data frame is converted to a JSON object, strings are stored as-is)
#' @param index The name of the Elasticsearch index to update
#' @param varname String indicating the parent variable that should be updated (when it does not exist, it will be created, all varnames are prefixed by computerCodes)
#' @param type Type of updating to be done, can be either 'set', 'add', or 'addnested'
#' @return A string usable as Elasticsearch bulk update command, in line-delimited JSON
#' @export
#' @examples
#' bulk_writer(x, index = 'maml', varname = 'updated_variable')
#################################################################################################
#################################### Bulk update writer ################################
#################################################################################################
bulk_writer <- function(x, index = 'maml', varname = 'updated_variable', type) {
  ### Create a json object if more than one variable besides _id, otherwise use value as-is
  if (length(x) > 2) {
    json <- toJSON(bind_rows(x)[-1], collapse = T)
  x} else {
    names(x) <- NULL
    json <- toJSON(x[-1], collapse = T)
  }
  if (varname == "tokens") {
    return(
      paste0('{"update": {"_index": "',index,'", "_type": "doc", "_id": "',x[1],'"}}
{ "script" : { "source": "ctx._source.tokens = params.code", "lang" : "painless", "params": { "code": ',json,'}}}')
    )
  }
  if (type == 'set') {
    return(
      paste0('{"update": {"_index": "',index,'", "_type": "doc", "_id": "',x[1],'"}}
{ "script" : { "source": "if (ctx._source.computerCodes != null) {ctx._source.computerCodes.',varname,' = params.code} else {ctx._source.computerCodes = params.object}", "lang" : "painless", "params": { "code": ',json,', "object": {"',varname,'": ',json,'} }}}')
    )
  }
  if (type == "add") {
    return(
      paste0('{"update": {"_index": "',index,'", "_type": "doc", "_id": "',x[1],'"}}
        {"script": {"source": "if (ctx._source.computerCodes != null && ctx._source.computerCodes.containsKey(\\"',varname,'\\")) {ctx._source.computerCodes.',varname,'.addAll(params.code)} else if (ctx._source.computerCodes != null) {ctx._source.computerCodes.',varname,' = params.code} else {ctx._source.computerCodes = params.object}", "lang" : "painless", "params": { "code": ',json,' , "object": {"',varname,'": ',json,'}}}}'
      )
    )
  }
}
