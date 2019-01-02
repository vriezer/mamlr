#' Parse raw text into a single field
#'
#' Parse raw text into a single field
#' @param out The original output data frame
#' @param field Either 'highlight' or '_source', for parsing of the highlighted search result text, or the original source text
#' @return a parsed output data frame including the additional column 'merged', containing the merged text
#' @examples
#' out_parser(out,field)

#################################################################################################
#################################### Parser function for output fields ##########################
#################################################################################################
out_parser <- function(out, field) {
  fncols <- function(data, cname) {
    add <-cname[!cname%in%names(data)]

    if(length(add)!=0) data[add] <- NA
    data
  }

  out <- fncols(out, c("highlight.text","highlight.title","highlight.teaser", "highlight.subtitle", "highlight.preteaser", '_source.text', '_source.title','_source.teaser','_source.subtitle','_source.preteaser'))
  if (field == 'highlight') {
    out <- replace(out, out=="NULL", NA)
    ### Replacing empty highlights with source text (to have the exact same text for udpipe to process)
    out$highlight.title[is.na(out$highlight.title)] <- out$`_source.title`[is.na(out$highlight.title)]
    out$highlight.text[is.na(out$highlight.text)] <- out$`_source.text`[is.na(out$highlight.text)]
    out$highlight.teaser[is.na(out$highlight.teaser)] <- out$`_source.teaser`[is.na(out$highlight.teaser)]
    out$highlight.subtitle[is.na(out$highlight.subtitle)] <- out$`_source.subtitle`[is.na(out$highlight.subtitle)]
    out$highlight.preteaser[is.na(out$highlight.preteaser)] <- out$`_source.preteaser`[is.na(out$highlight.preteaser)]

    out <- out %>%
      mutate(highlight.title = str_replace_na(highlight.title, replacement = '')) %>%
      mutate(highlight.subtitle = str_replace_na(highlight.subtitle, replacement = '')) %>%
      mutate(highlight.preteaser = str_replace_na(highlight.preteaser, replacement = '')) %>%
      mutate(highlight.teaser = str_replace_na(highlight.teaser, replacement = '')) %>%
      mutate(highlight.text = str_replace_na(highlight.text, replacement = ''))
    out$merged <- str_c(out$highlight.title,
                        out$highlight.subtitle,
                        out$highlight.preteaser,
                        out$highlight.teaser,
                        out$highlight.text,
                        sep = ". ")
  }

  if (field == '_source') {
    out <- out %>%
      mutate(`_source.title` = str_replace_na(`_source.title`, replacement = '')) %>%
      mutate(`_source.subtitle` = str_replace_na(`_source.subtitle`, replacement = '')) %>%
      mutate(`_source.preteaser` = str_replace_na(`_source.preteaser`, replacement = '')) %>%
      mutate(`_source.teaser` = str_replace_na(`_source.teaser`, replacement = '')) %>%
      mutate(`_source.text` = str_replace_na(`_source.text`, replacement = ''))
    out$merged <- str_c(out$`_source.title`,
                        out$`_source.subtitle`,
                        out$`_source.preteaser`,
                        out$`_source.teaser`,
                        out$`_source.text`,
                        sep = ". ")
  }

  ### Use correct interpunction, by inserting a '. ' at the end of every text field, then removing any duplicate occurences
  # Remove html tags, and multiple consequent whitespaces
  out$merged <- out$merged %>%
    str_replace_all("<.{0,20}?>", " ") %>%
    str_replace_all('(\\. ){2,}', '. ') %>%
    str_replace_all('([!?.])\\.','\\1') %>%
    str_replace_all("\\s+"," ")
  return(out)
}
