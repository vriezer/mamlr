#' Parse raw text into a single field
#'
#' Parse raw text from the MaML database into a single field
#' @param out The original output data frame
#' @param field Either 'highlight' or '_source', for parsing of the highlighted search result text, or the original source text
#' @param clean Boolean indicating whether the results should be cleaned by removing words matching regex (see code)
#' @param cores Number of cores to use for parallel processing, defaults to detectCores() (all cores available)
#' @return a parsed output data frame including the additional column 'merged', containing the merged text
#' @export
#' @examples
#' out_parser(out,field)

#################################################################################################
#################################### Parser function for output fields ##########################
#################################################################################################
out_parser <- function(out, field, clean = F, cores = 1) {
  plan(multiprocess, workers = cores)
  fncols <- function(data, cname) {
    add <-cname[!cname%in%names(data)]

    if(length(add)!=0) data[add] <- NA
    data
  }

  out <- fncols(out, c("highlight.text","highlight.title","highlight.teaser", "highlight.subtitle", "highlight.preteaser", '_source.text', '_source.title','_source.teaser','_source.subtitle','_source.preteaser'))
  par_parser <- function(row, out, field, clean) {
    doc <- out[row,]
    if (field == 'highlight') {
      doc <- replace(doc, doc=="NULL", NA)
      ### Replacing empty highlights with source text (to have the exact same text for udpipe to process)
      doc$highlight.title[is.na(doc$highlight.title)] <- doc$`_source.title`[is.na(doc$highlight.title)]
      doc$highlight.text[is.na(doc$highlight.text)] <- doc$`_source.text`[is.na(doc$highlight.text)]
      doc$highlight.teaser[is.na(doc$highlight.teaser)] <- doc$`_source.teaser`[is.na(doc$highlight.teaser)]
      doc$highlight.subtitle[is.na(doc$highlight.subtitle)] <- doc$`_source.subtitle`[is.na(doc$highlight.subtitle)]
      doc$highlight.preteaser[is.na(doc$highlight.preteaser)] <- doc$`_source.preteaser`[is.na(doc$highlight.preteaser)]

      doc <- doc %>%
        mutate(highlight.title = str_replace_na(highlight.title, replacement = '')) %>%
        mutate(highlight.subtitle = str_replace_na(highlight.subtitle, replacement = '')) %>%
        mutate(highlight.preteaser = str_replace_na(highlight.preteaser, replacement = '')) %>%
        mutate(highlight.teaser = str_replace_na(highlight.teaser, replacement = '')) %>%
        mutate(highlight.text = str_replace_na(highlight.text, replacement = ''))
      doc$merged <- str_c(doc$highlight.title,
                          doc$highlight.subtitle,
                          doc$highlight.preteaser,
                          doc$highlight.teaser,
                          doc$highlight.text,
                          '',
                          sep = ". ")
    }

    if (field == '_source') {
      doc <- doc %>%
        mutate(`_source.title` = str_replace_na(`_source.title`, replacement = '')) %>%
        mutate(`_source.subtitle` = str_replace_na(`_source.subtitle`, replacement = '')) %>%
        mutate(`_source.preteaser` = str_replace_na(`_source.preteaser`, replacement = '')) %>%
        mutate(`_source.teaser` = str_replace_na(`_source.teaser`, replacement = '')) %>%
        mutate(`_source.text` = str_replace_na(`_source.text`, replacement = ''))
      doc$merged <- str_c(doc$`_source.title`,
                          doc$`_source.subtitle`,
                          doc$`_source.preteaser`,
                          doc$`_source.teaser`,
                          doc$`_source.text`,
                          '',
                          sep = ". ")
    }

    ### Use correct interpunction, by inserting a '. ' at the end of every text field, then removing any duplicate occurences
    # Remove html tags, and multiple consequent whitespaces
    # Regex removes all words consisting of or containing numbers, @#$%
    # Punctuation is only filtered out when not followed by a whitespace character, and when the word contains any of the characters above
    # Regex also used in merger function
    ### Old regex, used for duplicate detection:
    # \\S*?[0-9@#$%]+[^\\s!?.,;:]*
    doc$merged <- doc$merged %>%
      str_replace_all("<.{0,20}?>", " ") %>%
      str_replace_all('(\\. ){2,}', '. ') %>%
      str_replace_all('([!?.])\\.','\\1') %>%
      str_replace_all("\\s+"," ") %>%
      {if(clean == T) str_replace_all(.,"\\S*?[0-9@#$%]+([^\\s!?.,;:]|[!?.,:;]\\S)*", "")  else . }
    return(doc)
  }
  out <- bind_rows(future_lapply(seq(1,length(out[[1]]),1), par_parser, out = out, clean = clean, field = field))
}
