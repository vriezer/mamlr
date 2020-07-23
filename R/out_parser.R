#' Parse raw text into a single field
#'
#' Parse raw text from the MaML database into a single field
#' @param out The original output data frame
#' @param field Either 'highlight' or '_source', for parsing of the highlighted search result text, or the original source text
#' @param clean Boolean indicating whether the results should be cleaned by removing words matching regex (see code)
#' @return a parsed output data frame including the additional column 'merged', containing the merged text
#' @export
#' @examples
#' out_parser(out,field)

#################################################################################################
#################################### Parser function for output fields ##########################
#################################################################################################
out_parser <- function(out, field, clean = F) {
  fncols <- function(data, cname) {
    add <-cname[!cname%in%names(data)]

    if(length(add)!=0) data[, (add) := (NA)]
    data
  }

  out <- fncols(out, c("highlight.text","highlight.title","highlight.teaser", "highlight.subtitle", "highlight.preteaser", '_source.text', '_source.title','_source.teaser','_source.subtitle','_source.preteaser'))
  par_parser <- function(row, out, field, clean) {
    doc <- out[row,]
    if (field == 'highlight') {

      doc <- doc %>%
        unnest(cols = starts_with("highlight")) %>%
      mutate(across(starts_with("highlight"), na_if, "NULL")) %>%
        mutate(highlight.title = coalesce(highlight.title, `_source.title`),
               highlight.subtitle = coalesce(highlight.subtitle, `_source.subtitle`),
               highlight.preteaser = coalesce(highlight.preteaser, `_source.preteaser`),
               highlight.teaser = coalesce(highlight.teaser, `_source.teaser`),
               highlight.text = coalesce(highlight.text, `_source.text`)
        ) %>%
        mutate(highlight.title = str_replace_na(highlight.title, replacement = ''),
               highlight.subtitle = str_replace_na(highlight.subtitle, replacement = ''),
               highlight.preteaser = str_replace_na(highlight.preteaser, replacement = ''),
               highlight.teaser = str_replace_na(highlight.teaser, replacement = ''),
               highlight.text = str_replace_na(highlight.text, replacement = '')
        ) %>%
        mutate(
          merged = str_c(highlight.title,
                         highlight.subtitle,
                         highlight.preteaser,
                         highlight.teaser,
                         highlight.text,
                         '',
                         sep = ". ")
        )
    }

    if (field == '_source') {
      doc <- doc %>%
        mutate(`_source.title` = str_replace_na(`_source.title`, replacement = ''),
               `_source.subtitle` = str_replace_na(`_source.subtitle`, replacement = ''),
               `_source.preteaser` = str_replace_na(`_source.preteaser`, replacement = ''),
               `_source.teaser` = str_replace_na(`_source.teaser`, replacement = ''),
               `_source.text` = str_replace_na(`_source.text`, replacement = '')
               ) %>%
      mutate(
        merged = str_c(`_source.title`,
                       `_source.subtitle`,
                       `_source.preteaser`,
                       `_source.teaser`,
                       `_source.text`,
                       '',
                       sep = ". ")
      )
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
  return(par_parser(1:nrow(out), out=out, clean=clean, field=field))
}
