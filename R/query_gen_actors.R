#' Generate actor search queries based on data in actor db
#'
#' Generate actor search queries based on data in actor db
#' @param actor A row from the output of elasticizer() when run on the 'actor' index
#' @param country 2-letter string indicating the country for which to generate the queries, is related to inflected nouns, definitive forms and genitive forms of names etc.
#' @param identifier Identifier used to mark hits in the text, identifiers are prepended before the actual hit
#' @return A data frame containing the queries, related actor ids and actor function
#' @export
#' @examples
#' query_gen_actors(actor,country)

#################################################################################################
#################################### Actor search query generator ###############################
#################################################################################################
query_gen_actors <- function(actor, country, identifier) {
  highlight <- paste0('"highlight" : {
                      "fields" : {
                      "text" : {},
                      "teaser" : {},
                      "preteaser" : {},
                      "title" : {},
                      "subtitle" : {}
                      },
                      "number_of_fragments": 0,
                      "order": "none",
                      "type":"unified",
                      "fragment_size":0,
                      "pre_tags":"',identifier,'",
                      "post_tags": ""
}')
if (country == "no") {
  genitive <- 's'
  definitive <- 'en'
  definitive_genitive <- 'ens'
} else if (country == 'uk') {
  genitive <- '\'s'
  definitive <- 's'
  definitive_genitive <- ''
} else {
  genitive <- ''
  definitive <- ''
  definitive_genitive <- ''
}

  if (actor$`_source.function` == "Min" | actor$`_source.function` == "PM" | actor$`_source.function` == "PartyLeader") {
    lastname <- paste0('(',actor$`_source.lastName`,' OR ',actor$`_source.lastName`,genitive,')')
    ## Adding a separate AND clause for inclusion of only last name to highlight all occurences of last name
    ## Regardless of whether the last name hit is because of a minister name or a full name proximity hit
    query_string <- paste0('(((\\"',
                           actor$`_source.firstName`,
                           ' ',
                           actor$`_source.lastName`,
                           '\\"~5 OR \\"',
                           actor$`_source.firstName`,
                           ' ',
                           actor$`_source.lastName`,genitive,
                           '\\"~5) AND ',lastname)
  }
  if (actor$`_source.function` == "PartyLeader") {
    query_string <- paste0(query_string,'))')
    ids <- toJSON(unlist(lapply(c(actor$`_source.actorId`,actor$`_source.partyId`),str_c, "_pl")))
  }
  if (actor$`_source.function` == "Min" | actor$`_source.function` == "PM") {
    ## Modifiers are only applied to minister titles
    capital <- unlist(lapply(actor$`_source.ministerSearch`, str_to_title))
    capital_gen <- unlist(lapply(capital, str_c, genitive))
    capital_def <- unlist(lapply(capital, str_c, definitive))
    capital_defgen <- unlist(lapply(capital, str_c, definitive_genitive))
    gen <- unlist(lapply(actor$`_source.ministerSearch`, str_c, genitive))
    def <- unlist(lapply(actor$`_source.ministerSearch`, str_c, definitive))
    defgen <- unlist(lapply(actor$`_source.ministerSearch`, str_c, definitive_genitive))
    names <- paste(c(capital,capital_gen,gen,capital_def,def,defgen,capital_defgen), collapse = ' ')
    query_string <- paste0(query_string,') OR (',lastname,' AND (',names,')))')
    ids <- toJSON(unlist(lapply(c(actor$`_source.actorId`,actor$`_source.ministryId`,actor$`_source.partyId`),str_c, "_min")))
  }
  if (actor$`_source.function` == "Institution") {
    #uppercasing
    firstup <- function(x) {
      substr(x, 1, 1) <- toupper(substr(x, 1, 1))
      x
    }
    actor$`_source.startDate` <- "2000-01-01"
    actor$`_source.endDate` <- "2099-01-01"
    if (nchar(actor$`_source.institutionNameSearch`[[1]]) > 0) {
      upper <- unlist(lapply(actor$`_source.institutionNameSearch`, firstup))
      upper <- c(upper, unlist(lapply(upper, str_c, genitive)),
                 unlist(lapply(upper, str_c, definitive)),
                 unlist(lapply(upper, str_c, definitive_genitive)))
      capital <- unlist(lapply(actor$`_source.institutionNameSearch`, str_to_title))
      capital <- c(capital, unlist(lapply(capital, str_c, genitive)),
                 unlist(lapply(capital, str_c, definitive)),
                 unlist(lapply(capital, str_c, definitive_genitive)))
      base <- actor$`_source.institutionNameSearch`
      base <- c(base, unlist(lapply(base, str_c, genitive)),
                   unlist(lapply(base, str_c, definitive)),
                   unlist(lapply(base, str_c, definitive_genitive)))
      names <- paste(unique(c(upper,capital,base)), collapse = '\\" \\"')
      query_string <- paste0('(\\"',names,'\\")')
      ids <- toJSON(unlist(lapply(c(actor$`_source.institutionId`),str_c, "_f")))
      query <- paste0('{"query":
                    {"bool": {"filter":[{"term":{"country":"',country,'"}},
                    {"range":{"publication_date":{"gte":"',actor$`_source.startDate`,'","lte":"',actor$`_source.endDate`,'"}}},
                    {"query_string" : {
                    "default_operator" : "OR",
                    "allow_leading_wildcard" : "false",
                    "fields": ["text","teaser","preteaser","title","subtitle"],
                    "query" : "', query_string,'"
                    }
                    }
                    ]
                    } },',highlight,' }')
      df1 <- data.frame(query = query, ids = I(ids), type = actor$`_source.function`, stringsAsFactors = F)
    }
    if (nchar(actor$`_source.institutionNameSearchShort`[[1]]) > 0) {
      upper <- unlist(lapply(actor$`_source.institutionNameSearchShort`, firstup))
      upper <- c(upper, unlist(lapply(upper, str_c, genitive)),
                 unlist(lapply(upper, str_c, definitive)),
                 unlist(lapply(upper, str_c, definitive_genitive)))
      capital <- unlist(lapply(actor$`_source.institutionNameSearchShort`, str_to_title))
      capital <- c(capital, unlist(lapply(capital, str_c, genitive)),
                   unlist(lapply(capital, str_c, definitive)),
                   unlist(lapply(capital, str_c, definitive_genitive)))
      base <- actor$`_source.institutionNameSearchShort`
      base <- c(base, unlist(lapply(base, str_c, genitive)),
                unlist(lapply(base, str_c, definitive)),
                unlist(lapply(base, str_c, definitive_genitive)))
      names <- paste(unique(c(upper,capital,base)), collapse = '\\" \\"')
      query_string <- paste0('(\\"',names,'\\")')
      ids <- toJSON(unlist(lapply(c(actor$`_source.institutionId`),str_c, "_s")))
      query <- paste0('{"query":
                    {"bool": {"filter":[{"term":{"country":"',country,'"}},
                    {"range":{"publication_date":{"gte":"',actor$`_source.startDate`,'","lte":"',actor$`_source.endDate`,'"}}},
                    {"query_string" : {
                    "default_operator" : "OR",
                    "allow_leading_wildcard" : "false",
                    "fields": ["text","teaser","preteaser","title","subtitle"],
                    "query" : "', query_string,'"
                    }
                    }
                    ]
                    } },',highlight,' }')
      df2 <- data.frame(query = query, ids = I(ids), type = actor$`_source.function`, stringsAsFactors = F)
    }
    if (exists('df1') == T & exists('df2') == T) {
      return(bind_rows(df1,df2))
    } else if (exists('df1') == T) {
      return(df1)
    } else if (exists('df2') == T) {
      return(df2)
    }
  }
  if (actor$`_source.function` == "Party") {
    actor$`_source.startDate` <- "2000-01-01"
    actor$`_source.endDate` <- "2099-01-01"
    names <- paste(c(unlist(actor$`_source.partyNameSearchShort`)), collapse = '\\" \\"')
    query_string <- paste0('(\\"',names,'\\")')
    query <- paste0('{"query":
                    {"bool": {"filter":[{"term":{"country":"',country,'"}},
                    {"range":{"publication_date":{"gte":"',actor$`_source.startDate`,'","lte":"',actor$`_source.endDate`,'"}}},
                    {"query_string" : {
                    "default_operator" : "OR",
                    "allow_leading_wildcard" : "false",
                    "fields": ["text","teaser","preteaser","title","subtitle"],
                    "query" : "', query_string,'"
                    }
                    }
                    ]
                    } },',highlight,' }')
    ids <- c(toJSON(unlist(lapply(c(actor$`_source.partyId`),str_c, "_p"))))

    if (nchar(actor$`_source.partyNameSearch`[[1]]) > 0) {
      names <- paste(c(unlist(actor$`_source.partyNameSearch`)), collapse = '\\" \\"')
      query_string <- paste0('(\\"',names,'\\")')
      query2 <- paste0('{"query":
                       {"bool": {"filter":[{"term":{"country":"',country,'"}},
                       {"range":{"publication_date":{"gte":"',actor$`_source.startDate`,'","lte":"',actor$`_source.endDate`,'"}}},
                       {"query_string" : {
                       "default_operator" : "OR",
                       "allow_leading_wildcard" : "false",
                       "fields": ["text","teaser","preteaser","title","subtitle"],
                       "query" : "', query_string,'"
                       }
                       }
                       ]
                       } },',highlight,' }')


    ids <- c(ids, toJSON(unlist(lapply(c(actor$`_source.partyId`),str_c, "_p"))))
    query <- c(query, query2)
    fn <- c('PartyAbbreviation','Party')
    } else {
      fn <- c('PartyAbbreviation')
    }
    return(data.frame(query = query, ids = I(ids), type = fn, prefix = actor$`_source.searchAnd`, postfix = actor$`_source.searchAndNot`, stringsAsFactors = F))
  }

  query <- paste0('{"query":
                  {"bool": {"filter":[{"term":{"country":"',country,'"}},
                  {"range":{"publication_date":{"gte":"',actor$`_source.startDate`,'","lte":"',actor$`_source.endDate`,'"}}},
                  {"query_string" : {
                  "default_operator" : "OR",
                  "allow_leading_wildcard" : "false",
                  "fields": ["text","teaser","preteaser","title","subtitle"],
                  "query" : "', query_string,'"
                  }
                  }
                  ]
                  } },',highlight,' }')
  fn <- actor$`_source.function`
  return(data.frame(query = query, ids = I(ids), type = fn, stringsAsFactors = F))
}
