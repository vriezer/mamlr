#' Generate actor search queries based on data in actor db
#'
#' Generate actor search queries based on data in actor db
#' @param actor A row from the output of elasticizer() when run on the 'actor' index
#' @param country 2-letter string indicating the country for which to generate the queries, is related to inflected nouns, definitive forms and genitive forms of names etc.
#' @param pre_tags Highlighter pre-tag
#' @param post_tags Highlighter post-tag
#' @return A data frame containing the queries, related actor ids and actor function
#' @export
#' @examples
#' query_gen_actors(actor,country)

#################################################################################################
#################################### Actor search query generator ###############################
#################################################################################################
query_gen_actors <- function(actor, country, pre_tags, post_tags) {
  generator <- function(country, startdate, enddate, querystring, pre_tags, post_tags, actorid) {
    return(paste0('{"query":
                    {"bool": {
                      "filter":[
                        {"term":{"country":"',country,'"}},
                        {"range":{"publication_date":{"gte":"',startdate,'","lte":"',enddate,'"}}},
                        {"query_string" : {
                          "default_operator" : "OR",
                          "allow_leading_wildcard" : "false",
                          "fields": ["text","teaser","preteaser","title","subtitle"],
                          "query" : "', querystring,'"
                          }
                        }
                      ],
                      "must_not":[
                        {"term":{"computerCodes.actors.keyword":"',actorid,'"}}
                      ]
                    }
                  },
                "highlight" : {
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
                  "pre_tags":"', pre_tags,'",
                  "post_tags": "',post_tags,'"
                }
              }'))
  }
  ### Setting linguistic forms for each country ###
  if (country == "no" | country == "dk") {
    genitive <- 's'
    definitive <- 'en'
    definitive_genitive <- 'ens'
  } else if (country == 'uk') {
    genitive <- '\'s'
  } else if (country == 'nl' | country == 'be') {
    genitive <- 's'
  }

  ### Generating queries for individuals (ministers, PM, Party leaders and MPs)
  if (actor$`_source.function` == "Minister" | actor$`_source.function` == "PM" | actor$`_source.function` == "PartyLeader" | actor$`_source.function` == "MP") {
    ## Adding a separate AND clause for inclusion of only last name to highlight all occurences of last name
    ## Regardless of whether the last name hit is because of a minister name or a full name proximity hit

    ### If country is belgium, check if there is an apostrophe in middlenames, if so, search for last name both with capitalized and lowercased last name
    if (country == 'be') {
      if (T %in% str_detect(actor$`_source.middleNames`,"'")) {
        query_string <- paste0('(((\\"',
                               actor$`_source.firstName`,
                               ' ',
                               actor$`_source.lastName`,
                               '\\"~5 OR \\"',
                               actor$`_source.firstName`,
                               ' ',
                               tolower(actor$`_source.lastName`),
                               '\\"~5 OR \\"',
                               actor$`_source.firstName`,
                               ' ',
                               tolower(actor$`_source.lastName`),genitive,
                               '\\"~5 OR \\"',
                               actor$`_source.firstName`,
                               ' ',
                               actor$`_source.lastName`,genitive,
                               '\\"~5) AND ',lastname)
        lastname <- paste0('(',actor$`_source.lastName`,' OR ',actor$`_source.lastName`,genitive,' OR ',tolower(actor$`_source.lastName`),' OR ',tolower(actor$`_source.lastName`),genitive,')')
      } else {
        query_string <- paste0('(((\\"',
                               actor$`_source.firstName`,
                               ' ',
                               actor$`_source.lastName`,
                               '\\"~5 OR \\"',
                               actor$`_source.firstName`,
                               ' ',
                               actor$`_source.lastName`,genitive,
                               '\\"~5) AND ',lastname)
        lastname <- paste0('(',actor$`_source.lastName`,' OR ',actor$`_source.lastName`,genitive,')')
      }
    } else {
      query_string <- paste0('(((\\"',
                             actor$`_source.firstName`,
                             ' ',
                             actor$`_source.lastName`,
                             '\\"~5 OR \\"',
                             actor$`_source.firstName`,
                             ' ',
                             actor$`_source.lastName`,genitive,
                             '\\"~5) AND ',lastname)
      lastname <- paste0('(',actor$`_source.lastName`,' OR ',actor$`_source.lastName`,genitive,')')
    }


    ### If actor is a minister, generate minister search
    if (actor$`_source.function` == "Minister" | actor$`_source.function` == "PM") {
      capital <- unlist(lapply(actor$`_source.ministerSearch`, str_to_title))
      capital_gen <- unlist(lapply(capital, str_c, genitive))
      gen <- unlist(lapply(actor$`_source.ministerSearch`, str_c, genitive))
      # If country is no or dk, search for definitive minister forms as well (including genitive forms)
      if (country == "no" || country == "dk") {
        capital_def <- unlist(lapply(capital, str_c, definitive))
        capital_defgen <- unlist(lapply(capital, str_c, definitive_genitive))
        def <- unlist(lapply(actor$`_source.ministerSearch`, str_c, definitive))
        defgen <- unlist(lapply(actor$`_source.ministerSearch`, str_c, definitive_genitive))
        names <- paste(c(capital,capital_gen,gen,capital_def,def,defgen,capital_defgen), collapse = ' ')
      } else {
        names <- paste(c(capital,capital_gen,gen,actor$`_source.ministerSearch`), collapse = ' ')
      }
      # If country is uk, search for last name, minister names and minister titles (i.e. Johnson AND state AND secretary)
      if (country == "uk") {
        minister <- paste(c(str_to_title(actor$`_source.ministerName`),
                      actor$`_source.ministerName`,
                      str_c(str_to_title(actor$`_source.ministerName`),genitive),
                      str_c(actor$`_source.ministerName`,genitive)), collapse = ' ')
        query_string <- paste0(query_string,') OR (',lastname,' AND (',names,') AND (',unlist(minister),')))')
      } else if (country == "nl" | country == "be") { # If country is nl or be, add a requirement for Minister to the query
        query_string <- paste0(query_string,') OR (',lastname,' AND (',names,') AND ("Minister" OR "minister")))')
      } else {
        query_string <- paste0(query_string,') OR (',lastname,' AND (',names,')))')
      }
    } else { ### Else, generate search for first/last name only (MPs and Party leaders, currently)
      query_string <- paste0(query_string,'))')
    }
    ids <- list(c(actor$`_source.actorId`,str_c(actor$`_source.partyId`,'_a')))
    actorid <- actor$`_source.actorId`
    query <- generator(country, actor$`_source.startDate`, actor$`_source.endDate`, query_string, pre_tags, post_tags, actorid)
    return(data.frame(query = query, ids = I(ids), prefix = NA, postfix = NA, stringsAsFactors = F))
  }

  ### Query generation for party searches
  if (actor$`_source.function` == "Party") {
    actor$`_source.startDate` <- "2000-01-01"
    actor$`_source.endDate` <- "2099-01-01"
    if (nchar(actor$`_source.partyNameSearchShort`[[1]]) > 0) {
      # If uk, no or dk, search for both regular abbreviations, and genitive forms
      if (country == "uk" | country == "no" | country == "dk") {
        gen <- unlist(lapply(actor$`_source.partyNameSearchShort`, str_c, genitive))
        names <- paste(c(gen,actor$`_source.partyNameSearchShort`), collapse = '\\\" \\\"')
      } else {
        names <- paste(actor$`_source.partyNameSearchShort`, collapse = '\\\" \\\"')
      }
      # If no or dk, only keep genitive forms if the party abbreviation is longer than 1 character (2 including the genitive s itself)
      if (country == "dk" | country == "no") {
        gen <- gen[which(nchar(gen) > 2)]
      }
      query_string <- paste0('(\\\"',names,'\\\")')
      ids <- str_c(actor$`_source.partyId`,'_s')
      actorid <- str_c(actor$`_source.partyId`,'_s')
      query <- generator(country, actor$`_source.startDate`, actor$`_source.endDate`, query_string, pre_tags, post_tags, actorid)
      df1 <- data.frame(query = query, ids = I(ids), prefix = actor$`_source.notPrecededBy`, postfix = actor$`_source.notFollowedBy`, stringsAsFactors = F)
    }
    if (nchar(actor$`_source.partyNameSearch`[[1]]) > 0) {
      if (country == "uk" | country == "no" | country == "dk") {
        gen <- unlist(lapply(actor$`_source.partyNameSearch`, str_c, genitive))
        names <- paste(c(gen,actor$`_source.partyNameSearch`), collapse = '\\\" \\\"')
      } else {
        names <- paste(actor$`_source.partyNameSearch`, collapse = '\\\" \\\"')
      }
      query_string <- paste0('(\\\"',names,'\\\")')
      ids <- str_c(actor$`_source.partyId`,'_f')
      actorid <- str_c(actor$`_source.partyId`,'_f')
      query <- generator(country, actor$`_source.startDate`, actor$`_source.endDate`, query_string, pre_tags, post_tags, actorid)
      if (country == 'uk') {
        df2 <- data.frame(query = query, ids = I(ids), prefix = actor$`_source.notPrecededBy`, postfix = actor$`_source.notFollowedBy`, stringsAsFactors = F)
      } else {
        df2 <- data.frame(query = query, ids = I(ids), prefix = NA, postfix = NA, stringsAsFactors = F)
      }
    }
    if (exists('df1') == T & exists('df2') == T) {
      return(bind_rows(df1,df2))
    } else if (exists('df1') == T) {
      return(df1)
    } else if (exists('df2') == T) {
      return(df2)
    }
  }
  ### Institution function currently not used
  # if (actor$`_source.function` == "Institution") {
  #   #uppercasing
  #   firstup <- function(x) {
  #     substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  #     x
  #   }
  #   actor$`_source.startDate` <- "2000-01-01"
  #   actor$`_source.endDate` <- "2099-01-01"
  #   if (nchar(actor$`_source.institutionNameSearch`[[1]]) > 0) {
  #     upper <- unlist(lapply(actor$`_source.institutionNameSearch`, firstup))
  #     upper <- c(upper, unlist(lapply(upper, str_c, genitive)),
  #                unlist(lapply(upper, str_c, definitive)),
  #                unlist(lapply(upper, str_c, definitive_genitive)))
  #     capital <- unlist(lapply(actor$`_source.institutionNameSearch`, str_to_title))
  #     capital <- c(capital, unlist(lapply(capital, str_c, genitive)),
  #                  unlist(lapply(capital, str_c, definitive)),
  #                  unlist(lapply(capital, str_c, definitive_genitive)))
  #     base <- actor$`_source.institutionNameSearch`
  #     base <- c(base, unlist(lapply(base, str_c, genitive)),
  #               unlist(lapply(base, str_c, definitive)),
  #               unlist(lapply(base, str_c, definitive_genitive)))
  #     names <- paste(unique(c(upper,capital,base)), collapse = '\\" \\"')
  #     query_string <- paste0('(\\"',names,'\\")')
  #     ids <- toJSON(unlist(lapply(c(actor$`_source.institutionId`),str_c, "_f")))
  #     actorid <- str_c(actor$`_source.institutionId`,'_f')
  #     query <- generator(country, actor$`_source.startDate`, actor$`_source.endDate`, query_string, pre_tags, post_tags, actorid)
  #     df1 <- data.frame(query = query, ids = I(ids), stringsAsFactors = F)
  #   }
  #   if (nchar(actor$`_source.institutionNameSearchShort`[[1]]) > 0) {
  #     upper <- unlist(lapply(actor$`_source.institutionNameSearchShort`, firstup))
  #     upper <- c(upper, unlist(lapply(upper, str_c, genitive)),
  #                unlist(lapply(upper, str_c, definitive)),
  #                unlist(lapply(upper, str_c, definitive_genitive)))
  #     capital <- unlist(lapply(actor$`_source.institutionNameSearchShort`, str_to_title))
  #     capital <- c(capital, unlist(lapply(capital, str_c, genitive)),
  #                  unlist(lapply(capital, str_c, definitive)),
  #                  unlist(lapply(capital, str_c, definitive_genitive)))
  #     base <- actor$`_source.institutionNameSearchShort`
  #     base <- c(base, unlist(lapply(base, str_c, genitive)),
  #               unlist(lapply(base, str_c, definitive)),
  #               unlist(lapply(base, str_c, definitive_genitive)))
  #     names <- paste(unique(c(upper,capital,base)), collapse = '\\" \\"')
  #     query_string <- paste0('(\\"',names,'\\")')
  #     ids <- toJSON(unlist(lapply(c(actor$`_source.institutionId`),str_c, "_s")))
  #     actorid <- str_c(actor$`_source.institutionId`,'_s')
  #     query <- generator(country, actor$`_source.startDate`, actor$`_source.endDate`, query_string, pre_tags, post_tags, actorid)
  #     df2 <- data.frame(query = query, ids = I(ids), stringsAsFactors = F)
  #   }
  #   if (exists('df1') == T & exists('df2') == T) {
  #     return(bind_rows(df1,df2))
  #   } else if (exists('df1') == T) {
  #     return(df1)
  #   } else if (exists('df2') == T) {
  #     return(df2)
  #   }
  # }
}
