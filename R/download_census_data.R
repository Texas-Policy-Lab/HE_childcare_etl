#' @title Scrape census variables from the api website to avoid typos
#' @export
data_dictionary.census <- function(table, url) {
  
  url <- glue::glue(url, table = table)
  
  html <- xml2::read_html(url) %>% 
    rvest::html_table(fill = TRUE) %>% 
    .[[1]] 
  
  html <- html[!is.na(names(html))]
  
  html <- html %>% 
    dplyr::filter(Group == table) %>%
    dplyr::select(Name, Label) %>% 
    dplyr::mutate(est = ifelse(grepl("Estimate", Label), TRUE, FALSE),
                  est_anttn = ifelse(grepl("Annotation of Estimate", Label), TRUE, FALSE),
                  moe = ifelse(grepl("Margin of Error", Label), TRUE, FALSE),
                  moe_anttn = ifelse(grepl("Annotation of Margin of Error", Label), TRUE, FALSE),
                  est = ifelse(est & !est_anttn, TRUE, FALSE),
                  moe = ifelse(moe & !moe_anttn, TRUE, FALSE)
    ) %>% 
    dplyr::filter(est) %>% 
    dplyr::filter(!grepl("Total!!PERCENT ALLOCATED!!Industry", Label)) %>% 
    dplyr::mutate(Label = gsub("Estimate!!", "", Label))
  
  return(html)
  
}

data_dictionary.acs5.subject <- function(table,
                                         url = "https://api.census.gov/data/2018/acs/acs5/subject/groups/{table}.html") {
  
  df <- data_dictionary.census(table, url)
  
}

data_dictionary.acs5 <- function(table,
                                 url = "https://api.census.gov/data/2018/acs/acs5/groups/{table}.html") {
  
  df <- data_dictionary.census(table, url)
  
}

#' @title Get Census response
#' @description Formats the Census response endpoint
#' @param r list. The formatted endpoint response.
#' @inheritParams acs_api
#' @export
get.census_response <- function(r,
                                group_value_vars,
                                id_vars) {

  if(r$status_code == 200) {
    
    c <- httr::content(r)
    
    cnames <- unlist(c[[1]])
    
    c[[1]] <- NULL
    
    x <- lapply(c, 
                function(x) {do.call("cbind", x)
                })
    
    df <- do.call("rbind.data.frame", x)
    
    names(df) <- cnames
    
    df[group_value_vars] <- apply(df[group_value_vars], 2, as.numeric)
    df[id_vars] <- apply(df[id_vars], 2, as.character)
    
    return(df)
    
  } else {
    warning("status is not 200, returning null")
  }
}

#' @title Makes the call the the census api
#' @param id_vars vector. The ID variables to include in each table e.g. TRACT, COUNTY
#' @param value_vars vector. The non ID variables to pull down such as B10001E
#' @param key string. Census API key
#' @param table_type string. NULL for detailed table, subject for subject tables, profile for profile tables, and cprofile for comparison tables
#' @param path string. Extra paths to add to endpoint.
#' @param tract string. Default is "*" (all). Can specify a vector of selected tracts however.
#' @param state string. Default is 48 for Texas. Can specify a vector of selected states however.
#' @param endpoint string. The census endpoint for ACS 5.
#' @export
acs_api <- function(id_vars,
                    value_vars,
                    key,
                    year = 2018,
                    table_type = NULL,
                    path = "data/{year}/acs/acs5/{table_type}",
                    tract = "*",
                    state = "48",
                    endpoint = "https://api.census.gov/") {

  assertthat::assert_that(table_type %in% c(NULL, "subject", "profile", "cprofile"))

  var_groups <- split_calls(v = value_vars,
                            limit = c(50 - length(id_vars)))

  l <- lapply(var_groups, function(group_value_vars, endpoint, path, id_vars, tract, state, key) {

    r <- httr::GET(url = endpoint,
                   path = glue::glue(path, year = year, table_type = table_type),
                   query = list(get = paste(c(id_vars, group_value_vars), collapse = ","),
                                  `for` = glue::glue("tract:{tract}", tract = tract),
                                  `in` = glue::glue("state:{state}", state = state),
                                  key = key)
    )

    df <- get.census_response(r,
                              group_value_vars = group_value_vars,
                              id_vars = id_vars)    

  }, endpoint = endpoint, path = path, id_vars = id_vars,
  tract = tract, state = state, key = key)

  df <- l %>%
    purrr::reduce(dplyr::left_join)

  df[value_vars] <- apply(df[value_vars], 2, function(x) {ifelse(x %in% c(-666666666.0, -888888888), NA, x)})

  return(df)
}

#' @title Get Data for specified variables from ACS
#' @export
get.acs <- function(...) UseMethod("get.acs")

#' @title Get Data for specified variables from American Community Survey 5 year estimates.
#' @inheritParams acs_api
#' @export
get.acs5.default <- function(id_vars,
                             value_vars,
                             key, 
                             ...) {

  df <- acs_api(id_vars = id_vars,
                value_vars = value_vars,
                key = key,
                ...)

  return(df)
}

#' @title Get Data for specified variables from ACS Subject table
#' @inheritParams acs_api
#' @export
get.acs5.subject <- function(id_vars,
                             value_vars,
                             key,
                             table_type = "subject",
                             ...) {

  df <- get.acs5.default(id_vars = id_vars,
                         value_vars = value_vars,
                         key = key,
                         table_type = table_type)

  return(df)
}

