library(magrittr)

key <- yaml::read_yaml("api_key.yaml")

tables <- c("S2405")

#' @title Scrape census variables from the api website to avoid typos
#' @export
scrape_html <- function(url = "https://api.census.gov/data/2018/acs/acs5/subject/groups/{table}.html", table) {
  
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
    dplyr::mutate(Label = gsub("Estimate!!", "", Label))
  
  return(html)

}

#' @title Split the variables into multiple lists that are less than 50 so the census api call can work
#' @export
split_call <- function(id_vars, value_vars) {
  
  n_value_vars <- 50 - length(id_vars)
  
  n_calls <- ceiling(length(value_vars)/n_value_vars)
  
  calls <- split(value_vars, ceiling(seq_along(value_vars)/n_value_vars))
  
  
}

#' @title
#' @export
get_census_api_data <- function(value_vars,
                                id_vars,
                                key,
                                api = "https://api.census.gov/data/2018/acs/acs5/subject?get=",
                                tract_id = "*",
                                state_id = "48") {

  tract_str <- "&for=tract:{tract}"
  state_str <- "&in=state:{state}"
  key_str <- "&key={key}"
  
  vars <- paste(c(id_vars, value_vars), collapse = ",")

  url <- glue::glue(paste0(api, vars, tract_str, state_str, key_str), key = key,
                    tract = tract_id,
                    state = state_id)
  
  r <- httr::GET(url)
  
  c <- httr::content(r)
  
  cnames <- unlist(c[[1]])
  
  c[[1]] <- NULL
  
  x <- lapply(c, 
              function(x) {do.call("cbind", x)
              })
  
  df <- do.call("rbind.data.frame", x)
  
  names(df) <- cnames

  df[value_vars] <- apply(df[value_vars], 2, as.numeric)
  df[id_vars] <- apply(df[id_vars], 2, as.character)

  return(df)
}

#' @title Pull census data
#' @export
census_call <- function(id_vars,
                        value_vars,
                        key) {

  vars <- c(id_vars, value_vars)

  calls <- split_call(id_vars = id_vars,
                      value_vars = value_vars)

  l <- lapply(calls, get_census_api_data, id_vars = id_vars, key = key)

  df <- l %>% 
    purrr::reduce(dplyr::left_join)

  df[value_vars] <- apply(df[value_vars], 2, function(x) {
    ifelse(x == "-666666666.0", NA, x)
  })

  return(df)
}

### 

table_vars <- scrape_html(table = tables[[1]])

df <- census_call(id_vars = c("TRACT", "COUNTY"),
                  value_vars = table_vars$Name,
                  key = key$census)
