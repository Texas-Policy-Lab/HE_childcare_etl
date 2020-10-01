#' @title Creates the ACS table name to match census labeling
acs_table_name <- function(est_year, year, table) {
  name <- glue::glue("ACSDT{est_year}Y{year}.{table}.csv",
                     est_year = est_year,
                     year = year,
                     table = table)
  return(name)
}

#' @title Data Dictionary for ACS5
#' @inheritParams get.acs5
#' @param url string. The url to join with the endpoint.
#' @export
get.census_data_dict <- function(table,
                                 pth, 
                                 year,
                                 est_year,
                                 endpoint,
                                 path,
                                 keep_est,
                                 keep_annote_est,
                                 keep_moe,
                                 keep_annote_moe,
                                 url = "groups/{table}.html", ...) {

  url <- paste(endpoint, path, glue::glue(url, table = table), sep = "/")

  dct <- data.frame(est = keep_est,
                    annote_est = keep_annote_est,
                    moe = keep_moe,
                    annote_moe = keep_annote_moe) %>% 
    tidyr::gather(variable, keep)

  df <- xml2::read_html(url) %>% 
    rvest::html_table(fill = TRUE) %>% 
    .[[1]]

  df <- df[!is.na(names(df))]

  assertthat::assert_that(all(c("Name", "Label") %in% names(df)))

  df <- df %>%
    dplyr::mutate(est = stringr::str_ends(Name, "E"),
                  annote_est = stringr::str_ends(Name, "EA"),
                  moe = stringr::str_ends(Name, "M"),
                  annote_moe = stringr::str_ends(Name, "MA")) %>% 
    dplyr::filter(est | annote_est | moe | annote_moe) %>% 
    dplyr::select(Name, Label, est, annote_est, moe, annote_moe) %>% 
    tidyr::gather(variable, value, -c(Name, Label)) %>% 
    dplyr::left_join(dct) %>% 
    dplyr::filter(value & keep) %>% 
    dplyr::select(-c(value, variable, keep)) %>% 
    dplyr::rename(variable = Name,
                  label = Label)

  name <- acs_table_name(est_year = est_year,
                         year = year,
                         table = table)
  
  write.csv(df, file.path(pth,
                          paste0("data_dic_", name)),
            row.names = FALSE)

  return(df)
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
#' @inheritParams get.acs5
#' @export
get.census_api <- function(id_vars,
                           value_vars,
                           key,
                           year,
                           table_type,
                           path,
                           tract,
                           state,
                           endpoint,
                           var_limit = 50,
                           ...) {

  var_groups <- split_calls(v = value_vars,
                            limit = c(var_limit - length(id_vars)))

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

  }, endpoint = endpoint,
     path = path,
     id_vars = id_vars,
     tract = tract,
     state = state,
     key = key)

  df <- l %>%
    purrr::reduce(dplyr::left_join) %>%
    dplyr::select(-c(TRACT, COUNTY))

  df[value_vars] <- apply(df[value_vars], 2, function(x) {ifelse(x %in% c(-666666666.0, -888888888), NA, x)})

  return(df)
}


create_path <- function(...) UseMethod("create_path")

create_path.default <- function(cls, ...) {

  p <- glue::glue("data/{year}/acs/acs{est_year}/{table_type}",
                   year = cls$year,
                   est_year = cls$est_year,
                   table_type = cls$table_type)

  return(p)
}

create_path.detail <- function(cls, ...) {

  p <- glue::glue("data/{year}/acs/acs{est_year}",
                   year = cls$year,
                   est_year = cls$est_year)

  return(p)
}

#' @title Get Data for specified variables from American Community Survey XX year estimates.
#' @param id_vars vector. The ID variables to include in each table e.g. TRACT, COUNTY
#' @param value_vars vector. The non ID variables to pull down such as B10001E
#' @param keep_est logical. Keep all variables that are estimates. Default is TRUE
#' @param keep_annote_est logical.Keep all variables taht are annotations of estimates. Default is FALSE.
#' @param keep_moe logical. Keep all variables that margin of errors. Default is FALSE.
#' @param keep_annote_moe logical. Keep all variables that are annotations of margin of errors. Default is FALSE.
#' @param key string. Census API key.
#' @param table_type string. 'detail' for detailed table, 'subject' for subject tables, 'profile' for profile tables, and 'cprofile' for comparison tables
#' @param path string. Extra paths to add to endpoint.
#' @param tract string. Default is "*" (all). Can specify a vector of selected tracts however.
#' @param state string. Default is 48 for Texas. Can specify a vector of selected states however.
#' @param endpoint string. The census endpoint for ACS 5.
#' @param year int. The year
#' @param est_year. The estimate year, taking on the values of 1, 3, or 5.
#' @export
get.acs <- function(table,
                    pth,
                    id_vars,
                    value_vars = NULL,
                    year,
                    est_year,
                    table_type,
                    key,
                    keep_est = TRUE,
                    keep_annote_est = FALSE,
                    keep_moe = FALSE,
                    keep_annote_moe = FALSE,
                    tract = "*",
                    state = "48",
                    endpoint = "https://api.census.gov",
                    ...) {

  assertthat::assert_that(table_type %in% c("detail", "subject", "profile", "cprofile"))
  assertthat::assert_that(length(table_type) == 1)
  assertthat::assert_that(est_year %in% c(1, 3, 5))
  assertthat::assert_that(length(est_year) == 1)

  cls <- structure(list(pth = pth,
                        table = table,
                        id_vars = id_vars,
                        value_vars = value_vars,
                        key = key,
                        keep_est = keep_est,
                        keep_annote_est = keep_annote_est,
                        keep_moe = keep_moe,
                        keep_annote_moe = keep_annote_moe,
                        year = year,
                        est_year = est_year,
                        tract = tract,
                        state = state,
                        endpoint = endpoint,
                        table_type = table_type),
                   class = table_type)

  cls$path <- create_path(cls)
  
  if(is.null(cls$value_vars)) {

    labs <- do.call("get.census_data_dict", cls)

    cls$value_vars <- labs$variable
  }

  df <- do.call("get.census_api", cls)

  Hmisc::label(df) <- as.list(labs$label[match(names(df), labs$variable)])

  name <- acs_table_name(est_year = est_year,
                         year = year,
                         table = table)

  write.csv(df, file.path(pth, name), row.names = FALSE)

  return(df)
}
