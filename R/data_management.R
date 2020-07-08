#' @title Scrape census variables from the api website to avoid typos
#' @export
scrape_html <- function(table, url = "https://api.census.gov/data/2018/acs/acs5/subject/groups/{table}.html") {

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
    ifelse(x == -666666666.0, NA, x)
  })
  
  return(df)
}

#' @title Get data from datausaio
#' @export
datausaio <- function(url, cnty_fips) {

  l2 <- lapply(cnty_fips, function(fips, url) {

    url <- glue::glue(url, fips = fips)

    raw <- RCurl::getURL(url)

    json <- rjson::fromJSON(raw)

    data <- json[[1]]

    meta <- json[[2]]

    l <- lapply(1:length(data), function(x) {do.call(cbind, data[[x]])})

    df <- do.call(rbind, l) %>% 
      as.data.frame()
  }, url = url)

  df <- do.call(rbind, l2) %>% 
    as.data.frame()
}

#' @title Create industry crosswalk
create.ind_xwalk <- function(url, cnty_fips) {

  df <- datausaio(url, cnty_fips) %>% 
    dplyr::distinct(`ID Group`, Group, `ID Industry`, Industry) %>% 
    dplyr::rename(ind_group_id = `ID Group`,
                  ind_group = Group,
                  ind_id = `ID Industry`,
                  ind = Industry) %>% 
    dplyr::mutate(ind_group = tolower(ind_group),
                  ind = tolower(ind),
                  ind_group = gsub("&", "and", ind_group),
                  ind = gsub("&", "and", ind),
                  Label = ifelse(grepl("(group)", ind_group), paste(ind, ind, sep = "!!"),
                                 paste(ind_group, ind, sep = "!!"))
  )

  write.csv(df, "./data/ind_xwalk.csv", row.names = FALSE)

  ind_group <- df %>% 
    dplyr::distinct(ind_group, ind_group_id)

  write.csv(ind_group, "./data/ind_group_label.csv", row.names = FALSE)

  ind_group_xwalk <- df %>% 
    dplyr::distinct(ind_group_id, ind_id)

  write.csv(ind_group_xwalk, "./data/ind_group_xwalk.csv", row.names = FALSE)

  ind <- df %>% 
    dplyr::distinct(ind, ind_id)

  write.csv(ind, "./data/ind_label.csv", row.names = FALSE)

  return(df)
}

#' @title Create occupation crosswalk
create.occ_xwalk <- function(url, cnty_fips) {

  df <- datausaio(url, cnty_fips) %>%
    dplyr::distinct(`ID Group`, Group, `ID Subgroup`, Subgroup, `ID Occupation`, Occupation) %>% 
    dplyr::rename(occ_group_id = `ID Group`,
                  occ_group = Group,
                  occ_subgroup_id = `ID Subgroup`,
                  occ_subgroup = Subgroup,
                  occ_id = `ID Occupation`,
                  occ = Occupation) %>% 
    dplyr::mutate(occ_group = tolower(occ_group),
                  occ_subgroup = tolower(occ_subgroup),
                  occ = tolower(occ),
                  occ_group = gsub("&", "and", occ_group),
                  occ_subgroup = gsub("&", "and", occ_subgroup),
                  occ = gsub("&", "and", occ),
                  Label = paste(occ_group, occ_subgroup, occ, sep = "!!"),
                  Label = gsub("educational", "education", Label),
                  Label = gsub("firefighting", "fire fighting", Label))

  write.csv(occ_xwalk, "./data/occ_xwalk.csv", row.names = FALSE)

  occ_group <- df %>% 
    dplyr::distinct(occ_group, occ_group_id)

  write.csv(occ_group, "./data/occ_group_label.csv", row.names = FALSE)

  occ_group_subgroup_xwalk <- df %>% 
    dplyr::distinct(occ_group_id, occ_subgroup_id)

  write.csv(occ_group_subgroup_xwalk, "./data/occ_group_subgroup_xwalk.csv", row.names = FALSE)

  occ_subgroup <- df %>% 
    dplyr::distinct(occ_subgroup, occ_subgroup_id)

  write.csv(occ_subgroup, "./data/occ_subgroup_label.csv", row.names = FALSE)

  occ_subgroup_xwalk <- df %>% 
    dplyr::distinct(occ_subgroup_id, occ_id)

  write.csv(occ_subgroup_xwalk, "./data/occ_subgroup_xwalk.csv", row.names = FALSE)

  occ <- df %>% 
    dplyr::distinct(occ, occ_id)

  write.csv(occ, "./data/occ_label.csv", row.names = FALSE)

  return(df) 
}

#' @title Create occupation crosswalk between census variable names and dataio industry categories
dm.ind_census_datausaio <- function(url, cnty_fips, table) {
  
  ind_xwalk <- create.ind_xwalk(url = url, cnty_fips = cnty_fips)
  
  ind_census_vars <- scrape_html(table) %>% 
    dplyr::mutate(Label = tolower(Label)) %>% 
    dplyr::mutate(Label = gsub("total!!civilian employed population 16 years and over!!", "", Label))
  
   df <- stringr::str_split_fixed(ind_census_vars$Label, "!!", n = 2) %>% 
    as.data.frame() %>% 
    dplyr::mutate(V1 = as.character(V1),
                  V2 = as.character(V2),
                  V2.1 = ifelse(V2 == "", V1, V2),
                  ind_group = tolower(V1),
                  ind = tolower(V2.1),
                  Label = paste(ind_group, ind, sep = "!!")) %>% 
    dplyr::bind_cols(ind_census_vars) %>% 
    dplyr::select(c(Name, ind_group, ind, Label))
  
  ind_xwalk <- ind_xwalk %>%
    dplyr::left_join(df %>% 
                       dplyr::select(Name, Label)) %>% 
    dplyr::rename(var = Name) %>%
    dplyr::select(var, ind, ind_id)
  
  assertthat::assert_that(sum(is.na(ind_xwalk$Name)) == 0)
  
  return(ind_xwalk)
}

#' @title Create occupation crosswalk between census variable names and dataio occupation categories
dm.occ_census_datausaio <- function(url, cnty_fips, table) {

  occ_xwalk <- create.occ_xwalk(url = url, cnty_fips = cnty_fips)
  
  occ_census_vars <- scrape_html(table) %>%
    dplyr::mutate(Label = tolower(Label)) %>%
    dplyr::mutate(Label = gsub("total!!civilian employed population 16 years and over!!", "", Label),
                  Label = gsub("educational", "education", Label),
                  Label = gsub("firefighting", "fire fighting", Label))
  
  df <- stringr::str_split_fixed(occ_census_vars$Label, "!!", n = 3) %>% 
    as.data.frame() %>% 
    dplyr::mutate(V1 = as.character(V1),
                  V2 = as.character(V2),
                  V3 = as.character(V3),
                  V3.1 = ifelse(V3 == "", V2, V3),
                  V2.1 = ifelse(V3 == "", V1, V2),
                  occ_group = tolower(V1),
                  occ_subgroup = tolower(V2.1),
                  occ = tolower(V3.1),
                  Label = paste(occ_group, occ_subgroup, occ, sep = "!!")) %>% 
    dplyr::bind_cols(occ_census_vars) %>% 
    dplyr::select(c(Name, occ_group, occ_subgroup, occ, Label))

  occ_xwalk <- occ_xwalk %>%
    dplyr::left_join(df %>% 
                       dplyr::select(Name, Label)) %>% 
    dplyr::rename(var = Name) %>%
    dplyr::select(var, occ, occ_id)
  
  assertthat::assert_that(sum(is.na(occ_xwalk$Name)) == 0)
  
  return(occ_xwalk)
  
}
