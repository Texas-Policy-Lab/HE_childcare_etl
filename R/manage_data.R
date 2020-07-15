#' @title Tracts crosswalk
#' @description Creates a crosswalk between anchor tracts in harris county and surround tracts within 3 miles of each anchor tract
#' @param data_in_name string. The name of the data to read in.
#' @param data_in_pth string. The path to read the data in from.
#' @param data_out_name string. The name of the data to write out.
#' @param data_out_pth string. The path to wrtie the data out to.
#' @export
dm.tracts_xwalk <- function(data_in_name,
                            data_in_pth,
                            data_out_name,
                            data_out_pth,
                            harris_fips = "48201") {

  df <- readr::read_csv(file.path(data_in_pth, data_in_name))

  assertthat::assert_that(typeof(df$county1) == "character")

  df <- df %>% 
    dplyr::filter(county1 == harris_fips) %>% 
    dplyr::mutate(anchor_tract = paste0(county1, tract1),
                  surround_tract = paste0(county2, tract2))

  tracts <- df %>% 
    dplyr::distinct(anchor_tract) %>% 
    dplyr::mutate(surround_tract = anchor_tract)
  
  assertthat::assert_that(nrow(tracts) == 786)
  
  df <- df %>% 
    dplyr::filter(mi_to_tract <= 3) %>% 
    dplyr::bind_rows(tracts) %>% 
    dplyr::select(anchor_tract, surround_tract)

  write.csv(df, file.path(data_out_pth, data_out_name), row.names = FALSE)

  return(df)
}

#' @title Create a crosswalk between family ID and provider fips code
#' @param data_in_name string. The name to of the data to read in.
#' @param data_in_pth string. The path to read the data in from.
#' @param data_out_name string. The name of the data to write out.
#' @param data_out_pth string. The path to wrtie the data out to.
#' @param qtr vector. Quarters to read in. Default is c(1,2,3,4).
#' @export
dm.merge_acf <- function(data_in_name,
                         data_in_pth,
                         data_out_name,
                         data_out_pth,
                         qtr = 1:4,
                         sheet = "ChildrenParentsSettings") {

  lapply(c("future.apply", "future", "readxl"), dwnld_pkg)

  future::plan(multiprocess)

  fls <- glue::glue(data_in_name, qtr = qtr)

  pths <- file.path(data_in_pth, fls)

  l <- future.apply::future_lapply(qtr, function(qtr, data_in_name, data_in_pth, sheet) {

    fl <- glue::glue(data_in_name, qtr = qtr)

    pth <- file.path(data_in_pth, fl)
    
    df <- readxl::read_excel(path = pth, sheet = sheet) %>% 
      dplyr::mutate(quarter = qtr)
    
    }, data_in_name = data_in_name, data_in_pth = data_in_pth, sheet = sheet)

  df <- do.call(rbind, l)

  assertthat::assert_that(all(qtr %in% unique(df$quarter)))

  write.csv(df, file.path(data_out_pth, data_out_name), row.names = FALSE)

  return(df)
}

#' @title Create a crosswalk between family ID and provider fips code
#' @param acf_data_in_name string. The name of the acl data to read in.
#' @param acf_data_in_pth string. The path to read the acl data in from.
#' @param ccl_data_in_name string. The name of the ccl data to read in.
#' @param ccl_data_in_pth string. The path to the ccl data.
#' @export
dm.family_prvdr_zip <- function(acf_data_in_pth,
                                acf_data_in_name,
                                ccl_data_in_pth,
                                ccl_data_in_name,
                                data_out_pth,
                                data_out_name) {

  df <- readr::read_csv(file.path(acf_data_in_pth, acf_data_in_name)) %>% 
    dplyr::rename(operation_number = CCSettings.ProviderStateID,
                  parent_id = ParentsID,
                  parent_fips = Parents.FIPS,
                  parent_zip = Parents.FamilyZip) %>% 
    dplyr::filter(parent_fips == 48201) %>% 
    dplyr::distinct(operation_number, parent_id, parent_fips, parent_zip, quarter) %>% 
    dplyr::mutate(operation_number = as.character(operation_number)) %>% 
    dplyr::left_join(readr::read_csv(file.path(ccl_data_in_pth, ccl_data_in_name)) %>% 
                       dplyr::select(operation_number, latitude, longitude, county) %>% 
                       dplyr::mutate(operation_number = as.character(operation_number))
                     )

  write.csv(df, file.path(data_out_pth, data_out_name), row.names = FALSE)

  return(df)
}

#' @title CCL data management steps
#' @param ccl_data_in_pth string. The path to read the ccl data in from.
#' @param ccl_data_in_name string. The name of the raw data to read in.
#' @param ccl_data_out_pth string. The path to write the cleaned ccl data to.
#' @param ccl_data_out_name string. The name of the data to write out. 
dm.ccl <- function(ccl_data_in_pth,
                   ccl_data_in_name,
                   ccl_data_out_pth,
                   ccl_data_out_name) {

  df <- readr::read_csv(file.path(ccl_data_in_pth, ccl_data_in_name)) %>% 
    dplyr::mutate(operation_number = gsub("-.*", "", operation_number))

  write.csv(df, file.path(ccl_data_out_pth, ccl_data_out_name), row.names = FALSE)

  return(df)
}

#' @title Geocode addresses
#' @descrption Geocodes addresses using the Geocodio API
#' @param key string. The api key registered with your personal Geocodio account.
#' @param addresses vector. The list of addresses to geocode.
#' @export
dm.geocode_address <- function(key,
                               addresses,
                               version = "v1.6",
                               url = "https://api.geocod.io/{version}/geocode",
                               limit = 10000) {
  
  if (length(addresses) > limit) {
    calls <- split(addresses, ceiling(seq_along(addresses)/limit))
  } else {
    calls <- list(addresses)
  }

  l <- lapply(calls, function(call, url, version) {
    r <- httr::POST(url = glue::glue(url, version = version),
                    query = list(api_key = key),
                    body = list(call),
                    encode = "json")

    if(r$status_code == 200) {

      r <- jsonlite::fromJSON(httr::content(r,
                                            as = "text",
                                            encoding = "UTF-8"),
                              flatten = TRUE)
      r <- r$results
      browser()
      new_names <- gsub("\\.", "_", colnames(r))
      new_names <- gsub("response_input|address_components|fields", "", new_names)
      new_names <- gsub("[_]+", "_", new_names)
      new_names <- sub("^_", "", new_names)

      r <- set_names(r, new_names)

      as.data.frame(r)
    } else {
      warning("status not 200") 
    }

  }, url = url, version = version)
  
}
