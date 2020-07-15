#' @title Tracts crosswalk
#' @description Creates a crosswalk between anchor tracts in harris county and surround tracts within 3 miles of each anchor tract
#' @param data_name string. The name to of the data to read in.
#' @param data_in_pth string. The path to read the data in from.
#' @param data_out_pth string. The path to wrtie the data out to.
#' @export
dm.tracts_xwalk <- function(data_name,
                            data_in_pth,
                            data_out_pth,
                            harris_fips = "48201") {

  df <- readr::read_csv(file.path(data_in_pth, data_name))

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

  write.csv(df, file.path(data_out_pth, "tracts_xwalk.csv"), row.names = FALSE)

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
