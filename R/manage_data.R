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
