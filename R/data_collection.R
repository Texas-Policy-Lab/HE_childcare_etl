unlist_hhsc_ccl_data <- function(c) {

  create_row <- function(row) {

    nested <- lapply(row, length) %>% 
      purrr::keep(. > 1)
 
    if (length(nested) > 0) { 

      assertthat::assert_that(names(nested) %in% c("location_address_geo"))

      nested <- row[[names(nested)]]$coordinates

      names(nested) <- c("longitude", "latitude")

      keep_names <- lapply(row, length) %>% 
        purrr::keep(. <= 1)

      row <- row[names(keep_names)]

      row <- c(row, nested)
    }

    r <- do.call(cbind, row) %>% 
      as.data.frame()
    return(r)
  }

  l <- lapply(c$value, create_row)
  
  return(do.call(rbind.fill, l))
}

#' @title Returns the most recent HHSC CLL Daycare and Residential Operations Data
#' @description Link to data: https://data.texas.gov/Social-Services/HHSC-CCL-Daycare-and-Residential-Operations-Data/bc5r-88dy/data
get.hhsc_ccl_data <-  function(url = "https://data.texas.gov/api/odata/v4/bc5r-88dy",
                               df_all = data.frame()) {

  api_call <- function(url, df_all) {
  
    r <- httr::GET(url)
    
    if (r$status_code == 200) {
  
      c <- httr::content(r)
  
      df <- unlist_hhsc_ccl_data(c)
  
      df_all <- plyr::rbind.fill(df_all, df)
      
      return(list(df_all = df_all,
                  url = c[["@odata.nextLink"]]))
  
    } else {
      warning("status not 200")
    }
  }

  result <- api_call(url, df_all)
  
  while(!is.null(result$url)) {
    print(result$url)
    result <- api_call(url = result$url,
                       df_all = result$df_all) 
  }
  
  write.csv(df, "./data/HHSC_CLL.csv", row.names = FALSE)
  
  return(result$df_all)

}

dm.geocode_address <- function(url = "https://api.geocod.io/v1.6/") {
  
}





