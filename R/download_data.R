#' @title Unlist HHSC CCL data
#' @export
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
#' @export
get.hhsc_ccl_data <-  function(data_name,
                               data_in_pth,
                               url = "https://data.texas.gov/api/odata/v4/bc5r-88dy",
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
  
  df_all <- sapply(result$df_all, as.character) %>% 
    as.data.frame(stringsAsFactors = FALSE)
  
  write.csv(df_all, "./data/HHSC_CLL.csv", row.names = FALSE)
  
  return(df_all)
  
}

#' @title Get NBER Tract data
#' @description Link to data: http://data.nber.org/distance/2010/sf1/tract/sf12010tractdistance25miles.csv
#' @details We're using the 25 mile radius rather then the 5 miles radius because the 5 mile radius was missing two counties in Harris County
get.nber_tract_data <- function(data_name,
                                data_in_pth,
                                url = "http://data.nber.org/distance/2010/sf1/tract/{fl}",
                                fl = "sf12010tractdistance25miles.csv") {

  dwnld_pth <- file.path("./data/", fl)

  download.file(glue::glue(url, fl = fl), dwnld_pth)
}

#' @title Get ACF data
#' @description Link to data: https://www.twc.texas.gov/programs/childcare#dataAndReports
get.acf_data <- function(data_name,
                         data_in_pth,
                         url = "https://www.twc.texas.gov/files/partners/{fl}",
                         fl = "acf-801-q{qtr}-2018-twc.xlsx",
                         qtr = 1:4) {

  lapply(qtr, function(q) {

    dwnld_pth <- file.path("./data/", fl)

    download.file(glue::glue(url, fl = glue::glue(fl, qtr = q)), dwnld_pth)

  })

}

#' @title Get the neighborhood to census tract data
#' @description 
#' @export
get.kinder_neighborhood_tract_xwalk <- function(data_name,
                                                data_out_pth,
                                                pth = "https://www.datahouston.org/cta_crosswalk.txt") {
  
  df <- read.csv(url(pth)) %>% 
    dplyr::rename(anchor_tract = GEOID10) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(-id)
  
  return(df)
}
