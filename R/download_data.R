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
#' @param data_in_name string. The name to of the data to read in.
#' @param data_in_pth string. The path to read the data in from.
#' @export
get.hhsc_ccl_data <-  function(data_in_name,
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

  write.csv(df_all, file.path(data_in_pth, data_in_name), row.names = FALSE)
}

#' @title Get NBER Tract data
#' @description Link to data: http://data.nber.org/distance/2010/sf1/tract/sf12010tractdistance25miles.csv
#' @details We're using the 25 mile radius rather then the 5 miles radius because the 5 mile radius was missing two counties in Harris County
#' @param data_in_name string. The name of the file to download.
#' @param data_in_pth string. The path to read the data in from.
#' @export 
get.nber_tract_data <- function(data_in_name,
                                data_in_pth,
                                url = "http://data.nber.org/distance/2010/sf1/tract/{fl}") {

  dwnld_pth <- file.path(data_in_pth, data_in_name)

  download.file(glue::glue(url, fl = data_in_name), dwnld_pth)
}

#' @title Get ACF data
#' @description Link to data: https://www.twc.texas.gov/programs/childcare#dataAndReports
#' @param data_in_name string. The name to of the data to read in.
#' @param data_in_pth string. The path to read the data in from.
#' @export
get.acf_data <- function(data_in_name,
                         data_in_pth,
                         url = "https://www.twc.texas.gov/files/partners/{fl}",
                         qtr = 1:4) {

  lapply(qtr, function(q) {

    fl_name <- glue::glue(data_in_name, qtr = q)
    
    dwnld_pth <- file.path(data_in_pth, fl_name)

    download.file(glue::glue(url, fl = fl_name), destfile = dwnld_pth, mode = "wb")

  })

}

#' @title Get the neighborhood to census tract data
#' @description Download the neighbordhood to census tract cross walk create by the Kinder Institute. https://www.arcgis.com/apps/MapSeries/index.html?appid=95320b06677c438d91027cb5feb241bf
#' @param data_in_name string. The name to of the data to read in.
#' @param data_in_pth string. The path to read the data in from.
#' @export
get.kinder_neighborhood_tract_xwalk <- function(data_in_name,
                                                data_in_pth,
                                                pth = "https://www.datahouston.org/cta_crosswalk.txt") {

  df <- read.csv(url(pth)) %>%
    dplyr::rename(anchor_tract = GEOID10) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::select(-id)

  write.csv(df, file.path(data_in_pth, data_in_name), row.names = FALSE)
}

#' @title Get tract by latitude and longitude
#' @description Downloads shape file for Harris County (201) Texas (48) using the tigris package, which pulls the most recent shape from the United States Census Bureau.
#' @param data_in_name string. The name to of the data to read in.
#' @param data_in_pth string. The path to read the data in from.
#' @export
get.tract_shape <- function(data_in_name,
                            data_in_pth,
                            state_fips = 48,
                            county_fips = 201) {

  geo <- tigris::tracts(state = state_fips, county = county_fips, cb = TRUE)

  geo <- geo %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::rename(anchor_tract = geoid)

  write.csv(geo, file.path(data_in_pth, data_in_name), row.names = FALSE)
}

#' @title Get State FIPS and State Name Crosswalk
#' @description Downloads the crosswalk between county fips codes and county names for Texas from TIGRIS package
#' @param data_in_name string. The name to of the data to read in.
#' @param data_in_pth string. The path to read the data in from.
#' @export
get.state_fips_state_name_xwalk <- function(data_in_name,
                                            data_in_pth,
                                            state_fips = 48) {

  cnty <- tigris::counties(state = state_fips) %>% 
    dplyr::select(NAME, NAMELSAD, COUNTYFP) %>% 
    dplyr::rename(COUNTY_FIPS = COUNTYFP) %>% 
    dplyr::rename_all(tolower)
  
  sf::st_geometry(cnty) <- NULL

  assertthat::assert_that(nrow(cnty) == 254)

  write.csv(cnty, file.path(data_in_pth, data_in_name), row.names = FALSE)
}

#' @title Get ZIP shape
#' @param data_in_name string. The name to of the data to read in.
#' @param data_in_pth string. The path to read the data in from.
#' @export
get.zip_shape <- function(data_in_name,
                          data_in_pth,
                          state = 48) {

  geo <- tigris::zctas(state = state, cb = TRUE)
  
}

#' @title Get PULSE Public Use Files
#' @description Download data from: https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html
#' @param data_in_name string. The name to of the data to read in.
#' @param data_in_pth string. The path to read the data in from.
#' @export
get.pulse_puf <- function(data_in_name,
                          data_in_pth,
                          url = "https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html") {

  urls <- xml2::read_html(url) %>%
    rvest::html_nodes(".uscb-text-link") %>%
    rvest::html_attr("href")

  urls <- paste0("https:",urls[grepl(pattern = 'CSV', urls)])

  lapply(urls, function(url) {

    fl_name <- sub(".*/", "", url)
    
    dwnld_pth <- file.path(data_in_pth, fl_name)

    download.file(url, destfile = dwnld_pth, mode = "wb")

    l <- unlist(unzip(dwnld_pth))

    lapply(l, function(x) {
      file.rename(from = x,
                  to = file.path(data_in_pth, x))
    })
  })

}
