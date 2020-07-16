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
#' @param geocode logical. Default value is FALSE. Indicates whether geocoding off addresses should be applied.
dm.ccl <- function(ccl_data_in_pth,
                   ccl_data_in_name,
                   ccl_data_out_pth,
                   ccl_data_out_name,
                   geocode = FALSE,
                   key = key) {

  df <- readr::read_csv(file.path(ccl_data_in_pth, ccl_data_in_name)) %>% 
    dplyr::mutate(operation_number = gsub("-.*", "", operation_number))

  if(geocode) {

    geocode <- dm.geocode_address(addresses = ccl,
                                  key = key)

    assertthat::assert_that(all(c("lat", "long") %in% names(geocode)))

    df <- df %>% 
      dplyr::select(-c(county, latitude, longitude)) %>%
      dplyr::left_join(geocode %>% 
                         dplyr::rename(latitude = lat,
                                       longitude = long))

  }

  write.csv(df, file.path(ccl_data_out_pth, ccl_data_out_name), row.names = FALSE)

  return(df)
}

#' @title Pulls down bounding box parameters for Texas
#' @export
dm.tx_bounding_box <- function(url = "https://gist.githubusercontent.com/a8dx/2340f9527af64f8ef8439366de981168/raw/81d876daea10eab5c2675811c39bcd18a79a9212/US_State_Bounding_Boxes.csv",
                               texas_fips = "48") {

  df <- readr::read_csv(url)

  assertthat::assert_that(all(c("STATEFP", "xmin", "ymin", "xmax", "ymax") %in% names(df)))
  assertthat::assert_that(typeof(df$STATEFP) == "character")
    
  df <- df %>% 
    dplyr::filter(STATEFP == texas_fips)

  return(list(ul = list(lng = df$xmin, lat = df$ymax),
       lr = list(lng = df$xmax, lat = df$ymin)
       )
  )
}

#' @title Split calls
#' @description Splits list into multiple groups for batch calls with limits per call
#' @param v vector. Vector to split into multiple groups
#' @param limit integer. The max size of each group.
#' @export
split_calls <- function(v, limit) {

  if (length(v) > limit) {
    calls <- split(v, ceiling(seq_along(v)/limit))
  } else {
    calls <- list(v)
  }

  return(calls)
}

#' @title Response -> Dataframe
#' @description Turns the response from mapquest into a dataframe
#' @export
dm.geocode_request <- function(c) {

  l <- lapply(1:length(c$results), function(x) {
    
    row <- c$results[[x]]$locations[[1]]
    
    df <- data.frame(street = row$street,
                     neighborhood = row$adminArea6,
                     city  = row$adminArea5,
                     county = row$adminArea4,
                     state = row$adminArea3,
                     zip = row$postalCode,
                     lat = row$latLng$lat,
                     long = row$latLng$lng,
                     geocodeQualityCode = row$geocodeQualityCode,
                     mapURl= row$mapUrl,
                     stringsAsFactors = FALSE
    )
  })
  
  df <- do.call(rbind, l)
  
  return(df)
}

#' @title Drops poor quality geocodes
#' @description Use the geocodeQualityCode value returned to determine the quality of the geocode. https://developer.mapquest.com/documentation/geocoding-api/quality-codes/.
#' @export
dm.drop_poor_quality <- function(df,
                                 qualityCode = "A1|A3|A4") {
  
  poorQuality <- stringr::str_starts(string = df$geocodeQualityCode, pattern = qualityCode)
  
  df <- df %>% 
    dplyr::mutate(lat = ifelse(poorQuality, NA, lat),
                  long = ifelse(poorQuality, NA, long)
    )

  return(df)
}

#' @title Geocode addresses
#' @descrption Geocodes addresses using the Mapquest API
#' @param addresses vector. The list of addresses to geocode.
#' @param key string. The api key registered with your personal Mapquest account.
#' @export
dm.geocode_address <- function(addresses,
                               key,
                               version = "v1",
                               url = "http://www.mapquestapi.com/geocoding/{version}/batch?key={key}",
                               limit = 100) {

  bb <- dm.tx_bounding_box()

  assertthat::assert_that("location_address" %in% names(addresses))
  
  calls <- split_calls(v = addresses$location_address,
                       limit = limit)

  l <- lapply(calls, function(call, url, version, key) {
    r <- httr::POST(url = glue::glue(url, version = version, key = key),
                    query = list(key = key),
                    body = list(locations = call,
                                boundingBox = bb,
                                maxResults = 1),
                    encode = "json")

    if(r$status_code == 200) {

      c <- httr::content(r)

      df <- dm.geocode_request(c)

      return(df)

    } else {
      warning("status not 200") 
    }

  }, url = url, version = version, key = key)

  df <- do.call(rbind, l)
  
  df <- df %>% 
    dplyr::bind_cols(addresses %>% 
                       dplyr::select(operation_number))
  
  df <- dm.drop_poor_quality(df)

  return(df)
}

#' @title Geocode addresses
#' @descrption Geocodes addresses using the Mapquest API
#' @param latLng list. The list of addresses to geocode.
#' @param key string. The api key registered with your personal Mapquest account.
#' @examples 
#'  latLng <- list(lat = 30.333472, lng = -81.470448)
#'  key <- "XXXXX"
#'  address <- dm.reverse_geocode(latLng = latLng, key = key)
#' @export
dm.reverse_geocode <- function(latLng,
                               key,
                               version = "v1",
                               url = "http://www.mapquestapi.com/geocoding/{version}/reverse?key={key}") {

  l <- lapply(latLng, function(x, url, version, key) {
    r <- httr::POST(url = glue::glue(url, version = version, key = key),
                    query = list(key = key),
                    body = list(location = list(latLng = x)),
                    encode = "json")

    if(r$status_code == 200) {

      c <- httr::content(r)

      df <- dm.geocode_request(c)

    } else {
      warning("status not 200")
    }

  }, url = url, version = version, key = key)

  df <- do.call(rbind, l)

  return(df)
}

