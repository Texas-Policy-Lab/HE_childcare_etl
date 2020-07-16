library(magrittr)

api_key <- yaml::read_yaml("api_key.yaml")
config <- yaml::read_yaml("config.yaml")

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

data_in_pth <- config$data_pths$in_pth
data_out_pth <- config$data_pths$out_pth

tracts_xwalk <- dm.tracts_xwalk(data_in_name = config$data_in_names$nber,
                                data_in_pth = data_in_pth,
                                data_out_name = config$data_out_names$tracts_xwalk,
                                data_out_pth = data_out_pth)

acf <- dm.merge_acf(data_in_name = config$data_in_names$acf,
                    data_in_pth = data_in_pth,
                    data_out_name = config$data_out_names$acf_all, 
                    data_out_pth = data_out_pth)

ccl <- dm.ccl(ccl_data_in_pth = data_in_pth,
              ccl_data_in_name = config$data_in_names$ccl,
              ccl_data_out_pth = data_out_pth,
              ccl_data_out_name = config$data_out_names$ccl)

lat_long <- dm.geocode_address(addresses = ccl$location_address,
                               key = api_key$mapquest$key)

write.csv(lat_long, "mapquest_geocode.csv", row.names = FALSE)

ccl_address <- ccl %>%
  dplyr::filter(!is.na(latitude) & !is.na(longitude)) %>%
  dplyr::select(operation_number, latitude, longitude)

latLng <- lapply(1:nrow(ccl_address), function(add, df) {
  
  list(lat = df$latitude[add],
       lng = df$longitude[add])

}, df = ccl_address)

x <- dm.reverse_geocode(latLng = latLng,
                        key = api_key$mapquest$key)

family_prvdr_zip <- dm.family_prvdr_zip(acf_data_in_pth = data_out_pth,
                                        acf_data_in_name = config$data_out_names$acf_all,
                                        ccl_data_in_pth = data_out_pth,
                                        ccl_data_in_name = config$data_out_names$ccl,
                                        data_out_pth = data_out_pth,
                                        data_out_name = config$data_out_names$family_prvdr_zip)
