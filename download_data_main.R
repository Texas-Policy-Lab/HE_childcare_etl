library(plyr)
library(magrittr)

config <- yaml::read_yaml("config.yaml")
api_key <- yaml::read_yaml("api_key.yaml")

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

lapply(config$pkg, dwnld_pkg)

data_in_pth <- config$data_pths$in_pth

## Child care data

get.hhsc_ccl_data(data_in_name = config$data_in_names$ccl,
                  data_in_pth = data_in_pth)

get.acf_data(data_in_name = config$data_in_names$acf,
             data_in_pth = data_in_pth)

## Geographic data

get.nber_tract_data(data_in_name = config$data_in_names$nber,
                    data_in_pth = data_in_pth)

get.kinder_neighborhood_tract_xwalk(data_in_name = config$data_in_names$kinder_neighborhoods,
                                    data_in_pth = data_in_pth)

get.tract_shape(data_in_name = config$data_in_names$tract_shape,
                data_in_pth = data_in_pth)

get.state_fips_state_name_xwalk(data_in_name = config$data_in_names$state_fips_state_name_xwalk,
                                data_in_pth)

## Download census tables

subject <- sapply(config$data_in_names$census$subject, 
                  function(table, pth, id_vars, value_vars, table_type, key, year, est_year) {

  df <- get.acs(pth = pth,
                table = table,
                id_vars = id_vars,
                value_vars = value_vars,
                table_type = table_type,
                year = year,
                est_year = est_year,
                key = key)

}, 
   pth = data_in_pth,
   id_vars = c("TRACT", "COUNTY"),
   value_vars = NULL,
   table_type = "subject",
   year = config$data_in_names$census$year,
   est_year = config$data_in_names$census$est_year,
   key = api_key$census$key,
   USE.NAMES = TRUE,
   simplify = FALSE)

detail <- sapply(config$data_in_names$census$detail, 
                  function(table, pth, id_vars, value_vars, table_type, key, year, est_year) {
                    
                    df <- get.acs(pth = pth,
                                  table = table,
                                  id_vars = id_vars,
                                  value_vars = value_vars,
                                  table_type = table_type,
                                  year = year,
                                  est_year = est_year,
                                  key = key)
                    
                  }, 
                  pth = data_in_pth,
                  id_vars = c("TRACT", "COUNTY"),
                  value_vars = NULL,
                  table_type = "detail",
                  year = config$data_in_names$census$year,
                  est_year = config$data_in_names$census$est_year,
                  key = api_key$census$key,
                  USE.NAMES = TRUE,
                  simplify = FALSE)

pt <- get.poverty_thresholds(data_in_name = config$data_in_names$poverty_threshold,
                             data_in_pth = data_in_pth)
