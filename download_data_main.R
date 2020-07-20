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

s0601 <- get.acs5(data_in_name = config$data_in_names$census$S0601_2018,
                  data_in_pth = data_in_pth,
                  id_vars = c("TRACT", "COUNTY"),
                  value_vars = NULL,
                  table_type = "subject",
                  key = api_key$census$key)

s1201 <- get.acs5(data_in_name = config$data_in_names$census$S1201_2018,
                  data_in_pth = data_in_pth,
                  id_vars = c("TRACT", "COUNTY"),
                  value_vars = NULL,
                  table_type = "subject",
                  key = api_key$census$key)

s1501 <- get.acs5(data_in_name = config$data_in_names$census$S1501_2018,
                  data_in_pth = data_in_pth,
                  id_vars = c("TRACT", "COUNTY"),
                  value_vars = NULL,
                  table_type = "subject",
                  key = api_key$census$key)

s1901 <- get.acs5(data_in_name = config$data_in_names$census$S1901_2018,
                  data_in_pth = data_in_pth,
                  id_vars = c("TRACT", "COUNTY"),
                  value_vars = NULL,
                  table_type = "subject",
                  key = api_key$census$key)

s1902 <- get.acs5(data_in_name = config$data_in_names$census$S1902_2018,
                  data_in_pth = data_in_pth,
                  id_vars = c("TRACT", "COUNTY"),
                  value_vars = NULL,
                  table_type = "subject",
                  key = api_key$census$key)

get.pulse_puf(data_in_pth = data_in_pth)

save.image()
