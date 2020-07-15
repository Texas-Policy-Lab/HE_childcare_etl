config <- yaml::read_yaml("config.yaml")
api_key <- yaml::read_yaml("api_key.yaml")

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

data_in_pth <- config$data_pths$in_pth

get.nber_tract_data(data_in_name = config$data_in_names$nber,
                    data_in_pth = data_in_pth)

get.hhsc_ccl_data(data_in_name = config$data_in_names$ccl,
                  data_in_pth = data_in_pth)

get.acf_data(data_in_name = config$data_in_names$acf,
             data_in_pth = data_in_pth)

get.kinder_neighborhood_tract_xwalk(data_in_name = config$data_in_names$kinder_neighborhoods,
                                    data_in_pth = data_in_pth)

get.tract_shape(data_in_name = config$data_in_names$tract_shape,
                data_in_pth = data_in_pth)

get.state_fips_state_name_xwalk(data_in_name = config$data_in_names$state_fips_state_name_xwalk,
                                data_in_pth)
