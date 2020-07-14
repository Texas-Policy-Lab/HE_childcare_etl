config <- yaml::read_yaml("config.yaml")

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

data_in_pth <- config$data_pths$in_pth

get.nber_tract_data(data_name = config$data_names$nber,
                    data_in_pth = data_in_pth)

get.hhsc_ccl_data(data_name = config$data_names$ccl,
                  data_in_pth = data_in_pth)

get.acf_data(data_name = config$data_names$acf,
             data_in_pth = data_in_pth)

get.kinder_neighborhood_tract_xwalk(data_name = config$data_names$kinder_neighborhoods,
                                    data_in_pth = data_in_pth)

get.tract_shape(data_name = config$data_names$tract_shape,
                data_in_pth = data_in_pth)
                               
             
 