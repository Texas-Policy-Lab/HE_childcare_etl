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

family_prvdr_zip <- dm.family_prvdr_zip(acf_data_in_pth = data_out_pth,
                                        acf_data_in_name = config$data_out_names$acf_all,
                                        ccl_data_in_pth = data_out_pth,
                                        ccl_data_in_name = config$data_out_names$ccl,
                                        data_out_pth = data_out_pth,
                                        data_out_name = config$data_out_names$family_prvdr_zip)
