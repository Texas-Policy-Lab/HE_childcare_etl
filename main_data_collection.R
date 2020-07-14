library(magrittr)

api_key <- yaml::read_yaml("api_key.yaml")

ccl <- get.hhsc_ccl_data()

