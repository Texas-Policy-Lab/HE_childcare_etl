library(magrittr)

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

key <- yaml::read_yaml("api_key.yaml")
config <- yaml::read_yaml("config.yaml")

cnty <- readr::read_csv(here::here("./data/tx_counties.csv"))

occ_xwalk <- dm.occ_census_datausaio(url = config$occ,
                                     cnty_fips = cnty$county_fips,
                                     table = "S2401")

ind_xwalk <- dm.ind_census_datausaio(url = config$ind,
                                     cnty_fips = cnty$county_fips,
                                     table = "S2403")

occ <- census_call(id_vars = c("TRACT", "COUNTY"),
                   value_vars = occ_xwalk$var,
                   key = key$census) %>%
  dplyr::select(-c(tract, county, state)) %>% 
  tidyr::gather(var, value, -c(TRACT, COUNTY))

ind <- census_call(id_vars = c("TRACT", "COUNTY"),
                   value_vars = ind_xwalk$var,
                   key = key$census) %>% 
  dplyr::select(-c(tract, county, state)) %>% 
  tidyr::gather(var, value, -c(TRACT, COUNTY))
