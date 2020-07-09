library(magrittr)

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

key <- yaml::read_yaml("api_key.yaml")
config <- yaml::read_yaml("config.yaml")

cnty <- readr::read_csv(here::here("./data/tx_counties.csv"))

occ_reopening <- readr::read_csv("./data/occupation_reopening_pct.csv") %>% 
  tidyr::gather(phase, pct, -c(occ, occ_id)) %>% 
  dplyr::mutate(occ_id = as.character(occ_id))

ind_reopening <- readr::read_csv("./data/industry_reopening_pct.csv") %>% 
  tidyr::gather(phase, pct, -c(ind, ind_id)) %>% 
  dplyr::mutate(ind_id = as.character(ind_id))

occ_xwalk <- dm.occ_census_datausaio(url = config$occ,
                                     cnty_fips = cnty$county_fips,
                                     table = "S2401")

ind_xwalk <- dm.ind_census_datausaio(url = config$ind,
                                     cnty_fips = cnty$county_fips,
                                     table = "S2403")

occ <- census_call.acs5.subject(id_vars = c("TRACT", "COUNTY"),
                                value_vars = occ_xwalk$var,
                                key = key$census) %>%
  dplyr::select(-c(tract, county, state)) %>% 
  tidyr::gather(var, n_workers, -c(TRACT, COUNTY)) %>% 
  dplyr::left_join(occ_xwalk) %>%
  dplyr::mutate(occ_id = as.character(occ_id)) %>% 
  dplyr::left_join(occ_reopening %>% 
                     dplyr::select(-occ)) %>% 
  dplyr::mutate(adj_n_workers = (n_workers*pct)/100)

ind <- census_call.acs5.subject(id_vars = c("TRACT", "COUNTY"),
                                value_vars = ind_xwalk$var,
                                key = key$census) %>% 
  dplyr::select(-c(tract, county, state)) %>% 
  tidyr::gather(var, n_workers, -c(TRACT, COUNTY)) %>% 
  dplyr::left_join(ind_xwalk) %>% 
  dplyr::mutate(ind_id = as.character(ind_id)) %>% 
  dplyr::left_join(ind_reopening %>% 
                     dplyr::select(-ind)) %>% 
  dplyr::mutate(adj_n_workers = (n_workers*pct)/100)

n_kids_wwp <- dm.kids_working_parents(table = "B23008",
                                      id_vars = c("TRACT", "COUNTY"),
                                      key = key$census)

n_kids_li <- dm.kids_low_income(table = "B17024",
                                id_vars = c("TRACT", "COUNTY"),
                                key = key$census)

