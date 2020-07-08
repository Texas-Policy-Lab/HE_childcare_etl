library(magrittr)

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

key <- yaml::read_yaml("api_key.yaml")
config <- yaml::read_yaml("config.yaml")

cnty <- readr::read_csv(here::here("./data/tx_counties.csv"))

tables <- list("S2405", "S2401")

l <- lapply(tables, scrape_html)

dfs <- lapply(l, function(x) {
  census_call(id_vars = c("TRACT", "COUNTY"),
              value_vars = x$Name,
              key = key$census)
  })

ind <- datausaio(url = config$ind, cnty_fips = cnty$county_fips)

occ <- datausaio(url = config$occ, cnty_fips = cnty$county_fips)

ind_xwalk <- ind %>% 
  dplyr::distinct(`ID Group`, Group, `ID Industry`, Industry)

occ_xwalk <- occ %>%
  dplyr::distinct(`ID Group`, Group, `ID Subgroup`, Subgroup, `ID Occupation`, Occupation)
