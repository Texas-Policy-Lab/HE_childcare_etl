library(magrittr)

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

key <- yaml::read_yaml("api_key.yaml")
config <- yaml::read_yaml("config.yaml")

cnty <- readr::read_csv(here::here("./data/tx_counties.csv"))

occ_xwalk <- dm.occ_census_datausaio(url = config$occ, cnty_fips = cnty$county_fips, table = "S2401")

tables <- list(ind = "S2405", occ = "S2401")

l <- sapply(tables, scrape_html, USE.NAMES = TRUE, simplify = FALSE)

dfs <- sapply(l, function(x, key) {
  census_call(id_vars = c("TRACT", "COUNTY"),
              value_vars = x$Name,
              key = key)
  }, key = key$census, USE.NAMES = TRUE, simplify = FALSE)

ind_xwalk <- create.ind_xwalk(url = config$ind, cnty_fips = cnty$county_fips)

occ_xwalk <- create.occ_xwalk(url = config$occ, cnty_fips = cnty$county_fips)

ind_census_vars <- l[[1]]




