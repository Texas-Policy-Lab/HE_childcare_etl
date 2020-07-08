library(magrittr)

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

key <- yaml::read_yaml("api_key.yaml")

tables <- list("S2405", "S2401")

l <- lapply(tables, scrape_html)

dfs <- lapply(l, function(x) {
  census_call(id_vars = c("TRACT", "COUNTY"),
              value_vars = x$Name,
              key = key$census)
  })
