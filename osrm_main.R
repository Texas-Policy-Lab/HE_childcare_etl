library(osrm)
library(magrittr)

config <- yaml::read_yaml("config.yaml")

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

getOption("osrm.server")
options(osrm.server = config$port,
        osrm.profile = "driving")

df <- readr::read_csv(file.path(config$data_pth, "family_zip_prvdr.csv")) %>%
  dplyr::rename(parent_id = ParentsID) %>% 
  tidyr::drop_na(parent_id) %>%
  dplyr::mutate(familyzip = stringr::str_pad(as.character(Parents.FamilyZip),
                width = 5, side = "left", pad = "0"))

assertthat::assert_that(sum(is.na(df$Parents.FamilyZip)) == 0)

temp <- readr::read_tsv(file.path(config$data_pth, "2019_Gaz_zcta_national.txt")) %>%
  dplyr::mutate(familyzip = as.character(GEOID))

df <- df %>%
  dplyr::left_join(temp %>% 
                     dplyr::select(familyzip, INTPTLAT, INTPTLONG), by = c("familyzip")) %>% 
  tidyr::drop_na(INTPTLONG, INTPTLAT)

assertthat::assert_that(sum(is.na(df$INTPTLONG)) == 0)
assertthat::assert_that(sum(is.na(df$INTPTLAT)) == 0)

a <- df %>%
  dplyr::distinct(operation_number, .keep_all=TRUE) %>%
  dplyr::group_by(operation_number) %>%
  dplyr::select(operation_number, longitude, latitude) %>%
  dplyr::group_split()

names(a) <- a  %>% 
  purrr::map(magrittr::extract("operation_number"))

b <- df %>%
  dplyr::group_by(operation_number) %>%
  dplyr::select(operation_number, parent_id, INTPTLONG, INTPTLAT) %>%
  dplyr::group_split(.keep=FALSE)

names(b) <- names(a)

assertthat::assert_that(length(a) == length(b))

result <- lapply(names(a), function(x) {osrm::osrmTable(src = a[[x]],
                                                        dst = b[[x]])})

parse_result <- lapply(result, parse_osrm_table_result)

df <- do.call("rbind", parse_result) %>% 
  dplyr::rename(operation_number = src)

write.csv(df, "./data/prvdr_family_commute_duration.csv", row.names = FALSE)


# Even filtering for neighboring counties only, there are families that say they are in e.g. Montogmery County, but their zipcode is in Dallas COunty

#Montgomery 48339
#Liberty 48291
#Chambers 48071
#Galveston 48167
#Brazoria 48039
#Fort Bend 48157
#Waller 48473

neighbors <- c(48201, 48339, 48291, 48071, 48167, 48039, 48157, 48473)

commute <- readr::read_csv("./data/prvdr_family_commute_duration.csv") %>%
  dplyr::left_join(df %>%
                     dplyr::select(parent_id,
                                   Parents.FamilyZip, Parents.FIPS) %>%
                     dplyr::filter(Parents.FIPS %in% neighbors), by="parent_id") %>%
  dplyr::group_by(Parents.FamilyZip) %>%
  dplyr::summarise(count = dplyr::n(), mean = mean(duration), min = min(duration),
                   q80 = quantile(duration, probs=0.8), FIPS = mode(Parents.FIPS)) %>%
  tidyr::drop_na(Parents.FamilyZip)

write.csv(df, "./data/commuting_times_zipcode.csv", row.names = FALSE)
