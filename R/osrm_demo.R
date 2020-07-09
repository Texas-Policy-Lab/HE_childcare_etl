library(osrm)
library(magrittr)

getOption("osrm.server")
options(osrm.server = "http://127.0.0.1:5000/",
        osrm.profile = "driving")

config <- yaml::read_yaml("config.yaml")

df <- readr::read_csv(file.path(config$data_pth, "family_zip_prvdr.csv")) %>%
  dplyr::rename(parent_id = ParentsID) %>% 
  tidyr::drop_na(ParentsID) %>%
  dplyr::mutate(familyzip = as.character(Parents.FamilyZip))

assertthat::assert_that(sum(is.na(df$Parents.FamilyZip)) == 0)

temp <- readr::read_tsv(file.path(config$data_pth, "2019_Gaz_zcta_national.txt")) %>%
  dplyr::mutate(familyzip = as.character(GEOID))

df <- df %>%
  dplyr::left_join(temp %>% 
                     dplyr::select(familyzip, INTPTLAT, INTPTLONG), by = c("familyzip"))

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

result <- lapply(names(a)[1:5], function(x) {osrm::osrmTable(src = a[[x]], dst = b[[x]])})

parse_osrm_result <- function(result) {
 
  dur <- result[[1]]$durations
  
  dest <- result[[1]]$destinations
 
  dest <- dest %>% 
    dplyr::mutate(parent_id = row.names(dest),
                  duration = dur)
}


# #### DEMO
# 
# data("berlin")
# 
# A <- data.frame(id = 1:3,
#                 lon = rep(apotheke.df[1, c('lon')], 3),
#                 lat = rep(apotheke.df[1, c('lat')], 3),
#                 grp = c(1,2,3))
# 
# B <- data.frame(apotheke.df[11:20,c("id","lon","lat")],
#                 grp = rep(c(1,2,3), 4)[-1:-2])
# 
# a <- A %>%
#   dplyr::group_by(grp) %>%
#   dplyr::group_split()
# 
# b <- B %>%
#   dplyr::group_by(grp) %>%
#   dplyr::group_split()
# 
# t <- mapply(osrmTable, src=as.list(a), dst = as.list(b))
# 
# 
# distA2 <- osrmTable(src = apotheke.df[1,c("id","lon","lat")],
#                     dst = apotheke.df[11:20,c("id","lon","lat")])
# # First 5 rows and columns
# distA2$durations
