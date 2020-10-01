#' @title Manages the provider type data from DFPS
#' @description Data from: https://www.dfps.state.tx.us/Child_Care/Search_Texas_Child_Care/ppFacilitySearchResults.asp
#' @export
dm.dfps_prvdr_type <- function(data_in_name,
                          data_in_pth, 
                          prvdr_types = c("Licensed Center - Child Care Program",
                                          "Licensed Child-Care Home",
                                          "Registered Child-Care Home",
                                          "Licensed Center - Before/After School Program",
                                          "Licensed Center - School Age Program",
                                          "Small Employer Based Child Care",
                                          "Licensed Center")) {

  df <- readr::read_csv(file.path(data_in_pth, data_in_name))

  assertthat::assert_that(all(c("Type", "Operation #", "Infant", "Toddler", "Preschool", "School") %in% names(df)))
  assertthat::assert_that(all(unique(df$Type) %in% prvdr_types))
  assertthat::assert_that(all(unique(df$Infant) %in% c("N", "Y")))
  assertthat::assert_that(all(unique(df$Toddler) %in% c("N", "Y")))
  assertthat::assert_that(all(unique(df$Preschool) %in% c("N", "Y")))
  assertthat::assert_that(all(unique(df$School) %in% c("N", "Y")))

  df <- df %>%
    dplyr::rename(operation_number = `Operation #`) %>% 
    dplyr::mutate(center_prvdr = ifelse(grepl("Licensed Center", Type), TRUE, FALSE),
                  home_prvdr = ifelse(grepl("Home", Type), TRUE, FALSE),
                  after_schl_prvdr = ifelse(grepl("After School Program", Type), TRUE, FALSE),
                  infant = ifelse(Infant =="Y", TRUE, FALSE),
                  toddler = ifelse(Toddler == "Y", TRUE, FALSE),
                  preschool = ifelse(Preschool == "Y", TRUE, FALSE),
                  school = ifelse(School == "Y", TRUE, FALSE),
                  operation_number = as.character(operation_number)) %>% 
    dplyr::select(operation_number, center_prvdr, home_prvdr, after_schl_prvdr, infant, toddler, preschool, school)
  
  assertthat::assert_that(all(rowSums(df[, c("center_prvdr", "home_prvdr")]) <= 1))
  
  return(df)
}

#' @title Split string and convert to column
#' @param df dataframe.
#' @param var string. The name of the variable to split.
#' @export
dm.str_split_to_col <- function(df, var, split = ",") {

  vector <- as.vector(na.omit(unique(unlist(strsplit(df[[var]], split = split)))))

  cols <- sapply(vector, function(v, df) {
    df[v] <- ifelse(grepl(v, df[[var]]), 1, 0)
  }, df = df, USE.NAMES = TRUE, simplify = FALSE)

  df <- df %>% 
    dplyr::bind_cols(do.call(cbind, cols) %>% 
                      as.data.frame(stringsAsFactors = FALSE))

  return(df)
}

#' @title Data management steps for days of operation
#' @param df dataframe.
#' @export
dm.ccl_days_of_operation <- function(df,
                                     var = "days_of_operation") {
  
  assertthat::assert_that(all(c("operation_number", var) %in% names(df)))
  
  df <- df %>% 
    dplyr::select(dplyr::one_of("operation_number", var)) %>% 
    dm.str_split_to_col(var = var) %>% 
    dplyr::select(-var) 
  
  return(df)
}

dm.ccl_operation_type <- function(df,
                                  var = "operation_type",
                                  prvdr_types = c("Child Placing Agency", "Registered Child-Care Home",
                                                  "Licensed Child-Care Home", "Licensed Center",
                                                  "General Residential Operation")) {

  assertthat::assert_that(all(c("operation_number", var) %in% names(df)))
  assertthat::assert_that(all(unique(df$operation_type) %in% prvdr_types))

  df <- df %>% 
    dplyr::mutate(center_prvdr = ifelse(grepl("Licensed Center", operation_type), TRUE, FALSE),
                  home_prvdr = ifelse(grepl("Home", operation_type), TRUE, FALSE)
    )

  return(df)
}

