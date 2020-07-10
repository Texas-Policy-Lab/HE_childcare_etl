#' @title Parse the OSRM Table result
#' @param result list.
#' @export
parse_osrm_table_result <- function(r) {

    dur <- r$durations %>% 
    as.data.frame() %>% 
    tidyr::gather(parent_id, duration)

  dest <- r$destinations %>% 
    as.data.frame() %>%
    dplyr::mutate(parent_id = row.names(.),
                  src = row.names(r$sources %>% 
                                    as.data.frame()))
  
  df <- dest %>% 
    dplyr::left_join(dur)

  return(df)
}
