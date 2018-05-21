#' Get latitude longitude
#'
#' This function get latitude longitude of EUAS buildings
#' @param path to the all.db file, default is "csv_FY/db/"
#' @keywords latlon
#' @export
#' @examples
#' get_lat_lon_df()
get_lat_lon_df <- function(path) {
  if (missing(path)) {
    path = "csv_FY/db/"
  }
  con <- dbConnect(RSQLite::SQLite(), paste0(path, "all.db"))
  lat_lon_df =
    dbGetQuery(con, 'SELECT * FROM EUAS_latlng_2' ) %>%
    as_data_frame() %>%
    dplyr::mutate(`latlng`=gsub("\\[|\\]", "", `latlng`)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(`lat`=strsplit(`latlng`, ", ")[[1]][1]) %>%
    dplyr::mutate(`lng`=strsplit(`latlng`, ", ")[[1]][2]) %>%
    dplyr::mutate_at(vars(`lat`, `lng`), as.numeric) %>%
    dplyr::select(-`latlng`, -`geocoding_input`) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(`Building_Number`, `lat`, `lng`) %>%
    dplyr::rename(`latitude`=`lat`, `longitude`=`lng`) %>%
    slice(1) %>%
    dplyr::ungroup() %>%
    {.}
  dbDisconnect(con)
  return(lat_lon_df)
}
