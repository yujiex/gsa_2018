#' Compile weather to one data frame
#'
#' This function concatenates year by year downloaded weather_files into one data frame
#' @param b required, 8 digit building number, used in name the output file
#' @param v required, variable to compile
#' @param start_times required, a vector of start time for each individual files
#' @param path optional, directory to save downloaded data
#' @keywords concatenate
#' @export
#' @examples
#' compile_weather_to_one_df (b="xxxxxxxx", v, start_times, path=NULL)
compile_weather_to_one_df <- function(b, v, start_times, path=NULL) {
  print ("compile data")
  print(b)
  acc = NULL
  download_rounds = length(start_times)
  if (is.null(path)) {
    path_prefix = ""
  } else {
    path_prefix = sprintf("%s/", path)
  }
  for (j in 1:download_rounds) {
    date_min = start_times[j]
    filename = sprintf("%sbuilding_%s/%s_%s_%s.csv", path_prefix, v, b, v, date_min)
    print(filename)
    if (file.exists(filename)) {
      dfvar <- readr::read_csv(filename, col_types=readr::cols()) %>%
        tibble::as_data_frame() %>%
        dplyr::rename(!!rlang::sym(v):=`weighted`) %>%
        {.}
      acc <- rbind(acc, dfvar)
    } else {
      print(sprintf("file not exist %s_%s_%s.csv", b, v, date_min))
    }
  }
  dir.create(sprintf("%sbuilding_%s/compiled", path_prefix, v))
  acc %>%
    dplyr::rename(`Date`=`date`) %>%
    feather::write_feather(sprintf("%sbuilding_%s/compiled/%s_%s.feather", path_prefix, v, b, v))
}

#' Compile distance to one data frame
#'
#' This function concatenates year by year downloaded distance_files into one data frame
#' @param b required, 8 digit building number, used in name the output file
#' @param v required, variable to compile
#' @param start_times required, a vector of start time for each individual files
#' @param path optional, directory to save downloaded data
#' @keywords concatenate
#' @export
#' @examples
#' compile_weather_to_one_df (b="xxxxxxxx", v, start_times, path=NULL)
## compile distance to one data frame
compile_distance_to_one_df <- function(b, v, start_times, path=NULL) {
  ## compile individual building's weather station distance data to one data frame
  acc_distance = NULL
  ## for (b in buildings[start:length(buildings)]) {
  print(b)
  download_rounds = length(start_times)
  if (is.null(path)) {
    path_prefix = ""
  } else {
    path_prefix = sprintf("%s/", path)
  }
  for (j in 1:download_rounds) {
    date_min = start_times[j]
    filename = sprintf("%sbuilding_%s/%s_station_distance_%s_%s.csv", path_prefix, v, b, v, date_min)
    if (file.exists(filename)) {
      dfvar <- readr::read_csv(filename, col_types=readr::cols()) %>%
        dplyr::mutate(`varname`=v) %>%
        dplyr::mutate(`Name`=b) %>%
        dplyr::mutate(`start_time`=date_min) %>%
        tibble::as_data_frame() %>%
        {.}
      acc_distance <- rbind(acc_distance, dfvar)
    } else {
      print(sprintf("file not exist %s_%s_%s.csv", b, v, date_min))
    }
  }
  dir.create(sprintf("%sbuilding_%s/compiled", path_prefix, v))
  acc_distance %>%
    feather::write_feather(sprintf("%sbuilding_%s/compiled/%s_station_distance.feather", path_prefix, v, b))
}
