#' Get average temperature for a single building
#'
#' This function downloads 3 closest GHCND weather station data for variable v, and
#' computes a inverse weighted average of the variable
#' @param b required, 8 digit building number
#' @param start_str time to start downloading, a date string of yyyy-mm-dd format
#' @param end_str time to end downloading, a date string of yyyy-mm-dd format
#' @param v variable to compile
#' @param path optional, directory to save downloaded data.
#' @keywords noaa average daily temperature
#' @export
#' @examples
#' getAvgTemp(building="UT0032ZZ", start_str="2012-01-05", end_str="2015-01-30")
getWeightedDaily <- function(b, start_str, end_str, v, path=NULL, radius=100) {
  start_year = lubridate::ymd(sprintf("%s-01-01", substr(start_str, 1, 4)))
  end_year = lubridate::ymd(sprintf("%d-01-01", as.integer(substr(end_str, 1, 4))))
  duration = "years"
  start_times = seq(start_year, end_year, duration)
  ## don't need to consider leap year feb 29
  end_times = seq(start_year + lubridate::years(1) - lubridate::days(1),
                  end_year + lubridate::years(1) - lubridate::days(1), duration)
  ## only search regarding the variables interested
  ghcnd_data_var = ghcnd_data_full %>%
    dplyr::filter(`element` == v) %>%
    {.}
  if (!is.null(path)) {
    dir.create(sprintf("%s/building_%s", path, v))
  } else {
    dir.create(sprintf("building_%s", v))
  }
  print("path in getWeightedDaily")
  print(path)
  download_rounds = length(start_times)
  ## first run to download data
  get.noaa.weather::run_download(b, download_rounds, start_times, end_times, v, ghcnd_data_var, radius=radius,
                                 path=path)
  ## second run to compile data
  get.noaa.weather::run_download(b, download_rounds, start_times, end_times, v, ghcnd_data_var, radius=radius,
                                 path=path)
  get.noaa.weather::compile_weather_to_one_df(b, v, start_times, path=path)
  get.noaa.weather::compile_distance_to_one_df(b, v, start_times, path=path)
}

