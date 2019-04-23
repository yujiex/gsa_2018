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
getAvgTemp <- function(b, path=NULL, resolution="daily") {
  dfbuilding = downloadWeatherStartEnd %>%
    dplyr::filter(`Building_Number`==b) %>%
    {.}
  start_str = as.character(dfbuilding$start)
  end_str = as.character(dfbuilding$end)
  v = "TAVG"
  if (resolution == "hourly") {
    getWeightedHourly(b=b, start_str=start_str, end_str=end_str, v=v, path=path, radius=500)
  } else {
    getWeightedDaily(b=b, start_str=start_str, end_str=end_str, v=v, path=path, radius=500)
  }
}
