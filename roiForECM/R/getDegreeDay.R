#' Get HDD and CDD for a single building
#'
#' This function downloads 3 closest GHCND weather station data for variable v, and
#' computes a inverse weighted average of the variable
#' @param b required, 8 digit building number
#' @param start_str time to start downloading, a date string of yyyy-mm-dd format
#' @param end_str time to end downloading, a date string of yyyy-mm-dd format
#' @param path optional, directory to save downloaded data.
#' @param overwrite optional, whether to regenerate degreeday file
#' @keywords noaa average daily temperature
#' @export
#' @examples
#' getDegreeDay(b="UT0032ZZ", start_str="2016-10-01", end_str="2017-09-30")
getDegreeDay <- function(b, start_str=NULL, end_str=NULL, path=NULL, overwrite=FALSE) {
  if (is.null(start_str)) {
    dfbuilding = downloadWeatherStartEnd %>%
      dplyr::filter(`Building_Number`==b) %>%
      {.}
    start_str = as.character(dfbuilding$start)
    end_str = as.character(dfbuilding$end)
  }
  v = "TMIN"
  getWeightedDaily(b=b, start_str=start_str, end_str=end_str, v=v, path=path, radius=500)
  v = "TMAX"
  getWeightedDaily(b=b, start_str=start_str, end_str=end_str, v=v, path=path, radius=500)
  if (!is.null(path)) {
    dir.create(sprintf("%sbuilding_HDDCDD", path))
  }
  tmin_tmax_2_degreeday(b, path=path, overwrite=overwrite)
}
