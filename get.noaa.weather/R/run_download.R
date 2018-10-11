#' Run download of weather data
#'
#' This function downloads data from noaa ghcnd
#' computes a inverse weighted average of the temperature
#' @param b required, 8 digit building number
#' @param start_str time to start downloading, a date string of yyyy-mm-dd format
#' @param end_str time to end downloading, a date string of yyyy-mm-dd format
#' @param path optional, directory to save downloaded data.
#' @keywords noaa average daily temperature
#' @export
#' @examples
#' getAvgTemp(building="UT0032ZZ", start_str="2012-01-05", end_str="2015-01-30")
run_download <- function(b, download_rounds, start_times, end_times, v, ghcnd_data_var, radius=100, path=NULL) {
  for (j in 1:download_rounds) {
    ## for (j in 1:length(start_times)) {
    date_min = start_times[j]
    date_max = end_times[j]
    bad = NULL
    if (file.exists(sprintf("bad_%s_%s.csv", v, date_min))) {
      bad_acc = readr::read_csv(sprintf("bad_%s_%s.csv", v, date_min))$bad
    } else {
      bad_acc = NULL
    }
    good_ghcnd <- ghcnd_data_var %>%
      dplyr::filter(!(`id` %in% bad_acc))
    start = 1
    counter = start
    if (is.null(path)) {
      distance_file = sprintf("building_%s/%s_station_distance_%s_%s.csv", v, b, v, date_min)
      weather_file = sprintf("building_%s/%s_%s_%s.csv", v, b, v, date_min)
    } else {
      distance_file = sprintf("%s/building_%s/%s_station_distance_%s_%s.csv", path, v, b, v, date_min)
      weather_file = sprintf("%s/building_%s/%s_%s_%s.csv", path, v, b, v, date_min)
    }
    print(sprintf("distance_file: %s", distance_file))
    ## this part only downloads data
    if (!file.exists(distance_file)) {
      print(sprintf("%s --------------%s -----------", counter, b))
      b_loc = db.interface::get_lat_lon_df(building=b) %>%
        ## dplyr::rename(`Name`=`Building_Number`) %>%
        {.}
      acc <- NULL
      good_ghcnd <- good_ghcnd %>%
        dplyr::filter(!(`id` %in% bad)) %>%
        {.}
      print(sprintf("size of search space: %s", nrow(good_ghcnd)))
      result = get_nearby_ghcnd_stations_one_loc(lat_lon_df=b_loc, id_col_name="Building_Number",
                                                 ghcnd_data=good_ghcnd, v=v, radius=radius, limit=3,
                                                 date_min=date_min, date_max=date_max, year=NULL)
      print("final result---------")
      print(result$df)
      bad <- result$bad
      bad_acc <- c(bad_acc, bad)
      data.frame(bad=bad_acc) %>%
        readr::write_csv(sprintf("bad_%s_%s.csv", v, date_min))
      result$df %>%
        readr::write_csv(distance_file)
    ## when weather file is already downloaded, this part computes inverse weighted average
    } else if (!file.exists(weather_file)){
      print("compile weather for building")
      print(sprintf("%s --------------%s -----------", counter, b))
      df <- readr::read_csv(distance_file)
      if (nrow(df) == 0) {
        print("empty station distance df")
        next
      }
      weatheri = compile_weather_ghcnd_main(building=b, station_df=df,
                                date_min=date_min, date_max=date_max,
                                var=v,
                                format_fun=get.noaa.weather::format_noaa_temperature)
      print(weatheri)
      weatheri %>%
        readr::write_csv(weather_file)
      ## when weather output is compiled
      } else {
        print(sprintf("%s file exists for %s", v, b))
      }
      counter = counter + 1
  }
}
