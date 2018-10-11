library("dplyr")
library("rnoaa")
library("lubridate")

devtools::load_all("~/Dropbox/gsa_2017/db.interface")

devtools::load_all("~/Dropbox/gsa_2017/get.noaa.weather")

load("ghcnd_data_full.rda")
## getWeather <- function(building, start, end, duration) {

getWeatherData <- function(b, start_str, end_str, v) {
  start_year = lubridate::ymd(sprintf("%s-01-01", substr(start_str, 1, 4)))
  end_year = lubridate::ymd(sprintf("%d-01-01", as.integer(substr(end_str, 1, 4)) + 1))
  duration = "years"
  start_times = seq(start_year, end_year, duration)
  end_times = seq(start_year + years(1) - days(1), end_year + years(1) - days(1), duration)
  ## only search regarding the variables interested
  ghcnd_data_var = ghcnd_data_full %>%
    dplyr::filter(`element` == v) %>%
    {.}
  download_rounds = length(start_times)
  ## first run to download data
  run_download(download_rounds, start_times, end_times, v, ghcnd_data_var, radius=300)
  ## second run to compile data
  run_download(download_rounds, start_times, end_times, v, ghcnd_data_var)
  compile_weather_to_one_df(b, download_rounds, v, start_times)
  compile_distance_to_one_df(b, download_rounds, v, start_times)
}

## helper to getWeatherData
run_download <- function(download_rounds, start_times, end_times, v, ghcnd_data_var, radius=100) {
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
    ## this part only downloads data
    if (!file.exists(sprintf("building_%s/%s_station_distance_%s_%s.csv",
                            v, b, v, date_min))) {
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
        readr::write_csv(sprintf("building_%s/%s_station_distance_%s_%s.csv",
                                v, b, v, date_min))
    ## when weather file is already downloaded
    } else if (!file.exists(sprintf("building_%s/%s_%s_%s.csv",
                                    v, b, v, date_min))){
      print("compile weather for building")
      print(sprintf("%s --------------%s -----------", counter, b))
      df <- readr::read_csv(sprintf("building_%s/%s_station_distance_%s_%s.csv", v, b, v, date_min))
      weatheri = get.noaa.weather::compile_weather_ghcnd_main(building=b, station_df=df,
                                date_min=date_min, date_max=date_max,
                                var=v,
                                format_fun=get.noaa.weather::format_noaa_temperature)
      print(weatheri)
      weatheri %>%
        readr::write_csv(sprintf("building_%s/%s_%s_%s.csv", v, b, v, date_min))
      ## when weather output is compiled
      } else {
        print(sprintf("%s file exists for %s", v, b))
      }
      counter = counter + 1
  }
}

## compile weather to one data frame
compile_weather_to_one_df <- function(b, download_rounds, v, start_times) {
  print ("compile data")
  print(b)
  acc = NULL
  for (j in 1:download_rounds) {
    date_min = start_times[j]
    filename = sprintf("building_%s/%s_%s_%s.csv", v, b, v, date_min)
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
  acc %>%
    dplyr::rename(`Date`=`date`) %>%
    feather::write_feather(sprintf("building_%s/compiled/%s_%s.feather", v, b, v))
}

## compile distance to one data frame
compile_distance_to_one_df <- function(b, download_rounds, v, start_times) {
  ## compile individual building's weather station distance data to one data frame
  acc_distance = NULL
  ## for (b in buildings[start:length(buildings)]) {
  print(b)
  for (j in 1:download_rounds) {
    date_min = start_times[j]
    filename = sprintf("building_%s/%s_station_distance_%s_%s.csv", v, b, v, date_min)
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
  acc_distance %>% feather::write_feather(sprintf("building_%s/compiled/%s_station_distance.feather", v, b))
}

## b = "UT0032ZZ"
## start_str = "2007-11-29"
## end_str = "2016-09-01"
## v = "TAVG"
devtools::load_all("~/Dropbox/gsa_2017/get.noaa.weather")

load("../data/downloadWeatherStartEnd.rda")
load("../data/actionCollapse.rda")

## b = "IN1703ZZ"
b = "IL0302ZZ"
dfbuilding = downloadWeatherStartEnd %>%
  dplyr::filter(`Building_Number`==b) %>%
  {.}

head(dfbuilding)

start_str = as.character(dfbuilding$start)
end_str = as.character(dfbuilding$end)

start_str
end_str

v = "TAVG"
getWeatherData(b=b, start_str=start_str, end_str=end_str, v=v)
