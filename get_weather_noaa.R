accessToken = "hDldeqcNqFHyZlnYhnXUEhJEnpKLmGlJ"

library("rnoaa")
library("dplyr")
library("ggplot2")
library("feather")
library("rlang")

## --------------------------------------------------------------------------------- ##
## this section uses ghcnd, daily data
## --------------------------------------------------------------------------------- ##
lat_lon_df = get_lat_lon_df("csv_FY/db/all.db") %>%
  dplyr::mutate(`id` = `Building_Number`) %>%
  {.}

ghcnd_data <- rnoaa::ghcnd_stations()

nearby_stations_ghcnd <- rnoaa::meteo_nearby_stations(lat_lon_df = lat_lon_df, var = c("TAVG", "TMAX", "TMIN"),
                                                      station_data = ghcnd_data, limit = 5)

ids = lapply(nearby_stations_ghcnd, function(b) {
  return(b %>% dplyr::select(id))
})
id_df = do.call(rbind, ids) %>%
  dplyr::group_by(id) %>%
  slice(1) %>%
  dplyr::ungroup() %>%
  {.}

lst = lapply(head(id_df)$id, function(s) {
  weatherdata = rnoaa::ghcnd_search(s, date_min = date_min,
                                    date_max = date_max,
                                    var = "TAVG") %>%
    dplyr::select(TAVG) %>%
    na.omit() %>%
    {.}
})

s = "USC00504094"
weather_result_list = rnoaa::ghcnd_search(s, date_min = date_min,
                           date_max = date_max,
                           var = c("TAVG", "TMAX", "TMIN"))

weather_result_list_processed = lapply(names(weather_result_list), function(name) {
  df = result_list[[name]] %>%
    dplyr::select(-`id`, -`mflag`, -`sflag`, -`qflag`) %>%
    na.omit() %>%
    {.}
  return(df)
})

weather_df = Reduce(full_join, weather_result_list_processed)

acc = lapply(names(result_list), function(nm) {
  return(result_list %>% dplyr::select(get(nm)))
})

result = do.call(rbind, acc)

weather_df = do.call(rbind, lst)


test = rnoaa::ghcnd_search("USC00504094", date_min = "2002-9-1", date_max = "2017-10-1",
                           var = c("TAVG", "TMAX", "TMIN")) %>%
  na.omit()

## --------------------------------------------------------------------------------- ##
## this section uses isd, hourly data
## --------------------------------------------------------------------------------- ##
get_nearby_isd_stations <- function (lat_lon_df, isd_data, radius, limit, date_min, date_max) {
  v_isd_stations_search = Vectorize(rnoaa::isd_stations_search)
  result = v_isd_stations_search(lat = lat_lon_df$latitude, lon = lat_lon_df$longitude, radius = radius)
  acc = lapply(1:ncol(result), function(i){
    b = lat_lon_df$Building_Number[i]
    result[, i] %>% as_data_frame() %>%
      dplyr::select(`usaf`, `wban`, `begin`, `end`, `distance`) %>%
      dplyr::mutate(`Building_Number` = b)
  })
  nearby_stations_isd = do.call(rbind, acc)
  nearby_stations_isd <- nearby_stations_isd %>%
    na.omit() %>%
    {.}
  if (!missing(date_min)) {
    nearby_stations_isd <- nearby_stations_isd %>%
      dplyr::filter(`begin` < date_min) %>%
      {.}
  }
  if (!missing(date_max)) {
    nearby_stations_isd <- nearby_stations_isd %>%
      dplyr::filter(`end` > date_max) %>%
      {.}
  }
  if (!missing(limit)) {
    nearby_stations_isd <- nearby_stations_isd %>%
      dplyr::arrange(`Building_Number`, `distance`) %>%
      dplyr::group_by(Building_Number) %>%
      dplyr::filter(row_number() <= limit) %>%
      dplyr::ungroup() %>%
      {.}
  }
  return(nearby_stations_isd)
}

## tidy below to a function


get_nearby_isd_station_by_year <- function(year) {
  date_min = year * 10000 + 0101
  date_max = year * 10000 + 1231
  lat_lon_df = get_lat_lon_df()
  station_df = get_nearby_isd_stations(lat_lon_df, isd_data=isd_data, radius = 100, limit=5, date_min = date_min,
                                       date_max = date_max)
  return(station_df)
}

get_station_to_download <- function(station_df) {
  station_to_download = station_df %>%
    dplyr::select(`usaf`, `wban`) %>%
    dplyr::group_by(`usaf`, `wban`) %>%
    slice(1) %>%
    ungroup() %>%
    {.}
  return(station_to_download)
}

download_isd <- function(station_to_download, year, variables, start=NULL, end=NULL) {
  startIdx = 1
  endIdx = nrow(station_to_download)
  if (!missing(start)) {
    startIdx = start
  }
  if (!missing(end)) {
    endIdx = end
  }
  lapply(startIdx:endIdx, function(i){
    print(sprintf("----------%s---------------", i))
    tryCatch(
      res <- rnoaa::isd(usaf=station_to_download$usaf[i], wban=station_to_download$wban[i], additional=FALSE,
                        year=year) %>%
        {.}
    , warning=function(w) {print(sprintf("download failed for %s-%s", station_to_download$usaf[i],
                                        wban=station_to_download$wban[i]))},
      error=function(e) {print(sprintf("download failed for %s-%s", station_to_download$usaf[i],
                                      wban=station_to_download$wban[i]))},
      finally = print("no operation"))
  })
}

## download isd file to cache directory, and record station mappings
get_isdfile_by_year <- function(year) {
  print(sprintf("getting nearby stations for %s", year))
  station_df = get_nearby_isd_station_by_year(year)
  station_df %>%
    feather::write_feather(sprintf("csv_FY/weather/noaa/station_df_%s.feather", year))
  station_df = feather::read_feather(sprintf("csv_FY/weather/noaa/station_df_%s.feather", year))
  station_to_download = get_station_to_download(station_df)
  station_to_download %>%
    feather::write_feather(sprintf("csv_FY/weather/noaa/station_to_download_%s.feather", year))
  variables = c("temperature")
  print(sprintf("downloading stations for %s", year))
  weather_year = download_isd(station_to_download, year=year, variables=variables)
}

isd_data <-
  rnoaa::isd_stations(refresh = TRUE) %>%
  {.}
get_isdfile_by_year(year)

## data processing start
## to be filled

## deg f
format_noaa_temperature <- function(s) {
  return(as.double(s) / 10 * 9/5 + 32)
}
## deg f
format_noaa_dewpoint <- function(s) {
  return(as.double(s) / 10 * 9/5 + 32)
}
## meters per second
format_noaa_wind_speed <- function(s) {
  return(as.double(s) / 10)
}
## meters per second
format_noaa_wind_direction <- function(s) {
  return(as.double(s))
}

read_var_by_year <- function(station_df, buildings, var, format_function_list) {
  accWhole = NULL
  counter = 1
  var_quality = paste(var, "quality", sep = "_")
  name_formatted = paste0(var, "F")
  name_formatted_hour = paste0(var, "F", "hour")
  for (b in buildings[1:1]) {
  ## for (b in buildings) {
      print(paste0(counter, ", compute ", var, " for building ", b, "---"))
      dfTemp = station_df %>%
          dplyr::filter(`Building_Number`==b) %>%
          {.}
      ## print(head(dfTemp))
      acc = NULL
      for (i in 1:1) {
      ## for (i in 1:nrow(dfTemp)) {
        usaf <- dfTemp$usaf[i]
        wban <- dfTemp$wban[i]
        d = dfTemp$inv_dist[i]
        ## print(sprintf("%s-%s, weight %s", usaf, wban, d))
        tryCatch(
          {data <- rnoaa::isd(usaf=usaf, wban=wban, additional=FALSE, year=year) %>%
            dplyr::select(one_of("date", "time", var, var_quality)) %>%
            ## keep only data points with good quality
            dplyr::filter_(paste(var_quality, "%in% c(\"1\", \"5\")")) %>%
            {.}
            ## formatting data and unit converssion
            format_fun = format_function_list[[var]]
            print(format_fun)
            data <- data %>%
              ## dplyr::mutate(!!sym(name_formatted) := format_noaa_temperature(!!sym(var))) %>%
              dplyr::mutate(!!sym(name_formatted) := ((format_fun)(!!sym(var)))) %>%
              {.}
            print("2----------------------")
            print(head(data))
            data <- data %>%
              dplyr::mutate(`hour`=substr(`time`, start = 1, stop = 2)) %>%
              dplyr::group_by(`date`, `hour`) %>%
              dplyr::summarise(!!sym(name_formatted_hour):=mean(!!sym(name_formatted))) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(`wt`=d) %>%
              {.}
            print("3----------------------")
            print(head(data))
            acc = rbind(acc, data)
          },
          warning = function(w){
            print(sprintf("warning isd", w))
          },
          error = function(e){
            print(sprintf("error %s", e))
          },
          finally =  {
          }
        )
      }
      wtTemp = acc %>%
        dplyr::group_by(`date`, `hour`) %>%
        summarise(wTempF=weighted.mean(x=temperatureFhour, w=wt)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(`Date`=zoo::as.yearmon(substr(`date`, start=1, stop=6), "%Y %m")) %>%
        dplyr::group_by(`Date`) %>%
        summarize(`wTempFmonth` = mean(`wTempF`)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(`year`=format(`Date`, "%Y"),
                      `month`=format(`Date`, "%m")) %>%
        dplyr::select(-`Date`) %>%
        {.}
      wtTemp %>%
        feather::write_feather(sprintf("csv_FY/weather/isd_building/%s_monthly_weighted_%s_%s.feather",
                                       b, var, year))
      accWhole = rbind(accWhole, wtTemp)
      counter = counter + 1
  }
  return(accWhole)
}
## fixme:setup package

## example usecase

## The following fields are available in isd downloaded files
##  [1] "total_chars"                  "usaf_station"                
##  [3] "wban_station"                 "date"                        
##  [5] "time"                         "date_flag"                   
##  [7] "latitude"                     "longitude"                   
##  [9] "type_code"                    "elevation"                   
## [11] "call_letter"                  "quality"                     
## [13] "wind_direction"               "wind_direction_quality"      
## [15] "wind_code"                    "wind_speed"                  
## [17] "wind_speed_quality"           "ceiling_height"              
## [19] "ceiling_height_quality"       "ceiling_height_determination"
## [21] "ceiling_height_cavok"         "visibility_distance"         
## [23] "visibility_distance_quality"  "visibility_code"             
## [25] "visibility_code_quality"      "temperature"                 
## [27] "temperature_quality"          "temperature_dewpoint"        
## [29] "temperature_dewpoint_quality" "air_pressure"                
## [31] "air_pressure_quality"                                   

## compiles isd data to weighted monthly weather for each building
year = 2016
station_df = feather::read_feather(sprintf("csv_FY/weather/noaa/station_df_%s.feather", year)) %>%
  dplyr::mutate(`inv_dist`=1/`distance`) %>%
  {.}
buildings = unique(station_df$Building_Number)

format_function_list = list(temperature = format_noaa_temperature, wind_speed = format_noaa_wind_speed, wind_direction = format_noaa_wind_direction, temperature_dewpoint = format_noaa_dewpoint)

var = "temperature"
read_var_by_year(station_df, buildings, var, format_function_list)

## data processing end

