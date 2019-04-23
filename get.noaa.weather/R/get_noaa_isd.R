#' @importFrom magrittr %>%
NULL
#' Get a data frame of nearby weather stations
#'
#' This function get a data frame of nearby stations, with columns: usaf, wban,
#' begin, end, distance, and Building_Number
#' @param lat_lon_df a data frame containing a column "latitude", and a column
#'   "longitude"
get_nearby_isd_stations_one_loc <- function(lat_lon_df, isd_data, radius=NULL, limit=NULL,
                                            date_min=NULL, date_max=NULL, year=NULL) {
  print("start downloading")
  print(date_min)
  print(date_max)
  b = lat_lon_df[["Building_Number"]][[1]]
  v_out = tolower(v)
  print("-------------asdfasdfa----------------")
  print(head(lat_lon_df))
  print("-------------asdfasdfa----------------")
  ## nearbyStations =
  ##   get_nearby_isd_stations(lat_lon_df=lat_lon_df, isd_data=isd_data,
  ##                             ## return a lot and filter by whether having data
  ##                             radius=radius, limit=100, date_min=date_min,
  ##                           date_max=date_max, year=year) %>%
  ##   dplyr::mutate(wt=1/distance) %>%
  ##   {.}
  ## if (is.null(limit)) {
  ##   limit = 5
  ## }
  years = as.integer(substr(date_min, 1, 4)):(as.integer(substr(date_max, 1, 4)))
  print(years)
  result = compile_weather_isd_main(useSavedData=FALSE, years=years, building=b)
  print(head(result))
  ## good_stations = result$good
  ## bad_stations = result$bad
  ## nearbyStations <- nearbyStations %>%
  ##   dplyr::filter(`id` %in% good_stations) %>%
  ##   {.}
  ## return(list(df=nearbyStations, bad=bad_stations))
  return(list(df=result))
}

## todo: get lat lon by year to restrict to buildings that appear in the data set
## make a single building version of the analysis
#' Get a data frame of nearby weather stations
#'
#' This function get a data frame of nearby stations, with columns: usaf, wban,
#' begin, end, distance, and Building_Number
#' @param lat_lon_df a data frame containing a column "latitude", and a column
#'   "longitude"
#' @param isd_data optional, returned by rnoaa::isd_stations(refresh = TRUE)
#' @param radius (numeric) Radius (in km) to search from the lat,lon
#'   coordinates, used in isd_station_search
#' @param limit the maximum nearest stations returned for each building
#' @param date_min An integer giving the earliest date of the weather time
#'   series that the user would like in the final output. This integer should be
#'   formatted as yyyymmdd (20150101 for Jan 1, 2015)
#' @param date_max An integer giving the latest date of the weather time series
#'   that the user would like in the final output. This integer should be
#'   formatted as yyyymmdd (20150101 for Jan 1, 2015)
#' @keywords nearby isd
#' @export
#' @examples
#' get_nearby_isd_stations(lat_lon_df, isd_data=isd_data, radius = 100, limit=5,
#'   date_min = 20150101, date_max = 20151231)
get_nearby_isd_stations <- function (lat_lon_df, isd_data, radius, limit, date_min, date_max, year) {
  if (missing(isd_data)) {
    isd_data = rnoaa::isd_stations(refresh = TRUE)
  }
  if (missing(lat_lon_df)) {
    lat_lon_df = db.interface::get_lat_lon_df()
  }
  v_isd_stations_search = Vectorize(rnoaa::isd_stations_search)
  result = v_isd_stations_search(lat = lat_lon_df$latitude, lon = lat_lon_df$longitude, radius = radius)
  acc = lapply(1:ncol(result), function(i){
    b = lat_lon_df$Building_Number[i]
    result[, i] %>% tibble::as_data_frame() %>%
      dplyr::select(`usaf`, `wban`, `begin`, `end`, `distance`) %>%
      dplyr::mutate(`Building_Number` = b)
  })
  nearby_stations_isd = do.call(rbind, acc)
  print("-=-=-=-=-=-")
  print(nearby_stations_isd)
  print("-=-=-=-=-=-")
  nearby_stations_isd <- nearby_stations_isd %>%
    na.omit() %>%
    {.}
  ## print("date_min")
  ## print(date_min)
  ## print("date_max")
  ## print(date_max)
  print("filter date_min")
  if (!missing(date_min)) {
    nearby_stations_isd <- nearby_stations_isd %>%
      dplyr::mutate(threshmin = as.numeric(gsub("-", "", date_min))) %>%
      dplyr::filter(`begin` < threshmin) %>%
      {.}
  }
  print(nearby_stations_isd)
  print("filter date_max")
  if (!missing(date_max)) {
    nearby_stations_isd <- nearby_stations_isd %>%
      dplyr::mutate(threshmax = as.numeric(gsub("-", "", date_max))) %>%
      dplyr::filter(`end` > threshmax) %>%
      {.}
  }
  print(nearby_stations_isd)
  print("filter year")
  ## first filter by year
  if (!is.null(year)) {
    date_min = year * 10000 + 0101
    date_max = year * 10000 + 1231
    nearby_stations_isd <- nearby_stations_isd %>%
      dplyr::mutate(threshmin = as.numeric(gsub("-", "", date_min))) %>%
      dplyr::mutate(threshmax = as.numeric(gsub("-", "", date_max))) %>%
      dplyr::filter(`begin` < threshmin) %>%
      dplyr::filter(`end` > threshmax) %>%
      {.}
  }
  print(nearby_stations_isd)
  ## filter by number at last
  print("filter by limit")
  if (!missing(limit)) {
    nearby_stations_isd <- nearby_stations_isd %>%
      dplyr::arrange(`Building_Number`, `distance`) %>%
      dplyr::group_by(Building_Number) %>%
      dplyr::filter(row_number() <= limit) %>%
      dplyr::ungroup() %>%
      {.}
  }
  nearby_stations_isd <- nearby_stations_isd %>%
    dplyr::select(-starts_with("thresh")) %>%
    {.}
  print(nearby_stations_isd)
  return(nearby_stations_isd)
}

#' Get unique station ids in two columns: usaf, and wban
#'
#' This function get a data frame of unique isd stations, with columns: usaf, wban,
#' @param station_df a data frame containing at least the two columns: usaf, and wban
#' @keywords nearby isd
#' @export
#' @examples
#' lat_lon_df = db.interface::get_lat_lon_df()
#' get_unique_stations(lat_lon_df)
get_unique_stations <- function(station_df) {
  station_to_download = station_df %>%
    dplyr::select(`usaf`, `wban`) %>%
    dplyr::group_by(`usaf`, `wban`) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    {.}
  return(station_to_download)
}

#' Download isd files to cache directory, with error handler for download failure
#'
#' This function downloads unique stations in station_to_download to a cache
#' directory in rnoaa, it allows user to specify the starting and ending index
#' to allow for downloading part of the stations in station_to_download
#' @param station_to_download required, a data frame with two columns "usaf",
#'   and "wban"
#' @param year required, the year of weather data to download
#' @param start optional, the start index of the weather stations to download.
#'   This allows users to download part of the stations in station_to_download
#' @param end optional, the end index of the weather stations to download. This
#'   allows users to download part of the stations in station_to_download
#' @keywords download isd file
#' @export
#' @examples
#' download_isd(station_to_download, year=year, variables=variables)
download_isd <- function(station_to_download, year, start, end) {
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
    , warning=function(w) {print(sprintf("station %s-%s: %s", station_to_download$usaf[i],
                                        wban=station_to_download$wban[i], w))},
      error=function(e) {print(sprintf("station %s-%s: %s", station_to_download$usaf[i],
                                      wban=station_to_download$wban[i], e))},
      finally = print("no operation"))
  })
}

download_isd_stations_from_top <- function(stations, b, date_min, date_max, v, v_out, topn=5) {
  print(sprintf("number of stations to download %s--------", length(stations)))
  bad_stations = NULL
  good_stations = NULL
  print(sprintf("needs to return -----%s----- stations", topn))
  if (v == "TAVG") {
    var = "temperature"
    format_fun = format_noaa_temperature
  } else {
    return
  }
  for (s in (stations)) {
    ## print(sprintf("download %s", s))
    splitted = strsplit(s, "-")
    usaf = splitted[[1]][1]
    wban = splitted[[1]][2]
    years = as.integer(substr(date_min, 1, 4)):(as.integer(substr(date_max, 1, 4)))
    data = NULL
    ## print(paste(usaf, wban, date_min, date_max, v, v_out, collapse = ", "))
    for (year in years) {
      ## download to cache
      station_df = data.frame(usaf=usaf, wban=wban, `Building_Number`=b)
      download_isd(station_df, year=year)
      oneyear = read_var_by_year(station_df=station_df, var=var, format_fun=format_fun, year=year)
      print(head(oneyear))
      data <- rbind(data, oneyear)
    }
    print(head(data))
    ## if (!(var %in% names(data))){
    ##   print(sprintf("bad station %s, %s not in data", s, v_out))
    ##   bad_stations <- c(bad_stations, s)
    ## } else if (nrow(data) == 0) {
    ##   print(sprintf("bad station %s no data", s))
    ##   bad_stations <- c(bad_stations, s)
    ## } else if (NA %in% data[[v_out]]) {
    ##   print(sprintf("bad station %s containing NA", s))
    ##   bad_stations <- c(bad_stations, s)
    ## } else {
    ##   print(sprintf("good station %s", s))
    ##   good_stations <- c(good_stations, s)
    ##   if (length(good_stations) == topn) {
    ##     print("got all good stations")
    ##     print(good_stations)
    ##     return(list(good=good_stations, bad=bad_stations))
    ##   }
    ## }
  }
  ## print("failed to get enough good stations")
  ## print(good_stations)
  ## return(list(good=good_stations, bad=bad_stations))
}

## fixme: not sure if I should make radius and limit to be arguments of this function too
#' Download isd files to cache directory, and record station mappings to feather files
#'
#' This function downloads isd files of nearby stations for the set of buildings
#' with latitude and longitude specified in lat_lon_df. The isd files are saved
#' to a cache directory in rnoaa
#' @param year required, the year of isd weather data to download
#' @param lat_lon_df optional, default to use db.interface::get_lat_lon_df
#' @param isd_data optional, if missing, it will be created with
#'   rnoaa::isd_stations(refresh = TRUE), but will be a bit slow
#' @param saveResult2File optional, default to TRUE
#' @keywords download isd file
#' @export
#' @examples
#' get_isdfile_by_year(year, lat_lon_df, isd_data)
get_isdfile_by_year <- function(year, lat_lon_df, isd_data, saveResult2File) {
  ## print(sprintf("getting nearby stations for %s", year))
  if (missing(isd_data)) {
    isd_data <-
      rnoaa::isd_stations(refresh = TRUE) %>%
      {.}
  }
  print("isd_data")
  print(head(isd_data))
  station_df = get_nearby_isd_stations(lat_lon_df=lat_lon_df, isd_data=isd_data, radius=100, limit=5, year=year)
  station_to_download = get_unique_stations(station_df)
  if (missing(saveResult2File) || saveResult2File) {
    station_df %>%
      feather::write_feather(sprintf("get.noaa.weather/data/station_df_%s.feather", year))
    station_to_download %>%
      feather::write_feather(sprintf("get.noaa.weather/data/station_to_download_%s.feather", year))
  }
  ## print(station_df)
  ## print(sprintf("downloading stations for %s", year))
  weather_year = download_isd(station_to_download, year=year)
  return(station_df)
}

#' Unit conversion from degree C to F
#'
#' Unit conversion from degree C to F
#' @param c required, degree c to convert
#' @keywords unit conversion
#' @export
#' @examples
#' degreeCtoF(30)
degreeCtoF <- function(c) {
  return(c * 9/5 + 32)
}

format_noaa_temperature <- function(s) {
  return(degreeCtoF(as.double(s) / 10))
}

format_noaa_dewpoint <- function(s) {
  return(degreeCtoF(as.double(s) / 10))
}

## meters per second
format_noaa_wind_speed <- function(s) {
  return(as.double(s) / 10)
}

## meters per second
format_noaa_wind_direction <- function(s) {
  return(as.double(s))
}


#' Compile weather isd main routine
#'
#' This function downloads and compiles noaa isd files rnoaa
#' rnoaa::isd_stations(refresh = TRUE), but will be a bit slow
#' @param useSavedData required, if you want to use saved list of
#'   station_to_download_year.feather in the saved directory
#' @param var required, variable to compile
#' @param years required, a vector of years to compile, e.g. years = c(2015,
#'   2016, 2017)
#' @param building optional, if supplied, only compile results for a building,
#'   if missing, if missing, all building's latitude and longitude will be
#'   used
#' @param latitude optional, latitude of building, if latitude is supplied, we
#'   assume longitude and building are also supplied
#' @param longitude optional, longitude of building
#' @keywords download isd file
#' @export
#' @examples
#' for all buildings: compile_weather_isd_main(useSavedData=TRUE, years=c(2015, 2016, 2017))
#' for one building: compile_weather_isd_main(useSavedData=FALSE, years=c(2015, 2016, 2017), building="AK0000ZZ")
compile_weather_isd_main <- function(useSavedData, years, lat_lon_df, latitude, longitude, building, saveResult2File=FALSE, step="month") {
  format_function_list = list(temperature = format_noaa_temperature,
                              wind_speed = format_noaa_wind_speed,
                              wind_direction = format_noaa_wind_direction,
                              temperature_dewpoint = format_noaa_dewpoint)
  print("compile_weather_isd")
  print(years)
  ## may change it to process multiple variables
  var = "temperature"
  format_fun = format_function_list[[var]]
  weather = NULL
  if (!useSavedData) {
    ## fixme: change to standard way of loading package data
    load(file="~/Dropbox/gsa_2017/get.noaa.weather/data/isdData.rda")
    if (missing(lat_lon_df)) {
      if (missing(building)) {
        ## this branch computes for all buildings
        lat_lon_df = db.interface::get_lat_lon_df()
      } else {
        if (missing(latitude)) {
          ## print("222--------------")
          lat_lon_df = db.interface::get_lat_lon_df(building=building)
        } else {
        ## print("333--------------")
        lat_lon_df = data.frame(Building_Number=building, latitude=latitude, longitude=longitude)
        }
      }
    }
    if (is.null(lat_lon_df)) {
      print("cannot get weather data because of missing lat lon")
      return(NULL)
    }
    print("----000000----")
    print(lat_lon_df)
    ## print("444----------------")
    for (year in years) {
      print("year")
      print(year)
      station_df = get_isdfile_by_year(year, lat_lon_df, isd_data = isd_data, saveResult2File=saveResult2File) %>%
        dplyr::mutate(`inv_dist`=1/`distance`) %>%
        {.}
      print("station_df")
      print(station_df)
      ## print(sprintf("compile %s for year %s", var, year))
      weather_year = read_var_by_year(station_df=station_df, var=var, format_fun=format_fun, year=year, step=step)
      print(head(weather_year))
      weather = rbind(weather, weather_year)
    }
  } else {
    for (year in years) {
      station_df = feather::read_feather(sprintf("~/Dropbox/gsa_2017/get.noaa.weather/data/station_df_%s.feather", year)) %>%
        dplyr::mutate(`inv_dist`=1/`distance`) %>%
        {.}
      print(sprintf("compile %s for year %s", var, year))
      weather_year = read_var_by_year(station_df=station_df, var=var, format_fun=format_fun, year=year, step=step)
      weather = rbind(weather, weather_year)
    }
  }
  return(weather)
}

#' Compute weighted average temperature of nearby stations for buildings in station_df
#'
#' This function downloads and compiles noaa isd files rnoaa
#' rnoaa::isd_stations(refresh = TRUE), but will be a bit slow
#' @param station_df required, a dataframe containing station id (usaf and wban), and Building_Number
#' @param var required, the variable to compile, you can choose from the 4:
#' "temperature", "temperature_dewpoint", "wind_direction", "wind_speed"
#' @param format_fun required, function to format variable
#' @param year required, year of isd file to compile
#' @param step optional, summary, if month, take monthly average, if hour do not aggregate to month
#' @keywords download isd file
#' @export
#' @examples
#' read_var_by_year(station_df, var="temperature", format_fun=format_noaa_temperature, year=2015)
read_var_by_year <- function(station_df, var, format_fun, year, step="month") {
  buildings = unique(station_df$Building_Number)
  accWhole = NULL
  counter = 1
  var_quality = paste(var, "quality", sep = "_")
  name_formatted = paste0(var, "F")
  ## print(name_formatted)
  name_formatted_hour = paste0(var, "F", "hour")
  ## print(name_formatted_hour)
  weighted_name_formatted_hour = paste0("wt_", var, "F", "hour")
  ## print(weighted_name_formatted_hour)
  weighted_name_formatted_month = paste0("wt_", var, "F", "month")
  ## print(sprintf("name_formatted_hour: %s", name_formatted_hour))
  ## for (b in buildings[1:1]) {
  for (b in buildings) {
      print(paste0(counter, ", compute ", var, " for building ", b, "---"))
      dfTemp = station_df %>%
          dplyr::filter(`Building_Number`==b) %>%
          {.}
      acc = NULL
      ## for (i in 1:1) {
      for (i in 1:nrow(dfTemp)) {
        usaf <- dfTemp$usaf[i]
        wban <- dfTemp$wban[i]
        d = dfTemp$inv_dist[i]
        ## print(sprintf("%s-%s, weight %s", usaf, wban, d))
        tryCatch(
          {data <- rnoaa::isd(usaf=usaf, wban=wban, additional=FALSE, year=year) %>%
            dplyr::select(dplyr::one_of("date", "time", var, var_quality)) %>%
            ## keep only data points with good quality
            dplyr::filter_(paste(var_quality, "%in% c(\"1\", \"5\")")) %>%
            {.}
            data <- data %>%
              dplyr::mutate(!!rlang::sym(name_formatted) := ((format_fun)(!!rlang::sym(var)))) %>%
              {.}
            data <- data %>%
              dplyr::mutate(`hour`=substr(`time`, start = 1, stop = 2)) %>%
              dplyr::group_by(`date`, `hour`) %>%
              dplyr::summarise(!!rlang::sym(name_formatted_hour):=mean(!!rlang::sym(name_formatted))) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(`wt`=d) %>%
              {.}
            acc = rbind(acc, data)
            ## print(sprintf("number of rows in acc: %s", nrow(acc)))
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
    ## print(head(acc))
    ## acc %>% readr::write_csv("csv_FY/weather/acc.csv")
      wtTemp = acc %>%
        dplyr::group_by(`date`, `hour`) %>%
        dplyr::summarise(!!rlang::sym(weighted_name_formatted_hour):=weighted.mean(x=(!!rlang::sym(name_formatted_hour)), w=wt)) %>%
        dplyr::ungroup() %>%
        {.}
    ## wtTemp %>% readr::write_csv("csv_FY/weather/wtTemp.csv")
    ## fixme: add summary in different duration, and maybe agg method
    ## print(head(wtTemp))
      if (step=="month") {
        wtTemp = wtTemp %>%
          dplyr::mutate(`Date`=zoo::as.yearmon(substr(`date`, start=1, stop=6), "%Y %m")) %>%
          dplyr::group_by(`Date`) %>%
          dplyr::summarise(!!rlang::sym(weighted_name_formatted_month) := mean(!!rlang::sym(weighted_name_formatted_hour))) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(`year`=format(`Date`, "%Y"),
                        `month`=format(`Date`, "%m")) %>%
          dplyr::select(-`Date`) %>%
          {.}
      }
    ## wtTemp %>% readr::write_csv("csv_FY/weather/wtTemp_month.csv")
    ## print(head(wtTemp))
      ## wtTemp %>%
      ##   feather::write_feather(sprintf("%s/%s_monthly_weighted_%s_%s.feather", pathname,
      ##                                  b, var, year))
      accWhole = rbind(accWhole, wtTemp)
      counter = counter + 1
  }
  return(accWhole)
}

## test

## ## example usecase

## ## The following fields are available in isd downloaded files
## ##  [1] "total_chars"                  "usaf_station"                
## ##  [3] "wban_station"                 "date"                        
## ##  [5] "time"                         "date_flag"                   
## ##  [7] "latitude"                     "longitude"                   
## ##  [9] "type_code"                    "elevation"                   
## ## [11] "call_letter"                  "quality"                     
## ## [13] "wind_direction"               "wind_direction_quality"      
## ## [15] "wind_code"                    "wind_speed"                  
## ## [17] "wind_speed_quality"           "ceiling_height"              
## ## [19] "ceiling_height_quality"       "ceiling_height_determination"
## ## [21] "ceiling_height_cavok"         "visibility_distance"         
## ## [23] "visibility_distance_quality"  "visibility_code"             
## ## [25] "visibility_code_quality"      "temperature"                 
## ## [27] "temperature_quality"          "temperature_dewpoint"        
## ## [29] "temperature_dewpoint_quality" "air_pressure"                
## ## [31] "air_pressure_quality"                                   

## ## compiles isd data to weighted monthly weather for each building
## year = 2016
