#' Get a data frame of nearby weather stations
#'
#' This function get a data frame of nearby stations of ghcnd_data, in the form of a named list
#' @param lat_lon_df a data frame containing a column "latitude", and a column
#'   "longitude"
#' @param id_col_name optional, a unique identifier for each location, default to be "id"
#' @param ghcnd_data optional, returned by rnoaa::ghcnd_stations(refresh = TRUE)
#' @param radius (numeric) Radius (in km) to search from the lat,lon
#'   coordinates, used in isd_station_search
#' @param limit the maximum nearest stations returned for each building
#' @param date_min An integer giving the earliest date of the weather time
#'   series that the user would like in the final output. This integer should be
#'   formatted as yyyymmdd (20150101 for Jan 1, 2015)
#' @param date_max An integer giving the latest date of the weather time series
#'   that the user would like in the final output. This integer should be
#'   formatted as yyyymmdd (20150101 for Jan 1, 2015)
#' @param var optional, variable to download, default to TMIN
#' @param testing optional, if marked true, only the head part of lat_lon_df will be evaluated
#' @keywords nearby isd
#' @export
#' @examples
#' get_nearby_isd_stations(lat_lon_df, isd_data=isd_data, radius = 100, limit=5,
#'   date_min = 20150101, date_max = 20151231)
get_nearby_ghcnd_stations <- function (lat_lon_df, id_col_name, ghcnd_data, radius=NULL, limit=NULL, date_min=NULL,
                                       date_max=NULL, year=NULL, var="TMIN", testing=FALSE) {
  print("var")
  print(var)
  lat_lon_df <- lat_lon_df %>%
    dplyr::rename(`id`=`Name`) %>%
    {.}
  if (testing) {
    lat_lon_df <- head(lat_lon_df)
  }
  year_min = NULL
  year_max = NULL
  if (!is.null(date_min)) {
    year_min = as.numeric(substr(date_min, start=1, stop=4))
  }
  if (!is.null(date_min)) {
    year_max = as.numeric(substr(date_max, start=1, stop=4))
  }
  if (!is.null(year)) {
    year_min = year
    year_max = year
  }
  if (testing) {
    lat_lon_df <- head(lat_lon_df)
  }
  result = rnoaa::meteo_nearby_stations(lat_lon_df=lat_lon_df, station_data=ghcnd_data, var=var,
                               year_min=year_min, radius=radius, limit=limit)
  ## exclude stations with missing data
  ## rnoaa::ghcnd_search(stationid=s, date_min=date_min, date_max=date_max, var=v)[[v]] %>%
  return(result)
}

#' Download a list of stations, return the bad ones
#'
#' @param stations the list of stations to download, normally much larger than limit
#' @param date_min a string "yyyy-mm-dd"
#' @param date_max a string "yyyy-mm-dd"
#' @param v required, variable to download, e.g. TMIN
#' @param v_out required, the output variable from noaa ghcnd, e.g. tmin
#' @keywords nearby isd
#' @export
#' @examples
#' get_nearby_isd_stations(lat_lon_df, isd_data=isd_data, radius = 100, limit=5,
#'   date_min = 20150101, date_max = 20151231)
download_stations <- function(stations, date_min, date_max, v, v_out) {
  print(sprintf("number of stations to download %s--------", length(stations)))
  bad_stations = NULL
  for (s in (stations)) {
    ## print(sprintf("download %s", s))
    data = rnoaa::ghcnd_search(stationid=s, date_min=date_min, date_max=date_max, var=v)[[v_out]] %>%
      {.}
    ## print(data)
    if (nrow(data) == 0) {
      print(sprintf("bad station %s no data", s))
      bad_stations <- c(bad_stations, s)
    } else if (NA %in% data[[v_out]]) {
      print(sprintf("bad station %s containing NA", s))
      bad_stations <- c(bad_stations, s)
    }
  }
  return(bad_stations)
}

#' Download stations from top to bottom, return top n stations with no missing data
#'
#' @param stations the list of stations to download, normally much larger than limit
#' @param date_min a string "yyyy-mm-dd"
#' @param date_max a string "yyyy-mm-dd"
#' @param v required, variable to download, e.g. TMIN
#' @param v_out required, the output variable from noaa ghcnd, e.g. tmin
#' @param limit the number of good stations to return, default to 5
#' @keywords nearby isd
#' @export
#' @examples
#' get_nearby_isd_stations(lat_lon_df, isd_data=isd_data, radius = 100, limit=5,
#'   date_min = 20150101, date_max = 20151231)
download_stations_from_top <- function(stations, date_min, date_max, v, v_out, topn=5) {
  print(sprintf("number of stations to download %s--------", length(stations)))
  bad_stations = NULL
  good_stations = NULL
  print(sprintf("needs to return -----%s----- stations", topn))
  for (s in (stations)) {
    ## print(sprintf("download %s", s))
    data = rnoaa::ghcnd_search(stationid=s, date_min=date_min, date_max=date_max, var=v)[[v_out]] %>%
      {.}
    ## print(data)
    if (!(v_out %in% names(data))){
      print(sprintf("bad station %s, %s not in data", s, v_out))
      bad_stations <- c(bad_stations, s)
    } else if (nrow(data) == 0) {
      print(sprintf("bad station %s no data", s))
      bad_stations <- c(bad_stations, s)
    } else if (NA %in% data[[v_out]]) {
      print(sprintf("bad station %s containing NA", s))
      bad_stations <- c(bad_stations, s)
    } else {
      print(sprintf("good station %s", s))
      good_stations <- c(good_stations, s)
      if (length(good_stations) == topn) {
        print("got all good stations")
        print(good_stations)
        return(list(good=good_stations, bad=bad_stations))
      }
    }
  }
  print("failed to get enough good stations")
  print(good_stations)
  return(list(good=good_stations, bad=bad_stations))
}

#' Get a data frame of nearby weather stations for one location
#'
#' This function get a data frame of nearby stations for one location, when
#' there is missing data, we want to exclude the station and re-download, until
#' no missing data
#' @param lat_lon_df a data frame with one row, containing a column "latitude",
#'   a column "longitude", and an id column
#' @param id_col_name optional, a unique identifier for each location, default
#'   to be "id"
#' @param v required, variable to download
#' @param ghcnd_data optional, returned by rnoaa::ghcnd_stations(refresh = TRUE)
#' @param radius (numeric) Radius (in km) to search from the lat,lon
#'   coordinates, used in isd_station_search
#' @param limit the maximum nearest stations returned for each building
#' @param date_min An integer giving the earliest date of the weather time
#'   series that the user would like in the final output. This integer should be
#'   formatted as yyyymmdd (20150101 for Jan 1, 2015)
#' @param date_max An integer giving the latest date of the weather time series
#'   that the user would like in the final output. This integer should be
#'   formatted as yyyymmdd (20150101 for Jan 1, 2015)
#' @param v optional, variable to download, default to TMIN
#' @param testing optional, if marked true, only the head part of lat_lon_df
#'   will be evaluated
#' @keywords nearby isd
#' @export
#' @examples
#' get_nearby_isd_stations(lat_lon_df, isd_data=isd_data, radius = 100, limit=5,
#'   date_min = 20150101, date_max = 20151231)
get_nearby_ghcnd_stations_one_loc <- function (lat_lon_df, id_col_name="id", ghcnd_data, v, radius=NULL, limit=NULL,
                                               date_min=NULL, date_max=NULL, year=NULL) {
  print("start downloading")
  print(date_min)
  print(date_max)
  b = lat_lon_df[[id_col_name]][[1]]
  v_out = tolower(v)
  nearbyStations =
    get_nearby_ghcnd_stations(lat_lon_df=b_loc, ghcnd_data=ghcnd_data, var=v,
                              ## return a lot and filter by whether having data
                              radius=radius, limit=100, date_min=date_min,
                              date_max=date_max, year=year)[[b]]
  print("head of nearby stations")
  print(head(nearbyStations))
  stations = nearbyStations$id
  if (is.null(limit)) {
    limit = 5
  }
  result = download_stations_from_top(stations, date_min, date_max, v, v_out, topn=limit)
  good_stations = result$good
  bad_stations = result$bad
  nearbyStations <- nearbyStations %>%
    dplyr::filter(`id` %in% good_stations) %>%
    {.}
  return(list(df=nearbyStations, bad=bad_stations))
}

#' Convert named list to a data frame
#'
#' This function converts a named list data frame to a data frame, with a column
#' nameCol tagging the names of the named list
#' @param nameList the input named list, each element is a data frame
#' @param nameCol the column name to put the names in of the named list
#' @keywords named list to df
#' @export
#' @examples
#' nameListToDf(nearbyStations, "building")
nameListToDf <- function(nameList, nameCol) {
  acc = NULL
  for (b in names(nameList)) {
    df <- nameList[[b]] %>%
      dplyr::mutate(!!rlang::sym(nameCol):=b) %>%
      {.}
    acc <- rbind(acc, df)
  }
  return(acc)
}

#' Get a data frame of nearby weather stations for locations in the whole df,
#' removing NA stations or no data stations
#'
#' This function get a data frame of nearby stations for all locations in a data
#' frame, when there is missing data, we want to exclude the station and
#' re-download, until no missing data
#' @param lat_lon_df a data frame with one row, containing a column "latitude",
#'   a column "longitude", and an id column
#' @param id_col_name optional, a unique identifier for each location, default
#'   to be "id"
#' @param v required, variable to download
#' @param ghcnd_data optional, returned by rnoaa::ghcnd_stations(refresh = TRUE)
#' @param radius (numeric) Radius (in km) to search from the lat,lon
#'   coordinates, used in isd_station_search
#' @param limit the maximum nearest stations returned for each building
#' @param date_min An integer giving the earliest date of the weather time
#'   series that the user would like in the final output. This integer should be
#'   formatted as yyyymmdd (20150101 for Jan 1, 2015)
#' @param date_max An integer giving the latest date of the weather time series
#'   that the user would like in the final output. This integer should be
#'   formatted as yyyymmdd (20150101 for Jan 1, 2015)
#' @param var optional, variable to download, default to TMIN
#' @param testing optional, if marked true, only the head part of lat_lon_df
#'   will be evaluated
#' @keywords nearby isd
#' @export
#' @examples
#' get_nearby_isd_stations(lat_lon_df, isd_data=isd_data, radius = 100, limit=5,
#'   date_min = 20150101, date_max = 20151231)
get_nearby_ghcnd_stations_all_loc <- function (lat_lon_df, id_col_name="id", ghcnd_data, v, radius=NULL, limit=NULL,
                                               date_min=NULL, date_max=NULL, year=NULL, testing=FALSE) {
  print("start downloading-----------------------------")
  print(date_min)
  print(date_max)
  v_out = tolower(v)
  print(testing)
  if (testing) {
    lat_lon_df <- head(lat_lon_df, n=10)
    print("lat_lon_df--------")
    print(lat_lon_df)
    print("----------")
  }
  nearbyStations =
    get_nearby_ghcnd_stations(lat_lon_df=lat_lon_df, ghcnd_data=ghcnd_data,
                              radius=radius, limit=limit, date_min=date_min,
                              date_max=date_max, year=year)
  acc = nameListToDf(nearbyStations, "building")
  print(head(acc))
  print(tail(acc))
  stations = unique(acc$id)
  ## make all result in to a data frame from the list
  ## stations = nearbyStations$id
  bad_stations <- download_stations(stations, date_min, date_max, v, v_out)
  ## good_stations <- stations[!(stations %in% bad_stations)]
  print(sprintf("length of bad stations: %s", length(bad_stations)))
  ## print(sprintf("length of good stations: %s", good_stations))
  if (length(bad_stations) > 0) {
    ghcnd_data <- ghcnd_data %>%
      dplyr::filter(!(id %in% bad_stations)) %>%
      {.}
    print("needs redo start")
    needs_redo = acc %>%
      dplyr::filter(!(`id` %in% bad_stations)) %>%
      dplyr::group_by(`building`) %>%
      dplyr::filter(n() < 5) %>%
      dplyr::ungroup() %>%
      {.}
    print("needs redo end")
    print(head(needs_redo))
    lat_lon_redo = lat_lon_df %>%
      dplyr::filter(`Name` %in% needs_redo$building) %>%
      {.}
    print(sprintf("number of buildings needs redo: %s", nrow(lat_lon_redo)))
    print(sprintf("nrow of noaa stations: %s", nrow(ghcnd_data)))
    get_nearby_ghcnd_stations_all_loc(lat_lon_df=lat_lon_redo, id_col_name=id_col_name, ghcnd_data=ghcnd_data,
                                      v=v, radius=radius, limit=limit, date_min=date_min, date_max=date_max,
                                      year=year)
  } else {
    return(nameListToDf(nearbyStations, "building"))
  }
}

#' Compile weather ghcnd main routine
#'
#' This function downloads and compiles ghcnd files
#' @param var required, the variable to compile, default to "TMIN"
#' @param station_df required, a data frame containing the ids of nearby stations
#' @param building required, will be a column of returned data frame
#' @keywords download isd file
#' @export
#' @examples
#' for one building: compile_weather_isd_main(useSavedData=FALSE, years=c(2015, 2016, 2017), building="AK0000ZZ")
compile_weather_ghcnd_main <- function(building, station_df, date_min=NULL, date_max=NULL, var="TMIN", format_fun=get.noaa.weather::degreeCtoF) {
  station_df <- station_df %>%
    dplyr::mutate(`inv_dist`=1/`distance`) %>%
    {.}
  v = tolower(var)
  acc <- NULL
  ## for (i in 1:1) {
  for (i in 1:nrow(station_df)) {
    s <- station_df$id[i]
    d = station_df$inv_dist[i]
    ## print(s)
    ## print(d)
    ## print(date_min)
    ## print(date_max)
    ## print(v)
    tryCatch(
    {data <-
      rnoaa::ghcnd_search(stationid=s, date_min=date_min, date_max=date_max, var=v)[[v]] %>%
      dplyr::select(one_of("id", v, "date")) %>%
      dplyr::rename(`value`=!!rlang::sym(v)) %>%
      ## unit conversion
      dplyr::mutate(format_value = ((format_fun)(value))) %>%
      dplyr::mutate(`wt`=d) %>%
      {.}
      ## print(head(data))
      acc = rbind(acc, data)
      },
      warning = function(w){
        print(sprintf("warning ghcnd", w))
      },
      error = function(e){
        print(sprintf("error %s", e))
      },
      finally =  {
      }
    )
  }
  ## get inverse weighted variable
  wtTemp = acc %>%
    dplyr::group_by(`date`) %>%
    dplyr::summarise(`weighted`=weighted.mean(x=format_value, w=wt)) %>%
    dplyr::ungroup() %>%
    {.}
  ## fixme:covert to wide bookmark
  return(wtTemp)
}
