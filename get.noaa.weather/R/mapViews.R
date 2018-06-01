#' plot isd_stations
#'
#' This function plots the isd stations available for a year
#' @param isd_data optional, returned by rnoaa::isd_stations(refresh = TRUE)
#' @param year optional, if supplied only plot stations with data in that year
#' @param latitude optional, latitude of the center of the view
#' @param longitude optional, longitude of the center of the view
#' @param zoom optional, zoom level of the map view
#' @keywords plot isd station
#' @export
#' @examples
#' view_isd_stations_year(isd_data, year=2015)
view_isd_stations_year <- function(isd_data, year, latitude, longitude, zoom) {
  if (missing(isd_data)) {
    isd_data = rnoaa::isd_stations(refresh = TRUE) %>%
      dplyr::filter(!is.na(`lat`)) %>%
      dplyr::filter(!is.na(`lon`)) %>%
      {.}
  }
  if (!missing(year)) {
    date_min = year * 10000 + 0101
    date_max = year * 10000 + 1231
    isd_data <- isd_data %>%
      dplyr::filter(`begin` < date_min) %>%
      dplyr::filter(`end` > date_max) %>%
      {.}
  }
  m <-
    leaflet::leaflet(isd_data) %>%
    leaflet::addCircles(lng = ~lon, lat = ~lat) %>%
    {.}
  if (!missing(latitude)) {
    m <- m %>%
      leaflet::setView(lng = longitude, lat = latitude, zoom = zoom) %>%
      leaflet::addMarkers(lng = longitude, lat = latitude, , popup="target location") %>%
      {.}
  }
  m <- m %>%
    leaflet::addTiles()
  print(m)
}
