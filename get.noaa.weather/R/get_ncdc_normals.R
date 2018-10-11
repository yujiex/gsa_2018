#' Get monthly climate normals
#'
#' This function gets the monthly climate normals for a specific station
#' @param s required, station id
#' @keywords ncdc climate normals
#' @export
#' @examples
#' getAvgTemp(building="UT0032ZZ", start_str="2012-01-05", end_str="2015-01-30")
getClimateNormal <- function(s, field, dataset, path=NULL) {
  rnoaa::ncdc(datasetid=dataset, stationid=s, datatypeid=field, startdate = "2010-01-01", enddate = "2010-12-01")
}

getMonthlyNormalHDD <- function(s) {
  print(sprintf("getting hdd normals for %s", s))
  getClimateNormal(s, field="mly-htdd-normal", dataset="NORMAL_MLY", path=NULL)
}

getMonthlyNormalCDD <- function(s) {
  print(sprintf("getting cdd normals for %s", s))
  getClimateNormal(s, field="mly-cldd-normal", dataset="NORMAL_MLY", path=NULL)
}
