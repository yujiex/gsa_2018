#' Get latitude longitude
#'
#' This function get latitude longitude of EUAS buildings
#' @param path to the all.db file, default is "csv_FY/db/"
#' @keywords latlon
#' @export
#' @examples
#' get_lat_lon_df()
get_lat_lon_df <- function(path) {
  if (missing(path)) {
    path = "csv_FY/db/"
  }
  con <- dbConnect(RSQLite::SQLite(), paste0(path, "all.db"))
  lat_lon_df =
    dbGetQuery(con, 'SELECT * FROM EUAS_latlng_2' ) %>%
    as_data_frame() %>%
    dplyr::mutate(`latlng`=gsub("\\[|\\]", "", `latlng`)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(`lat`=strsplit(`latlng`, ", ")[[1]][1]) %>%
    dplyr::mutate(`lng`=strsplit(`latlng`, ", ")[[1]][2]) %>%
    dplyr::mutate_at(vars(`lat`, `lng`), as.numeric) %>%
    dplyr::select(-`latlng`, -`geocoding_input`) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(`Building_Number`, `lat`, `lng`) %>%
    dplyr::rename(`latitude`=`lat`, `longitude`=`lng`) %>%
    slice(1) %>%
    dplyr::ungroup() %>%
    {.}
  dbDisconnect(con)
  return(lat_lon_df)
}

#' Get the connection object of sqlite database with name "dbname"
#'
#' This function returns a connector to dbname
#' @param dbname required, the name string of the database, e.g. "all.db" has name "all"
#' @param path optional, the path to .db file, default is "csv_FY/db/"
#' @keywords connection sqlite
#' @export
#' @examples
#' connect("all")
connect <- function(dbname, path) {
  if (missing(path)) {
    path = "csv_FY/db/"
  }
  con <- dbConnect(RSQLite::SQLite(), paste0(path, dbname, ".db"))
  return(con)
}

#' Get all tables in a sqlite .db file
#'
#' This function returns a vector of all tables in the database
#' @param dbname required, the name string of the database, e.g. "all.db" has name "all"
#' @param path optional, the path to .db file, default is "csv_FY/db/"
#' @keywords all_tables sqlite
#' @export
#' @examples
#' get_all_tables("all")
get_all_tables <- function(dbname, path) {
  con <- connect(dbname)
  alltables = dbListTables(con)
  return(alltables)
}

#' Read table from db
#'
#' This function returns a table from database
#' @param dbname required, the name string of the database, e.g. "all.db" has name "all"
#' @param tablename required, the name string of the table to view
#' @param path optional, the path to .db file, default is "csv_FY/db/"
#' @param cols optional, the columns to select
#' @keywords view head table sqlite
#' @export
#' @examples
#' view_head_of_table(dbname="all", tablename="EUAS_type")
read_table_from_db <- function(dbname, tablename, path, cols) {
  con = connect(dbname, path)
  df =
    dbGetQuery(con, sprintf('SELECT * FROM %s', tablename)) %>%
    as_data_frame() %>%
    {.}
  if (!missing(cols)) {
    df <- df %>%
      dplyr::select(one_of(cols)) %>%
      {.}
  }
  return(df)
}

#' Print the head of a table in a sqlite database
#'
#' This function returns the head of a table in a sqlite database
#' @param dbname required, the name string of the database, e.g. "all.db" has name "all"
#' @param tablename required, the name string of the table to view
#' @param path optional, the path to .db file, default is "csv_FY/db/"
#' @keywords view head table sqlite
#' @export
#' @examples
#' view_head_of_table(dbname="all", tablename="EUAS_type")
view_head_of_table <- function(dbname, tablename, path) {
  con = connect(dbname, path)
  df =
    dbGetQuery(con, sprintf('SELECT * FROM %s', tablename)) %>%
    as_data_frame() %>%
    head()
  return(df)
}

#' Print the head of a table "tablename" in a sqlite database "dbname"
#'
#' This function returns the column names of a table in a sqlite database
#' @param dbname required, the name string of the database, e.g. "all.db" has name "all"
#' @param tablename required, the name string of the table to view
#' @param path optional, the path to .db file, default is "csv_FY/db/"
#' @keywords view names table sqlite
#' @export
#' @examples
#' view_names_of_table(dbname="all", tablename="EUAS_type")
view_names_of_table <- function(dbname, tablename, path) {
  con = connect(dbname, path)
  df =
    dbGetQuery(con, sprintf('SELECT * FROM %s', tablename)) %>%
    as_data_frame() %>%
    names()
  return(df)
}

#' Get all buildings
#'
#' This function returns a data frame with one column, Building_Number,
#' containing all buildings in the EUAS database
#' @keywords get all buildings
#' @export
#' @examples
#' get_euas_buildings()
get_euas_buildings <- function() {
  con = connect("all")
  df =
    dbGetQuery(con, 'SELECT DISTINCT Building_Number FROM EUAS_monthly') %>%
    as_data_frame() %>%
    {.}
  return(df)
}

## how many buildings in region i, category j, type k

##' Get the count of buildings with region, category (A, C, I), type (office, etc.), and year filters applied
##'
##' This function get latitude longitude of EUAS buildings
##' @param path to the all.db file, default is "csv_FY/db/"
##' @keywords latlon
##' @export
##' @examples
## get_count <- function() {
## }
