#'@importFrom magrittr %>%
NULL
#' Get latitude longitude
#'
#' This function get latitude longitude of EUAS buildings
#' @param path to the all.db file, default is "csv_FY/db/"
#' @param building optional, if supplied, only this building's data is returned
#' @keywords latlon
#' @export
#' @examples
#' get_lat_lon_df(building="XXXXXXXX")
get_lat_lon_df <- function(path, building) {
  if (missing(path)) {
    path = "~/Dropbox/gsa_2017/csv_FY/db/"
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), paste0(path, "all.db"))
  if (missing(building)) {
  lat_lon_df =
    DBI::dbGetQuery(con, "SELECT * FROM EUAS_latlng_2" ) %>%
    tibble::as_data_frame() %>%
    {.}
  } else {
    lat_lon_df =
      DBI::dbGetQuery(con, sprintf("SELECT * FROM EUAS_latlng_2 WHERE Building_Number = \'%s\'", building)) %>%
      tibble::as_data_frame() %>%
      {.}
  }
  lat_lon_df <- lat_lon_df %>%
    dplyr::mutate(`latlng`=gsub("\\[|\\]", "", `latlng`)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(`lat`=strsplit(`latlng`, ", ")[[1]][1]) %>%
    dplyr::mutate(`lng`=strsplit(`latlng`, ", ")[[1]][2]) %>%
    dplyr::mutate_at(dplyr::vars(`lat`, `lng`), as.numeric) %>%
    dplyr::select(-`latlng`, -`geocoding_input`) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(`Building_Number`, `lat`, `lng`) %>%
    dplyr::rename(`latitude`=`lat`, `longitude`=`lng`) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    {.}
  DBI::dbDisconnect(con)
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
    path = "~/Dropbox/gsa_2017/csv_FY/db/"
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), paste0(path, dbname, ".db"))
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
  alltables = DBI::dbListTables(con)
  DBI::dbDisconnect(con)
  return(alltables)
}

#' Read table from db
#'
#' This function returns a table from database
#' @param dbname required, the name string of the database, e.g. "all.db" has name "all"
#' @param tablename required, the name string of the table to view
#' @param path optional, the path to .db file, default is "csv_FY/db/"
#' @param cols optional, the columns to select
#' @param building optional, only read data for this building
#' @keywords view head table sqlite
#' @export
#' @examples
#' view_head_of_table(dbname="all", tablename="EUAS_type")
read_table_from_db <- function(dbname, tablename, path, cols, building) {
  con = connect(dbname, path)
  ## print("read table from %s %s.db", getwd(), dbname)
  if (missing(building)) {
    df =
      DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", tablename)) %>%
      tibble::as_data_frame() %>%
      {.}
  } else {
    df =
      DBI::dbGetQuery(con, sprintf("SELECT * FROM %s WHERE Building_Number = \'%s\'", tablename, building)) %>%
      tibble::as_data_frame() %>%
      {.}
  }
  if (!missing(cols)) {
    df <- df %>%
      dplyr::select(dplyr::one_of(cols)) %>%
      {.}
  }
  DBI::dbDisconnect(con)
  return(df)
}

#' Get unique values from a column
#'
#' This function returns a table from database
#' @param dbname required, the name string of the database, e.g. "all.db" has name "all"
#' @param tablename required, the name string of the table to view
#' @param path optional, the path to .db file, default is "csv_FY/db/"
#' @param col required, the column to get unique value of
#' @keywords view head table sqlite
#' @export
#' @examples
#' get_unique_value_column(dbname="all", tablename="EUAS_type_recode", col="Building_Type")
get_unique_value_column <- function(dbname, tablename, path, col) {
  vals = read_table_from_db(dbname, tablename, path, col) %>%
    dplyr::distinct(!!(rlang::sym(col)))
  return(vals)
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
    DBI::dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT 5", tablename)) %>%
    tibble::as_data_frame() %>%
    {.}
  DBI::dbDisconnect(con)
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
    DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", tablename)) %>%
    tibble::as_data_frame() %>%
    names()
  DBI::dbDisconnect(con)
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
    DBI::dbGetQuery(con, "SELECT DISTINCT Building_Number FROM EUAS_monthly") %>%
    tibble::as_data_frame() %>%
    {.}
  DBI::dbDisconnect(con)
  return(df)
}

#' Get list of buildings
#'
#' This function returns a vector of building id's
#' @param region optional, region number
#' @param buildingType optional, building type
#' @param year optional, restrict to data with fiscal year = year, or in the vector of years
#' @param category optional, restrict to data with category in a vector of
#'   categories, c(a vector of categories, e.g. "A", "I")
#' @keywords get all buildings
#' @export
#' @examples
#' get_buildings()
get_buildings <- function(region, buildingType, year, category) {
  con = connect("all")
  df =
    DBI::dbGetQuery(con, "SELECT * FROM eui_by_fy_tag") %>%
    tibble::as_data_frame() %>%
    dplyr::filter(`Gross_Sq.Ft` != 0) %>%
    dplyr::filter(`eui_elec` != 0) %>%
    dplyr::mutate(`Region_No.` = as.numeric(`Region_No.`)) %>%
    {.}
  if (!missing(region)) {
    df <- df %>%
      dplyr::filter(`Region_No.` == region) %>%
      {.}
  }
  if (!missing(year)) {
    df <- df %>%
      dplyr::filter(`Fiscal_Year` %in% year) %>%
      {.}
  }
  if (!missing(buildingType)) {
    df <- df %>%
      dplyr::filter(`Building_Type` == buildingType) %>%
      {.}
  }
  if (!missing(category)) {
    df <- df %>%
      dplyr::filter(`Cat` %in% category) %>%
      {.}
  }
  print(sprintf("number of buildings: %s", nrow(df)))
  DBI::dbDisconnect(con)
  return(unique(df$Building_Number))
}

#' Get the count of buildings with region, category (A, C, I), type (office, etc.), and year filters applied
#'
#' This function returns the count group by region, category, and type, if any
#' of the parameters are specified, they will be used in filtering the results
#' @param region optional, a string vector of region numbers, or a single
#'   region number
#' @param category optional, a subset of A, B, C, D, E, I
#' @param type optional, a string (e.g. "Office"), or a string vector (e.g. c("Office", "Courthouse"))of building type
#' @param year optional, a double vector of years, or a single year
#' @param fOrC optional, specify one of "F" (fiscal year) or "C" (calendar year), default to "F"
#' @param gbvars optional, if unspecified, group by all 4 variables, otherwise, only group by the ones specified in gbvars
#' @keywords query count
#' @export
#' @examples
#' get_count(region=1, category=c("A", "C", "I"), "office", "F", gbvars="Cat")
get_count <- function(region, category, type, year, fOrC, gbvars) {
  if (missing(fOrC) || (fOrC == "F")) {
    year_col = "Fiscal_Year"
  } else {
    year_col = "year"
  }
  con = connect("all")
  df =
    DBI::dbGetQuery(con, sprintf("SELECT [Region_No.], %s, Cat, Building_Type, lowElectricity, lowGas, highEnoughELectricityGas, zeroSqft FROM eui_by_fy_tag", year_col)) %>%
    tibble::as_data_frame() %>%
    {.}
  print(head(df))
  if (!missing(region)) {
    region = as.character(region)
    if (is.vector(region)) {
      df = df %>%
        dplyr::filter(`Region_No.` %in% region) %>%
        {.}
    } else {
      df = df %>%
        dplyr::filter(`Region_No.` == region) %>%
        {.}
    }
  }
  print(head(df))
  if (!missing(category)) {
    df = df %>%
      dplyr::filter(`Cat` %in% category) %>%
      {.}
  }
  print(head(df))
  if (!missing(type)) {
    df = df %>%
      dplyr::filter(`Building_Type` %in% type) %>%
      {.}
  }
  print(head(df))
  if (!missing(year)) {
    if (is.vector(year)) {
      df = df %>%
        dplyr::filter_(sprintf("%s %%in%% c(%s)", year_col, paste(as.character(year), collapse = ","))) %>%
        {.}
    } else {
      df = df %>%
        dplyr::filter_(sprintf("%s == %s", year_col, year)) %>%
        {.}
    }
  }
  print(head(df))
  if (missing(gbvars)) {
    df = df %>%
      dplyr::group_by(`Region_No.`, `Fiscal_Year`, `Cat`, `Building_Type`) %>%
      {.}
  } else {
    df = df %>%
      dplyr::group_by_at(vars(dplyr::one_of(gbvars))) %>%
      {.}
  }
  df <- df %>%
    dplyr::summarise(count = n(),
                     lowElectricity_n = sum(lowElectricity),
                     lowGas_n = sum(lowGas),
                     highEnoughElectricityGas_n = sum(highEnoughElectricityGas),
                     zeroSqft_n = sum(zeroSqft)) %>%
    {.}
  DBI::dbDisconnect(con)
  return(df)
}
