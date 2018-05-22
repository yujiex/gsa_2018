#' Unify building type
#'
#' This function compiles building type information to a data frame containing
#' Building_Number, Building_Type, data_source. The input tables are
#' from a database other_input. There are currently three sources of
#' building type: their order of importance are
#' "euas_database_of_buildings_cmu::[GSA Property Type]" >
#' "PortfolioManager_sheet0_input::Self-Selected_Primary_Function" >
#' "Entire_GSA_Building_Portfolio_input::Predominant_Use")))
#' @keywords unify building type
#' @export
#' @examples
#' unify_euas_type()
unify_euas_type <- function() {
  df = get_euas_buildings()
  con <- connect("other_input")
  df1 = dbGetQuery(con, "SELECT DISTINCT Building_Number, Predominant_Use AS Building_Type FROM Entire_GSA_Building_Portfolio_input") %>%
    as_data_frame() %>%
    dplyr::mutate(`data_source`="Entire_GSA_Building_Portfolio_input::Predominant_Use") %>%
    {.}
  df2 = dbGetQuery(con, "SELECT DISTINCT Building_Number, [Self-Selected_Primary_Function] AS Building_Type FROM PortfolioManager_sheet0_input") %>%
    as_data_frame() %>%
    dplyr::mutate(`data_source`="PortfolioManager_sheet0_input::Self-Selected_Primary_Function") %>%
    {.}
  df3 = dbGetQuery(con, "SELECT Building_Number, [GSA Property Type] AS Building_Type FROM euas_database_of_buildings_cmu") %>%
    as_data_frame() %>%
    dplyr::mutate(`data_source`="euas_database_of_buildings_cmu::[GSA Property Type]") %>%
    {.}
  dbDisconnect(con)
  dftype = dplyr::bind_rows(df1, df2, df3) %>%
    dplyr::mutate(`data_source` = factor(`data_source`,
                                         levels = c("euas_database_of_buildings_cmu::[GSA Property Type]",
                                                    "PortfolioManager_sheet0_input::Self-Selected_Primary_Function",
                                                    "Entire_GSA_Building_Portfolio_input::Predominant_Use"))) %>%
    dplyr::arrange(`Building_Number`, `data_source`) %>%
    dplyr::group_by(`Building_Number`) %>%
    slice(1) %>%
    dplyr::ungroup() %>%
    {.}
  result = df %>% dplyr::left_join(dftype) %>%
    {.}
  con <- connect("all")
  dbWriteTable(con, "EUAS_type", result, overwrite=TRUE)
  print("Created table: EUAS_type")
  dbDisconnect(con)
  return(result)
}

#' Drop a table from .db
#'
#' This function deletes a table from database
#' @keywords drop table
#' @param dbname required, the name string of the database, e.g. "all.db" has name "all"
#' @param tablename required, the name string of the table to view
#' @export
#' @examples
#' drop_table_from_db("all", "EUAS_type_")
drop_table_from_db <- function(dbname, tablename) {
  con = connect(dbname)
  dbRemoveTable(con, tablename)
  dbDisconnect(con)
  print(sprintf("Dropped table: %s", tablename))
}

#' Join EUAS_monthly and EUAS_type
#'
#' This function joins EUAS_monthly and EUAS_type
#' @keywords drop table
#' @export
#' @examples
#' join_type_and_energy()
join_type_and_energy <- function() {
  con = connect("all")
  df1 = dbGetQuery(con, "SELECT * FROM EUAS_monthly") %>%
    as_data_frame() %>%
    {.}
  df2 = dbGetQuery(con, "SELECT Building_Number, Building_Type FROM EUAS_type")
    as_data_frame() %>%
    {.}
  df = df1 %>%
    dplyr::left_join(df2, by="Building_Number") %>%
    {.}
  dbWriteTable(con, "EUAS_monthly_with_type", df, overwrite=TRUE)
  print("Created table: EUAS_monthly_with_type")
  dbDisconnect(con)
}

#' Get eui by fiscal year
#'
#' This function produces EUAS_yearly table, with all columns aggregated to
#' yearly
#' @keywords eui by year
#' @param fOrC optional, "F" for fiscal year, "C" for calendar year
#' @export
#' @examples
#' get_eui_by_year("F")
get_eui_by_year <- function(fOrC) {
  if (missing(fOrC) || (fOrC == "F")) {
    year_col = "Fiscal_Year"
  } else {
    year_col = "year"
  }
  con = connect("all")
  df =
    ## dbGetQuery(con, "SELECT * FROM EUAS_monthly_with_type LIMIT 100") %>%
    dbGetQuery(con, "SELECT * FROM EUAS_monthly_with_type") %>%
    as_data_frame() %>%
    dplyr::select(-`index`, -`month`, -`Fiscal_Month`) %>%
    {.}
  df_numeric = df %>%
    dplyr::select(-`Gross_Sq.Ft`) %>%
    dplyr::group_by(`Building_Number`, !!rlang::sym(year_col)) %>%
    dplyr::summarise_if(is.numeric, funs(sum)) %>%
    dplyr::ungroup() %>%
    ## dplyr::summarise_if(is.character, funs(first)) %>%
    {.}
  df_char = df %>%
    dplyr::select(`Building_Number`, !!rlang::sym(year_col), `State`, `Cat`, `Gross_Sq.Ft`, `Region_No.`, `Service_Center`, `Area_Field_Office`, `Building_Designation`, `Building_Type`) %>%
    dplyr::group_by(`Building_Number`, !!rlang::sym(year_col)) %>%
    dplyr::summarise_all(funs(first)) %>%
    dplyr::ungroup() %>%
    {.}
  df_year = df_char %>%
    dplyr::left_join(df_numeric) %>%
    {.}
  dbWriteTable(con, "eui_by_fy", df, overwrite=TRUE)
  dbDisconnect(con)
  print("Created table: eui_by_fy")
}

#' Add energy sanity check
#'
#' This function produces EUAS_yearly table, with all columns aggregated to
#' yearly
#' @keywords eui by year
#' @export
#' @examples
#' add_quality_tag_energy()
add_quality_tag_energy <- function() {
  con = connect("all")
  df = dbGetQuery(con, "SELECT * FROM EUAS_monthly_with_type") %>%
    dplyr::mutate(`lowElectricity`=(`eui_elec` <= 12),
                  `lowGas`=(`eui_gas` <= 3),
                  `zeroSqft`=(`Gross_Sq.Ft` == 0)) %>%
    {.}
  df %>%
    readr::write_csv("csv_FY/db_build_temp_csv/eui_by_fy_tag.csv")
}

#' Join EUAS_monthly and EUAS_type
#'
#' This function joins EUAS_monthly and EUAS_type
#' @keywords drop table
#' @export
main_db_build <- function() {
  ## unify_euas_type()
  ## join_type_and_energy()
  ## get_eui_by_year("F")
  ## add_quality_tag_energy()
}
