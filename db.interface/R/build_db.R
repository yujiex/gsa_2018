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

#' Recode building type
#'
#' This function recode different variations of building types, e.g. Office and
#' Office Building is combined into the Office category
#' @keywords recode building type
#' @export
#' @examples
#' recode_euas_type()
recode_euas_type <- function() {
  df = read_table_from_db(dbname = "all", tablename = "EUAS_type")
  df = df %>%
    dplyr::mutate_at(vars(`Building_Type`), recode, "Office Building"="Office", "All Other"="Other", "Non-Refrigerated Warehouse"="Warehouse") %>%
    {.}
  write_table_to_db(df=df, dbname = "all", tablename = "EUAS_type_recode", overwrite = TRUE)
}

#' Write table from db
#'
#' This function writes a table to database
#' @param df required, data frame to write to database
#' @param dbname required, the name string of the database, e.g. "all.db" has name "all"
#' @param tablename required, the name string of the table to view
#' @param path optional, the path to .db file, default is "csv_FY/db/"
#' @param overwrite optional, whether to overwrite, default to FALSE
#' @keywords write table sqlite
#' @export
#' @examples
#' view_head_of_table(dbname="all", tablename="EUAS_type")
write_table_to_db <- function(df, dbname, tablename, path, overwrite) {
  con = connect(dbname, path)
  if (missing(overwrite)) {
    overwrite = FALSE
  }
  dbWriteTable(con, tablename, df, overwrite=overwrite)
  print(sprintf("Write table %s to %s.db", tablename, dbname))
  dbDisconnect(con)
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
#' @keywords join
#' @export
#' @examples
#' join_type_and_energy()
join_type_and_energy <- function() {
  con = connect("all")
  df1 = dbGetQuery(con, "SELECT * FROM EUAS_monthly") %>%
    as_data_frame() %>%
    {.}
  df2 = dbGetQuery(con, "SELECT * FROM EUAS_type_recode")
    as_data_frame() %>%
    {.}
  df = df1 %>%
    dplyr::left_join(df2, by="Building_Number") %>%
    dplyr::rename(`type_data_source`=`data_source`) %>%
    {.}
  dbWriteTable(con, "EUAS_monthly_with_type", df, overwrite=TRUE)
  print("Created table: EUAS_monthly_with_type")
  dbDisconnect(con)
}

#' Get eui by fiscal year
#'
#' This function produces eui_by_fy table, with all columns aggregated to
#' yearly. The returned table is a superset of the old eui_by_fy
#' @keywords eui by year
#' @param fOrC optional, "F" for fiscal year, "C" for calendar year
#' @export
#' @examples
#' get_eui_by_year("F")
get_eui_by_year <- function(fOrC) {
  if (missing(fOrC) || (fOrC == "F")) {
    year_col = "Fiscal_Year"
    non_year_col = "year"
  } else {
    year_col = "year"
    non_year_col = "Fiscal_Year"
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
    dplyr::select(`Building_Number`, !!rlang::sym(year_col), `State`, `Cat`, `Gross_Sq.Ft`, `Region_No.`, `Service_Center`, `Area_Field_Office`, `Building_Designation`, `Building_Type`, `type_data_source`) %>%
    dplyr::group_by(`Building_Number`, !!rlang::sym(year_col)) %>%
    dplyr::summarise_all(funs(first)) %>%
    dplyr::ungroup() %>%
    {.}
  df_year = df_char %>%
    dplyr::left_join(df_numeric) %>%
    dplyr::select(-one_of(non_year_col)) %>%
    {.}
  dbWriteTable(con, "eui_by_fy", df_year, overwrite=TRUE)
  dbDisconnect(con)
  print("Created table: eui_by_fy")
}

#' Add energy sanity check
#'
#' This function produces EUAS_yearly table, with all columns aggregated to
#' yearly
#' @keywords quality tag
#' @export
#' @examples
#' add_quality_tag_energy()
add_quality_tag_energy <- function() {
  con = connect("all")
  df = dbGetQuery(con, "SELECT * FROM eui_by_fy") %>%
    dplyr::mutate(`lowElectricity`=(`eui_elec` <= 12) & (!(`Gross_Sq.Ft` == 0)),
                  `lowGas`=(`eui_gas` <= 3) & (!(`Gross_Sq.Ft` == 0)),
                  `highEnoughElectricityGas`=(`eui_gas` > 3) & (`eui_elec` > 12) & (!(`Gross_Sq.Ft` == 0)),
                  `zeroSqft`=(`Gross_Sq.Ft` == 0)) %>%
    {.}
  df %>%
    readr::write_csv("csv_FY/db_build_temp_csv/eui_by_fy_tag.csv")
  dbWriteTable(con, "eui_by_fy_tag", df, overwrite=TRUE)
  dbDisconnect(con)
  print("Created table: eui_by_fy_tag")
}

#' Remove old energy data based on index
#'
#' This function removes the old energy data
#' @keywords drop old
#' @export
#' @examples
#' remove_old_energy_data()
remove_old_energy_data <- function() {
  con = connect("all")
  df = dbGetQuery(con, "SELECT * FROM EUAS_monthly") %>%
    dplyr::arrange(`Building_Number`, `Fiscal_Year`, `Fiscal_Month`, `index`) %>%
    dplyr::group_by(`Building_Number`, `Fiscal_Year`, `Fiscal_Month`) %>%
    dplyr::slice(n()) %>%
    dplyr::ungroup()
    {.}
  df %>%
    readr::write_csv("csv_FY/db_build_temp_csv/EUAS_monthly.csv")
  dbWriteTable(con, "EUAS_monthly", df, overwrite=TRUE)
  dbDisconnect(con)
  print("Created table: EUAS_monthly")
}

#' Add chilled water eui
#'
#' This function adds chilled water eui
#' @keywords modify db
#' @export
#' @examples
#' compute_eui()
compute_eui <- function(df, energy_input, eui_output, sqftcol, mult) {
  if (missing(mult)) {
    mult = 1
  }
  df = df %>%
    dplyr::mutate(!!(rlang::sym(eui_output)) := !!(rlang::sym(energy_input)) / !!(rlang::sym(sqftcol)) * mult) %>%
    {.}
  return(df)
}

#' Add chilled water eui
#'
#' This function adds chilled water eui
#' @keywords modify db
#' @export
#' @examples
#' add_chilled_water_eui()
add_chilled_water_eui <- function() {
  con = connect("all")
  df = dbGetQuery(con, "SELECT * FROM EUAS_monthly") %>%
    compute_eui(energy_input = "Chilled_Water_(Ton_Hr)", eui_output = "eui_chilledWater", sqftcol="Gross_Sq.Ft",
                mult=12) %>%
    dplyr::mutate(`eui_total` = `eui_elec` + `eui_gas` + `eui_steam` + `eui_oil` + `eui_chilledWater`) %>%
    dplyr::mutate(`Chilled_Water_(kBtu)` = `Chilled_Water_(Ton_Hr)` * 12) %>%
    dplyr::mutate(`Total_(kBtu)` = `Electric_(kBtu)` + `Gas_(kBtu)` + `Oil_(kBtu)` + `Steam_(kBtu)` + `Chilled_Water_(kBtu)`) %>%
    {.}
  df %>%
    readr::write_csv("csv_FY/db_build_temp_csv/EUAS_monthly.csv")
  dbWriteTable(con, "EUAS_monthly", df, overwrite=TRUE)
  dbDisconnect(con)
  print("Created table: EUAS_monthly")
}

#' Join EUAS_monthly and EUAS_type
#'
#' This function joins EUAS_monthly and EUAS_type
#' @keywords drop table
#' @export
#' @examples
#' main_db_build()
main_db_build <- function() {
  ## remove_old_energy_data()
  ## unify_euas_type()
  add_chilled_water_eui()
  recode_euas_type()
  join_type_and_energy()
  get_eui_by_year("F")
  add_quality_tag_energy()
}
