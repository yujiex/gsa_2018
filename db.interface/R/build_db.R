#'@importFrom pipeR %>>%
#'@importFrom magrittr %>%
NULL
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
  df1 = DBI::dbGetQuery(con, "SELECT DISTINCT Building_Number, Predominant_Use AS Building_Type FROM Entire_GSA_Building_Portfolio_input") %>%
    tibble::as_data_frame() %>%
    dplyr::mutate(`data_source`="Entire_GSA_Building_Portfolio_input::Predominant_Use") %>%
    {.}
  df2 = DBI::dbGetQuery(con, "SELECT DISTINCT Building_Number, [Self-Selected_Primary_Function] AS Building_Type FROM PortfolioManager_sheet0_input") %>%
    tibble::as_data_frame() %>%
    dplyr::mutate(`data_source`="PortfolioManager_sheet0_input::Self-Selected_Primary_Function") %>%
    {.}
  df3 = DBI::dbGetQuery(con, "SELECT Building_Number, [GSA Property Type] AS Building_Type FROM euas_database_of_buildings_cmu") %>%
    tibble::as_data_frame() %>%
    dplyr::mutate(`data_source`="euas_database_of_buildings_cmu::[GSA Property Type]") %>%
    {.}
  DBI::dbDisconnect(con)
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
  write_table_to_db(df=result, dbname="all", "EUAS_type", overwrite=TRUE)
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
    dplyr::mutate_at(dplyr::vars(`Building_Type`), dplyr::recode, "Office Building"="Office", "All Other"="Other", "Non-Refrigerated Warehouse"="Warehouse") %>%
    {.}
  write_table_to_db(df=df, dbname = "all", tablename = "EUAS_type_recode", overwrite = TRUE)
}

#' Write table from db
#'
#' This function writes a table to database
#' @param df required, data frame to write to database
#' @param dbname required, the name string of the database, e.g. "all.db" has
#'   name "all"
#' @param tablename required, the name string of the table to view
#' @param path optional, the path to .db file, default is "csv_FY/db/"
#' @param overwrite optional, whether to overwrite, default to FALSE. If
#'   overwrite is set to true, a backup csv file will be created in db_build_temp_csv
#' @keywords write table sqlite
#' @export
#' @examples
#' view_head_of_table(dbname="all", tablename="EUAS_type")
write_table_to_db <- function(df, dbname, tablename, path, overwrite) {
  con = connect(dbname, path)
  if (missing(overwrite)) {
    overwrite = FALSE
  }
  DBI::dbWriteTable(con, tablename, df, overwrite=overwrite)
  if (overwrite) {
    backup_table(dbname=dbname, tablename=tablename)
  }
  print(sprintf("Write table %s to %s.db", tablename, dbname))
  DBI::dbDisconnect(con)
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
  DBI::dbDisconnect(con)
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
  df1 = DBI::dbGetQuery(con, "SELECT * FROM EUAS_monthly") %>%
    tibble::as_data_frame() %>%
    {.}
  df2 = DBI::dbGetQuery(con, "SELECT * FROM EUAS_type_recode")
    tibble::as_data_frame() %>%
    {.}
  DBI::dbDisconnect(con)
  df = df1 %>%
    dplyr::left_join(df2, by="Building_Number") %>%
    dplyr::rename(`type_data_source`=`data_source`) %>%
    {.}
  write_table_to_db(df, dbname="all", tablename="EUAS_monthly_with_type", overwrite=TRUE)
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
    ## DBI::dbGetQuery(con, "SELECT * FROM EUAS_monthly_with_type LIMIT 100") %>%
    DBI::dbGetQuery(con, "SELECT * FROM EUAS_monthly_with_type") %>%
    tibble::as_data_frame() %>%
    dplyr::select(-`index`, -`month`, -`Fiscal_Month`) %>%
    {.}
  DBI::dbDisconnect(con)
  df_numeric = df %>%
    dplyr::select(-`Gross_Sq.Ft`) %>%
    dplyr::group_by(`Building_Number`, !!rlang::sym(year_col)) %>%
    dplyr::summarise_if(is.numeric, dplyr::funs(sum)) %>%
    dplyr::ungroup() %>%
    ## dplyr::summarise_if(is.character, dplyr::funs(first)) %>%
    {.}
  df_char = df %>%
    dplyr::select(`Building_Number`, !!rlang::sym(year_col), `State`, `Cat`, `Gross_Sq.Ft`, `Region_No.`, `Service_Center`, `Area_Field_Office`, `Building_Designation`, `Building_Type`, `type_data_source`) %>%
    ## dplyr::select(`Building_Number`, !!rlang::sym(year_col), `State`, `Cat`, `Gross_Sq.Ft`, `Region_No.`, `Service_Center`, `Area_Field_Office`, `Building_Designation`, `Building_Type`, `type_data_source`, `state_abbr`) %>%
    dplyr::group_by(`Building_Number`, !!rlang::sym(year_col)) %>%
    dplyr::summarise_all(dplyr::funs(first)) %>%
    dplyr::ungroup() %>%
    {.}
  df_year = df_char %>%
    dplyr::left_join(df_numeric) %>%
    dplyr::select(-dplyr::one_of(non_year_col)) %>%
    {.}
  write_table_to_db(df=df_year, dbname="all", tablename="eui_by_fy", overwrite=TRUE)
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
  df = DBI::dbGetQuery(con, "SELECT * FROM eui_by_fy") %>%
    dplyr::mutate(`lowElectricity`=(`eui_elec` <= 12) & (!(`Gross_Sq.Ft` == 0)),
                  `lowGas`=(`eui_gas` <= 3) & (!(`Gross_Sq.Ft` == 0)),
                  `highEnoughElectricityGas`=(`eui_gas` > 3) & (`eui_elec` > 12) & (!(`Gross_Sq.Ft` == 0)),
                  `zeroSqft`=(`Gross_Sq.Ft` == 0)) %>%
    {.}
  DBI::dbDisconnect(con)
  write_table_to_db(df=df, dbname="all", tablename="eui_by_fy_tag", overwrite=TRUE)
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
  df = DBI::dbGetQuery(con, "SELECT * FROM EUAS_monthly") %>%
    dplyr::arrange(`Building_Number`, `Fiscal_Year`, `Fiscal_Month`, `index`) %>%
    dplyr::group_by(`Building_Number`, `Fiscal_Year`, `Fiscal_Month`) %>%
    dplyr::slice(n()) %>%
    dplyr::ungroup()
    {.}
  df %>%
    readr::write_csv("csv_FY/db_build_temp_csv/EUAS_monthly.csv")
  DBI::dbWriteTable(con, "EUAS_monthly", df, overwrite=TRUE)
  DBI::dbDisconnect(con)
  print("Created table: EUAS_monthly")
}

#' Compute monthly eui
#'
#' This function adds chilled water eui
#' @param df required, data frame containing input, output, and sqft column
#' @param energy_input required, input column to compute eui
#' @param eui_output required, output column name
#' @param sqftcol required, the sqft column
#' @param mult optional, unit conversion multiplier for kBtu
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
  df = DBI::dbGetQuery(con, "SELECT * FROM EUAS_monthly") %>%
    ## conversion to kbtu
    dplyr::mutate(`Chilled_Water_(kBtu)` = `Chilled_Water_(Ton_Hr)` * 12) %>%
    dplyr::mutate(`Oil_(kBtu)` = `Oil_(Gallon)` * ((139 + 138 + 146 + 150)/4)) %>%
    dplyr::mutate(`Other_(kBtu)` = `Other_(mmBTU)` * 1000) %>%
    dplyr::mutate(`Total_(kBtu)` = `Electric_(kBtu)` + `Gas_(kBtu)` + `Oil_(kBtu)` + `Steam_(kBtu)` + `Chilled_Water_(kBtu)` + `Other_(kBtu)`) %>%
    dplyr::mutate(`Total_(Cost)` = `Electricity_(Cost)` + `Gas_(Cost)` + `Oil_(Cost)` + `Steam_(Cost)` + `Chilled_Water_(Cost)` + `Other_(Cost)`) %>%
    compute_eui(energy_input = "Chilled_Water_(kBtu)", eui_output = "eui_chilledWater", sqftcol="Gross_Sq.Ft",
                mult=1) %>%
    compute_eui(energy_input = "Other_(kBtu)", eui_output = "eui_other", sqftcol="Gross_Sq.Ft",
                mult=1) %>%
    compute_eui(energy_input = "Oil_(kBtu)", eui_output = "eui_oil", sqftcol="Gross_Sq.Ft",
                mult=1) %>%
    dplyr::mutate(`eui_total` = `eui_elec` + `eui_gas` + `eui_steam` + `eui_oil` + `eui_chilledWater` + `eui_other`) %>%
    {.}
  DBI::dbDisconnect(con)
  write_table_to_db(df=df, dbname="all", tablename="EUAS_monthly", overwrite=TRUE)
}

#' Ship db
#'
#' Deliver the database with key table
#' @keywords ship db
#' @export
#' @examples
#' get_ship_db()
get_ship_db <- function() {
  tables = c("EUAS_monthly", "EUAS_address", "EUAS_ecm", "EUAS_latlng_2", "eui_by_fy_tag", "EUAS_ecm_program", "EUAS_type", "EUAS_type_recode")
  for (table in tables) {
    df <- read_table_from_db(dbname="all", tablename=table)
    write_table_to_db(df=df, dbname="all_ship", tablename=table, overwrite = TRUE)
  }
}

#' Back up a table to a csv file
#'
#' This function backs up the specified table in db_build_temp_csv, with a timestamp attached to the filename
#' @param dbname required, the name string of the database, e.g. "all.db" has name "all"
#' @param tablename required, the name string of the table to view
#' @keywords backup table
#' @export
#' @examples
#' backup_table(dbname="all", tablename="EUAS_address")
backup_table <- function(dbname, tablename) {
  df = db.interface::read_table_from_db(dbname=dbname, tablename=tablename)
  timestamp = Sys.time()
  filename = sprintf("csv_FY/db_build_temp_csv/backups/%s_%s.csv", tablename, timestamp)
  df %>%
    readr::write_csv(filename)
  print(sprintf("write to backup file: %s", filename))
}

#' Check duplicate of table
#'
#' This function checks duplicates of a table, group by columns
#' @param dbname required, the name string of the database, e.g. "all.db" has name "all"
#' @param tablename required, the name string of the table to view
#' @param groupby_vars required, columns to group by at
#' @keywords backup table
#' @export
#' @examples
#' backup_table(dbname="all", tablename="EUAS_address")
check_duplicates <- function(dbname, tablename, groupby_vars) {
  print(sprintf("check duplicates of %s in %s.db", tablename, dbname))
  df = db.interface::read_table_from_db(dbname=dbname, tablename=tablename) %>%
    tibble::as_data_frame() %>%
    {.}
  dups <- df %>%
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(groupby_vars))) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::ungroup() %>%
    {.}
  if (nrow(dups) == 0) {
    print("no duplicate")
  } else {
    print("has duplicates")
  }
}

#' Recode state to abbreviation
#'
#' This function converts state full name in EUAS data to state abbreviation, as
#' the current state in the data is a mixture of abbreviation and full name
#' @keywords state to abbr
#' @export
#' @examples
#' recode_state_abbr()
recode_state_abbr <- function() {
  df = db.interface::read_table_from_db(dbname="all", tablename="EUAS_monthly") %>%
    {.}
  print("check duplicates of EUAS_monthly")
  df %>%
    dplyr::group_by(`Building_Number`, `Fiscal_Year`, `Fiscal_Month`) %>%
    dplyr::filter(n() > 1) %>%
    head() %>%
    print()
  dflookup = readr::read_csv("input/FY/state_abbr.csv") %>%
    tibble::as_data_frame() %>%
    {.}
  dflookup <- dflookup %>%
    dplyr::rename(`State`=`state_full_name`) %>%
    dplyr::bind_rows(data.frame(State=unique(dflookup$state_abbr), state_abbr=unique(dflookup$state_abbr))) %>%
    {.}
  dflookup %>%
    readr::write_csv("csv_FY/db_build_temp_csv/dflookup_dup.csv")
  print("check duplicates of dflookup")
  dflookup %>%
    dplyr::group_by(`State`) %>%
    dplyr::filter(n() > 1) %>%
    head() %>%
    print()
  print("check NA in state_abbr")
  dflookup %>%
    dplyr::filter(is.na(`state_abbr`)) %>%
    head() %>%
    print()
  print(head(dflookup))
  print("check anti join")
  df %>%
    dplyr::select(-dplyr::one_of("state_abbr")) %>%
    dplyr::anti_join(dflookup, by="State") %>%
    nrow() %>%
    print()
  df <- df %>%
    ## if this column is not in there, it will throw out a warning
    dplyr::select(-dplyr::one_of("state_abbr")) %>%
    dplyr::left_join(dflookup, by="State") %>%
    dplyr::mutate(`state_abbr_from_id` = substr(`Building_Number`, start=1, stop=2)) %>%
    ## for NA state, if it has "AX", its state does not equal the first two character of the id
    dplyr::mutate(`state_abbr` = ifelse(is.na(`state_abbr`),
                                 ifelse(`state_abbr_from_id` == "AX", NA, `state_abbr_from_id`), `state_abbr`)) %>%
    dplyr::select(-`state_abbr_from_id`) %>%
    {.}
  print("check NA in no join var")
  df %>%
    dplyr::select(`Building_Number`, `Fiscal_Year`, `Fiscal_Month`, `state_abbr`) %>%
    dplyr::filter(is.na(`state_abbr`)) %>%
    head() %>%
    print()
  print("check duplicates of EUAS_monthly after join")
  df %>%
    dplyr::select(`Building_Number`, `Fiscal_Year`, `Fiscal_Month`) %>%
    dplyr::group_by(`Building_Number`, `Fiscal_Year`, `Fiscal_Month`) %>%
    dplyr::filter(n() > 1) %>%
    head() %>%
    print()
  write_table_to_db(df=df, dbname="all", tablename="EUAS_monthly", overwrite = TRUE)
}

#' Correct city name
#'
#' This function overwrites the city name error, or unify their names
#' @keywords city name
#' @export
#' @examples
#' correct_city_name()
correct_city_name <- function() {
  df_correction = readr::read_csv("input/FY/city_name_correction.csv") %>%
    tibble::as_data_frame() %>%
    dplyr::select(`Building_Number`, `City`, `correct_city_name`) %>%
    ## only keep those that are actually corrected
    dplyr::filter(`City` != `correct_city_name`) %>%
    {.}
  df = db.interface::read_table_from_db(dbname="all", tablename="EUAS_address") %>%
    dplyr::select(`City`, `Building_Number`, `source`) %>%
    dplyr::mutate(`City`=toupper(`City`)) %>%
    dplyr::left_join(df_correction, by=c("Building_Number", "City")) %>%
    dplyr::mutate(`City`=ifelse(is.na(`correct_city_name`), `City`, `correct_city_name`)) %>%
    dplyr::mutate(`source`=ifelse(is.na(`correct_city_name`), `source`, "manual correction")) %>%
    dplyr::group_by(`Building_Number`, `City`) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(`Building_Number`, `City`, `source`) %>%
    ## readr::write_csv("csv_FY/db_build_temp_csv/corrected_name.csv")
    {.}
  write_table_to_db(df, dbname = "all", tablename = "EUAS_city", overwrite = TRUE)
}

#' compute heating and cooling energy consumption per square foot
#'
#' This function computes the heating and cooling energy use per square foot for
#' each building. Assume heating fuels include gas, oil, and steam; cooling fuel
#' include electricity and chilled water. If there's zero gas + oil + steam for
#' a year, assume electricity heating.
#' @keywords heating cooling eui
#' @export
#' @examples
#' get_heating_cooling_eui
get_heating_cooling_eui <- function(debugFlag=FALSE) {
  df = db.interface::read_table_from_db(dbname="all", tablename="EUAS_monthly") %>%
    tibble::as_data_frame() %>%
    dplyr::mutate(`Cooling_(kBtu)` = `Electric_(kBtu)` + `Chilled_Water_(kBtu)`,
                  `Heating_(kBtu)` = `Gas_(kBtu)` + `Oil_(kBtu)` + `Steam_(kBtu)`) %>%
    ## convertion factors are found here: https://portfoliomanager.zendesk.com/hc/en-us/articles/216670148-What-are-the-Site-to-Source-Conversion-Factors-
    dplyr::mutate(`Cooling_(kBtu)_source` = `Electric_(kBtu)` * 3.14 + `Chilled_Water_(kBtu)` * 1.0,
                  `Heating_(kBtu)_source` = `Gas_(kBtu)` * 1.05 + `Oil_(kBtu)` * 1.01 + `Steam_(kBtu)` * 1.2) %>%
    {.}
  if ("electric_heating" %in% names(df)) {
    df <- df %>%
      dplyr::select(-`electric_heating`) %>%
      {.}
  }
  df_tag = df %>%
    dplyr::select(`Building_Number`, `Heating_(kBtu)`, `Fiscal_Year`) %>%
    dplyr::group_by(`Building_Number`, `Fiscal_Year`) %>%
    dplyr::summarise(`electric_heating` = (sum(`Heating_(kBtu)`) == 0)) %>%
    dplyr::ungroup() %>%
    {.}
  df <- df %>%
    dplyr::left_join(df_tag, by=c("Building_Number", "Fiscal_Year")) %>%
    dplyr::mutate(`Heating_(kBtu)` = ifelse(`electric_heating`, `Electric_(kBtu)`, `Heating_(kBtu)`),
                  `Heating_(kBtu)_source` = ifelse(`electric_heating`, `Electric_(kBtu)` * 3.14,
                                                   `Heating_(kBtu)_source`)) %>%
    dplyr::mutate(`Electric_(kBtu)_source` = `Electric_(kBtu)` * 3.14) %>%
    dplyr::mutate(`Gas_(kBtu)_source` = `Gas_(kBtu)` * 1.05) %>%
    dplyr::mutate(`eui_elec_source` = `Electric_(kBtu)_source` / `Gross_Sq.Ft`,
                  `eui_gas_source` = `Gas_(kBtu)_source` / `Gross_Sq.Ft`,
                  `eui_heating` = `Heating_(kBtu)` / `Gross_Sq.Ft`,
                  `eui_cooling` = `Cooling_(kBtu)` / `Gross_Sq.Ft`,
                  `eui_heating_source` = `Heating_(kBtu)_source` / `Gross_Sq.Ft`,
                  `eui_cooling_source` = `Cooling_(kBtu)_source` / `Gross_Sq.Ft`) %>%
    {.}
  if (debugFlag) {
    df %>%
      dplyr::select(`Building_Number`, `Fiscal_Year`, `Fiscal_Month`, `Electric_(kBtu)`, `Chilled_Water_(kBtu)`,
                    `Gas_(kBtu)`, `Oil_(kBtu)`, `Steam_(kBtu)`, `electric_heating`, `Heating_(kBtu)`, `Cooling_(kBtu)`,
                    `Heating_(kBtu)_source`, `Cooling_(kBtu)_source`, `Electric_(kBtu)_source`, `Gas_(kBtu)_source`) %>%
      readr::write_csv("~/Dropbox/gsa_2017/csv_FY/db_build_temp_csv/EUAS_monthly_heating_cooling_source.csv")
  } else {
    write_table_to_db(df, dbname = "all", tablename = "EUAS_monthly", overwrite = TRUE)
  }
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
  ## recode_state_abbr()
  ## check_duplicates(dbname="all", tablename="EUAS_monthly",
  ##                  groupby_vars=c("Building_Number", "Fiscal_Year", "Fiscal_Month"))
  ## correct_city_name()
  ## check_duplicates(dbname="all", tablename="EUAS_city",
  ##                  groupby_vars=c("Building_Number", "City", "source"))
  ## unify_euas_type()
  ## check_duplicates(dbname="all", tablename="EUAS_type",
  ##                  groupby_vars=c("Building_Number", "Building_Type", "data_source"))
  ## add_chilled_water_eui()
  ## check_duplicates(dbname="all", tablename="EUAS_monthly",
  ##                  groupby_vars=c("Building_Number", "Fiscal_Year", "Fiscal_Month"))
  get_heating_cooling_eui(debugFlag=FALSE)
  check_duplicates(dbname="all", tablename="EUAS_monthly",
                   groupby_vars=c("Building_Number", "Fiscal_Year", "Fiscal_Month"))
  recode_euas_type()
  check_duplicates(dbname="all", tablename="EUAS_type_recode",
                   groupby_vars=c("Building_Number", "Building_Type", "data_source"))
  join_type_and_energy()
  check_duplicates(dbname="all", tablename="EUAS_monthly_with_type",
                   groupby_vars=c("Building_Number", "Fiscal_Year", "Fiscal_Month"))
  get_eui_by_year("F")
  check_duplicates(dbname="all", tablename="eui_by_fy",
                   groupby_vars=c("Building_Number", "Fiscal_Year"))
  add_quality_tag_energy()
  check_duplicates(dbname="all", tablename="eui_by_fy_tag",
                   groupby_vars=c("Building_Number", "Fiscal_Year"))
}
