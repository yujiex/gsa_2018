library("dplyr")
library("locpol")
library("DBI")
library("RSQLite")
library("ggplot2")
library("readr")
library("readxl")

## read in building energy data from
readFieldDB <- function (fields, table, year) {
  con <- dbConnect(RSQLite::SQLite(), "csv_FY/db/all.db")
  fieldStr = paste(fields, collapse = ",")
  if (missing(year)) {
    querystr = sprintf("SELECT %s FROM %s", fieldStr, table)
  } else {
    querystr = sprintf("SELECT %s FROM %s WHERE year='%s'", fieldStr, table, year)
  }
  out = dbGetQuery(con,  querystr) %>%
    as_data_frame() %>%
    dplyr::group_by_at(vars(one_of(fields))) %>%
    dplyr::summarise(n()) %>%
    slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-`n()`) %>%
    {.}
  dbDisconnect(con)
  return(out)
}

dfEnergy = readFieldDB(c("Building_Number", "year", "month", "eui_elec", "eui_gas", "eui_oil", "eui_steam"),
                       "EUAS_monthly") %>%
  {.}

df_loc = readFieldDB(c("Building_Number", "latlng"), "EUAS_monthly_weather") %>%
  dplyr::mutate(`latlng`=gsub("\\[|\\]", "", `latlng`)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(`lat`=strsplit(`latlng`, ", ")[[1]][1]) %>%
  dplyr::mutate(`lng`=strsplit(`latlng`, ", ")[[1]][2]) %>%
  dplyr::mutate_at(vars(`lat`, `lng`), as.numeric) %>%
  {.}
head(df_loc)

dfEnergy %>%
  dplyr::left_join(df_loc) %>%
  readr::write_csv("csv_FY/toBImap.csv")

tmp = readr::read_csv("csv_FY/toBImap.csv") %>%
  dplyr::select(-month, -latlng) %>%
  dplyr::group_by(Building_Number, year) %>%
  dplyr::summarise(eui_elec = sum(eui_elec), eui_gas = sum(eui_gas), eui_oil = sum(eui_oil),
                   eui_steam = sum(eui_steam), lat = mean(lat), lng = mean(lng)) %>%
  dplyr::mutate(`year`=as.Date(sprintf("%s-01-01", year))) %>%
  na.omit() %>%
  readr::write_csv("csv_FY/toBImap_small_allyear.csv")

## following just some random crap, read in excel energy file
readxl::read_excel("input/FY/EUAS/EUAS_AllRegions_2016-2017.xlsx",sheet=2) %>%
  as_data_frame() %>%
  dplyr::select(`Region No.`, `Fiscal Month`, `Building Number`, `Gross Sq.Ft`, `Electricity (KWH)`, `Steam (Thou. lbs)`, `Gas (Cubic Ft)`, `Oil (Gallon)`) %>%
  dplyr::mutate(`eui_elec` = `Electricity (KWH)` * 3.412 / `Gross Sq.Ft`,
                `eui_gas` = `Gas (Cubic Ft)` * 1.026 / `Gross Sq.Ft`,
                `eui_oil` = `Oil (Gallon)` * (139 + 138 + 146 + 150)/4 / `Gross Sq.Ft`,
                `eui_steam` = `Steam (Thou. lbs)` * 1194 / `Gross Sq.Ft`) %>%
  na.omit() %>%
  dplyr::group_by(`Region No.`, `Building Number`) %>%
  dplyr::summarise(eui_elec = sum(eui_elec), eui_gas = sum(eui_gas), eui_oil = sum(eui_oil),
                   eui_steam = sum(eui_steam)) %>%
  na.omit() %>%
  dplyr::left_join(df_loc, by=c("Building Number" = "Building_Number")) %>%
  dplyr::select(-`latlng`) %>%
  readr::write_csv("csv_FY/toBImap_small_2017.csv")
