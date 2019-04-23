library("dplyr")
library("readr")
library("tidyr")
library("readxl")

setwd("~/Dropbox/gsa_2017/faultDetectionSkySpark/data-raw/skyspark fault detection sparks download/")

devtools::load_all("~/Dropbox/gsa_2017/db.interface")

files = list.files()

## last time
## acc_rule = NULL
## for (f in files) {
##   b = substr(f, 1, 8)
##   print(sprintf("---------------%s---------------", b))
##   df = readr::read_csv(f) %>%
##     tibble::as_data_frame() %>%
##     dplyr::select(`Date`, `dur`, `ruleRef`, `Cost`) %>%
##     dplyr::mutate(`rule`=substr(`ruleRef`, 32, nchar(`ruleRef`)),
##                   `dur`=as.numeric(gsub("h", "", `dur`))) %>%
##     dplyr::mutate_at(vars(Cost), function(x) as.numeric(gsub("$", "", x, fixed=TRUE))) %>%
##     dplyr::select(-`ruleRef`) %>%
##     dplyr::group_by(`rule`) %>%
##     dplyr::summarise(`earliest`=min(Date),
##                      `latest`=max(Date),
##                      `count`=n(),
##                      `Cost`=sum(`Cost`),
##                      `duration_hour`=sum(`dur`)) %>%
##     dplyr::mutate(`building`=b) %>%
##     {.}
##   acc_rule = rbind(acc_rule, df)
## }

## acc_rule %>%
##   readr::write_csv("../rule_summary.csv")

## this time, account for one building split in multiple files
acc_rule = NULL
for (f in files) {
  b = substr(f, 1, 8)
  print(sprintf("---------------%s---------------", b))
  df = readr::read_csv(f) %>%
    tibble::as_data_frame() %>%
    {.}
  if (!("eCost" %in% names(df))) {
    df$eCost = NA
  }
  if (!("mCost" %in% names(df))) {
    df$mCost = NA
  }
  if (!("sCost" %in% names(df))) {
    df$sCost = NA
  }
  ## print(("eCost" %in% names(df)) && ("mCost" %in% names(df)) && ("sCost" %in% names(df)))
  df <- df %>%
    dplyr::select(`Date`, `dur`, `ruleRef`, `equipRef`, `Cost`, `eCost`, `mCost`, `sCost`) %>%
    ## remove na cost, so that when group by and summarise, no NA is produced
    ## dplyr::filter(!is.na(`Cost`)) %>%
    dplyr::mutate(`rule`=substr(`ruleRef`, 32, nchar(`ruleRef`)),
                  `dur`=as.numeric(gsub("h", "", `dur`))) %>%
    dplyr::mutate_at(vars(Cost, eCost, mCost, sCost), function(x) as.numeric(gsub("$", "", x, fixed=TRUE))) %>%
    dplyr::select(-`ruleRef`) %>%
    dplyr::mutate(`building`=b) %>%
    {.}
  print(head(df))
  acc_rule = rbind(acc_rule, df)
}

acc_rule %>%
  readr::write_csv("../all_building_rule_2018.csv")

acc_rule %>%
  na.omit() %>%
  dplyr::group_by(`building`, `rule`) %>%
  dplyr::summarise(`earliest`=min(Date),
                   `latest`=max(Date),
                   `count`=n(),
                   `Cost`=sum(`Cost`),
                   `eCost`=sum(`eCost`),
                   `mCost`=sum(`mCost`),
                   `sCost`=sum(`sCost`),
                   `duration_hour`=sum(`dur`)) %>%
  dplyr::ungroup() %>%
  readr::write_csv("../rule_summary.csv")

acc_rule %>%
  ## na.omit() %>%
  ## dplyr::mutate(eCost=ifelse(is.na(eCost), 0, eCost)) %>%
  ## dplyr::mutate(sCost=ifelse(is.na(sCost), 0, sCost)) %>%
  ## dplyr::mutate(mCost=ifelse(is.na(mCost), 0, mCost)) %>%
  ## dplyr::mutate(Cost=ifelse(is.na(Cost), 0, Cost)) %>%
  dplyr::group_by(`building`, `rule`) %>%
  dplyr::summarise(`earliest`=min(Date),
                   `latest`=max(Date),
                   `count`=n(),
                   `Cost`=sum(`Cost`),
                   `eCost`=sum(`eCost`),
                   `mCost`=sum(`mCost`),
                   `sCost`=sum(`sCost`),
                   `duration_hour`=sum(`dur`)) %>%
  dplyr::ungroup() %>%
  readr::write_csv("../rule_summary_with_na.csv")

acc_rule %>%
  na.omit() %>%
  dplyr::group_by(`building`, `rule`) %>%
  dplyr::summarise(`earliest`=min(Date),
                   `latest`=max(Date),
                   `count`=n(),
                   `Cost`=sum(`Cost`),
                   `eCost`=sum(`eCost`),
                   `mCost`=sum(`mCost`),
                   `sCost`=sum(`sCost`),
                   `duration_hour`=sum(`dur`)) %>%
  dplyr::ungroup() %>%
  readr::write_csv("../rule_summary.csv")

gsalink_old = readr::read_csv("~/Dropbox/gsa_2017/input/FY/ECM info/GSAlink_Buildings_First_55_Opiton_26_Start_Stop_Dates.csv") %>%
  dplyr::select(`Building Name`, `Building ID`) %>%
  dplyr::rename(`Building_Number`=`Building ID`, `name_first_55`=`Building Name`) %>%
  {.}

euas = readr::read_csv("~/Dropbox/gsa_2017/input/FY/static info/euas_database_of_buildings_cmu.csv") %>%
  dplyr::select(`Building Name`, `Location Facility Code`) %>%
  dplyr::distinct(`Building Name`, `Location Facility Code`) %>%
  dplyr::rename(`Building_Number`=`Location Facility Code`, `name_euas`=`Building Name`) %>%
  {.}

head(euas)

## get building names
readxl::read_excel("../list_of_gsalink_buildings.xlsx", sheet=2) %>%
  dplyr::mutate(`name`=substr(`spark_entry`, 12, nchar(`spark_entry`))) %>%
    dplyr::mutate(`Building_Number`=substr(`spark_entry`, 1, 8)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(`idloc`=regexpr(`Building_Number`, `name`, fixed=TRUE)[1]) %>%
    dplyr::mutate(`name`=ifelse(`idloc`!=-1, substr(`name`, 1, idloc - 2), `name`)) %>%
    dplyr::mutate(`idloc`=regexpr(" EL.Total", `name`, fixed=TRUE)[1]) %>%
    dplyr::mutate(`name`=ifelse(`idloc`!=-1, substr(`name`, 1, idloc - 1), `name`)) %>%
    dplyr::filter(`status`=="downloaded") %>%
    dplyr::mutate(`name`=ifelse(`name`=="", NA, `name`)) %>%
    dplyr::select(-`idloc`) %>%
    dplyr::left_join(gsalink_old, by="Building_Number") %>%
    dplyr::left_join(euas, by="Building_Number") %>%
    dplyr::mutate(`name_last`=ifelse(is.na(`name`), `name_first_55`, `name`)) %>%
    dplyr::mutate(`name_last`=ifelse(is.na(`name_last`), `name_euas`, `name_last`)) %>%
    readr::write_csv("../downloaded_building_name.csv")

## db.interface::main_db_build()

euas_area = db.interface::read_table_from_db(dbname="all", tablename="EUAS_area_latest", cols=c("Building_Number", "Gross_Sq.Ft")) %>%
  tibble::as_data_frame() %>%
  {.}

ion_area_download =
  readr::read_csv("../area_from_ion.csv") %>%
  {.}

euas_database_of_buildings = readr::read_csv("../../../input/FY/static info/euas_database_of_buildings_cmu.csv") %>%
  dplyr::select(`Location Facility Code`, `Building GSF`) %>%
  dplyr::distinct(`Location Facility Code`, `Building GSF`) %>%
  dplyr::rename(`Building_Number`=`Location Facility Code`, `Gross_Sq.Ft`=`Building GSF`) %>%
  {.}

area_the_rest =
  readr::read_csv("../gsalink_building_area.csv") %>%
  dplyr::filter(is.na(`Gross_Sq.Ft`)) %>%
  dplyr::select(-`Gross_Sq.Ft`) %>%
  dplyr::left_join(euas_database_of_buildings) %>%
  {.}

head(area_the_rest)

area_info = euas_area %>%
  dplyr::bind_rows(ion_area_download) %>%
  dplyr::bind_rows(area_the_rest) %>%
  {.}

## get building sqft
## 23 buildings without sqft as they are not in EUAS
df_area = readxl::read_excel("../list_of_gsalink_buildings.xlsx", sheet=2) %>%
  dplyr::mutate(`Building_Number`=substr(`spark_entry`, 1, 8)) %>%
  dplyr::filter(`status`=="downloaded") %>%
  dplyr::select(`Building_Number`) %>%
  dplyr::left_join(area_info) %>%
  {.}

df_area %>%
  dplyr::rename(`building`=`Building_Number`, `GSF`=`Gross_Sq.Ft`) %>%
  readr::write_csv("../gsalink_building_area.csv")

## get energy data availability
energy_files = list.files(path="../gsalink_energy", pattern="*.html")
energy_df = data.frame(filename=energy_files)

energy_df <-
  energy_df %>%
  tibble::as_data_frame() %>%
  dplyr::mutate(`filename`=as.character(`filename`)) %>%
  dplyr::mutate(`Building_Number`=substr(filename, 1, 8),
                `type`=substr(filename, 10, nchar(filename) - 5),
                `exist`=1) %>%
  dplyr::select(-`filename`) %>%
  tidyr::spread(`type`, `exist`, fill=0) %>%
  dplyr::mutate(`has electric`=ifelse(`kWh Del Int` + `kWh del-rec Int` + `kWh Rec Int`>0, 1, 0),
                `has gas`=`Natural Gas Vol Int`) %>%
  {.}

readxl::read_excel("../list_of_gsalink_buildings.xlsx", sheet=2) %>%
  dplyr::mutate(`Building_Number`=substr(`spark_entry`, 1, 8)) %>%
  dplyr::filter(`status`=="downloaded") %>%
  dplyr::select(`Building_Number`) %>%
  dplyr::left_join(energy_df) %>%
  dplyr::mutate_all(function (x) ifelse(is.na(x), 0, x)) %>%
  readr::write_csv("../gsalink_energy_file_availability.csv")

head(energy_df)

