library("DBI")
library("dplyr")
library("tidyr")
library("readxl")
library("readr")

setwd("/home/yujiex/Dropbox/gsa_2017/csv_FY/db")

con <- dbConnect(RSQLite::SQLite(), "all.db")

df_action = dbGetQuery(con, "SELECT * FROM EUAS_ecm") %>%
    as_data_frame() %>%
    tidyr::drop_na("Substantial_Completion_Date") %>%
    {.}

setwd("/home/yujiex/Dropbox/gsa_2017/")

df2 = readxl::read_excel("input/FY/ECM info/Light-Touch M&V - ARRA Targets to Actuals and Commissioning Details(1).xlsx", sheet=2, skip=3) %>%
    as_data_frame() %>%
    ## dplyr::select(`Building ID`, `Total ARRA Obligation`, `ARRA Substantial Completion Date`, `Advanced Metering`, `Building Envelope`, `Building Tune Up`, `HVAC`, `Indoor Environmental Quality`, `Lighting`, `Renewable Energy`, `Water`) %>%
    dplyr::select(`Building ID`, `Total ARRA Obligation`) %>%
    dplyr::filter(!(`Building ID` %in% c("CO0039ZZ", "IL0235FC",
                                         "MA0153ZZ", "WA0045ZZ"))) %>%
    rename(`Building_Number`=`Building ID`,
           `Cost`=`Total ARRA Obligation`) %>%
    {.}

names(df2)

df3 = readr::read_csv("input/FY/ECM info/dup_LightTouch_manualSelect.csv") %>%
    as_data_frame() %>%
    dplyr::select(`Building ID`, `Total ARRA Obligation`) %>%
    rename(`Building_Number`=`Building ID`,
           `Cost`=`Total ARRA Obligation`) %>%
    {.}

df_cost = rbind(df2, df3) %>%
    dplyr::mutate(`source_cost`="Light Touch") %>%
    {.}

df_ac = df_action %>%
    left_join(df_cost, by="Building_Number") %>%
    tidyr::drop_na("Cost") %>%
    ## readr::write_csv("temp/joined_cost_lightTouch.csv")
    dplyr::mutate(`Bundled budget`=TRUE) %>%
    {.}

df1 = readxl::read_excel("input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx", sheet=2, skip=3) %>%
    as_data_frame() %>%
    rename(`Building_Number`=`Building ID`,
           `Substantial_Completion_Date`=`Substantial Completion Date`) %>%
    dplyr::select(`Building_Number`, `Substantial_Completion_Date`, `System Cost ($)`) %>%
    dplyr::filter(!is.na(`System Cost ($)`), !is.na(`Substantial_Completion_Date`)) %>%
    dplyr::group_by(`Building_Number`, `System Cost ($)`) %>%
    slice(1) %>%
    dplyr::mutate(ECM_combined_header = "Indoor_Lighting") %>%
    dplyr::mutate(detail_level_ECM = "Indoor") %>%
    dplyr::mutate(high_level_ECM = "Lighting") %>%
    dplyr::mutate(source_detail = "LED projects in gBUILD with SCDs 6-15-2017_sheet2") %>%
    dplyr::mutate(source_highlevel = "LED projects in gBUILD with SCDs 6-15-2017_sheet2") %>%
    rename(`Cost`=`System Cost ($)`) %>%
    dplyr::mutate(`source_cost`="LED projects in gBUILD with SCDs 6-15-2017_sheet2") %>%
    {.}

df2 = readxl::read_excel("input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx", sheet=3, skip=3) %>%
    as_data_frame() %>%
    rename(`Building_Number`=`Building ID`,
           `Substantial_Completion_Date`=`Substantial Completion Date`) %>%
    dplyr::select(`Building_Number`, `Substantial_Completion_Date`, `System Cost ($)`) %>%
    dplyr::filter(!is.na(`System Cost ($)`), !is.na(`Substantial_Completion_Date`)) %>%
    dplyr::group_by(`Building_Number`, `System Cost ($)`) %>%
    slice(1) %>%
    dplyr::mutate(ECM_combined_header = "Outdoor_Lighting") %>%
    dplyr::mutate(detail_level_ECM = "Outdoor") %>%
    dplyr::mutate(high_level_ECM = "Lighting") %>%
    dplyr::mutate(source_highlevel = "LED projects in gBUILD with SCDs 6-15-2017_sheet3") %>%
    dplyr::mutate(source_detail = "LED projects in gBUILD with SCDs 6-15-2017_sheet3") %>%
    rename(`Cost`=`System Cost ($)`) %>%
    dplyr::mutate(`source_cost`="LED projects in gBUILD with SCDs 6-15-2017_sheet3") %>%
    {.}

df_led = rbind(df1, df2) %>%
    dplyr::mutate(`Substantial_Completion_Date`=as.character(`Substantial_Completion_Date`)) %>%
    dplyr::mutate(`Bundled budget`=FALSE) %>%
    {.}

## df_ac = df_ac %>%
##     dplyr::mutate(`Substantial_Completion_Date` = as.POSIXct(`Substantial_Completion_Date`, format="%Y-%m-%d")) %>%
##     {.}

dplyr::bind_rows(df_ac, df_led) %>%
    dplyr::select(`Building_Number`, 
                  `high_level_ECM`,`detail_level_ECM`, 
                  `Substantial_Completion_Date`, `Cost`,
                  `Bundled budget`, `source_highlevel`,
                  `source_detail`, `source_cost`) %>%
    dplyr::arrange(`Building_Number`, `high_level_ECM`,
                   `detail_level_ECM`) %>%
    dplyr::rename(`SCD`=`Substantial_Completion_Date`) %>%
    readr::write_csv("csv_FY/master_table/ecm_date_cost.csv")
