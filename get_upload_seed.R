library("DBI")
library("reshape2")
library("ggplot2")
library("dplyr")
library("readr")
library("readxl")
library("RColorBrewer")
library("lubridate")

con <- dbConnect(RSQLite::SQLite(), "csv_FY/db/all.db")
alltables = dbListTables(con)

df1 = dbGetQuery(con, 'SELECT * FROM EUAS_monthly' ) %>%
    as_data_frame() %>%
    dplyr::select("Building_Number", "Electric_(kBtu)", "Gas_(kBtu)",
                  "year", "month", "Electricity_(Cost)",
                  "Gas_(Cost)") %>%
    {.}

df2 = readr::read_csv("seeddb/gsa_energy/sourcetable/gsa_static_import_singlebuilding.csv") %>%
    as_data_frame() %>%
    dplyr::rename(`Building_Number`=`|Building_Number`,
                  `Street_Address`=`|Street_Address`) %>%
    dplyr::select(`Building_Number`, `Street_Address`) %>%
    {.}

energy_use = df1 %>%
    dplyr::select("Building_Number", "year", "month",
                  "Electric_(kBtu)", "Gas_(kBtu)") %>%
    melt(id.vars=c("Building_Number", "year", "month"),
         variable.name="Meter Type", value.name="Usage/Quantity") %>%
    dplyr::mutate_at(vars(`Meter Type`), recode,
                     "Electric_(kBtu)" = "Electricity",
                     "Gas_(kBtu)" = "Natural Gas") %>%
    {.}
unique(energy_use$"Meter Type")

energy_cost = df1 %>%
    dplyr::select("Building_Number", "year", "month",
                  "Electricity_(Cost)", "Gas_(Cost)") %>%
    melt(id.vars=c("Building_Number", "year", "month"), variable.name="Meter Type", value.name="Cost ($)") %>%
    dplyr::mutate_at(vars(`Meter Type`), recode,
                     "Electricity_(Cost)" = "Electricity",
                     "Gas_(Cost)" = "Natural Gas") %>%
    {.}
unique(energy_cost$"Meter Type")

energy_use %>%
    dplyr::left_join(energy_cost, by=c("Building_Number", "year", "month", "Meter Type")) %>%
    dplyr::left_join(df2, by=c("Building_Number")) %>%
    dplyr::mutate(`year`=as.character(`year`)) %>%
    dplyr::mutate(`month`=as.character(`month`)) %>%
    dplyr::mutate(`Start Date`=as.Date(paste(`year`, `month`, "1", sep = "-" ), format = "%Y-%m-%d")) %>%
    dplyr::mutate(`End Date`=as.Date(paste(`year`, `month`, days_in_month(`Start Date`), sep = "-" ), format = "%Y-%m-%d")) %>%
    dplyr::rename(`Property Name`=`Building_Number`,
                  `Street Address`=`Street_Address`) %>%
    dplyr::mutate(`Usage Units`="kBtu (thousand Btu)") %>%
    dplyr::select(-`year`, -`month`) %>%
    dplyr::mutate(`Custom ID`=NA) %>%
    dplyr::mutate(`Custom Meter ID`=NA) %>%
    dplyr::select(`Street Address`, `Property Name`, `Custom ID`,
                  `Custom Meter ID`, `Meter Type`, `Start Date`,
                  `End Date`, `Usage/Quantity`, `Usage Units`,
                  `Cost ($)`) %>%
    write_csv("seeddb/gsa_energy/upload.csv")
