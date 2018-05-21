library("dplyr")
library("DBI")
library("RSQLite")
library("ggplot2")
library("readr")
library("readxl")

con <- dbConnect(RSQLite::SQLite(), "csv_FY/db/all.db")

alltables = dbListTables(con)

querystr = paste0("SELECT DISTINCT [Building_Number], [Region_No.], year, month, [Gross_Sq.Ft], [eui_elec], [eui_gas], [Electric_(kBtu)], [Gas_(kBtu)], [Electricity_(Cost)], [Gas_(Cost)] FROM EUAS_monthly")
dfEnergy = dbGetQuery(con,  querystr) %>%
  as_data_frame() %>%
  dplyr::mutate(`Electric_cost_per_sqft`=`Electricity_(Cost)` / `Gross_Sq.Ft`) %>%
  dplyr::mutate(`Gas_cost_per_sqft`=`Gas_(Cost)` / `Gross_Sq.Ft`) %>%
  {.}

## querystr = paste0("SELECT * FROM EUAS_type")
## dbGetQuery(con,  querystr) %>%
##   as_data_frame() %>%
##   head()

dfType = readxl::read_excel("input/FY/static info/euas database of buildings cmu.xlsx",sheet = 1) %>%
  as_data_frame() %>%
  na.omit() %>%
  {.}

dfEnergy %>%
  dplyr::left_join(dfType, by="Building_Number") %>%
  na.omit() %>%
  dplyr::filter(`Gross_Sq.Ft` > 0) %>%
  dplyr::mutate(`Region_No.`=as.integer(`Region_No.`)) %>%
  dplyr::group_by(`Region_No.`, `GSA Property Type`, `year`) %>%
  summarise(`Total Electricity kBtu `=sum(`Electric_(kBtu)`), `Total_Gas kBtu`=sum(`Gas_(kBtu)`),
            `Total Electricity Cost`=sum(`Electricity_(Cost)`), `Total Gas Cost`=sum(`Gas_(Cost)`),
            `Total Electricity kBtu / sqft `=sum(`eui_elec`), `Total_Gas kBtu / sqft`=sum(`eui_gas`),
            `Total Electricity Cost / sqft`=sum(`Electric_cost_per_sqft`), `Total Gas Cost / sqft`=sum(`Gas_cost_per_sqft`)) %>%
  readr::write_csv("csv_FY/region9/energyAndCost.csv")


querystr = paste0("SELECT DISTINCT [Building_Number], [Region_No.], [Gross_Sq.Ft], Fiscal_Year, Fiscal_Month, [Electric_(kBtu)], [Gas_(kBtu)], [Electricity_(Cost)], [Gas_(Cost)] FROM EUAS_monthly")
dbGetQuery(con,  querystr) %>%
  as_data_frame() %>%
  dplyr::mutate(`Region_No.`=as.integer(`Region_No.`)) %>%
  dplyr::group_by(`Region_No.`, `Fiscal_Year`, `Building_Number`) %>%
  summarise(`Total Electricity kBtu`=sum(`Electric_(kBtu)`), `Total Gas kBtu`=sum(`Gas_(kBtu)`),
            `Total Electricity Cost`=sum(`Electricity_(Cost)`), `Total Gas Cost`=sum(`Gas_(Cost)`),
            `Total Sq.Ft`=mean(`Gross_Sq.Ft`)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(`Region_No.`, `Fiscal_Year`) %>%
  summarise(`Total Electricity kBtu`=sum(`Total Electricity kBtu`), `Total Gas kBtu`=sum(`Total Gas kBtu`),
            `Total Electricity Cost`=sum(`Total Electricity Cost`), `Total Gas Cost`=sum(`Total Gas Cost`),
            `Total Sq.Ft`=sum(`Total Sq.Ft`)) %>%
  dplyr::mutate(`Electricity kBtu / sqft` = `Total Electricity kBtu` / `Total Sq.Ft`,
                `Gas kBtu / sqft` = `Total Gas kBtu` / `Total Sq.Ft`,
                `Electricity Cost / sqft` = `Total Electricity Cost` / `Total Sq.Ft`,
                `Gas Cost / sqft` = `Total Gas Cost` / `Total Sq.Ft`) %>%
  readr::write_csv("csv_FY/energyKbtuCostByRegionYear.csv")
