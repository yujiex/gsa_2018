library(dplyr)

devtools::load_all("db.interface")

devtools::load_all("get.noaa.weather")

devtools::load_all("lean.analysis")

devtools::load_all("summarise.and.plot")

summarise.and.plot::gsa_national_median_energy_gsf()

summarise.and.plot::national_overview(category=c("I", "A"), year=2017)

lat_lon_df = db.interface::get_lat_lon_df()

devtools::load_all("db.interface")
db.interface::main_db_build()

ecm_program = db.interface::read_table_from_db(dbname="all", tablename="EUAS_ecm_program") %>%
  {.}

head(ecm_program)

unique(ecm_program$`ECM_program`)

dfregion = 
  db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_monthly", cols = c("Building_Number", "Region_No.")) %>%
  distinct(`Region_No.`, `Building_Number`) %>%
  {.}

db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_ecm", cols = c("Building_Number", "high_level_ECM", "Substantial_Completion_Date")) %>%
  dplyr::filter(!is.na(`Substantial_Completion_Date`)) %>%
    dplyr::left_join(dfregion, by="Building_Number") %>%
    dplyr::filter(`Region_No.`==3) %>%
    dplyr::distinct(`Building_Number`)

db.interface::read_table_from_db(dbname="all", tablename="EUAS_monthly", cols=c("Building_Number", "Electric_(kBtu)", "Fiscal_Year", "Fiscal_Month")) %>%
  dplyr::filter(`Building_Number`=="ND0018ZZ",
                `Fiscal_Year`=="2017") %>%
  print()

study_set = get_buildings(year=2017, category=c("A", "I"))

df <- db.interface::read_table_from_db("all", tablename="eui_by_fy_tag") %>%
  dplyr::filter(`Building_Number` %in% study_set) %>%
    dplyr::filter(`Fiscal_Year`==2017) %>%
    {.}
median(df$`eui_total`)

df %>%
  dplyr::filter(`Fiscal_Year`==2017,
                `Building_Number` %in% study_set) %>%
  dplyr::group_by(`Building_Number`, `Building_Type`) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(`Region_No.`, `Building_Type`) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(`Region_No.`==11)

df = db.interface::read_table_from_db(dbname="all", tablename="actual_vs_weather_normalized")
study_set = get_buildings(year=2017, category=c("A", "I"))
print(head(df))
df <- df %>%
  dplyr::filter(`Building_Number` %in% study_set) %>%
  {.}

df %>%
  readr::write_csv("~/Dropbox/gsa_2017/csv_FY/db_build_temp_csv/actual_vs_weather_normalized_671.csv")

df = readr::read_csv("temp/cmp_normalized_actual_671building.csv")

head(df)
thresh = 0.05

df_regional <-
  df %>%
  dplyr::mutate(`percent_diff`=(`Site Energy Use (kBtu)` - `EUAS Site Energy (kBtu)`) / `EUAS Site Energy (kBtu)`) %>%
  dplyr::mutate(`percent_diff`=ifelse(is.na(`percent_diff`), 0, `percent_diff`)) %>%
  dplyr::select(`Building_Number`, `Fiscal_Year`, `Gross_Sq.Ft`, `Region_No.`, `EUAS Site Energy (kBtu)`, `normalized w/ credits only`, `normalized w/debit + credits`, `percent_diff`) %>%
  dplyr::mutate(`normalized w/ credits only` = ifelse(`percent_diff` > thresh, `EUAS Site Energy (kBtu)`, `normalized w/ credits only`)) %>%
  dplyr::mutate(`normalized w/debit + credits` = ifelse(`percent_diff` > thresh, `EUAS Site Energy (kBtu)`, `normalized w/debit + credits`)) %>%
  dplyr::select(-`percent_diff`) %>%
  tidyr::gather(`type`, `kbtu`, `EUAS Site Energy (kBtu)`:`normalized w/debit + credits`) %>%
  na.omit() %>%
  dplyr::group_by(`Fiscal_Year`, `Region_No.`, `type`) %>%
  dplyr::summarise(kbtu = sum(kbtu), `Gross_Sq.Ft`=sum(`Gross_Sq.Ft`)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(eui = `kbtu`/`Gross_Sq.Ft`) %>%
  {.}

df_regional %>%
  dplyr::mutate(`Region_No.`=factor(`Region_No.`, levels=as.character(1:11)),
                `Fiscal_Year`=factor(as.integer(`Fiscal_Year`))) %>%
  dplyr::select(-`kbtu`, -`Gross_Sq.Ft`) %>%
  tidyr::spread(`type`, `eui`) %>%
  readr::write_csv("~/Dropbox/gsa_2017/csv_FY/db_build_temp_csv/cmp_table_normalize_1617_regional.csv")

df_regional %>%
  dplyr::filter(`Fiscal_Year` %in% c(2015, 2017)) %>%
  dplyr::mutate(`Region_No.`=factor(`Region_No.`, levels=as.character(1:11)),
                `Fiscal_Year`=factor(as.integer(`Fiscal_Year`))) %>%
  ## use 2015 EUAS as baseline for all three facets
  dplyr::group_by(`Region_No.`, `Fiscal_Year`) %>%
  dplyr::mutate(`eui`=ifelse(`Fiscal_Year`==2015, first(`eui`), `eui`)) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(x=Fiscal_Year, y=`eui`, fill=`type`)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::scale_fill_brewer(palette="Set2") +
  ggplot2::facet_wrap(.~`Region_No.`, ncol=11) +
  ## ggplot2::facet_wrap(`type`~`Region_No.`, ncol=11) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
                 legend.position = "bottom")
ggplot2::ggsave("~/Dropbox/gsa_2017/plot_temp/normalized_eui_regional.png", width=10, height=5)

## plot for just region 5
df_regional %>%
  dplyr::filter(`Fiscal_Year` %in% c(2015, 2017), `Region_No.`==5) %>%
  dplyr::mutate(`Region_No.`=factor(`Region_No.`, levels=as.character(1:11)),
                `Fiscal_Year`=factor(as.integer(`Fiscal_Year`))) %>%
  dplyr::group_by(`Region_No.`, `Fiscal_Year`) %>%
  dplyr::mutate(`eui`=ifelse(`Fiscal_Year`==2015, first(`eui`), `eui`)) %>%
  dplyr::ungroup() %>%
  ## dplyr::mutate(`type`=recode(`type`, "actual"="EUAS", "normalized_only_good"="normalized w/ credits only",
  ##                             "normalized_fill_na"="normalized w/debit + credit")) %>%
  ## dplyr::mutate(`type`=factor(`type`, levels=c("EUAS", "normalized w/ credits only", "normalized w/debit + credit"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=Fiscal_Year, y=`eui`, fill=`type`, label=sprintf("%.0f", `eui`))) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::scale_fill_brewer(palette="Set2") +
  ggplot2::geom_text(vjust=-1) +
  ggplot2::xlab("Fiscal Year") +
  ggplot2::ylab("GSF weighted average EUI") +
  ggplot2::expand_limits(y=85) +
  ggplot2::facet_wrap(.~`type`, ncol=11) +
  ## ggplot2::facet_wrap(`type`~`Region_No.`, ncol=11) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
                 legend.position = "bottom", text = ggplot2::element_text(size=10))
ggplot2::ggsave("~/Dropbox/gsa_2017/plot_temp/normalized_eui_regional_5.png", width=5, height=5)

## national
df_national <- df %>%
  dplyr::select(`Building_Number`, `Fiscal_Year`, `Gross_Sq.Ft`, `actual`, `normalized_fill_na`, `normalized_only_good`) %>%
  tidyr::gather(`type`, `kbtu`, `actual`:`normalized_only_good`) %>%
  na.omit() %>%
  dplyr::group_by(`Fiscal_Year`, `type`) %>%
  dplyr::summarise(kbtu = sum(kbtu), `Gross_Sq.Ft`=sum(`Gross_Sq.Ft`)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(eui = `kbtu`/`Gross_Sq.Ft`) %>%
  {.}

df_national %>%
  dplyr::mutate(`Fiscal_Year`=factor(as.integer(`Fiscal_Year`))) %>%
  dplyr::select(-`kbtu`, -`Gross_Sq.Ft`) %>%
  tidyr::spread(`type`, `eui`) %>%
  readr::write_csv("~/Dropbox/gsa_2017/csv_FY/db_build_temp_csv/cmp_table_normalize_1617_national.csv")

df_national %>%
  dplyr::mutate(`Fiscal_Year`=factor(as.integer(`Fiscal_Year`))) %>%
  ggplot2::ggplot(ggplot2::aes(x=Fiscal_Year, y=`eui`, fill=`type`)) +
  ggplot2::geom_bar(stat="identity", position="dodge") +
  ggplot2::scale_fill_brewer(palette="Set2") +
  ## ggplot2::facet_wrap(.~`type`) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
                 legend.position = "bottom")
ggplot2::ggsave("~/Dropbox/gsa_2017/plot_temp/normalized_eui_national.png", width=10, height=8)

df = db.interface::read_table_from_db(dbname="all", tablename="actual_vs_weather_normalized")
study_set = get_buildings(year=2017, category=c("A", "I"))
df %>%
  dplyr::filter(`Building_Number` %in% study_set) %>%

db.interface::dump_static()

db.interface::get_all_tables(dbname="all")

db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_monthly", cols=c("Building_Number", "Fiscal_Year", "Fiscal_Month", "datacenter_sqft", "lab_sqft", "Gross_Sq.Ft")) %>%
  dplyr::filter(`datacenter_sqft` + `lab_sqft` > `Gross_Sq.Ft`) %>%
    readr::write_csv("temp/trouble_area.csv")

db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_monthly") %>%
  dplyr::select(`Building_Number`, `datacenter_sqft`, `lab_sqft`, `Region_No.`, `Gross_Sq.Ft`) %>%
  dplyr::filter(`Region_No.`==5) %>%
  dplyr::filter(`datacenter_sqft` + `lab_sqft` > 0) %>%
  dplyr::group_by_all() %>%
  dplyr::slice(1) %>%
  print()

## count number of buildings in each region
db.interface::read_table_from_db(dbname="all", tablename="eui_by_fy_tag", cols=c("Building_Number", "Fiscal_Year", "Region_No.", "eui_elec", "State", "Cat")) %>%
  dplyr::filter(`Fiscal_Year`==2017) %>%
    dplyr::filter(`Cat` %in% c("A", "I")) %>%
    dplyr::filter(`eui_elec` > 0) %>%
    dplyr::filter(`Region_No.`==2) %>%
    dplyr::group_by(`Region_No.`, `State`) %>%
    dplyr::summarise(n()) %>%
    print()

## plot histogram of region eui
db.interface::read_table_from_db(dbname="all", tablename="eui_by_fy_tag", cols=c("Building_Number", "Fiscal_Year", "Region_No.", "eui_total", "State", "Cat")) %>%
  dplyr::filter(`Fiscal_Year`==2017) %>%
    ## dplyr::filter(`Cat` %in% c("A", "I")) %>%
    ## dplyr::filter(`eui_elec` > 0) %>%
    dplyr::filter(`Region_No.`==5) %>%
    ggplot2::ggplot(ggplot2::aes(x=`eui_total`)) +
    ggplot2::geom_histogram()


readr::read_csv("input/FY/static info/GSA National Energy Reduction Target Workbook FY17_sheet3.csv", skip = 3) %>%
  dplyr::select(-one_of(paste0("X", 1:78))) %>%
    dplyr::select(1:38) %>%
    head(-1) %>%
    dplyr::rename(`Standardized DD`=`Standardized HDD`,
                  `Standardized DD_1`=`Standardized CDD`,
                  `Degree Day Multiplier`=`Heating Degree Day Multiplier`,
                  `Degree Day Multiplier_1`=`Cooling Degree Day Multiplier`,
                  ) %>%
    readr::write_csv("temp/model3.csv")

readr::read_csv("input/FY/static info/GSA National Energy Reduction Target Workbook FY17_sheet7.csv") %>%
  tibble::as_data_frame() %>%
  dplyr::select(-one_of(paste0("X", 1:14))) %>%
  dplyr::select(1:7) %>%
  dplyr::rename(`Building_Number`=`Location Code`) %>%
  na.omit() %>%
  readr::write_csv("temp/sheet6.csv")

readr::read_csv("input/FY/static info/GSA National Energy Reduction Target Workbook FY17_sheet10.csv") %>%
  tibble::as_data_frame() %>%
    dplyr::rename(`Building_Number`=`Building #`) %>%
    dplyr::group_by(`Building_Number`) %>%
    dplyr::filter(n() > 1) %>%
    head()

db.interface::get_all_tables(dbname="interval_ion")

dfregion = db.interface::read_table_from_db(dbname="all", tablename="EUAS_monthly",
                                            cols=c("Building_Number", "Region_No.")) %>%
  dplyr::group_by_all() %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

db.interface::read_table_from_db(dbname="interval_ion", tablename="electric_id") %>%
  dplyr::rename(`Building_Number`=`id`) %>%
  dplyr::left_join(dfregion, by="Building_Number") %>%
  readr::write_csv("~/Dropbox/gsa_2017/csv_FY/interval_elec_region.csv")

db.interface::view_head_of_table(dbname = "all", tablename = "EUAS_monthly")[,5:10]

db.interface::view_names_of_table(dbname = "other_input", tablename = "euas_database_of_buildings_cmu")

db.interface::view_names_of_table(dbname = "all", tablename = "EUAS_monthly")

db.interface::view_names_of_table(dbname = "other_input", tablename = "Entire_GSA_Building_Portfolio_input")

db.interface::view_names_of_table(dbname = "all", tablename = "eui_by_fy_tag")

db.interface::read_table_from_db(dbname = "all", tablename = "eui_by_fy_tag") %>%
  dplyr::filter(`Gross_Sq.Ft` != 0) %>%
  dplyr::filter(`eui_elec` != 0) %>%
  dplyr::filter(`Cat` %in% c("A", "I")) %>%
  ## dplyr::filter(`Fiscal_Year` == 2017) %>%
  dplyr::filter(`Fiscal_Year` == 2017, `Building_Type`=="Office") %>%
  dplyr::group_by(`Region_No.`) %>%
  dplyr::summarise(`min_eui`=min(`eui_total`), `max_eui`=max(`eui_total`), `Building_Number`=first(`Building_Number`)) %>%
  dplyr::ungroup() %>%
  ## dplyr::select(`Building_Number`, `Fiscal_Year`, `eui_total`, `Region_No.`) %>%
  print()
    ## print(n=581)

devtools::load_all("db.interface")
db.interface::view_names_of_table(dbname = "all", tablename = "EUAS_monthly")

db.interface::view_head_of_table(dbname = "all", tablename = "EUAS_latlng_2")

dbname="all"
tablename="EUAS_monthly"
colname="state_abbr"
## db.interface::view_names_of_table(dbname = dbname, tablename = tablename)
NA %in% get_unique_value_column(dbname = dbname, tablename = tablename, col=colname)

db.interface::view_names_of_table(dbname = "all", tablename = "EUAS_ecm")

read_normalize_table_from_pm <- function(filename, sheetid, year) {
  df = readxl::read_excel(sprintf("~/Dropbox/gsa_2017/input/FY/weather_normalized_energy/%s.xlsx", filename), sheet=sheetid, skip=5)
  dfnew <- df %>%
    dplyr::select(`US Agency Designated Covered Facility ID`, `Weather Normalized Site Energy Use (kBtu)`, `Site Energy Use (kBtu)`) %>%
    dplyr::mutate(`Weather Normalized Site Energy Use (kBtu)`=as.numeric(`Weather Normalized Site Energy Use (kBtu)`)) %>%
    dplyr::mutate(`Site Energy Use (kBtu)`=as.numeric(`Site Energy Use (kBtu)`)) %>%
    na.omit() %>%
    dplyr::rename(`Building_Number`=`US Agency Designated Covered Facility ID`) %>%
    dplyr::group_by(`Building_Number`) %>%
    dplyr::summarise(`Weather Normalized Site Energy Use (kBtu)` = sum(`Weather Normalized Site Energy Use (kBtu)`),
                      `Site Energy Use (kBtu)`=sum(`Site Energy Use (kBtu)`)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(`Fiscal_Year`=year) %>%
    {.}
  return(dfnew)
}

df17 = read_normalize_table_from_pm(filename="FY17WeatherNorm_CMU_BldgsOnly", sheetid=2, year=2017)
df16 = read_normalize_table_from_pm(filename="FY17WeatherNorm_CMU_BldgsOnly", sheetid=3, year=2016)
df15 = read_normalize_table_from_pm(filename="CMU_ESTAR DATA_FY12_FY15", sheetid=6, year=2015)
df14 = read_normalize_table_from_pm(filename="CMU_ESTAR DATA_FY12_FY15", sheetid=5, year=2014)
df13 = read_normalize_table_from_pm(filename="CMU_ESTAR DATA_FY12_FY15", sheetid=4, year=2013)

normalized = df13 %>%
  dplyr::bind_rows(df14) %>%
  dplyr::bind_rows(df15) %>%
  dplyr::bind_rows(df16) %>%
  dplyr::bind_rows(df17) %>%
  {.}
df = read_table_from_db(dbname="all", tablename="eui_by_fy_tag") %>%
  dplyr::select(`Building_Number`, `Fiscal_Year`, `Total_(kBtu)`, `Gross_Sq.Ft`, `Region_No.`) %>%
  dplyr::left_join(normalized, by=c("Building_Number", "Fiscal_Year")) %>%
  {.}
study_set = get_buildings(year=2017, category=c("A", "I"))
df <- df %>%
  dplyr::filter(`Fiscal_Year` %in% 2013:2017,
                `Building_Number` %in% study_set) %>%
  dplyr::arrange(`Fiscal_Year`, `Building_Number`) %>%
  dplyr::rename(`EUAS Site Energy (kBtu)`=`Total_(kBtu)`,
                `PM Weather Normalized Site Energy Use (kBtu)`=`Weather Normalized Site Energy Use (kBtu)`) %>%
  dplyr::mutate(`Debit`=pmax(0, `PM Weather Normalized Site Energy Use (kBtu)` - `Site Energy Use (kBtu)`)) %>%
  dplyr::mutate(`Credit`=pmax(0, `Site Energy Use (kBtu)` - `PM Weather Normalized Site Energy Use (kBtu)`)) %>%
  tidyr::replace_na(list(`Debit`=0, `Credit`=0)) %>%
  dplyr::mutate(`normalized w/ credits only`=`EUAS Site Energy (kBtu)` - `Credit`) %>%
  dplyr::mutate(`normalized w/debit + credits`=`EUAS Site Energy (kBtu)` - `Credit` + `Debit`) %>%
  {.}
print(head(df))

df %>%
  readr::write_csv("temp/cmp_normalized_actual_671building.csv")

df = readr::read_csv("temp/cmp_normalized_actual_671building.csv")

## df <- df %>%
##     dplyr::filter(`Fiscal_Year` %in% 2013:2017) %>%
##     dplyr::arrange(`Fiscal_Year`, `Building_Number`) %>%
##     dplyr::rename(`actual`=`Total_(kBtu)`,
##                   `normalized`=`Weather Normalized Site Energy Use (kBtu)`) %>%
##     dplyr::mutate(`normalized_fill_na`=ifelse(is.na(`normalized`), `actual`, `normalized`)) %>%
##     dplyr::mutate(`normalized_only_good`=pmin(`actual`, `normalized_fill_na`)) %>%
##     {.}
##   print(head(df))

## print eui_total of a building
db.interface::read_table_from_db(dbname="all", tablename="eui_by_fy_tag", cols=c("Building_Number", "Fiscal_Year", "eui_total")) %>%
  dplyr::filter(`Building_Number`=="IN1703ZZ") %>%
    dplyr::arrange(desc(`Fiscal_Year`)) %>%
    print()

## print category and type of a building
building="UT0032ZZ"
db.interface::read_table_from_db(dbname="all", tablename="EUAS_monthly_with_type", cols=c("Building_Number", "Fiscal_Year", "Building_Type", "Cat", "Region_No.")) %>%
  dplyr::filter(`Building_Number`==building) %>%
    dplyr::arrange(desc(`Fiscal_Year`)) %>%
    print()

## print sqft of a building
building="UT0032ZZ"
db.interface::read_table_from_db(dbname="all", tablename="eui_by_fy_tag", cols=c("Building_Number", "Fiscal_Year", "Gross_Sq.Ft")) %>%
  dplyr::filter(`Building_Number`==building) %>%
    dplyr::arrange(desc(`Fiscal_Year`)) %>%
    print()

## print year built of a building
building="OK0063ZZ"
db.interface::read_table_from_db(dbname="other_input", tablename="Entire_GSA_Building_Portfolio_input", cols=c("Building Number", "Year Built")) %>%
  ## dplyr::filter(`Building Number`=="IL0302ZZ") %>%
    dplyr::filter(`Building_Number`==building) %>%
    print()

db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_monthly", cols=c("Building_Number", "year", "month", "Electric_(kBtu)", "Gas_(kBtu)", "Oil_(kBtu)", "Steam_(kBtu)")) %>%
  dplyr::filter(`year` > 2014) %>%
  dplyr::filter(`Building_Number` == "DC0028ZZ") %>%
  dplyr::select(-`Building_Number`) %>%
  head()

db.interface::read_table_from_db(dbname = "all", tablename = "eui_by_fy_tag") %>%
  dplyr::filter(`Region_No.`=="9", `Gross_Sq.Ft`==0) %>%
    .$Building_Number

devtools::load_all("db.interface")
building = "NY7077ZZ"
result <- db.interface::get_lat_lon_df(building=building)
sprintf("%s,%s", result$latitude[[1]], result$longitude[[1]])
db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_latlng_2") %>%
  dplyr::filter(`Building_Number`==building) %>%
    print()

devtools::load_all("db.interface")
## add_quality_tag_energy()
## main_db_build()
get_ship_db()
## join_source_latlng()

db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_monthly") %>%
  dplyr::filter(Fiscal_Year == 2016) %>%
    readr::write_csv("temp.csv")

devtools::load_all("db.interface")
get_unique_value_column(dbname="all", tablename="EUAS_ecm", col="source_detail")
get_unique_value_column(dbname="all", tablename="EUAS_ecm", col="source_highlevel")

get_unique_value_column(dbname="all", tablename="EUAS_address", col="source")

get_unique_value_column(dbname="all", tablename="EUAS_city", col="Building_Number")

get_unique_value_column(dbname="all", tablename="EUAS_type_recode", col="data_source")

get_unique_value_column(dbname="all", tablename="EUAS_type_recode", col="Building_Type")

## ---------------------------------------------------------------------------------

devtools::load_all("summarise.and.plot")
get_filter_set(category=c("A", "I"), year=2017) %>%
  dplyr::left_join(db.interface::get_lat_lon_df()) %>%
  dplyr::select(-`index`) %>%
  readr::write_csv("csv_FY/powerMapData2017.csv")

devtools::load_all("summarise.and.plot")
get_filter_set(category=c("A", "I"), year=2017, region="9") %>%
  dplyr::group_by(`Building_Type`, `Cat`) %>%
  dplyr::summarise(`eui_median` = median(`eui_total`), cnt = n()) %>%
  {.}

devtools::load_all("summarise.and.plot")
get_filter_set(category=c("A", "I"), year=2017, region="9") %>%
  dplyr::filter(`Building_Type`=="Office") %>%
  dplyr::select(`Building_Number`, `eui_total`) %>%
  dplyr::arrange(desc(`eui_total`)) %>%
  head()

devtools::load_all("summarise.and.plot")
get_filter_set(category=c("A", "I"), year=2017) %>%
  readr::write_csv("csv_FY/eui_2017.csv")

devtools::load_all("summarise.and.plot")
get_filter_set(category=c("A", "I"), year=2017, region="9") %>%
  dplyr::group_by(`Building_Type`, `Cat`) %>%
  dplyr::summarise(`median_eui` = median(`eui_total`), `cnt`=n(), `maximum`=max(`eui_total`))

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## page 3 boxes start
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
devtools::load_all("summarise.and.plot")
df_reduction = get_filter_set(region="9") %>%
  ## df_reduction = get_filter_set(category=c("A", "I"), region="9") %>%
  dplyr::filter(`Fiscal_Year` %in% 2013:2017) %>%
  dplyr::select(`Fiscal_Year`, `Total_(kBtu)`, `Gross_Sq.Ft`) %>%
  dplyr::group_by(`Fiscal_Year`) %>%
  dplyr::summarise(`total_kbtu` = sum(`Total_(kBtu)`), `total_sqft`=sum(`Gross_Sq.Ft`), `building_count`=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`kbtu/sqft`=`total_kbtu` / `total_sqft`) %>%
  {.}
print("2 year reduction EUI")
print(df_reduction$`kbtu/sqft`[[5]] - df_reduction$`kbtu/sqft`[[3]])
print("5 year reduction EUI")
print(df_reduction$`kbtu/sqft`[[5]] - df_reduction$`kbtu/sqft`[[1]])

df_reduction %>%
  readr::write_csv("csv_FY/temp_query/region9kbtuPerSqft.csv")

devtools::load_all("summarise.and.plot")
df_reduction = get_filter_set() %>%
  ## df_reduction = get_filter_set(category=c("A", "I")) %>%
  dplyr::filter(`Fiscal_Year` %in% 2013:2017) %>%
  dplyr::select(`Fiscal_Year`, `Total_(kBtu)`, `Gross_Sq.Ft`) %>%
  dplyr::group_by(`Fiscal_Year`) %>%
  dplyr::summarise(`total_kbtu` = sum(`Total_(kBtu)`), `total_sqft`=sum(`Gross_Sq.Ft`), `building_count`=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`kbtu/sqft`=`total_kbtu` / `total_sqft`) %>%
  {.}
print("2 year reduction EUI")
print(df_reduction$`kbtu/sqft`[[5]] - df_reduction$`kbtu/sqft`[[3]])
print("5 year reduction EUI")
print(df_reduction$`kbtu/sqft`[[5]] - df_reduction$`kbtu/sqft`[[1]])

df_reduction %>%
  readr::write_csv("csv_FY/temp_query/nationalkbtuPerSqft.csv")

devtools::load_all("summarise.and.plot")
df_reduction = get_filter_set(category=c("A", "I"), region="9") %>%
  dplyr::filter(`Fiscal_Year` %in% 2013:2017) %>%
  dplyr::select(`Fiscal_Year`, `Total_(Cost)`, `Gross_Sq.Ft`) %>%
  dplyr::group_by(`Fiscal_Year`) %>%
  dplyr::summarise(`total_Cost` = sum(`Total_(Cost)`), `total_sqft`=sum(`Gross_Sq.Ft`), `building_count`=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`Cost/sqft`=`total_Cost` / `total_sqft`) %>%
  {.}
print("2 year reduction Cost")
print(df_reduction$`Total_(Cost)`[[5]] - df_reduction$`Total_(Cost)`[[3]])
print("5 year reduction Cost")
print(df_reduction$`Total_(Cost)`[[5]] - df_reduction$`Total_(Cost)`[[1]])

df_reduction %>%
  readr::write_csv("csv_FY/temp_query/region9CostPerSqft.csv")

devtools::load_all("summarise.and.plot")
## df_reduction = get_filter_set() %>%
df_reduction = get_filter_set(category=c("A", "I")) %>%
  dplyr::filter(`Fiscal_Year` %in% 2013:2017) %>%
  dplyr::select(`Fiscal_Year`, `Total_(Cost)`, `Gross_Sq.Ft`) %>%
  dplyr::group_by(`Fiscal_Year`) %>%
  dplyr::summarise(`total_Cost` = sum(`Total_(Cost)`), `total_sqft`=sum(`Gross_Sq.Ft`), `building_count`=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`Cost/sqft`=`total_Cost` / `total_sqft`) %>%
  {.}
print("2 year reduction Cost")
print(df_reduction$`total_Cost`[[5]] - df_reduction$`total_Cost`[[3]])
print("5 year reduction Cost")
print(df_reduction$`total_Cost`[[5]] - df_reduction$`total_Cost`[[1]])

df_reduction %>%
  readr::write_csv("csv_FY/temp_query/nationalCostPerSqft.csv")
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## page 3 boxes end
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

devtools::load_all("summarise.and.plot")
median_summary()

for (region in as.character(1:11)) {
  dollar_saving(category=c("I", "A"), year=2017, region=region, method="hybrid")
}

devtools::load_all("summarise.and.plot")
## dollar_saving(category=c("I", "A"), year=2017, region="9", method="hybrid", legendloc="bottom", topn=8, botn=7, ylimit=NULL, hjust=0)
## yrightLimits = list("1"=600000, "2"=1500000, "3"=300000, "4"=800000, "5"=300000, "6"=500000,
##                     "7"=250000, "8"=200000, "9"=900000, "10"=200000, "11"=10000000)
yrightLimits = list("1"=0, "2"=0, "3"=0, "4"=0, "5"=0,
                 "6"=0, "7"=0, "8"=0, "9"=30000, "10"=0,
                 "11"=0)
## for the orange plus green one
## yadjusts = list("1"=0, "2"=0, "3"=0, "4"=0, "5"=0,
##                 "6"=0, "7"=0, "8"=0, "9"=1000000, "10"=0,
##                 "11"=0)
yadjusts = list("1"=0, "2"=0, "3"=0, "4"=0, "5"=0,
                "6"=0, "7"=0, "8"=0, "9"=30000, "10"=0,
                "11"=0)
expLimits = list("1"=NULL, "2"=NULL, "3"=NULL, "4"=NULL, "5"=NULL,
                 "6"=NULL, "7"=NULL, "8"=NULL, "9"=500000, "10"=NULL,
                 "11"=NULL)
for (r in as.character(9:9)) {
  print("11111111111")
  print(r)
  dollar_saving(category=c("I", "A"), year=2017, region=r, method="hybrid", legendloc="bottom", topn=8, botn=7, yrightLimit=yrightLimits[[r]], yleftLimit=0, expLimit=, hjust=0.2, fontFamily="System Font", mod=1000, fontsize=10, yadjust=yadjusts[[r]], plotGreen=FALSE)
}

devtools::load_all("summarise.and.plot")
dollar_saving(category=c("I", "A"), year=2017, region="9", method="own")

df1 = readr::read_csv("csv_FY/dollar_saving_own_median_2017_region9.csv") %>%
  dplyr::rename(`eui_median_own`=`eui_median`,
                `Potential_Saving_own`=`Potential_Saving`,
                ) %>%
  {.}
df2 = readr::read_csv("csv_FY/dollar_saving_cbecs_median_2017_region9.csv") %>%
  dplyr::rename(`eui_median_cbecs`=`eui_median`,
                `Potential_Saving_cbecs`=`Potential_Saving`,
                ) %>%
  {.}

df1 %>%
  dplyr::full_join(df2, by=c("Building_Number", "eui_total")) %>%
  dplyr::arrange(desc(`Potential_Saving_cbecs`)) %>%
  readr::write_csv("csv_FY/cmp_single_building_dollar_saving.csv")

head(db.interface::get_lat_lon_df())

devtools::load_all("summarise.and.plot")
national_overview_over_years(category=c("I", "A"), years=c(2013, 2014, 2015, 2016, 2017), pal="Set3")

devtools::load_all("summarise.and.plot")
national_overview_facetRegion(category=c("I", "A"), years=c(2015, 2017))

devtools::load_all("summarise.and.plot")

summarise.and.plot::national_overview(category=c("I", "A"), year=2017)
## national_overview(category=c("I", "A"), year=2017, region="9")
## national_overview_over_years(category=c("I", "A"), years=c(2013, 2014, 2015, 2016, 2017), pal="Set3")
## national_overview_over_years(category=c("I", "A"), years=c(2013, 2014, 2015, 2016, 2017), region="9", pal="Set3")
## national_overview_over_years(category=c("I", "A"), pal="Set3")
## gb_agg_ratio(df, groupvar = "Fiscal_Year", numerator_var = c("Electric_(kBtu)", "Gas_(kBtu)", "Oil_(kBtu)", "Steam_(kBtu)", "Chilled_Water_(kBtu)"), denominator_var = "Gross_Sq.Ft", aggfun=sum, valuename="kBtu/sqft", varname="Fuel Type")

dftemp = db.interface::read_table_from_db(dbname = "all", tablename = "eui_by_fy_tag") %>%
  dplyr::filter(`Gross_Sq.Ft` != 0) %>%
  dplyr::filter(`eui_elec` != 0) %>%
  {.}
p = stackbar(df=dftemp, xcol="Fiscal_Year", fillcol="Cat", ylabel="Building Count", legendloc = "bottom", xlabel="Fiscal Year",orderByHeight=FALSE,
               tit="Building Category Count by Fiscal Year", verbose=FALSE, facetvar="Region_No.")
print(p)

## ---------------------------------------------------------------------------------

devtools::load_all("get.noaa.weather")
get.noaa.weather::compile_weather_isd_main(useSavedData=TRUE)

df = get_nearby_isd_stations(head(lat_lon_df), isd_data=isd_data, radius = 100,
                             limit=5, date_min = 20150101, date_max = 20151231)

df = get_nearby_isd_stations(head(lat_lon_df), isd_data=isd_data, radius = 100,
                             date_min = 20150101, date_max = 20151231)
head(df)

df = get_nearby_isd_stations(head(lat_lon_df), isd_data=isd_data, radius = 10,
                             date_min = 20150101, date_max = 20151231)
df


df = get_nearby_isd_stations(head(lat_lon_df), isd_data=isd_data, radius = 10,
                             date_min = 20160101, date_max = 20161231)
df

## ---------------------------------------------------------------------------------

devtools::load_all("db.interface")

data.frame(`Building_Number`=db.interface::get_buildings(year=2017, category = c("A", "I"))) %>%
  readr::write_csv("~/Dropbox/gsa_2017/temp/buildings_671.csv")

buildings = db.interface::get_buildings(region=9, buildingType="Office")

db.interface::get_buildings(region=9)

load(file="~/Dropbox/gsa_2017/get.noaa.weather/data/isdData.rda")

devtools::load_all("get.noaa.weather")

view_isd_stations_year(isd_data=isd_data, year=2015, latitude=33.560347, longitude=-117.713329, zoom=10)

get_nearby_isd_stations(lat_lon_df = data.frame(Building_Number="NV7300Z", latitude=33.560231, longitude=-117.713318), radius=10, limit=5, year=2015)

devtools::load_all("lean.analysis")
## set.seed(0)
## x <- c(1:10, 13:22)
## y <- numeric(20)
## ## Create first segment
## y[1:10] <- 20:11 + rnorm(10, 0, 1.5)
## ## Create second segment
## y[11:20] <- seq(11, 15, len=10) + rnorm(10, 0, 1.5)

devtools::load_all("lean.analysis")

fontSizeStackLean = 10
region="9"
## lowRange = NULL
## highRange = NULL
plotXLimits = NULL
## for source eui
plotYLimits = c(-0.5, 53)
majorgrid=seq(0, 55, 10)
minorgrid=seq(0, 55, 5)
## for site eui
## plotYLimits = c(-0.5, 18)
## majorgrid=seq(0, 18, 8)
## minorgrid=seq(0, 18, 4)
stacked_fit_plot(region=region, buildingType="Office", year=2017,
                 category=c("I", "A"), plotType="elec",
                 method=lean.analysis::piecewise_linear,
                 methodLabel="piecewise", lowRange=60, highRange=80,
                 plotXLimits=plotXLimits, plotYLimits=plotYLimits,
                 fontSize=fontSizeStackLean, legendloc="right",
                 vline_position=80, plot_col="eui_elec_source",
                 ## vline_position=80, plot_col="eui_elec",
                 majorgrid=majorgrid, minorgrid=minorgrid)

stacked_fit_plot(region=region, buildingType="Office", year=2017,
                 category=c("I", "A"), plotType="gas",
                 method=lean.analysis::piecewise_linear,
                 methodLabel="piecewise", plotXLimits=plotXLimits,
                 plotYLimits=plotYLimits, fontSize=fontSizeStackLean,
                 legendloc="right", vline_position=50,
                 plot_col="eui_gas_source", majorgrid=majorgrid,
                 ## plot_col="eui_gas", majorgrid=majorgrid,
                 minorgrid=minorgrid)

## following generate stacked lean for one single building
devtools::load_all("lean.analysis")
fontSizeStackLean = 10
majorgrid=NULL
minorgrid=NULL
lowRange=NULL
highRange=NULL
plotXLimits=NULL
plotYLimits=NULL
stacked_fit_plot(buildingNumber = "MO0039ZZ", plotType="gas",
                 method=lean.analysis::piecewise_linear,
                 methodLabel="piecewise", lowRange=lowRange, highRange=highRange,
                 plotXLimits=plotXLimits, plotYLimits=plotYLimits,
                 fontSize=fontSizeStackLean, legendloc="right",
                 vline_position_gas=30, plot_col="eui_heating_source",
                 ## vline_position=80, plot_col="eui_elec",
                 majorgrid=majorgrid, minorgrid=minorgrid, debugFlag=TRUE, cvrmse_upper=100.0)

## following has old setup
## stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="elec", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=60, highRange=80, plotXLimits=c(44, 100), plotYLimits=c(-0.5, 17.5), fontSize=fontSizeStackLean, legendloc="right", vline_position=80, plot_col="eui_elec")
## stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::piecewise_linear, methodLabel="piecewise", plotXLimits=c(44, 100), plotYLimits=c(-0.5, 17.5), fontSize=fontSizeStackLean, legendloc="right", vline_position=50, plot_col="eui_gas")

devtools::load_all("lean.analysis")
fontSizeStackLean = 10
region="5"
## lowRange = NULL
## highRange = NULL
plotXLimits = NULL
## for source eui
cvrmse_upper = 0.35
if (region == "1") {
  yupper = 20
  plotYLimits = c(-0.5, yupper)
  majorgrid=seq(0, yupper, 10)
  minorgrid=seq(0, yupper, 5)
  lowRange = 30
  highRange = 70
  vline_gas = 30
  vline_elec = 70
} else if (region == "2") {
  yupper = 40
  plotYLimits = c(-0.5, yupper)
  majorgrid=seq(0, yupper, 10)
  minorgrid=seq(0, yupper, 5)
  lowRange = 30
  highRange = 70
  vline_gas = 30
  vline_elec = 70
} else if (region == "3") {
  yupper = 30
  plotYLimits = c(-0.5, yupper)
  majorgrid=seq(0, yupper, 10)
  minorgrid=seq(0, yupper, 5)
  lowRange = 40
  highRange = 70
  vline_gas = 40
  vline_elec = 70
} else if (region == "4") {
  yupper = 20
  plotYLimits = c(-0.5, yupper)
  majorgrid=seq(0, yupper, 10)
  minorgrid=seq(0, yupper, 5)
  lowRange = 45
  highRange = 75
  vline_gas = 45
  vline_elec = 75
} else if (region == "5") {
  ## plotYLimits = NULL
  ## majorgrid = NULL
  ## minorgrid = NULL
  ## lowRange = 30
  ## highRange = 70
  ## vline_gas = 30
  ## vline_elec = 70
  plotYLimits = c(-0.5, 30)
  majorgrid=seq(0, 30, 10)
  minorgrid=seq(0, 50, 5)
  lowRange = 30
  highRange = 70
  vline_gas = 30
  vline_elec = 70
} else if (region == "6") {
  plotYLimits = NULL
  majorgrid = NULL
  minorgrid = NULL
  lowRange = 30
  highRange = 70
  vline_gas = 30
  vline_elec = 70
  ## yupper = 20
  ## plotYLimits = c(-0.5, yupper)
  ## majorgrid=seq(0, yupper, 10)
  ## minorgrid=seq(0, yupper, 5)
  ## lowRange = 30
  ## highRange = 70
  ## vline_gas = 30
  ## vline_elec = 70
} else if (region == "7") {
  yupper = 30
  plotYLimits = c(-0.5, yupper)
  majorgrid=seq(0, yupper, 10)
  minorgrid=seq(0, yupper, 5)
  lowRange = 50
  highRange = 80
  vline_gas = 50
  vline_elec = 80
} else if (region == "8") {
  yupper = 25
  plotYLimits = c(-0.5, yupper)
  majorgrid=seq(0, yupper, 10)
  minorgrid=seq(0, yupper, 5)
  lowRange = NULL
  highRange = NULL
  vline_gas = 30
  vline_elec = 70
} else if (region == "9") {
  yupper = NULL
  plotYLimits = NULL
  majorgrid = NULL
  minorgrid = NULL
  lowRange = NULL
  highRange = NULL
  vline_gas = 50
  vline_elec = 80
} else if (region == "10") {
  yupper = 50
  plotYLimits = c(-0.5, yupper)
  majorgrid=seq(0, yupper, 10)
  minorgrid=seq(0, yupper, 5)
  lowRange = 40
  highRange = 60
  vline_gas = 40
  vline_elec = 60
} else if (region == "11") {
  yupper = 40
  plotYLimits = c(-0.5, yupper)
  majorgrid=seq(0, yupper, 10)
  minorgrid=seq(0, yupper, 5)
  lowRange = 30
  highRange = 75
  vline_gas = 30
  vline_elec = 75
}

stacked_fit_plot(region=region, buildingType="Office", year=2017,
                 category=c("I", "A"), plotType="elec",
                 method=lean.analysis::piecewise_linear,
                 methodLabel="piecewise", lowRange=lowRange, highRange=highRange,
                 plotXLimits=plotXLimits, plotYLimits=plotYLimits,
                 fontSize=fontSizeStackLean, legendloc="right",
                 vline_position_elec=vline_elec, vline_position_gas=vline_gas,
                 ## plot_col="eui_cooling_source",
                 ## vline_position=80,
                 plot_col="eui_elec_source",
                 majorgrid=majorgrid, minorgrid=minorgrid, cvrmse_upper=cvrmse_upper)

stacked_fit_plot(region=region, buildingType="Office", year=2017,
                 category=c("I", "A"), plotType="gas",
                 method=lean.analysis::piecewise_linear,
                 methodLabel="piecewise", plotXLimits=plotXLimits,
                 plotYLimits=plotYLimits, fontSize=fontSizeStackLean,
                 legendloc="right", vline_position_elec=vline_elec, vline_position_gas=vline_gas,
                 ## plot_col="eui_heating_source", majorgrid=majorgrid,
                 plot_col="eui_gas_source", majorgrid=majorgrid,
                 minorgrid=minorgrid, cvrmse_upper=cvrmse_upper)

fontSizeStackLean = 10
region = "1"
lowRange = NULL
highRange = NULL
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="elec", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=lowRange, highRange=highRange, plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=60)

## need to run twice
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::piecewise_linear, methodLabel="piecewise", plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=30)

fontSizeStackLean = 10
region = "2"
lowRange = 40
highRange = 60
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="elec", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=lowRange, highRange=highRange, plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=70)

region = "2"
## need to run twice
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::piecewise_linear, methodLabel="piecewise", plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=30)

fontSizeStackLean = 10
region = "3"
lowRange = NULL
highRange = NULL
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="elec", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=lowRange, highRange=highRange, plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=70)

## need to run twice
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::piecewise_linear, methodLabel="piecewise", plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=30)

fontSizeStackLean = 10
region = "4"
lowRange = 50
highRange = 70
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="elec", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=lowRange, highRange=highRange, plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=70)

region = "4"
## need to run twice
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::piecewise_linear, methodLabel="piecewise", plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=50)

fontSizeStackLean = 10
region = "5"
lowRange = NULL
highRange = NULL
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="elec", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=lowRange, highRange=highRange, plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=70)

region = "5"
## need to run twice
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::piecewise_linear, methodLabel="piecewise", plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=30)

fontSizeStackLean = 10
region = "6"
lowRange = NULL
highRange = NULL
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="elec", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=lowRange, highRange=highRange, plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=70)

region = "6"
## need to run twice
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::piecewise_linear, methodLabel="piecewise", plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=30)

fontSizeStackLean = 10
region = "7"
lowRange = NULL
highRange = NULL
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="elec", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=lowRange, highRange=highRange, plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=80)

region = "7"
## need to run twice
lowRange = 55
highRange = 80
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=lowRange, highRange=highRange, plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=50)


fontSizeStackLean = 10
region = "8"
lowRange = 35
highRange = 65
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="elec", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=lowRange, highRange=highRange, plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=70)

region = "8"
## need to run twice
lowRange = NULL
highRange = NULL
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=lowRange, highRange=highRange, plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=30)

fontSizeStackLean = 10
region = "10"
lowRange = NULL
highRange = NULL
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="elec", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=lowRange, highRange=highRange, plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=60)

region = "10"
## need to run twice
lowRange = NULL
highRange = NULL
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=lowRange, highRange=highRange, plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=40)

fontSizeStackLean = 10
region = "11"
lowRange = NULL
highRange = NULL
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="elec", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=lowRange, highRange=highRange, plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=70)

region = "11"
## need to run twice
lowRange = NULL
highRange = NULL
plotXLimits = NULL
plotYLimits = NULL
stacked_fit_plot(region=region, buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=lowRange, highRange=highRange, plotXLimits=plotXLimits, plotYLimits=plotYLimits, fontSize=fontSizeStackLean, legendloc="right", vline_position=30)

## stacked_fit_plot(region="9", buildingType="Office", year=2017, category=c("I", "A"), plotType="elec", method=lean.analysis::polynomial_deg_2, methodLabel="poly2", lowRange=60, highRange=80, plotXLimits=c(44, 100), plotYLimits=c(-0.5, 17.5))
## stacked_fit_plot(region="9", buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::polynomial_deg_2, methodLabel="poly2", plotXLimits=c(40, 90), plotYLimits=c(-0.5, 17.5))
## stacked_fit_plot(region="9", buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::piecewise_linear, methodLabel="piecewise", plotXLimits=c(44, 100), plotYLimits=c(1.7, 16.6), minorgrid=seq(2, 14, 2), majorgrid=seq(4, 16, 4))
## stacked_fit_plot(region="9", buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::piecewise_linear, methodLabel="piecewise", plotXLimits=c(40, 90))

devtools::load_all("lean.analysis")
## plot lean image
for (region in 5:5) {
  plot_regional (region=region, suffix="source_heating_cooling", elec_col="eui_cooling_source", gas_col="eui_heating_source")
  ## plot_regional (region=region, suffix="source_electric_gas", elec_col="eui_elec_source", gas_col="eui_gas_source")
}

## copy the top 20 images to page_data using their ranks as name
devtools::load_all("lean.analysis")
for (regionnum in 5:5) {
  copy_image_rename_with_rank (region=regionnum, suffix="source_heating_cooling", plotType="elec", pagedatakey="cooling")
  copy_image_rename_with_rank (region=regionnum, suffix="source_heating_cooling", plotType="gas", pagedatakey="heating")
  copy_image_rename_with_rank (region=regionnum, suffix="source_heating_cooling", plotType="base", pagedatakey="baseload")
}

## Following generates building-by-building side by side cmp of using the two sources
devtools::load_all("lean.analysis")
generate_building_by_building_cmp()

devtools::load_all("db.interface")

devtools::load_all("get.noaa.weather")

devtools::load_all("lean.analysis")
## buildingNumber="IL0214ZZ"
## buildingNumber="MI0402ZZ"
## buildingNumber="MI0724SB"
buildingNumber="MN0600ZZ"
## buildingNumber="MI0000DI"
xlimits = NULL
ylimits = NULL
elec_col = "eui_cooling_source"
gas_col = "eui_heating_source"
## elec_col = "eui_elec_source"
## gas_col = "eui_gas_source"
## presuffix="_source_electric_gas"
plot_lean_subset(buildingNumber=buildingNumber, year=2017, plotType="elec", category=c("I", "A"), plotXLimit=xlimits, plotYLimit=ylimits, elec_col=elec_col, gas_col=gas_col, debugFlag=TRUE, plotPoint=TRUE)

## for building TX0211ZZ
building = "TX0211ZZ"
plotType = "base"
energy = db.interface::read_table_from_db(dbname="all", tablename="EUAS_monthly_with_type",
                                          cols=c("Fiscal_Year", "Fiscal_Month", "year", "month", "Building_Type","eui_elec", "eui_gas", "Cat"), building=building) %>%
  dplyr::arrange(-`year`, -`month`) %>%
  dplyr::filter(year %in% 2013:2016) %>%
  {.}
print(building)
print(head(energy))
lat_lon_df = db.interface::get_lat_lon_df(building=building)
lean_result = lean_analysis(energy = energy, lat_lon_df = lat_lon_df, id=building, plotType=plotType, debug=TRUE)

test_lean_analysis_db()

test_fit()


readxl::read_excel("~/Dropbox/gsa_2017/input/FY/EUAS/EUAS_AllRegions_2016-2017.xlsx", sheet=2) %>%
  dplyr::group_by(`Building Number`, `Fiscal Year`, `Fiscal Month`) %>%
    dplyr::filter(n() > 1) %>%
  head()

print("asdfasdf")




devtools::load_all("roiForECM")
roiForECM::roiBuilding(building="UT0032ZZ")
## roiForECM::roiBuilding(building="IN1703ZZ")
## roiForECM::add_lean(building="IN1703ZZ")

library("dplyr")

devtools::load_all("db.interface")
devtools::load_all("get.noaa.weather")
devtools::load_all("lean.analysis")

devtools::load_all("roiForECM")
## roiForECM::getAvgTemp("NV0294ZZ", path="~/Dropbox/gsa_2017/roiForECM/data-raw")
roiForECM::roiForAll()

load("roiForECM/data/actionCollapse.rda")

head(actionCollapse)

## compute HDD + CDD weather normalization
buildings = db.interface::get_buildings(year=2017, category = c("A", "I"))

devtools::load_all("get.noaa.weather")

devtools::load_all("roiForECM")
for (b in buildings) {
  getDegreeDay(b=b, start_str="2016-10-01", end_str="2017-09-30", path="~/Dropbox/gsa_2017/weather_normalization/")
}

## try out downloading climate normal
devtools::load_all("get.noaa.weather")
print(getMonthlyNormalHDD(s="GHCND:USC00503163")$data)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## weather normalized page 10, use weather_normalize.R
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##


