
lat_lon_df = db.interface::get_lat_lon_df()

db.interface::get_all_tables(dbname="other_input")

db.interface::view_head_of_table(dbname = "all", tablename = "EUAS_type")

db.interface::view_names_of_table(dbname = "all", tablename = "EUAS_monthly_with_type")

db.interface::view_names_of_table(dbname = "other_input", tablename = "PortfolioManager_sheet0_input")

db.interface::view_names_of_table(dbname = "all", tablename = "eui_by_fy_tag")

devtools::load_all("db.interface")
## add_quality_tag_energy()
main_db_build()

db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_monthly") %>%
  dplyr::filter(Fiscal_Year == 2016) %>%
    readr::write_csv("temp.csv")

get_unique_value_column(dbname="all", tablename="EUAS_type_recode", col="Building_Type")

get_unique_value_column(dbname="all", tablename="EUAS_type_recode", col="data_source")

## ---------------------------------------------------------------------------------

devtools::load_all("summarise.and.plot")
## national_overview(category=c("A", "C", "I"), year=2017)
national_overview(category=c("I", "A"), year=2017)

## ---------------------------------------------------------------------------------

devtools::load_all("get.noaa.weather")

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

