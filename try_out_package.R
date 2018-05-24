
lat_lon_df = db.interface::get_lat_lon_df()

db.interface::get_all_tables(dbname="other_input")

db.interface::view_names_of_table(dbname = "all", tablename = "EUAS_monthly")

db.interface::view_names_of_table(dbname = "all", tablename = "EUAS_monthly_with_type")

db.interface::view_names_of_table(dbname = "other_input", tablename = "PortfolioManager_sheet0_input")

db.interface::view_names_of_table(dbname = "all", tablename = "eui_by_fy_tag")

devtools::load_all("db.interface")
## add_quality_tag_energy()
## main_db_build()

db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_monthly") %>%
  dplyr::filter(Fiscal_Year == 2016) %>%
    readr::write_csv("temp.csv")

get_unique_value_column(dbname="all", tablename="EUAS_type_recode", col="Building_Type")

get_unique_value_column(dbname="all", tablename="EUAS_type_recode", col="data_source")

## ---------------------------------------------------------------------------------

devtools::load_all("summarise.and.plot")
## national_overview(category=c("I", "A"), year=2017)
national_overview_over_years(category=c("I", "A"), years=c(2013, 2014, 2015, 2016, 2017), pal="Set3")
## national_overview_over_years(category=c("I", "A"), pal="Set3")
## gb_agg_ratio(df, groupvar = "Fiscal_Year", numerator_var = c("Electric_(kBtu)", "Gas_(kBtu)", "Oil_(kBtu)", "Steam_(kBtu)", "Chilled_Water_(kBtu)"), denominator_var = "Gross_Sq.Ft", aggfun=sum, valuename="kBtu/sqft", varname="Fuel Type")

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

