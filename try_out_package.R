
lat_lon_df = db.interface::get_lat_lon_df()

db.interface::get_all_tables(dbname="all")

db.interface::view_head_of_table(dbname = "all", tablename = "EUAS_monthly_with_type")

db.interface::view_names_of_table(dbname = "other_input", tablename = "euas_database_of_buildings_cmu")

devtools::load_all("db.interface")
main_db_build()

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

