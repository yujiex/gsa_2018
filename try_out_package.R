devtools::load_all("db.interface")
lat_lon_df = db.interface::get_lat_lon_df()

devtools::load_all("get.noaa.weather")

df = get_nearby_isd_stations(lat_lon_df, isd_data=isd_data, radius = 100,
                             limit=5, date_min = 20150101, date_max = 20151231)
