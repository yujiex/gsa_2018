lat_lon_df = db.interface::get_lat_lon_df()

db.interface::get_all_tables(dbname="all")

db.interface::get_all_tables(dbname="other_input")

db.interface::view_head_of_table(dbname = "all", tablename = "EUAS_monthly")[,5:10]

db.interface::view_names_of_table(dbname = "all", tablename = "EUAS_monthly_with_type")

db.interface::view_names_of_table(dbname = "other_input", tablename = "PortfolioManager_sheet0_input")

db.interface::view_names_of_table(dbname = "all", tablename = "EUAS_ecm")

db.interface::read_table_from_db(dbname = "all", tablename = "eui_by_fy_tag") %>%
  dplyr::filter(`Region_No.`=="9", `Gross_Sq.Ft`==0) %>%
    .$Building_Number

devtools::load_all("db.interface")
## add_quality_tag_energy()
main_db_build()

db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_monthly") %>%
  dplyr::filter(Fiscal_Year == 2016) %>%
    readr::write_csv("temp.csv")

get_unique_value_column(dbname="all", tablename="EUAS_ecm", col="high_level_ECM")

get_unique_value_column(dbname="all", tablename="EUAS_type_recode", col="data_source")

## ---------------------------------------------------------------------------------

devtools::load_all("summarise.and.plot")
## national_overview(category=c("I", "A"), year=2017)
## national_overview(category=c("I", "A"), year=2017, region="9")
national_overview_over_years(category=c("I", "A"), years=c(2013, 2014, 2015, 2016, 2017), pal="Set3")
## national_overview_over_years(category=c("I", "A"), years=c(2013, 2014, 2015, 2016, 2017), region="9", pal="Set3")
## national_overview_over_years(category=c("I", "A"), pal="Set3")
## gb_agg_ratio(df, groupvar = "Fiscal_Year", numerator_var = c("Electric_(kBtu)", "Gas_(kBtu)", "Oil_(kBtu)", "Steam_(kBtu)", "Chilled_Water_(kBtu)"), denominator_var = "Gross_Sq.Ft", aggfun=sum, valuename="kBtu/sqft", varname="Fuel Type")

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

buildings = db.interface::get_buildings(region=9, buildingType="Office", year=2017)


load(file="~/Dropbox/gsa_2017/get.noaa.weather/data/isdData.rda")

evtools::load_all("get.noaa.weather")

view_isd_stations_year(isd_data=isd_data, year=2015, latitude=33.560347, longitude=-117.713329, zoom=10)

get_nearby_isd_stations(lat_lon_df = data.frame(Building_Number="NV7300ZZ", latitude=33.560231, longitude=-117.713318), radius=10, limit=5, year=2015)

devtools::load_all("lean.analysis")
## set.seed(0)
## x <- c(1:10, 13:22)
## y <- numeric(20)
## ## Create first segment
## y[1:10] <- 20:11 + rnorm(10, 0, 1.5)
## ## Create second segment
## y[11:20] <- seq(11, 15, len=10) + rnorm(10, 0, 1.5)

devtools::load_all("lean.analysis")
stacked_fit_plot(region="9", buildingType="Office", year=2017, category=c("I", "A"), plotType="elec", method=lean.analysis::polynomial_deg_2, methodLabel="poly2", lowRange=60, highRange=80)

stacked_fit_plot(region="9", buildingType="Office", year=2017, category=c("I", "A"), plotType="elec", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=60, highRange=80)
stacked_fit_plot(region="9", buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::polynomial_deg_2, methodLabel="poly2")
stacked_fit_plot(region="9", buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::piecewise_linear, methodLabel="piecewise")

## plot lean image
plot_lean_subset(region=9, buildingType="Office", year=2017, plotType="gas", category=c("I", "A"))
plot_lean_subset(region=9, buildingType="Office", year=2017, plotType="elec", category=c("I", "A"))
plot_lean_subset(region=9, buildingType="Office", year=2017, plotType="base", category=c("I", "A"))

generate_lean_tex(plotType="elec", region=9)
generate_lean_tex(plotType="gas", region=9)

test_lean_analysis_db()

test_fit()
