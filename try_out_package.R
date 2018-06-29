library(dplyr)

lat_lon_df = db.interface::get_lat_lon_df()

db.interface::get_all_tables(dbname="all")

db.interface::get_all_tables(dbname="other_input")

db.interface::view_head_of_table(dbname = "all", tablename = "EUAS_monthly")[,5:10]

db.interface::view_names_of_table(dbname = "other_input", tablename = "euas_database_of_buildings_cmu")

db.interface::view_names_of_table(dbname = "all", tablename = "EUAS_monthly_with_type")

devtools::load_all("db.interface")
db.interface::view_names_of_table(dbname = "all", tablename = "EUAS_address")

db.interface::view_head_of_table(dbname = "all", tablename = "EUAS_latlng_2")

dbname="all"
tablename="EUAS_latlng_2"
colname="source"
db.interface::view_names_of_table(dbname = dbname, tablename = tablename)
get_unique_value_column(dbname = dbname, tablename = tablename, col=colname)

db.interface::view_names_of_table(dbname = "all", tablename = "EUAS_ecm")

db.interface::view_names_of_table(dbname = "all", tablename = "EUAS_type")

db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_address") %>%
  dplyr::filter(`source`=="Entire_GSA_Building_Portfolio_input") %>%
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
main_db_build()
## get_ship_db()
## join_source_latlng()

db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_monthly") %>%
  dplyr::filter(Fiscal_Year == 2016) %>%
    readr::write_csv("temp.csv")

get_unique_value_column(dbname="all", tablename="EUAS_ecm", col="high_level_ECM")


get_unique_value_column(dbname="all", tablename="EUAS_type_recode", col="data_source")

get_unique_value_column(dbname="all", tablename="EUAS_type_recode", col="Building_Type")

## ---------------------------------------------------------------------------------

devtools::load_all("summarise.and.plot")
get_filter_set(category=c("A", "I"), year=2017) %>%
  dplyr::left_join(db.interface::get_lat_lon_df()) %>%
  dplyr::select(-`index`) %>%
  readr::write_csv("csv_FY/powerMapData2017.csv")

devtools::load_all("summarise.and.plot")
get_filter_set(category=c("A", "I"), year=2017) %>%
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

national_overview_over_years(category=c("I", "A"), years=c(2013, 2014, 2015, 2016, 2017), pal="Set3")

national_overview_facetRegion(category=c("I", "A"), years=c(2015, 2017))

national_overview(category=c("I", "A"), year=2017)
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

buildings = db.interface::get_buildings(region=9, buildingType="Office", year=2017)


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
## stacked_fit_plot(region="9", buildingType="Office", year=2017, category=c("I", "A"), plotType="elec", method=lean.analysis::polynomial_deg_2, methodLabel="poly2", lowRange=60, highRange=80, plotXLimits=c(44, 100), plotYLimits=c(-0.5, 17.5))
## stacked_fit_plot(region="9", buildingType="Office", year=2017, category=c("I", "A"), plotType="elec", method=lean.analysis::piecewise_linear, methodLabel="piecewise", lowRange=60, highRange=80, plotXLimits=c(44, 100))
## stacked_fit_plot(region="9", buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::polynomial_deg_2, methodLabel="poly2", plotXLimits=c(40, 90), plotYLimits=c(-0.5, 17.5))
stacked_fit_plot(region="9", buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::piecewise_linear, methodLabel="piecewise", plotXLimits=c(44, 100), plotYLimits=c(1.7, 16.6), minorgrid=seq(2, 14, 2), majorgrid=seq(4, 16, 4))
## stacked_fit_plot(region="9", buildingType="Office", year=2017, category=c("I", "A"), plotType="gas", method=lean.analysis::piecewise_linear, methodLabel="piecewise", plotXLimits=c(40, 90))

devtools::load_all("lean.analysis")
## plot lean image
## maybe add in a whether to redo plotting tag?
plot_lean_subset(region=9, buildingType="Office", year=2017, plotType="base", category=c("I", "A"), sourceEnergy=TRUE, plotXLimit=c(43, 97), plotYLimit=c(-1, 60), topn=16, botn=4)

plot_lean_subset(region=9, buildingType="Office", year=2017, plotType="gas", category=c("I", "A"), sourceEnergy=TRUE, plotXLimit=c(43, 97), plotYLimit=c(-1, 40), topn=20, botn=0)
plot_lean_subset(region=9, buildingType="Office", year=2017, plotType="elec", category=c("I", "A"), sourceEnergy=TRUE, plotXLimit=c(43, 97), plotYLimit=c(-1, 60), topn=20, botn=0)

devtools::load_all("lean.analysis")
generate_lean_tex(plotType="base", region=9, topn=8, botn=4, category="I")

generate_lean_tex(plotType="base", region=9, topn=4, botn=4, category="A")

generate_lean_tex(plotType="gas", region=9, topn=20, botn=0)
generate_lean_tex(plotType="elec", region=9, topn=20, botn=0)

test_lean_analysis_db()

test_fit()


readxl::read_excel("~/Dropbox/gsa_2017/input/FY/EUAS/EUAS_AllRegions_2016-2017.xlsx", sheet=2) %>%
  dplyr::group_by(`Building Number`, `Fiscal Year`, `Fiscal Month`) %>%
    dplyr::filter(n() > 1) %>%
  head()

