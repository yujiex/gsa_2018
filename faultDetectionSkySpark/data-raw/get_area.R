library("dplyr")

study_set = readr::read_csv("has_energy_ecost.csv") %>%
  .$building

devtools::load_all("~/Dropbox/gsa_2017/db.interface")

euas_area = db.interface::read_table_from_db(dbname="all", tablename="EUAS_area_latest", cols=c("Building_Number", "Gross_Sq.Ft")) %>%
  tibble::as_data_frame() %>%
  dplyr::mutate(`source`="euas") %>%
  {.}

ion_area_download =
  readr::read_csv("area_from_ion.csv") %>%
  dplyr::mutate(`source`="ion") %>%
  {.}

euas_database_of_buildings = readr::read_csv("euas_database_of_buildings_cmu.csv") %>%
  dplyr::select(`Location Facility Code`, `Building GSF`) %>%
  dplyr::distinct(`Location Facility Code`, `Building GSF`) %>%
  dplyr::rename(`Building_Number`=`Location Facility Code`, `Gross_Sq.Ft`=`Building GSF`) %>%
  dplyr::mutate(`source`="euas_database_of_buildings_cmu.csv") %>%
  {.}

area_info = euas_area %>%
  dplyr::bind_rows(ion_area_download) %>%
  dplyr::bind_rows(euas_database_of_buildings) %>%
  dplyr::mutate(`source_order`=ifelse(source=="euas", 1, ifelse(source=="ion", 2, 3))) %>%
  dplyr::arrange(`Building_Number`, `source_order`) %>%
  dplyr::group_by(`Building_Number`) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::rename(`building`=`Building_Number`, `GSF`=`Gross_Sq.Ft`) %>%
  {.}

area_info %>%
  readr::write_csv("area_info_allbuilding.csv")

## get building sqft
df_area =
  data.frame(building=study_set) %>%
  tibble::as_data_frame() %>%
  dplyr::left_join(area_info) %>%
  {.}

area_info %>%
  readr::write_csv("area_info.csv")

df_area %>%
  dplyr::select(building, GSF) %>%
  readr::write_csv("../data/gsalink_building_area.csv")
