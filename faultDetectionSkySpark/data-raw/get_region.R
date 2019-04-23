library("dplyr")

study_set = readr::read_csv("has_energy_ecost.csv") %>%
  .$building

head(study_set)

devtools::load_all("~/Dropbox/gsa_2017/db.interface")

region = db.interface::read_table_from_db(dbname = "all", tablename="EUAS_monthly", cols=c("Building_Number", "Region_No.")) %>%
  tibble::as_data_frame() %>%
  {.}

region <- region %>%
  dplyr::distinct(`Building_Number`, `Region_No.`) %>%
  dplyr::rename(`building`=`Building_Number`) %>%
  dplyr::mutate(`region`=as.integer(`Region_No.`)) %>%
  dplyr::select(-`Region_No.`) %>%
  {.}

region_other = readr::read_csv("euas_database_of_buildings_cmu.csv") %>%
  tibble::as_data_frame() %>%
  dplyr::mutate(`region`=as.integer(`Region Code`),
                `building`=`Location Facility Code`) %>%
  dplyr::select(`building`, `region`) %>%
  dplyr::mutate(`source`="euas_database_of_buildings_cmu.csv") %>%
  {.}

region_info <- region %>%
  dplyr::mutate(`source`="euas") %>%
  dplyr::bind_rows(region_other) %>%
  ## value "euas" as a better source
  dplyr::arrange(building, source, region) %>%
  dplyr::group_by(building) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  {.}

region_info %>%
  readr::write_csv("region_info.csv")

data.frame(building=study_set) %>%
  tibble::as_data_frame() %>%
  dplyr::left_join(region_info) %>%
  dplyr::select(building, region) %>%
  readr::write_csv("../data/gsalink_region.csv")
