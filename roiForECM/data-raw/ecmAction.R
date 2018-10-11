library(dplyr)

devtools::load_all("../../db.interface")

dfECM =
  db.interface::read_table_from_db(dbname="all", tablename="EUAS_ecm") %>%
  tibble::as_data_frame() %>%
  dplyr::select(-`ECM_combined_header`) %>%
  dplyr::arrange(`Substantial_Completion_Date`) %>%
  dplyr::filter(!is.na(`Substantial_Completion_Date`)) %>%
  dplyr::mutate(`Substantial_Completion_Date`=gsub(" 00:00:00", "", `Substantial_Completion_Date`)) %>%
  dplyr::mutate(`Substantial_Completion_Date`=gsub("/", "-", `Substantial_Completion_Date`)) %>%
  {.}

devtools::use_data(dfECM, pkg="~/Dropbox/gsa_2017/roiForECM", overwrite=TRUE)
