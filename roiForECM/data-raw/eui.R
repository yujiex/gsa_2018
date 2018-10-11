library(dplyr)

devtools::load_all("../../db.interface")

eui =
  db.interface::read_table_from_db(dbname="all", tablename="EUAS_monthly" ,
                                   cols=c("Building_Number", "year", "month", "eui_elec", "eui_gas")) %>%
  tibble::as_data_frame() %>%
  {.}

devtools::use_data(eui, pkg="~/Dropbox/gsa_2017/roiForECM", overwrite=TRUE)
