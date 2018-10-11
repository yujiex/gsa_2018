library(dplyr)

devtools::load_all("../../db.interface")

energy =
  db.interface::read_table_from_db(dbname="all", tablename="EUAS_monthly" ,
                                   cols=c("Building_Number", "year", "month", "Electricity_(KWH)", "Electric_(kBtu)",
                                          "Gas_(kBtu)", "Gas_(Cubic_Ft)", "eui_elec", "eui_gas")) %>%
  tibble::as_data_frame() %>%
  {.}

devtools::use_data(energy, pkg="~/Dropbox/gsa_2017/roiForECM", overwrite=TRUE)
