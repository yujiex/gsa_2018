library(dplyr)

devtools::load_all("../../db.interface")

utilityCost =
  db.interface::read_table_from_db(dbname="all", tablename="EUAS_monthly" ,
                                   cols=c("Building_Number", "year", "month", "Electricity_(Cost)",
                                          "Gas_(Cost)", "Electricity_(KWH)", "Gas_(Cubic_Ft)")) %>%
  tibble::as_data_frame() %>%
  dplyr::mutate(`Electric ($/KWH)`=`Electricity_(Cost)`/`Electricity_(KWH)`,
                `Gas ($/Cubic Ft)`=`Gas_(Cost)`/`Gas_(Cubic_Ft)`) %>%
  dplyr::select(`Building_Number`, `year`, `month`, `Electric ($/KWH)`, `Gas ($/Cubic Ft)`) %>%
  {.}

devtools::use_data(utilityCost, pkg="~/Dropbox/gsa_2017/roiForECM", overwrite=TRUE)
