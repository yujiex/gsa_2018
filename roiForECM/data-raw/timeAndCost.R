library("dplyr")

devtools::load_all("../../db.interface")
load("../data/actionAndCost.rda")

timeAndCost =
  actionAndCost %>%
  dplyr::select(`Building_Number`, `action_time`, `Cost`) %>%
  dplyr::arrange(`Building_Number`, `action_time`, `Cost`) %>%
  dplyr::group_by(`Building_Number`, `action_time`) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  {.}

devtools::use_data(timeAndCost, pkg="~/Dropbox/gsa_2017/roiForECM", overwrite=TRUE)
