library(dplyr)

devtools::load_all("../../db.interface")
load("../data/actionAndCost.rda")

actionCollapse = actionAndCost %>%
  dplyr::group_by(`Building_Number`, `action_time`, `high_level_ECM`) %>%
  dplyr::summarise(`detail_collapse`=paste0(`detail_level_ECM`, collapse = "\n    ")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`high_level_collapse`=paste0(`high_level_ECM`, "\n    ", `detail_collapse`)) %>%
  dplyr::group_by(`Building_Number`, `action_time`) %>%
  dplyr::summarise(`high_level_collapse`=paste0(`high_level_collapse`, collapse = "\n")) %>%
  dplyr::ungroup() %>%
  {.}

devtools::use_data(actionCollapse, pkg="~/Dropbox/gsa_2017/roiForECM", overwrite=TRUE)
