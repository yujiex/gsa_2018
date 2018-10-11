library(dplyr)
library(lubridate)

devtools::load_all("../../db.interface")
load("../data/dfECM.rda")
load("../data/cost.rda")

cost <- cost %>%
  dplyr::mutate(`action_time`=lubridate::ymd(`action_time`)) %>%
  {.}

actionAndCost = dfECM %>%
  dplyr::mutate(`action_time`=lubridate::ymd(`Substantial_Completion_Date`)) %>%
  dplyr::select(-`Substantial_Completion_Date`, -`source_highlevel`, -`source_detail`) %>%
  dplyr::left_join(cost, by=c("Building_Number", "action_time")) %>%
  dplyr::select(`Building_Number`, `action_time`, `Cost`, `high_level_ECM`, `detail_level_ECM`) %>%
  {.}

devtools::use_data(actionAndCost, pkg="~/Dropbox/gsa_2017/roiForECM", overwrite=TRUE)
