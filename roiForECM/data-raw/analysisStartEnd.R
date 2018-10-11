library("dplyr")
library("lubridate")

load("../data/dfECM.rda")

analysisStartEnd = dfECM %>%
  dplyr::mutate(`action_time`=lubridate::ymd(`Substantial_Completion_Date`)) %>%
  dplyr::select(`Building_Number`, `action_time`) %>%
  dplyr::group_by_all() %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(`Building_Number`, `action_time`) %>%
  dplyr::group_by(`Building_Number`) %>%
  dplyr::mutate(`next_action_start`=lead(`action_time`)) %>%
  dplyr::mutate(`prev_action_end`=lag(`action_time`)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(`analysis_start`=
                  lubridate::add_with_rollback(action_time, lubridate::years(-3), roll_to_first = TRUE),
                `analysis_end`=
                  lubridate::add_with_rollback(action_time, lubridate::years(3), roll_to_first = TRUE)
                ) %>%
  tidyr::replace_na(list(`prev_action_end` = ymd("1970-01-01"), `next_action_start` = ymd("2050-01-01"))) %>%
  dplyr::mutate(`analysis_start`=dplyr::if_else(`prev_action_end` < `analysis_start`, `analysis_start`, `prev_action_end`)) %>%
  dplyr::mutate(`analysis_end`=dplyr::if_else(`next_action_start` < `analysis_end`, `next_action_start`, `analysis_end`)) %>%
  dplyr::select(-`next_action_start`, -`prev_action_end`) %>%
  {.}

devtools::use_data(analysisStartEnd, pkg="~/Dropbox/gsa_2017/roiForECM", overwrite=TRUE)
