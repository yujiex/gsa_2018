library("dplyr")

load("../data/analysisStartEnd.rda")

downloadWeatherStartEnd = analysisStartEnd %>%
  dplyr::select(-`action_time`) %>%
  dplyr::group_by(`Building_Number`) %>%
  dplyr::summarise(`start`=min(`analysis_start`), `end`=max(`analysis_end`)) %>%
  dplyr::ungroup() %>%
  {.}

devtools::use_data(downloadWeatherStartEnd, pkg="~/Dropbox/gsa_2017/roiForECM", overwrite=TRUE)
