library(readxl)

cost =
  readxl::read_excel("~/Dropbox/gsa_2017/input/FY/ECM info/March_Updated_Light-Touch M_V - ARRA Targets to Actuals and Commissioning Details.xlsx", sheet=1, skip=3) %>%
  head(-4) %>%
  dplyr::select(`Building ID`, `Total Obligation`, `Substantial Completion Date1`) %>%
  dplyr::rename(`Building_Number`=`Building ID`,
                `action_time`=`Substantial Completion Date1`,
                `Cost`=`Total Obligation`) %>%
  dplyr::group_by(`Building_Number`, `action_time`) %>%
  dplyr::summarise(`Cost` = sum(`Cost`)) %>%
  dplyr::ungroup() %>%
  {.}

devtools::use_data(cost, pkg="~/Dropbox/gsa_2017/roiForECM", overwrite=TRUE)
