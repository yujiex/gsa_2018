library("rnoaa")
library("dplyr")

isd_data_full = rnoaa::isd_stations(refresh = TRUE)

isd_data_full <- isd_data_full %>%
  dplyr::mutate(`id`=sprintf("%s_%s", `usaf`, `wban`)) %>%
  {.}

devtools::use_data(isd_data_full, pkg="~/Dropbox/gsa_2017/roiForECM", overwrite = TRUE)
