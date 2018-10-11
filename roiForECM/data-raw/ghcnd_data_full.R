library(rnoaa)

ghcnd_data_full = rnoaa::ghcnd_stations(refresh = TRUE)

devtools::use_data(ghcnd_data_full, pkg="~/Dropbox/gsa_2017/roiForECM", overwrite = TRUE)
