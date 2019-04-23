library("readr")
library("dplyr")

files = list.files("gsalink_weather/", "*.csv")

head(files)

for (f in files) {
  print(f)
  df = readr::read_csv(paste0("gsalink_weather/", f))
  df %>%
    dplyr::mutate(`00`=wt_temperatureFhour, `15`=wt_temperatureFhour,
                  `30`=wt_temperatureFhour, `45`=wt_temperatureFhour) %>%
    dplyr::select(-`wt_temperatureFhour`) %>%
    tidyr::gather(min, temperature, `00`:`45`) %>%
    dplyr::arrange(date, hour, min) %>%
    readr::write_csv(paste0("gsalink_weather_15min/", f))
}
