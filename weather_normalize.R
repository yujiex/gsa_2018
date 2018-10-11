library("feather")
library("dplyr")
library("ggrepel")

devtools::load_all("db.interface")

devtools::load_all("get.noaa.weather")

devtools::load_all("lean.analysis")

region = "9"

## get the list of buildings
buildings13to17 =
  db.interface::read_table_from_db(dbname="all", tablename="eui_by_fy_tag", cols=c("Building_Number", "Region_No.", "Gross_Sq.Ft", "eui_elec", "Cat", "Fiscal_Year")) %>%
  tibble::as_data_frame() %>%
  dplyr::filter(`Gross_Sq.Ft` != 0) %>%
  dplyr::filter(`eui_elec` != 0) %>%
  dplyr::filter(`Region_No.` == region) %>%
  dplyr::filter(`Cat` %in% c("A", "I")) %>%
  dplyr::filter(`Fiscal_Year` %in% 2013:2017) %>%
  dplyr::group_by(`Building_Number`) %>%
  dplyr::filter(n() == 5) %>%
  dplyr::ungroup() %>%
  distinct(`Building_Number`) %>%
  .$`Building_Number`

## which(buildings13to17=="WI0350ZZ")

## get degree day
devtools::load_all("roiForECM")
for (b in buildings13to17[1:length(buildings13to17)]) {
  roiForECM::getDegreeDay(b=b, start_str="2012-10-01", end_str="2017-09-30", path="~/Dropbox/gsa_2017/weather_normalization/", overwrite=FALSE)
}

## get climate normals
## use the closest station if it has data
normalfilename = "mly-cldd-normal"
getnormalmethod = getMonthlyNormalCDD
for (b in buildings13to17[1:length(buildings13to17)]) {
  acc = lapply(list.files(path="~/Dropbox/gsa_2017/weather_normalization/building_TMIN/", pattern=sprintf("%s_station_distance_*", b)), function(f) {
    df = readr::read_csv(paste0("~/Dropbox/gsa_2017/weather_normalization/building_TMIN/", f))
  })
  distance =
    do.call(rbind, acc) %>%
    dplyr::group_by(`id`) %>%
    dplyr::slice(1) %>%
    dplyr::arrange(`distance`, `id`) %>%
    {.}
  for(station in distance$id) {
    normal_file = sprintf("~/Dropbox/gsa_2017/weather_normalization/climate_normal/%s_%s.csv", station, normalfilename)
    if (!file.exists(normal_file)) {
      getnormalmethod(s=sprintf("GHCND:%s", station))$data %>%
        readr::write_csv(normal_file)
      Sys.sleep(0.21)
    }
  }
}

varname = "hdd"
normalfilename = "mly-htdd-normal"
## varname = "cdd"
## normalfilename = "mly-cldd-normal"
## get normal for each building with closest distance
## for (b in buildings13to17[1:1]) {
for (b in buildings13to17[1:length(buildings13to17)]) {
  acc = lapply(list.files(path="~/Dropbox/gsa_2017/weather_normalization/building_TMIN/", pattern=sprintf("%s_station_distance_*", b)), function(f) {
    df = readr::read_csv(paste0("~/Dropbox/gsa_2017/weather_normalization/building_TMIN/", f))
  })
  distance =
    do.call(rbind, acc) %>%
    dplyr::select(`id`, `distance`) %>%
    dplyr::group_by(`id`) %>%
    dplyr::slice(1) %>%
    dplyr::arrange(`distance`, `id`) %>%
    {.}
  acc_normal = NULL
  for(station in distance$id) {
    normal_file = sprintf("~/Dropbox/gsa_2017/weather_normalization/climate_normal/%s_%s.csv", station, normalfilename)
    if (file.exists(normal_file)) {
      df = readr::read_csv(normal_file) %>%
        {.}
      acc_normal = rbind(acc_normal, df)
    }
  }
  acc_normal <- acc_normal %>%
    dplyr::mutate(id=gsub("GHCND:", "", station)) %>%
    dplyr::select(-`station`) %>%
    dplyr::left_join(distance, by="id") %>%
    dplyr::filter(value != -7777) %>%
    dplyr::filter(fl_c %in% c("C", "S", "R")) %>%
    dplyr::arrange(`date`, `distance`) %>%
    dplyr::group_by(`date`) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    {.}
  acc_normal %>%
    dplyr::select(date, value) %>%
    dplyr::rename(!!rlang::sym(sprintf("%s_normal", varname)):=`value`) %>%
    readr::write_csv(sprintf("~/Dropbox/gsa_2017/weather_normalization/building_normal_%s/%s.csv", varname, b))
}

## combine all climate normals in one df
varname = "hdd"
acc = lapply(buildings13to17, function(b) {
  df =
    readr::read_csv(sprintf("~/Dropbox/gsa_2017/weather_normalization/building_normal_%s/%s.csv", varname, b)) %>%
    dplyr::mutate(`Building_Number`=b) %>%
    dplyr::mutate(`month`=lubridate::month(date)) %>%
    dplyr::select(`Building_Number`, `month`, ends_with("normal")) %>%
    {.}
})
hdd_normal_allbuilding = do.call(rbind, acc)

varname = "cdd"
acc = lapply(buildings13to17, function(b) {
  df =
    readr::read_csv(sprintf("~/Dropbox/gsa_2017/weather_normalization/building_normal_%s/%s.csv", varname, b)) %>%
    dplyr::mutate(`Building_Number`=b) %>%
    dplyr::mutate(`month`=lubridate::month(date)) %>%
    dplyr::select(`Building_Number`, `month`, ends_with("normal")) %>%
    {.}
})
cdd_normal_allbuilding = do.call(rbind, acc)

hdd_normal_annual = hdd_normal_allbuilding %>%
  dplyr::group_by(`Building_Number`) %>%
  dplyr::summarise(`hdd_normal`=sum(`hdd_normal`)) %>%
  {.}

cdd_normal_annual = cdd_normal_allbuilding %>%
  dplyr::group_by(`Building_Number`) %>%
  dplyr::summarise(`cdd_normal`=sum(`cdd_normal`)) %>%
  {.}

## combine hdd and cdd into one file
acc = lapply(buildings13to17, function(b) {
  df =
    feather::read_feather(sprintf("~/Dropbox/gsa_2017/weather_normalization/building_HDDCDD/%s.feather", b)) %>%
    dplyr::mutate(`Building_Number`=b) %>%
    {.}
})

dd_allbuilding =
  do.call(rbind, acc) %>%
  dplyr::mutate(`Fiscal_Year` = ifelse(as.integer(`month`) > 9, as.numeric(as.integer(year) + 1),
                                       as.numeric(year))) %>%
  dplyr::group_by(`Building_Number`, `Fiscal_Year`) %>%
  dplyr::summarise(`HDD`=sum(`HDD`), `CDD`=sum(`CDD`)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(`Fiscal_Year` != "2012") %>%
  {.}

## normalization step
db.interface::read_table_from_db(dbname="all", tablename="eui_by_fy_tag",
                                 cols=c("Building_Number", "Fiscal_Year", "eui_total", "Cat")) %>%
  dplyr::filter(`Building_Number` %in% buildings13to17,
                `Fiscal_Year` %in% 2013:2017) %>%
    dplyr::left_join(hdd_normal_annual, by="Building_Number") %>%
    dplyr::left_join(cdd_normal_annual, by="Building_Number") %>%
    dplyr::left_join(dd_allbuilding, by=c("Building_Number", "Fiscal_Year")) %>%
    dplyr::mutate(`Building_Number`=sprintf("(%s) %s", `Cat`, `Building_Number`)) %>%
    dplyr::mutate(`eui_normal`=`eui_total` / (HDD + CDD) * (hdd_normal + cdd_normal)) %>%
    dplyr::select(`Building_Number`, `Fiscal_Year`, `eui_normal`) %>%
    tidyr::spread(`Fiscal_Year`, `eui_normal`) %>%
    readr::write_csv(sprintf("~/Dropbox/gsa_2017/weather_normalization/page10_region%s.csv", region))

## compare before and after normalized
cmp_plot_df = db.interface::read_table_from_db(dbname="all", tablename="eui_by_fy_tag",
                                 cols=c("Building_Number", "Fiscal_Year", "eui_total", "Cat")) %>%
  dplyr::filter(`Building_Number` %in% buildings13to17,
                `Fiscal_Year` %in% 2013:2017) %>%
    dplyr::left_join(hdd_normal_annual, by="Building_Number") %>%
    dplyr::left_join(cdd_normal_annual, by="Building_Number") %>%
    dplyr::left_join(dd_allbuilding, by=c("Building_Number", "Fiscal_Year")) %>%
    dplyr::mutate(`Building_Number`=sprintf("(%s) %s", `Cat`, `Building_Number`)) %>%
    dplyr::mutate(`eui_normal`=`eui_total` / (HDD + CDD) * (hdd_normal + cdd_normal)) %>%
    dplyr::select(`Building_Number`, `Fiscal_Year`, `eui_normal`, `eui_total`) %>%
    {.}

dflabel = cmp_plot_df %>%
  dplyr::filter(`Fiscal_Year`==2017) %>%
  {.}

cmp_plot_df %>%
  ggplot2::ggplot(ggplot2::aes(x=`Fiscal_Year`, y=`eui_normal`, color=`Building_Number`, group=`Building_Number`)) +
  ggplot2::geom_line() +
  ## ggrepel::geom_text_repel(data=dflabel, ggplot2::aes(x=`Fiscal_Year`, y=`eui_normal`, label=`Building_Number`, hjust=0.5)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none")
ggplot2::ggsave("~/Dropbox/gsa_2017/weather_normalization/region5_normal.png", width=5, height=8, unit="in")

cmp_plot_df %>%
  ggplot2::ggplot(ggplot2::aes(x=`Fiscal_Year`, y=`eui_total`, color=`Building_Number`, group=`Building_Number`)) +
  ggplot2::geom_line() +
  ## ggrepel::geom_text_repel(data=dflabel, ggplot2::aes(x=`Fiscal_Year`, y=`eui_normal`, label=`Building_Number`, hjust=0.5)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none")
ggplot2::ggsave("~/Dropbox/gsa_2017/weather_normalization/region5_non_normal.png", width=5, height=8, unit="in")
