library("dplyr")
library("readr")

studyset = readr::read_csv("../data/has_energy_ecost.csv") %>%
  .$building
namelookup = readr::read_csv("../data/gsalink_name_in_rulefile.csv") %>%
  tibble::as_data_frame() %>%
  {.}

acc = NULL
for (b in studyset) {
  print(b)
  name = paste0(namelookup[namelookup$building==b,]$name, " ")
  df = readr::read_csv(sprintf("ruleStartEndByBuilding/%s_2018.csv", b)) %>%
    dplyr::mutate(equipRef=substr(equipRef, 32, nchar(equipRef))) %>%
    dplyr::mutate(equipRef=gsub(name, "", equipRef)) %>%
    dplyr::distinct(equipRef) %>%
    dplyr::mutate(building=b) %>%
    {.}
  acc <- rbind(acc, df)
}

acc %>%
  dplyr::mutate(equipRefBackup=equipRef) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(equipRef=gsub(paste0(building, "."), "", equipRef, fixed=TRUE)) %>%
  dplyr::mutate(equipRef=gsub(paste0(building, "/"), "", equipRef, fixed=TRUE)) %>%
  dplyr::mutate(equipRef=gsub(paste0(building, "-"), "", equipRef, fixed=TRUE)) %>%
  dplyr::mutate(equipRef=gsub(paste0(building, " - "), "", equipRef, fixed=TRUE)) %>%
  dplyr::mutate(equipRef=gsub("DC0021ZZ GSA", "", equipRef)) %>%
  dplyr::mutate(group=substr(equipRef, 1, regexpr("[- ._]", equipRef)[[1]] - 1)) %>%
  dplyr::mutate(group=ifelse(nchar(group)==0, substr(equipRef, 1, regexpr("[0-9]", equipRef)[[1]] - 1), group)) %>%
  dplyr::mutate(group=ifelse(nchar(group)==0, equipRef, group)) %>%
  dplyr::mutate_at(vars(group), funs(recode),
                   "AHUs"="AHU",
                   "AHU1"="AHU",
                   "AHU2"="AHU",
                   "Air"="AHU"
                   ) %>%
  dplyr::ungroup() %>%
  dplyr::rename(equip=equipRef) %>%
  readr::write_csv("building_component.csv")
