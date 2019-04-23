library("readr")
library("dplyr")

closed_sparks =
  readr::read_csv("completed_sparks_ncmms.csv") %>%
  dplyr::select(-`total_rec`, -`ticketid`, -`status`) %>%
  dplyr::rename(`building`=`gsasaddresscode`, `rule`=`gsalinkrulename`,
                `start`=`gsalinkinitialsparkdate`, `end`=`actualfinish`) %>%
  dplyr::mutate(start=as.POSIXct(start, format="%b %d, %Y, %I:%M %p")) %>%
  dplyr::mutate(end=as.POSIXct(end, format="%b %d, %Y, %I:%M %p")) %>%
  dplyr::mutate(`start_year`=format(start, "%Y"),
                `end_year`=format(end, "%Y")) %>%
  {.}

closed_sparks %>%
  readr::write_csv("closed_sparks.csv")

closed_sparks %>%
  dplyr::filter(`end_year`=="2018") %>%
  dplyr::select(-`start_year`, -`end_year`) %>%
  readr::write_csv("closed_sparks_2018.csv")

study_set =
  readr::read_csv("../data/has_energy_ecost.csv") %>%
  .$building

has_closed_sparks <- closed_sparks %>%
  dplyr::filter(building %in% study_set) %>%
  distinct(building) %>%
  .$building

length(has_closed_sparks)

setdiff(study_set, has_closed_sparks)

closed_sparks %>%
  dplyr::filter(building %in% study_set) %>%
  readr::write_csv("../data/closedsparks_2018_energy_ecost.csv")

closed_sparks <- closed_sparks %>%
  dplyr::rename(equipRef=description) %>%
  dplyr::mutate(start=as.Date(start),
                end=as.Date(end)) %>%
  {.}

all_rule = readr::read_csv("../data-raw/all_building_rule_2018.csv") %>%
  tibble::as.tibble() %>%
  {.}

all_rule <- all_rule %>%
  dplyr::filter(building %in% study_set) %>%
  {.}

namelookup = readr::read_csv("../data/gsalink_name_in_rulefile.csv") %>%
  tibble::as_data_frame() %>%
  {.}

equipname <- all_rule %>%
  dplyr::left_join(namelookup) %>%
  dplyr::mutate(name=paste0(name, " ")) %>%
  dplyr::mutate(equipRef=substr(equipRef, 32, nchar(equipRef))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(equipRef=gsub(name, "", equipRef)) %>%
  {.}

equipname %>%
  dplyr::rename(start=Date) %>%
  dplyr::left_join(closed_sparks, by=c("building", "rule", "equipRef", "start")) %>%
  readr::write_csv("../data-raw/join_ncmms_skyspark") %>%
  {.}

equipname %>%
  dplyr::group_by(building, Date, equipRef, rule) %>%
  dplyr::filter(n() > 1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(building, Date, equipRef, rule) %>%
  distinct(building) %>%
  nrow()
