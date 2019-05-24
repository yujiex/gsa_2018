library("dplyr")
library("readr")

reg.result = readr::read_csv("reg_result/allrules.csv") %>%
  tibble::as_tibble() %>%
  {.}

reg.result <- reg.result %>%
  dplyr::rename(rule=covariate) %>%
  tidyr::spread(occtype, coefficient) %>%
  dplyr::filter(!(rule %in% c("(Intercept)", "F"))) %>%
  {.}

buildings = reg.result %>%
  dplyr::distinct(building) %>%
  .$building

length(buildings)

all.rule <- readr::read_csv("all_building_rule_2018.csv") %>%
  tibble::as_tibble() %>%
  dplyr::filter(building %in% buildings) %>%
  {.}

all.rule %>%
  dplyr::filter(building %in% buildings) %>%
  dplyr::select(rule, eCost, dur) %>%
  dplyr::filter(!is.na(eCost)) %>%
  dplyr::group_by(rule) %>%
  dplyr::summarise_all(sum) %>%
  dplyr::ungroup() %>%
  dplyr::rename(duration.hour=dur) %>%
  head()

rule.summary <- readr::read_csv("rule_summary.csv") %>%
  tibble::as_tibble() %>%
  {.}

