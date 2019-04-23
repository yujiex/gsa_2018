library("readr")
library("dplyr")

df = readr::read_csv("../data/has_energy_ecost.csv") %>%
  tibble::as.tibble() %>%
  {.}

dfarea =
  readr::read_csv("../data/gsalink_building_area.csv") %>%
  {.}

buildings = df$building

energy.summary =
  readr::read_csv("../data-raw/energy_summary.csv") %>%
  tibble::as.tibble() %>%
  dplyr::select(building, total, variable) %>%
  tidyr::spread(variable, total) %>%
  {.}

rule.summary =
  readr::read_csv("../data-raw/rule_summary.csv") %>%
  tibble::as_tibble() %>%
  dplyr::select(building, Cost, eCost, mCost, sCost) %>%
  dplyr::group_by(building) %>%
  dplyr::summarise_all(funs(sum)) %>%
  dplyr::ungroup() %>%
  {.}

## this is used in filling excel file "M_E_S cost"
df %>%
  dplyr::select(building) %>%
  dplyr::left_join(dfarea) %>%
  dplyr::left_join(energy.summary) %>%
  dplyr::left_join(rule.summary) %>%
  dplyr::mutate_at(vars(ends_with("Int"), ends_with("gal")), funs(perGSF = ./GSF)) %>%
  dplyr::mutate_at(vars(ends_with("Cost")), funs(perGSF = ./GSF)) %>%
  readr::write_csv("../data-raw/building_total_summary.csv")
