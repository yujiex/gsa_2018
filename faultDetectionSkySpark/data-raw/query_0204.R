library("dplyr")

studyset = readr::read_csv("../data/has_energy_ecost.csv") %>%
  dplyr::select(building)

head(studyset)

readr::read_csv("closed_sparks.csv") %>%
  dplyr::filter(rule %in% c("Unoccupied Cooling Setpoint Out of Range", "Occupied Cooling Setpoint Out of Range")) %>%
  ## dplyr::distinct(building, rule) %>%
  ## dplyr::mutate(value=1) %>%
  ## tidyr::spread(rule, value, fill=0) %>%
  readr::write_csv("building_with_top_2_ecost.csv")
