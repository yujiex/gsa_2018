library("dplyr")

studyset = readr::read_csv("../data/has_energy_ecost.csv") %>%
  .$building

head(studyset)

rulesummary = readr::read_csv("rule_summary.csv") %>%
  dplyr::filter(building %in% studyset) %>%
  {.}

rulesummary %>%
  dplyr::group_by(rule) %>%
  dplyr::summarise(eCost=sum(eCost)) %>%
  dplyr::ungroup() %>%
  head()

rulesummary %>%
  distinct(building) %>%
  nrow()
## has 68 buildings

rules_in_studyset = rulesummary %>%
  distinct(rule) %>%
  {.}

rules_in_studyset %>%
  readr::write_csv("../data/rule_in_studyset.csv")
