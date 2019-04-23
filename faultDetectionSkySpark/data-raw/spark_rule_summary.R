library("dplyr")
library("readr")

acc_rule = readr::read_csv("all_building_rule_2018.csv")

head(acc_rule)

acc_rule %>%
  dplyr::filter(is.na(Cost)) %>%
  head()

study_set = readr::read_csv("../data/has_energy_ecost.csv") %>%
  .$building

head(study_set)

acc_rule %>%
  dplyr::filter(!is.na(`Cost`)) %>%
  dplyr::group_by(`building`, `rule`) %>%
  dplyr::summarise(`earliest`=min(Date),
                   `latest`=max(Date),
                   `count`=n(),
                   `Cost`=sum(`Cost`),
                   `eCost`=sum(`eCost`),
                   `mCost`=sum(`mCost`),
                   `sCost`=sum(`sCost`),
                   `duration_hour`=sum(`dur`)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(rule) %>%
  dplyr::summarise(`duration_hour`=sum(`duration_hour`)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(`duration_hour`)) %>%
  head()

rulesummary <- acc_rule %>%
  ## na.omit() %>%
  dplyr::group_by(`building`, `rule`) %>%
  dplyr::summarise(`earliest`=min(Date),
                   `latest`=max(Date),
                   `count`=n(),
                   `Cost`=sum(`Cost`),
                   `eCost`=sum(`eCost`),
                   `mCost`=sum(`mCost`),
                   `sCost`=sum(`sCost`),
                   `duration_hour`=sum(`dur`)) %>%
  dplyr::ungroup() %>%
  {.}

rulesummary %>%
  readr::write_csv("../data/spark_rule_summary_2018.csv")

rulesummary %>%
  dplyr::filter(building %in% study_set) %>%
  readr::write_csv("../data/spark_rule_summary_2018_studyset.csv")

readr::read_csv("../data/spark_rule_summary_2018.csv") %>%
  dplyr::select(-earliest, -latest, -count, -building) %>%
  dplyr::group_by(rule) %>%
  dplyr::summarise_all(sum) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(-`duration_hour`) %>%
  readr::write_csv("../data/rule_summary.csv")

readr::read_csv("../data/spark_rule_summary_2018_studyset.csv") %>%
  dplyr::select(-earliest, -latest, -count, -building) %>%
    dplyr::group_by(rule) %>%
    dplyr::summarise_all(sum) %>%
    readr::write_csv("../data/rule_summary_studyset.csv")

readr::read_csv("../data/has_energy_ecost.csv") %>%
  head()

readr::read_csv("../data/has_energy_ecost.csv") %>%
