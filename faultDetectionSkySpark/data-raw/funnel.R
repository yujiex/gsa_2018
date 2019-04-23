library("readr")
library("dplyr")

allspark = readxl::read_excel("list_of_gsalink_buildings.xlsx", sheet=2) %>%
  dplyr::mutate(`Building_Number`=substr(`spark_entry`, 1, 8)) %>%
    dplyr::select(-`spark_entry`) %>%
    dplyr::rename(`building`=`Building_Number`) %>%
    {.}

allspark %>%
  dplyr::group_by(`status`) %>%
  dplyr::summarise(n())

# A tibble: 2 x 2
## status     `n()`
## <chr>      <int>
## 1 downloaded    89
## 2 no data       64

energy_status = readr::read_csv("energy_data_existance.csv")

head(energy_status)

head(allspark)

has_energy =
  allspark %>%
  dplyr::filter(`status`=="downloaded") %>%
  dplyr::left_join(energy_status) %>%
  dplyr::mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
  dplyr::mutate(`has_energy_water`=(`Domestic H2O Int gal` +
+ `kWh Del Int` + `kWh del-rec Int` + `kWh Rec Int` + `Natural Gas Vol Int` > 0)) %>%
dplyr::filter(`has_energy_water`) %>%
  {.}

nrow(has_energy)

hasEnergyCost =
  readr::read_csv("rule_summary.csv") %>%
  dplyr::filter(`eCost` > 0) %>%
  dplyr::select(`building`, `eCost`) %>%
  dplyr::group_by(`building`) %>%
  dplyr::summarise(`eCost`=sum(`eCost`)) %>%
  {.}

has_energy_ecost = has_energy %>%
  dplyr::left_join(hasEnergyCost) %>%
  dplyr::select(`building`, `status`, `has_energy_water`, `eCost`) %>%
  dplyr::filter(!is.na(`eCost`)) %>%
  {.}

nrow(has_energy_ecost)

has_energy_ecost %>%
  readr::write_csv("has_energy_ecost.csv")

closed_sparks_2018 = readr::read_csv("closed_sparks_2018.csv",
                                     col_types = cols(gsalinkteci = col_double())) %>%
  tibble::as.tibble() %>%
  dplyr::distinct(building) %>%
  .$building

head(closed_sparks_2018)

has_energy_ecost_closespark <- has_energy_ecost %>%
  dplyr::filter(building %in% closed_sparks_2018) %>%
  {.}

df_area = readr::read_csv("gsalink_building_area.csv")

eCostByRule =
  readr::read_csv("rule_summary.csv") %>%
  dplyr::filter(building %in% has_energy_ecost_closespark$building) %>%
  ## dplyr::filter(building %in% has_energy_ecost$building) %>%
  dplyr::left_join(df_area) %>%
  dplyr::filter(`eCost` > 0) %>%
  dplyr::group_by(`rule`) %>%
  dplyr::summarise(`eCost`=sum(`eCost`), `GSF`=sum(`GSF`)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`eCost/GSF` = `eCost` / `GSF`) %>%
  {.}

eCostByRule %>%
  dplyr::arrange(desc(eCost)) %>%
  readr::write_csv("eCostByRule.csv")

top10 = eCostByRule %>%
  dplyr::arrange(desc(eCost)) %>%
  head(n=10) %>%
  .$rule

top10

readr::read_csv("rule_summary.csv") %>%
  ## dplyr::filter(building %in% has_energy_ecost$building) %>%
  dplyr::filter(building %in% has_energy_ecost_closespark$building) %>%
  dplyr::filter(`rule` %in% top10) %>%
  distinct(building) %>%
  nrow()

top10 = eCostByRule %>%
  dplyr::arrange(desc(`eCost/GSF`)) %>%
  head(n=10) %>%
  .$rule

top10

readr::read_csv("rule_summary.csv") %>%
  dplyr::filter(building %in% has_energy_ecost$building) %>%
    dplyr::filter(`rule` %in% top10) %>%
    distinct(building) %>%
    head()
