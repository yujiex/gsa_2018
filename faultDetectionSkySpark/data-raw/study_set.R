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

has_energy =
  allspark %>%
  dplyr::filter(`status`=="downloaded") %>%
  dplyr::left_join(energy_status) %>%
  dplyr::mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x)) %>%
  dplyr::mutate(`has_energy`=(`kWh Del Int` + `kWh del-rec Int` + `kWh Rec Int` + `Natural Gas Vol Int` > 0)) %>%
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
  dplyr::select(`building`, `status`, `has_energy_water`, `has_energy`, `eCost`) %>%
  dplyr::filter(!is.na(`eCost`)) %>%
  {.}

head(has_energy_ecost)

## this is the current study set
has_energy_ecost %>%
  readr::write_csv("../data/has_energy_ecost.csv")
