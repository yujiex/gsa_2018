library("dplyr")
library("readr")
library("tidyr")

## ## single enrgy type data
## setwd("ion download/")
## varname = NA

## all utilities
setwd("../NEW Data with ALL Utilities/")

files = list.files(pattern="*.csv")
varname = "BTU - From Utility Int"

## split energy into single files of building by energy type
for (f in files) {
  print(sprintf("--------------%s---------------", f))
  if (is.na(varname)) {
    varname = substr(f, 1, regexpr("_", f)[[1]] - 1)
  }
  df =
    readr::read_csv(f) %>%
    dplyr::select(-starts_with("X")) %>%
    ## readr::problems() %>%
    ## print()
    tidyr::gather(`building`, !!rlang::sym(varname), -`Timestamp`) %>%
    na.omit() %>%
    {.}
  buildings = unique(df$building)
  for (b in buildings) {
    print(b)
    df %>%
      dplyr::filter(building==b) %>%
      dplyr::select(-`building`) %>%
      readr::write_csv(sprintf("../building_energy/%s_%s.csv", b, varname))
  }
}

## summary statistics
acc_summary = NULL
for (f in files) {
  print(sprintf("--------------%s---------------", f))
  varname = substr(f, 1, regexpr("_", f)[[1]] - 1)
  df = readr::read_csv(f) %>%
    dplyr::select(-starts_with("X")) %>%
    dplyr::mutate_at(vars(-`Timestamp`), as.numeric) %>%
    tidyr::gather(`building`, !!rlang::sym(varname), -`Timestamp`) %>%
    na.omit() %>%
    dplyr::mutate(`Date`=as.Date(substr(`Timestamp`, 1, 10), format="%m/%d/%Y")) %>%
    dplyr::group_by(`building`) %>%
    dplyr::summarise(`start_date`=min(`Date`),
                     `end_date`=max(`Date`),
                     `min`=min(!!rlang::sym(varname)),
                     `median`=median(!!rlang::sym(varname)),
                     `mean`=mean(!!rlang::sym(varname)),
                     `max`=max(!!rlang::sym(varname)),
                     `total`=sum(!!rlang::sym(varname))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(`variable`=varname) %>%
    {.}
  acc_summary <- rbind(acc_summary, df)
  print(tail(acc_summary))
}

acc_summary %>%
  readr::write_csv("../energy_summary.csv")
  ## readr::write_csv("../domestic_water_summary.csv")

readr::read_csv("../energy_summary.csv") %>%
  tibble::as_data_frame() %>%
  dplyr::mutate(`end_year`=format(`end_date`, "%Y")) %>%
  dplyr::mutate(`exist`=ifelse(`end_year`==2018, 1, 0)) %>%
  dplyr::select(`building`, `variable`, `exist`) %>%
  tidyr::spread(`variable`, `exist`, fill=0) %>%
  readr::write_csv("../energy_data_existance.csv")
