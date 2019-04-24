library("readxl")
library("dplyr")

devtools::load_all("db.interface")

getwd()

df_category = readr::read_csv("input/FY/skyspark/spark_rule_classification_manual.csv") %>%
  tibble::as_data_frame() %>%
  dplyr::mutate(`Rule Name`=gsub("_ _", " ", `Rule Name`)) %>%
  dplyr::mutate(`Rule Name`=tolower(`Rule Name`)) %>%
  {.}

df_rule =
  readr::read_csv("input/FY/skyspark/cstSparkBuildingAsset - Equipment Top 10 - HGRID (1).csv", skip=5) %>%
  tibble::as_data_frame() %>%
  dplyr::mutate(`Total Estimated Cost Impact`=(gsub("\\$ ", "", `Total Estimated Cost Impact`))) %>%
  dplyr::mutate(`Total Estimated Cost Impact`=as.numeric(gsub(",", "", `Total Estimated Cost Impact`))) %>%
  dplyr::rename(`Rule Name`=`Rule Reference`) %>%
  dplyr::mutate(`Rule Name`=tolower(`Rule Name`)) %>%
  dplyr::mutate_at(vars(`Rule Name`), recode,
                   "ahu outside damper stuck closed"="ahu outdoor damper stuck closed",
                   "ahu outside damper stuck open"="ahu outdoor damper stuck open",
                   ) %>%
  {.}

df <- df_rule %>%
  dplyr::left_join(df_category, by="Rule Name") %>%
  {.}

## existing rule counts by category
df_category %>%
  dplyr::group_by(`Category`) %>%
  dplyr::summarise(`number of rules`=n()) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(x=`Category`, y=`number of rules`, label=`number of rules`)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::geom_text(nudge_y = 2) +
  ggplot2::ggtitle("Number of rules per category") +
  ggplot2::theme_bw()
ggplot2::ggsave("ppt/images/rule_category_count.png", width=3, height=3,
                units="in")

## existing rule counts by component
df_category %>%
  dplyr::filter(`Category`=="HVAC") %>%
  dplyr::group_by(`Component`) %>%
  dplyr::summarise(`number of rules`=n()) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(x=`Component`, y=`number of rules`, label=`number of rules`)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::geom_text(nudge_y = 1) +
  ggplot2::ggtitle("Number of rules per components for HVAC") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90))
ggplot2::ggsave("ppt/images/rule_component_count.png", width=5, height=3,
                units="in")

df_category = readxl::read_excel("input/FY/skyspark/spark_rule_classification_manual.xlsx") %>%
  tibble::as_data_frame() %>%
  {.}
## existing rule counts by sub component
df_category %>%
  dplyr::select(`Category`, `Component`, `Sub component`) %>%
  ## dplyr::filter(`Category`=="HVAC") %>%
  dplyr::group_by_all() %>%
  dplyr::summarise(n()) %>%
  {.}

temp =

  df_category %>%
  ## dplyr::filter(`Component`=="Boiler") %>%
  dplyr::filter(`Rule Name`=="Boiler Cycling")

  {.}

sprintf("#%s#", temp$`Rule Name`)

df %>%
  dplyr::select(`Building ID`, `Category`) %>%
  head()

library(kableExtra)

data.frame(a=1:3) %>%
  slice(2:nrow(.))

  knitr::kable() %>%
  kableExtra::kable_styling(full_width = F) %>%
  kableExtra::column_spec(1, width=20em)

df %>%
  dplyr::filter(is.na(`Category`)) %>%
  distinct(`Rule Name`)
