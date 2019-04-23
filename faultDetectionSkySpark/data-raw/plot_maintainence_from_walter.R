library("readxl")
library("dplyr")
library("ggplot2")
library("ggrepel")

devtools::load_all("~/Dropbox/gsa_2017/db.interface")

## contains all unique buildings
dfarea = readr::read_csv("area_info_allbuilding.csv") %>%
  dplyr::mutate(`Building Code`=substr(building, 1, 6)) %>%
  dplyr::group_by(`Building Code`) %>%
  dplyr::filter(n() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(-`source`, -`source_order`) %>%
  {.}

allgsalink =
  readxl::read_excel("list_of_gsalink_buildings.xlsx", sheet=2) %>%
  dplyr::mutate(`building`=substr(`spark_entry`, 1 ,8)) %>%
  distinct(building) %>%
  dplyr::mutate(shortname=substr(building, 1, 6)) %>%
  {.}

studyset = readr::read_csv("../data/has_energy_ecost.csv") %>%
  dplyr::mutate(shortname=substr(building, 1, 6)) %>%
  dplyr::select(building, shortname) %>%
  {.}

alleuas = db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_monthly", cols="Building_Number") %>%
  {.}

alleuas <- alleuas %>%
  dplyr::rename(building=`Building_Number`) %>%
  dplyr::distinct(building) %>%
  dplyr::mutate(shortname=substr(building, 1, 6)) %>%
  {.}

head(alleuas)

head(studyset$shortname)

df <- readxl::read_excel("maintenance_cost_from_walter/Historic O&M for Owned Buildings FY2008 - FY2017.xlsx",
                         skip=2,
                         sheet=1,
                         col_types = c("guess", "guess", "text", "text", "text",
                                       "text", "text", "text", "text", "text",
                                       "text", "text")) %>%
  {.}

head(df)

## cost.type = "A40 Mechanic Oper-Maint"
cost.type = "A30 Utilities/Fuel"

toplot <- df %>%
  tidyr::fill(`Building Code`) %>%
  dplyr::filter(`Net Income Label`==cost.type) %>%
  dplyr::mutate_at(vars(starts_with("20")), funs(as.numeric)) %>%
  dplyr::mutate(group=ifelse(`Building Code` %in% allgsalink$shortname, "GSALink",
                             ifelse(`Building Code` %in% alleuas$shortname, "EUAS non GSALink", "Other"))) %>%
  dplyr::filter(group != "Other") %>%
  dplyr::left_join(dfarea, by="Building Code") %>%
  dplyr::filter(!is.na(GSF)) %>%
  tidyr::gather(`Fiscal Year`, !!rlang::sym(cost.type), `2008`:`2017`) %>%
  dplyr::mutate_at(vars(cost.type), funs(`per GSF`=./GSF)) %>%
  dplyr::rename_at(vars("per GSF"), funs(paste(cost.type, .))) %>%
  {.}

df1 = toplot %>%
  dplyr::filter(GSF!=0) %>%
  dplyr::filter(group=="EUAS non GSALink") %>%
  {.}

df2 = toplot %>%
  dplyr::filter(GSF!=0) %>%
  dplyr::filter(group=="GSALink") %>%
  {.}

if (cost.type == "A40 Mechanic Oper-Maint") {
  df1plot <- df1 %>%
    dplyr::filter(!(`Building Code` %in% c("DC0001", "DC0459"))) %>%
    {.}
  df2plot <- df2 %>%
   dplyr::filter(!(`Building Code` %in% c("NY0282"))) %>%
    {.}
} else if (cost.type == "A30 Utilities/Fuel") {
  df1plot <- df1 %>%
    dplyr::filter(!(`Building Code` %in% c("DC0459"))) %>%
    {.}
  df2plot <- df2 %>%
    dplyr::filter(!(`Building Code` %in% c("NY0282"))) %>%
    {.}
}

df1plot <- df1plot %>%
  dplyr::group_by(`Building Code`) %>%
  dplyr::filter(sum(!!rlang::sym(cost.type) < 0) == 0) %>%
  dplyr::ungroup()
df2plot <- df2plot %>%
  dplyr::group_by(`Building Code`) %>%
  dplyr::filter(sum(!!rlang::sym(cost.type) < 0) == 0) %>%
  dplyr::ungroup()

df1plot %>%
  dplyr::bind_rows(df2plot) %>%
  dplyr::distinct(`Building Code`, `group`) %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()

ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x=`Fiscal Year`, y=!!rlang::sym(cost.type), group=`Building Code`, colour=group), data=df1plot) +
    ggplot2::geom_point(ggplot2::aes(x=`Fiscal Year`, y=!!rlang::sym(cost.type), group=`Building Code`, colour=group), data=df1plot) +
    ggplot2::geom_text(ggplot2::aes(label=`Building Code`, x="2017", y=!!rlang::sym(cost.type), group=`Building Code`, colour=group), data=df1plot%>%dplyr::filter(`Fiscal Year`=="2017")) +
    ggplot2::geom_line(ggplot2::aes(x=`Fiscal Year`, y=!!rlang::sym(cost.type), group=`Building Code`, colour=group), data=df2plot) +
    ggplot2::geom_point(ggplot2::aes(x=`Fiscal Year`, y=!!rlang::sym(cost.type), group=`Building Code`, colour=group), data=df2plot) +
    ggplot2::geom_text(ggplot2::aes(label=`Building Code`, x="2017", y=!!rlang::sym(cost.type), group=`Building Code`, colour=group), data=df2plot%>%dplyr::filter(`Fiscal Year`=="2017")) +
  ggplot2::ggtitle(sprintf("Historic O&M for All Buildings: %s", cost.type),
                   subtitle = "GSALink vs non GSALink") +
  ggplot2::ylab(paste0(cost.type, "($)")) +
  ggrepel::geom_text_repel()
ggplot2::ggsave(filename=sprintf("../plots/OandM_%s_filter.png", gsub("/", "-", fixed = TRUE, cost.type)))

df1plot.grouped = df1plot %>%
  dplyr::mutate(status=ifelse(`Fiscal Year` %in% c("2010", "2011", "2012"), "pre gsalink",
                       ifelse(`Fiscal Year` %in% c("2015", "2016", "2017"),
                              "post gsalink", NA))) %>%
  dplyr::filter(!is.na(status)) %>%
  dplyr::group_by(`Building Code`, status) %>%
  dplyr::summarise(!!rlang::sym(cost.type) := mean(!!rlang::sym(cost.type)),
                   group=first(group)) %>%
  dplyr::ungroup()

df2plot.grouped = df2plot %>%
  dplyr::mutate(status=ifelse(`Fiscal Year` %in% c("2010", "2011", "2012"), "pre gsalink",
                       ifelse(`Fiscal Year` %in% c("2015", "2016", "2017"),
                              "post gsalink", NA))) %>%
  dplyr::filter(!is.na(status)) %>%
  dplyr::group_by(`Building Code`, status) %>%
  dplyr::summarise(!!rlang::sym(cost.type) := mean(!!rlang::sym(cost.type)),
                   group=first(group)) %>%
  dplyr::ungroup()

df1plot.grouped %>%
  dplyr::bind_rows(df2plot.grouped) %>%
  dplyr::mutate(status=factor(status, levels=c("pre gsalink", "post gsalink"))) %>%
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(colour=status, y=!!rlang::sym(cost.type), x=group)) +
  ggplot2::ggtitle(sprintf("Historic O&M for All Buildings: %s", cost.type),
                   subtitle = "GSALink vs non GSALink") +
  ggplot2::theme()
  ggplot2::ggsave(filename=sprintf("../plots/OandM_%s_filter_box.png", gsub("/", "-", fixed = TRUE, cost.type)))

if (cost.type == "A40 Mechanic Oper-Maint") {
  df1plot <- df1 %>%
    dplyr::filter(!(`Building Code` %in% c("DC0001"))) %>%
    {.}
  df2plot <- df2 %>%
    dplyr::filter(!(`Building Code` %in% c("DC1456"))) %>%
    {.}
} else if (cost.type == "A30 Utilities/Fuel") {
  df1plot <- df1 %>%
    ## dplyr::filter(!(`Building Code` %in% c("DC0459"))) %>%
    {.}
  df2plot <- df2 %>%
    ## dplyr::filter(!(`Building Code` %in% c("NY0282"))) %>%
    {.}
}
df1plot <- df1plot %>%
  dplyr::group_by(`Building Code`) %>%
  dplyr::filter(sum(!!rlang::sym(cost.type) < 0) == 0) %>%
  dplyr::ungroup()
df2plot <- df2plot %>%
  dplyr::group_by(`Building Code`) %>%
  dplyr::filter(sum(!!rlang::sym(cost.type) < 0) == 0) %>%
  dplyr::ungroup()

df1plot %>%
  dplyr::bind_rows(df2plot) %>%
  dplyr::distinct(`Building Code`, `group`) %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup()

ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x=`Fiscal Year`, y=!!rlang::sym(paste(cost.type, "per GSF")), group=`Building Code`, colour=group), data=df1plot) +
  ggplot2::geom_point(ggplot2::aes(x=`Fiscal Year`, y=!!rlang::sym(paste(cost.type, "per GSF")), group=`Building Code`, colour=group), data=df1plot) +
  ggplot2::geom_text(ggplot2::aes(label=`Building Code`, x="2017", y=!!rlang::sym(paste(cost.type, "per GSF")), group=`Building Code`, colour=group), data=df1plot%>%dplyr::filter(`Fiscal Year`=="2017")) +
  ggplot2::geom_line(ggplot2::aes(x=`Fiscal Year`, y=!!rlang::sym(paste(cost.type, "per GSF")), group=`Building Code`, colour=group), data=df2plot) +
  ggplot2::geom_point(ggplot2::aes(x=`Fiscal Year`, y=!!rlang::sym(paste(cost.type, "per GSF")), group=`Building Code`, colour=group), data=df2plot) +
  ggplot2::geom_text(ggplot2::aes(label=`Building Code`, x="2017", y=!!rlang::sym(paste(cost.type, "per GSF")), group=`Building Code`, colour=group), data=df2plot%>%dplyr::filter(`Fiscal Year`=="2017")) +
  ggplot2::ggtitle(sprintf("Historic O&M for All Buildings: %s per GSF", cost.type),
                   subtitle = "GSALink vs non GSALink") +
  ggplot2::ylab(paste0(cost.type, "($/sqft)"))
  ggplot2::ggsave(filename=sprintf("../plots/OandM_%s_perGSF_filter.png", gsub("/", "-", fixed = TRUE, cost.type)))

df1plot.grouped = df1plot %>%
  dplyr::mutate(status=ifelse(`Fiscal Year` %in% c("2010", "2011", "2012"), "pre gsalink",
                       ifelse(`Fiscal Year` %in% c("2015", "2016", "2017"),
                              "post gsalink", NA))) %>%
  dplyr::filter(!is.na(status)) %>%
  dplyr::group_by(`Building Code`, status) %>%
  dplyr::summarise(!!rlang::sym(paste(cost.type, "per GSF")) := mean(!!rlang::sym(paste(cost.type, "per GSF"))),
                   group=first(group)) %>%
  dplyr::ungroup()

df2plot.grouped = df2plot %>%
  dplyr::mutate(status=ifelse(`Fiscal Year` %in% c("2010", "2011", "2012"), "pre gsalink",
                       ifelse(`Fiscal Year` %in% c("2015", "2016", "2017"),
                              "post gsalink", NA))) %>%
  dplyr::filter(!is.na(status)) %>%
  dplyr::group_by(`Building Code`, status) %>%
  dplyr::summarise(!!rlang::sym(paste(cost.type, "per GSF")) := mean(!!rlang::sym(paste(cost.type, "per GSF"))),
                   group=first(group)) %>%
  dplyr::ungroup()

df1plot.grouped %>%
  dplyr::bind_rows(df2plot.grouped) %>%
  dplyr::mutate(status=factor(status, levels=c("pre gsalink", "post gsalink"))) %>%
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(colour=status, y=!!rlang::sym(paste(cost.type, "per GSF")), x=group)) +
  ggplot2::ggtitle(sprintf("Historic O&M for All Buildings: %s", cost.type),
                   subtitle = "GSALink vs non GSALink") +
  ggplot2::theme()
  ggplot2::ggsave(filename=sprintf("../plots/OandM_%s_perGSF_filter_box.png", gsub("/", "-", fixed = TRUE, cost.type)))

## this has wrong plotting order of group

## toplot %>%
##   ggplot2::ggplot(ggplot2::aes(x=`Fiscal Year`, y=!!rlang::sym(cost.type),
##                                group=`Building Code`, colour=group)) +
##   ggplot2::ggtitle(sprintf("Historic O&M for All Buildings: %s", cost.type),
##                    subtitle = "GSALink vs non GSALink") +
##   ggplot2::ylab(paste0(cost.type, "($)")) +
##   ggplot2::geom_line() +
##   ggplot2::geom_point()
## ggplot2::ggsave(filename=sprintf("../plots/OandM_%s.png", cost.type))

## toplot %>%
##   dplyr::filter(GSF!=0) %>%
##   ## dplyr::mutate(group=factor(group, levels=c("EUAS non GSALink", "GSALink"))) %>%
##   dplyr::arrange(group, `Building Code`, `Fiscal Year`) %>%
##   ggplot2::ggplot(ggplot2::aes(x=`Fiscal Year`, y=!!rlang::sym(paste(cost.type, "per GSF")),
##                                group=`Building Code`, colour=group)) +
##   ggplot2::ggtitle(sprintf("Historic O&M for All Buildings: %s per GSF", cost.type),
##                    subtitle = "GSALink vs non GSALink") +
##   ggplot2::ylab(paste0(cost.type, "($/sqft)")) +
##   ggplot2::geom_line() +
##   ggplot2::geom_point()
## ggplot2::ggsave(filename=sprintf("../plots/OandM_%s_perGSF.png", cost.type))
