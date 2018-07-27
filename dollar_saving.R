library(dplyr)

#'get_filter_set(category=c("A", "I"), year=2017, region="9")
get_filter_set <- function(category, type, year, region) {
  ## remove 0 sqft and 0 electricity
  ## use your interface to database here
  df = db.interface::read_table_from_db(dbname = "all", tablename = "eui_by_fy_tag") %>%
    dplyr::filter(`Gross_Sq.Ft` != 0) %>%
    dplyr::filter(`eui_elec` != 0) %>%
    {.}
  ## use your interface to database here
  if (!missing(category)) {
    df <- df %>%
      dplyr::filter(`Cat` %in% category) %>%
      {.}
  }
  if (!missing(type)) {
    df <- df %>%
      dplyr::filter(`Building_Type` %in% type) %>%
      {.}
  }
  if (!missing(region)) {
    df <- df %>%
      dplyr::filter(`Region_No.` == region) %>%
      {.}
  }
  df = df %>%
    dplyr::mutate(`Region_No.` = factor(`Region_No.`, levels = as.character(1:11))) %>%
    dplyr::mutate(`Cat` = factor(`Cat`, levels=c("I", "A"))) %>%
    {.}
  if (!missing(year)) {
    df <- df %>%
      dplyr::filter(`Fiscal_Year` == year) %>%
      {.}
  }
  return(df)
}

#' Potential dollar saving based on median
#'
#' This function plots the potential dollar savings based on some median eui
#' @param category optional, a subset of A, B, C, D, E, I to include
#' @param type optional, a string (e.g. "Office"), or a string vector (e.g.
#'   c("Office", "Courthouse")) of building type
#' @param years optional, the years to plot
#' @param region optional, the region to plot
#' @param reference optional, cbecs, own, or hybrid
#' @param topn optional, plot top n records
#' @param botn optional, plot bottom n records
#' @param legendloc optional, default to bottom
#' @param yleftLimit optional, horizontal left limit of the plot
#' @param yrightLimit optional, horizontal right limit of the plot
#' @param expLimit optional, expanded limit to include
#' @param fontFamily optional, the font family
#' @param mod optional, savings will be rounded to "mod" number of 0's, e.g. 1000
#' @param fontsize optional, font size for all text
#'   will make Potential_Saving return in thousands
#' @param plotGreen optional, whether to plot the green bars, default to FALSE
#' @keywords dollar saving median
#' @export
#' @examples
#' dollar_saving(category=c("I", "A"), year=2017, region="9")
dollar_saving <- function(category, type, year, region, method="own", topn=10, botn=10, legendloc="bottom",
                          yleftLimit=0, yrightLimit=0, expLimit=0, hjust, fontFamily="System Font", mod=1000,
                          fontsize=10, yadjust=0, plotGreen=FALSE) {
  df = get_filter_set(category, type, year, region)
  if (missing(region)) {
    regionTag = ""
  } else {
    regionTag = sprintf(", region %s", region)
  }
  if (method == "own") {
    df <- df %>%
      dplyr::select(`Building_Number`, `Fiscal_Year`, `Cat`, `Building_Type`, `eui_total`, `Total_(Cost)`, `Gross_Sq.Ft`, `Total_(kBtu)`) %>%
      dplyr::group_by(`Cat`, `Building_Type`, `Fiscal_Year`) %>%
      dplyr::mutate(`eui_median` = median(`eui_total`)) %>%
      dplyr::ungroup() %>%
      {.}
    df %>%
      dplyr::select(`Building_Number`, `Fiscal_Year`, `Cat`, `Building_Type`, `eui_total`, `Total_(Cost)`, `Gross_Sq.Ft`, `Total_(kBtu)`) %>%
      dplyr::group_by(`Cat`, `Building_Type`, `Fiscal_Year`) %>%
      dplyr::summarise(`eui_median` = median(`eui_total`), `building_count`=n()) %>%
      dplyr::mutate(`region`=region) %>%
      readr::write_csv(sprintf("csv_FY/eui_median_region_%s.csv", region))
  } else if (method == "cbecs") {
    ## read median table of cbecs
    df_median = readr::read_csv("csv_FY/national_median.csv") %>%
      na.omit() %>%
      dplyr::select(-`PM_type`) %>%
      {.}
    df <- df %>%
      dplyr::left_join(df_median, by="Building_Type") %>%
      {.}
    df %>%
      readr::write_csv(sprintf("csv_FY/join_pm_median_%s_region%s.csv", year, region))
  } else if (method == "hybrid") {
    df_median = readr::read_csv("csv_FY/hybrid_gsa_national_cbecs_median.csv") %>%
      dplyr::mutate(`Cat`=as.character(`Cat`)) %>%
      na.omit() %>%
      dplyr::select(-`source`) %>%
      {.}
    df <- df %>%
      dplyr::mutate(`Cat`=as.character(`Cat`)) %>%
      dplyr::left_join(df_median, by=c("Building_Type", "Cat")) %>%
      {.}
  }
  df <- df %>%
    dplyr::mutate(`Potential_Saving` = (`eui_total` - `eui_median`) * `Gross_Sq.Ft` * (`Total_(Cost)` / `Total_(kBtu)`)) %>%
    ## dplyr::mutate(`Potential_Saving` = `Potential_Saving` * 1e-6) %>%
    dplyr::mutate(`Building_Number` = ifelse(`Cat` == "I", sprintf("(I) %s", `Building_Number`), `Building_Number`)) %>%
    ## round to inaccurating the result XD
    dplyr::mutate(`Potential_Saving`=`Potential_Saving` %/% mod * mod) %>%
    dplyr::arrange(desc(`Potential_Saving`)) %>%
    {.}
  df %>%
    dplyr::select(`Building_Number`, `Building_Type`, `eui_total`, `eui_median`, `Potential_Saving`, `Gross_Sq.Ft`, `Total_(Cost)`) %>%
    dplyr::mutate(`dollar / sqft` = round(`Total_(Cost)` / `Gross_Sq.Ft`, 2)) %>%
    dplyr::mutate_at(vars(`Potential_Saving`, `Gross_Sq.Ft`, `Total_(Cost)`, `eui_total`), function (x) format(x, big.mark=",", trim=TRUE)) %>%
    readr::write_csv(sprintf("csv_FY/dollar_saving_%s_median_%s_region%s.csv", method, year, region))
  ## df %>%
  ##   dplyr::select(`Building_Number`, `Building_Type`, `eui_total`, `eui_median`, `Potential_Saving`) %>%
  ##   head() %>%
  ##   print()
  ## get the default palette
  pal_values <- scales::hue_pal()(2)
  dfTop <- df %>%
    dplyr::filter(`Potential_Saving` > 0) %>%
    top_n(topn) %>%
    {.}
  dfBottom <- df %>%
    dplyr::filter(`Potential_Saving` <= 0) %>%
    top_n(n=(-1)*botn) %>%
    {.}
  options("scipen"=100, "digits"=4)
  ## df <- rbind(dfTop, dfBottom)
  max_abs = max(max(dfTop$`Potential_Saving`), max((-1) * dfBottom$`Potential_Saving`))
  print(max_abs)
  p <- dfTop %>%
    ## na.omit() %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(`Building_Number`, `Potential_Saving`), y=`Potential_Saving`,
                                 label=sprintf("$%s/year", format(`Potential_Saving`, big.mark=",", trim=TRUE)),
                                 )) +
    ggplot2::geom_bar(stat="identity", fill="#F89728") +
    ggplot2::geom_text(family=fontFamily, size=fontsize/3.3, hjust=-0.1, colour="#303030") +
    ggplot2::theme()
  yranges = (ggplot2::ggplot_build(p)$layout$panel_ranges[[1]]$y.range)
  ## ylimit = yranges + c(yleftLimit, yrightLimit)
  if (plotGreen) {
    ylimit = c(0, (max_abs + yadjust))
  } else {
    ylimit = yranges + c(0, yadjust)
  }
  p <- p +
    ggplot2::coord_flip(ylim=ylimit) +
    ggplot2::xlab("Building Number") +
    ## ggplot2::ggtitle(sprintf("Potential dollar saving%s (%s)", regionTag, method)) +
    ggplot2::ggtitle("Hypothetical savings: estimated annual savings \nif your building's EUI were reduced to national median") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position=legendloc, panel.grid.major = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                    axis.title = ggplot2::element_blank(),
                    text=ggplot2::element_text(size=fontsize, family=fontFamily)
                    )
  print(p)
  if (missing(region)) {
    ggsave(sprintf(file="region_report_img/regional/%s_median_potential_dollar_orange", method))
  } else {
    ggplot2::ggsave(file=sprintf("region_report_img/regional/%s_median_potential_dollar_region_%s_orange.png", method, region), width=5, height=3, units = "in")
  }
  if (plotGreen) {
    p <- dfBottom %>%
      ## na.omit() %>%
      dplyr::mutate(`Potential_Saving` = `Potential_Saving`) %>%
      ggplot2::ggplot(ggplot2::aes(x = reorder(`Building_Number`, `Potential_Saving`), y=`Potential_Saving`,
                                  label=sprintf("$%s/year", format(`Potential_Saving`, big.mark=",", trim=TRUE)),
                                  )) +
      ggplot2::geom_bar(stat="identity", fill="#8CC482") +
      ggplot2::geom_text(family=fontFamily, size=fontsize/3, hjust=1.1) +
      ggplot2::theme()
    yranges = (ggplot2::ggplot_build(p)$layout$panel_ranges[[1]]$y.range)
    ## ylimit = yranges + c(yleftLimit, yrightLimit)
    ylimit = c((-1)*(max_abs + yadjust), 0)
    p <- p +
      ggplot2::coord_flip(ylim=ylimit) +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::xlab("Building Number") +
      ## ggplot2::ggtitle(sprintf("Potential dollar saving%s (%s)", regionTag, method)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position=legendloc, panel.grid.major = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                    axis.title = ggplot2::element_blank(),
                    text=ggplot2::element_text(size=fontsize, family=fontFamily)
                    )
    print(p)
    if (missing(region)) {
      ggsave(sprintf(file="region_report_img/regional/%s_median_potential_dollar_green", method))
    } else {
      ggplot2::ggsave(file=sprintf("region_report_img/regional/%s_median_potential_dollar_region_%s_green.png", method, region), width=5, height=3, units = "in")
    }
  }
}

yadjusts = list("1"=0, "2"=0, "3"=0, "4"=0, "5"=0,
                "6"=0, "7"=0, "8"=0, "9"=30000, "10"=0,
                "11"=0)
for (r in as.character(9:9)) {
  dollar_saving(category=c("I", "A"), year=2017, region=r, method="hybrid", legendloc="bottom", topn=8, botn=7, yrightLimit=yrightLimits[[r]], yleftLimit=0, expLimit=, hjust=0.2, fontFamily="System Font", mod=1000, fontsize=10, yadjust=yadjusts[[r]], plotGreen=FALSE)
}
