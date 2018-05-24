#' Stacked bar plot
#'
#' This function produces a stacked bar plot
#' @param df required, a dataframe to plot
#' @param xcol the horizontal variable
#' @param fillcol the fill column
#' @param ycol the y column
#' @param ylabel required, label for y axis
#' @param xlabel required, label for x axis
#' @param tit plot title
#' @param legendloc legend location: top, bottom, c(0.8, 0.2)
#' @param legendOrient legend orientation: "v" or "h"
#' @param pal color palette
#' @param pal_values colors in the palette to use
#' @param aggfun count or sum
#' @param labelFormat optional, string format function of label
#' @keywords query count
#' @export
#' @examples
#' stackbar(df=df, xcol="Fiscal_Year", fillcol="Cat", ylabel="Building Count", tit="EUAS Building Count By Category")
stackbar <- function(df, xcol, fillcol, ycol, orderByHeight, ylabel, xlabel, tit, legendloc, legendOrient, pal, pal_values, labelFormat) {
  if (missing(labelFormat)) {
    labelFormat = "%s"
  }
  if (missing(ycol)) {
    ## dfcount = plyr::count(df, c(xcol, fillcol))
    dfcount = df %>%
      dplyr::group_by_at(vars(one_of(xcol, fillcol))) %>%
      dplyr::summarise(`freq` = n()) %>%
      {.}
    print(knitr::kable(dfcount))
    if (orderByHeight) {
      g = ggplot2::ggplot(dfcount, ggplot2::aes_string(x=sprintf("reorder(%s, -freq)", xcol), y="freq", fill=fillcol, label="freq")) +
        ggplot2::theme()
    } else {
      g = ggplot2::ggplot(dfcount, ggplot2::aes_string(x=xcol, y="freq", fill=fillcol, label="freq")) +
        ggplot2::theme()
    }
  } else {
    dfcount = df %>%
      dplyr::group_by_at(vars(one_of(xcol, fillcol))) %>%
      dplyr::summarise(`total` = sum(!!rlang::sym(ycol))) %>%
      {.}
    print(knitr::kable(dfcount))
    dfcount = dfcount %>%
      dplyr::mutate(`total_label` = sprintf(labelFormat, `total`)) %>%
      {.}
    if (orderByHeight) {
      g = ggplot2::ggplot(dfcount, ggplot2::aes_string(x = sprintf("reorder(%s, -total)", xcol), y="total", fill=fillcol, label="total_label")) +
        ggplot2::theme()
    } else {
      g = ggplot2::ggplot(dfcount, ggplot2::aes_string(x=xcol, y="total", fill=fillcol, label="total_label")) +
        ggplot2::theme()
    }
  }
  g <- g +
    ggplot2::geom_bar(stat="identity", position = "stack") +
    ggplot2::ylab(ylabel) +
    ggplot2::xlab(xlabel) +
    ggplot2::labs(title=tit) +
    ggplot2::geom_text(size=2.5, position = ggplot2::position_stack(vjust = 0.5))
    ## ggplot2::geom_text(ggplot2::aes(y=mid_y), size=2.5)
  if (!missing(legendloc)) {
    g <- g + ggplot2::theme(legend.position=legendloc)
  }
  if (missing(legendOrient)) {
    legendOrient = "h"
  }
  if (legendOrient == "v") {
    g <- g + ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
  if (missing(pal)) {
    pal = "Set3"
  }
  if (!missing(pal_values)) {
    g <- g + ggplot2::scale_fill_manual(values=pal_values)
  } else {
    g <- g + ggplot2::scale_fill_brewer(palette=pal)
  }
  return(g)
}

#' Group by ratio
#'
#' This function first group by some variable A, compute some aggregation of the
#' numerator variables, take ratio of each numerator variables with the denominator_var
#' @param df the data frame containing the variables
#' @param groupvar the variable to group by for ratio compare
#' @param aggfun the aggregation function, e.g. sum, mean
#' @param numerator_var the vector of variables as the numerators
#' @keywords ratio group by plot
#' @export
#' @examples
#' ss(df, groupvar = "Fiscal_Year", numerator_var = c("Electricity_(Cost)", "Gas_(kBtu)", "Oil_(kBtu)", "Steam__(kBtu)", "Chilled_Water_(kBtu)"), denominator_var = "Gross_Sq.Ft")
gb_agg_ratio <- function(df, groupvar, numerator_var, denominator_var, aggfun, valuename, varname) {
  df_group = df %>%
    dplyr::select(one_of(c(groupvar, numerator_var, denominator_var, numerator_var))) %>%
    dplyr::group_by_at(vars(one_of(groupvar))) %>%
    dplyr::summarise_at(vars(one_of(c(numerator_var, denominator_var))), aggfun) %>%
    tidyr::gather_(varname, valuename, numerator_var) %>%
    dplyr::mutate(!!(rlang::sym(valuename)) := !!(rlang::sym(valuename)) / !!(rlang::sym(denominator_var))) %>%
    {.}
  return(df_group)
}

#' National overview: cnt, eui, sqft
#'
#' This function groups the national level plots, may decide on a filter
#' @param category optional, a subset of A, B, C, D, E, I to include
#' @param type optional, a string (e.g. "Office"), or a string vector (e.g. c("Office", "Courthouse")) of building type
#' @param year optional, the year to plot
#' @keywords query count
#' @export
#' @examples
#' national_overview(category=c("A", "C", "I"), year=2017)
national_overview <- function(category, type, year, pal_values) {
  df = db.interface::read_table_from_db(dbname = "all", tablename = "eui_by_fy_tag")
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
  if (!missing(year)) {
    df <- df %>%
      dplyr::filter(`Fiscal_Year` == year) %>%
      {.}
  }
  df = df %>%
    dplyr::mutate(`Region_No.` = factor(`Region_No.`, levels = as.character(1:11))) %>%
    dplyr::mutate(`Cat` = factor(`Cat`, levels=c("I", "A", "C", "B", "D", "E"))) %>%
    {.}
  nrecord = nrow(df)
  p = stackbar(df=df, xcol="Region_No.", fillcol="Cat", ylabel="Building Count", xlabel="region",orderByHeight=FALSE,
               pal_values = pal_values, tit=sprintf("%s Building Category Count by Region (n = %s)", year, nrecord))
  print(p)
  ## ## ggsave(file=sprintf("region_report_img/national/cat_cnt_by_region_%s.png", year),
  ## ##        width=8, height=4, units="in")
  p = stackbar(df=df, xcol="Region_No.", fillcol="Cat", ycol="Gross_Sq.Ft", ylabel="Gross_Sq.Ft",xlabel="region",
               orderByHeight=FALSE, pal_values = pal_values,
               tit=sprintf("%s Building Gross_Sq.Ft by Region (n = %s)", year, nrecord))
  print(p)
  ## ## ggsave(file=sprintf("region_report_img/national/sqft_sum_by_region_%s.png", year),
  ## ##        width=8, height=4, units="in")
  p = stackbar(df=df, xcol="Building_Type", fillcol="Cat", ylabel="Building Count", xlabel="Building Type",
               legendOrient="v", pal_values = pal_values,
               tit=sprintf("%s Building Category Count by Building Type (n = %s)", year, nrecord),
               orderByHeight=TRUE)
  print(p)
  p = stackbar(df=df, xcol="Building_Type", fillcol="Cat", ycol="Gross_Sq.Ft", ylabel="Gross_Sq.Ft", xlabel="Building Type",
               legendOrient="v", pal_values = pal_values,
               tit=sprintf("%s Building Category Gross_Sq.Ft by Building Type (n = %s)", year, nrecord),
               orderByHeight=TRUE)
  print(p)
  ## ggsave(file=sprintf("region_report_img/national/cat_cnt_by_type_%s.png", year),
  ##        width=8, height=6, units="in")
}

#' National overview over years
#'
#' This function plots kbtu and cost per sqft over years
#' @param category optional, a subset of A, B, C, D, E, I to include
#' @param type optional, a string (e.g. "Office"), or a string vector (e.g. c("Office", "Courthouse")) of building type
#' @param years optional, the years to plot
#' @keywords query count
#' @export
#' @examples
#' national_overview_over_years(category=c("I", "A"), years=c(2015, 2016, 2017))
national_overview_over_years <- function(category, type, years, pal) {
  df <- db.interface::read_table_from_db(dbname = "all", tablename = "eui_by_fy_tag")
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
  if (!missing(years)) {
    df <- df %>%
      dplyr::filter(`Fiscal_Year` %in% years) %>%
      {.}
  }
  df_agg_eui = gb_agg_ratio(df, groupvar = "Fiscal_Year", numerator_var = c("Electric_(kBtu)", "Gas_(kBtu)", "Oil_(kBtu)", "Steam_(kBtu)", "Chilled_Water_(kBtu)"), denominator_var = "Gross_Sq.Ft", aggfun=sum, valuename="kBtu/sqft", varname="FuelType") %>%
    dplyr::mutate(`FuelType` = gsub("_\\(kBtu\\)", "", `FuelType`)) %>%
    {.}
  if (missing(pal)) {
    pal="Set3"
  }
  p = stackbar(df=df_agg_eui, xcol="Fiscal_Year", fillcol="FuelType", ycol="kBtu/sqft", ylabel="kBtu/sqft",
               xlabel="Fiscal_Year", legendloc = "bottom", legendOrient="h", tit="kBtu / sqft over year",
               orderByHeight=FALSE, labelFormat="%.1f")
  print(p)
  df_agg_cost = gb_agg_ratio(df, groupvar = "Fiscal_Year", numerator_var = c("Electricity_(Cost)", "Gas_(Cost)", "Oil_(Cost)", "Steam_(Cost)", "Chilled_Water_(Cost)"), denominator_var = "Gross_Sq.Ft", aggfun=sum, valuename="Cost/sqft", varname="FuelType") %>%
    dplyr::mutate(`FuelType` = gsub("_\\(Cost\\)", "", `FuelType`)) %>%
    {.}
  p = stackbar(df=df_agg_cost, xcol="Fiscal_Year", fillcol="FuelType", ycol="Cost/sqft", ylabel="Cost/sqft",
               xlabel="Fiscal_Year", legendloc = "bottom", legendOrient="h", tit="Cost / sqft over year",
               orderByHeight=FALSE, labelFormat="%.3f")
  print(p)
  df_agg_eui_type = gb_agg_ratio(df, groupvar = c("Fiscal_Year", "Building_Type", "Region_No."), numerator_var = c("Electric_(kBtu)", "Gas_(kBtu)", "Oil_(kBtu)", "Steam_(kBtu)", "Chilled_Water_(kBtu)"), denominator_var = "Gross_Sq.Ft", aggfun=sum, valuename="kBtu/sqft", varname="FuelType") %>%
    dplyr::mutate(`Region_No.` = factor(`Region_No.`, levels = as.character(1:11))) %>%
    dplyr::mutate(`FuelType` = gsub("_\\(kBtu\\)", "", `FuelType`)) %>%
    {.}
  ## print(knitr::kable(df_agg_eui_type))
  for (btype in c("Office", "Courthouse", "CT/Office", "Other - Public Services")) {
    dftemp = df_agg_eui_type %>%
      dplyr::ungroup() %>%
      dplyr::filter(`Building_Type` == btype) %>%
      dplyr::select(-`Building_Type`) %>%
      {.}
    print(knitr::kable(dftemp))
    p = dftemp %>%
      ggplot2::ggplot(ggplot2::aes(x=`Fiscal_Year`, y=`kBtu/sqft`, fill=`FuelType`, label=sprintf("%.1f", `kBtu/sqft`))) +
      ggplot2::geom_bar(stat="identity", position = "stack") +
      ## ggplot2::ylab(ylabel) +
      ## ggplot2::xlab(xlabel) +
      ggplot2::labs(title=sprintf("EUI trend for %s by region", btype)) +
      ggplot2::geom_text(size=2.5, position = ggplot2::position_stack(vjust = 0.5)) +
      ggplot2::scale_fill_brewer(palette=pal) +
      ggplot2::theme(legend.position="bottom") +
      ggplot2::facet_wrap(~`Region_No.`) +
      ggplot2::theme()
    print(p)
  }
}

## national 2017
## count by type
## sqft by type

## national 2013 to 2017
## eui gas, elec
## cost/sqft

## office eui reduction from 2013 to 2017, each year
## map office eui percent reduction from 2013 to 2017, sort by percent reduction
## office eui by region 2017 (boxplot ish)
