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
#' @param legendloc legend location
#' @param legendOrient legend orientation: "v" or "h"
#' @param pal color palette
#' @param pal_values colors in the palette to use
#' @param aggfun count or sum
#' @keywords query count
#' @export
#' @examples
#' stackbar(df=df, xcol="Fiscal_Year", fillcol="Cat", ylabel="Building Count", tit="EUAS Building Count By Category")
stackbar <- function(df, xcol, fillcol, ycol, orderByHeight, ylabel, xlabel, tit, legendloc, legendOrient, pal, pal_values) {
  if (missing(ycol)) {
    dfcount = plyr::count(df, c(xcol, fillcol))
  } else {
    dfcount = df %>%
      dplyr::group_by(!!rlang::sym(xcol), !!rlang::sym(fillcol)) %>%
      dplyr::summarise(`total` = sum(!!rlang::sym(ycol))) %>%
      {.}
  }
  print(head(dfcount))
  ## dfcount <- transform(dfcount, mid_y = ave(dfcount$freq, dfcount[,xcol], FUN = function(val) cumsum(val) - (0.5 * val)))
  ## print(head(dfcount))
  if (missing(ycol)) {
    if (orderByHeight) {
      g = ggplot2::ggplot(dfcount, ggplot2::aes_string(x=sprintf("reorder(%s, -freq)", xcol), y="freq", fill=fillcol, label="freq")) +
        ggplot2::theme()
    } else {
      g = ggplot2::ggplot(dfcount, ggplot2::aes_string(x=xcol, y="freq", fill=fillcol, label="freq")) +
        ggplot2::theme()
    }
  } else {
    if (orderByHeight) {
      g = ggplot2::ggplot(dfcount, ggplot2::aes_string(x = sprintf("reorder(%s, -total)", xcol), y="total", fill=fillcol, label="total")) +
        ggplot2::theme()
    } else {
      g = ggplot2::ggplot(dfcount, ggplot2::aes_string(x=xcol, y="total", fill=fillcol, label="total")) +
        ggplot2::theme()
    }
  }
  g <- g +
    ggplot2::geom_bar(stat="identity") +
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
national_overview <- function(category, type, year) {
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
               pal_values = c("#FFFFB3", "#8DD3C7"), tit=sprintf("%s Building Category Count by Region", year))
  ggsave(file=sprintf("region_report_img/national/cat_cnt_by_region_%s.png", year),
         width=8, height=4, units="in")
  p = stackbar(df=df, xcol="Region_No.", fillcol="Cat", ycol="Gross_Sq.Ft", ylabel="Gross_Sq.Ft",xlabel="region",
               orderByHeight=FALSE, pal_values = c("#FFFFB3", "#8DD3C7"),
               tit=sprintf("%s Building Gross_Sq.Ft by Region", year))
  print(p)
  ggsave(file=sprintf("region_report_img/national/sqft_sum_by_region_%s.png", year),
         width=8, height=4, units="in")
  p = stackbar(df=df, xcol="Building_Type", fillcol="Cat", ylabel="Building Count", xlabel="Building Type",
               legendOrient="v", pal_values = c("#FFFFB3", "#8DD3C7"),
               tit=sprintf("%s Building Category Count by Building Type (n = %s)", year, nrecord),
               orderByHeight=TRUE)
  ggsave(file=sprintf("region_report_img/national/cat_cnt_by_type_%s.png", year),
         width=8, height=6, units="in")
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
