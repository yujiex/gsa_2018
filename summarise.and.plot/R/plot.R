#' Stacked bar plot
#'
#' This function produces a stacked bar plot, with labels for each stacked
#' segment and also the height of the bar. The height labeling part is copied
#' from: https://gist.github.com/svigneau/05148a7031172c2bc70d
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
#' @param width optional, the width of the bars
#' @param verbose if TRUE print data to the output
#' @param scaler optional, if supplied, scale the input
#' @param facetNcol optional, number of columns in facet plot
#' @param labelCutoff optional, upper bound for label to be visible
#' @keywords query count
#' @export
#' @examples
#' stackbar(df=df, xcol="Fiscal_Year", fillcol="Cat", ylabel="Building Count", tit="EUAS Building Count By Category")
stackbar <- function(df, xcol, fillcol, ycol, orderByHeight, ylabel, xlabel, tit, legendloc, legendOrient, pal, pal_values, labelFormat, width, verbose, scaler, facetvar=NULL, facetNcol=NULL, labelCutoff=NULL) {
  ncategory = length(unique(df[[fillcol]]))
  if (missing(labelFormat)) {
    labelFormat = "%s"
  }
  if (missing(ycol)) {
    ## dfcount = plyr::count(df, c(xcol, fillcol))
    dfcount = df %>%
      dplyr::group_by_at(vars(one_of(facetvar, xcol, fillcol))) %>%
      dplyr::summarise(`freq` = n()) %>%
      dplyr::ungroup() %>%
      {.}
    dftotal = df %>%
      dplyr::group_by_at(vars(one_of(facetvar, xcol))) %>%
      dplyr::summarise(`freq` = n()) %>%
      dplyr::ungroup() %>%
      {.}
    if (!is.null(facetvar)) {
      dftotal <- dftotal %>%
        ## dplyr::mutate_if(is.factor, as.character) %>%
        tidyr::complete(!!rlang::sym(facetvar), !!rlang::sym(xcol), fill=list(`freq`=0)) %>%
        {.}
    }
    if (!missing(scaler)) {
      dfcount <- dfcount %>%
        dplyr::mutate(`freq` = scaler * `freq`) %>%
        {.}
      dftotal <- dftotal %>%
        dplyr::mutate(`freq` = scaler * `freq`) %>%
        {.}
    }
    totals = as.vector(dftotal$freq)
    pos <- rep(totals, each=ncategory)
    barHeightLabel <- unlist(lapply(as.character(totals), function(x) c(rep("", ncategory-1),
                                                                        sprintf(labelFormat, as.numeric(x)))))
    ## print(dftotal, n=25)
    ## dftotal %>%
    ##   readr::write_csv("dftotal.csv")
    ## print("-------------------------")
    ## print(totals)
    ## print(pos)
    ## print(barHeightLabel)
    ## print("-------------------------")
    ## print(dfcount, n=25)
    if (!is.null(facetvar)) {
      dfcount <- dfcount %>%
        ## dplyr::mutate_if(is.factor, as.character) %>%
        tidyr::complete(!!rlang::sym(facetvar), !!rlang::sym(xcol), !!rlang::sym(fillcol), fill=list(`freq`=0)) %>%
        {.}
      ## dfcount %>%
      ##   readr::write_csv("dfcount.csv")
      ## print(dfcount, n=25)
    } else {
      dfcount <- dfcount %>%
        ## dplyr::mutate_if(is.factor, as.character) %>%
        tidyr::complete(!!rlang::sym(xcol), !!rlang::sym(fillcol), fill=list(`freq`=0)) %>%
        {.}
    }
    dfcount <- data.frame(dfcount, pos=pos, barHeightLabel=barHeightLabel)
    if (!missing(verbose) && verbose) {
      print(knitr::kable(dfcount))
    }
    dfcount
    if (is.null(labelCutoff)) {
      dfcount = dfcount %>%
        dplyr::mutate(`total_label` = `freq`) %>%
        {.}
    } else {
      dfcount = dfcount %>%
        dplyr::mutate(`total_label` = ifelse(`freq` > labelCutoff, as.character(`freq`), "")) %>%
        {.}
    }
    if (orderByHeight) {
      g = ggplot2::ggplot(dfcount, ggplot2::aes_string(x=sprintf("reorder(%s, -freq)", xcol), y="freq", fill=fillcol)) +
        ggplot2::theme()
    } else {
      g = ggplot2::ggplot(dfcount, ggplot2::aes_string(x=xcol, y="freq", fill=fillcol)) +
        ggplot2::theme()
    }
  } else {
    dfcount = df %>%
      dplyr::group_by_at(vars(one_of(facetvar, xcol, fillcol))) %>%
      dplyr::summarise(`total` = sum(!!rlang::sym(ycol))) %>%
      dplyr::ungroup() %>%
      {.}
    dftotal = df %>%
      dplyr::group_by_at(vars(one_of(facetvar, xcol))) %>%
      dplyr::summarise(`total` = sum(!!rlang::sym(ycol))) %>%
      dplyr::ungroup() %>%
      {.}
    if (!is.null(facetvar)) {
      dftotal <- dftotal %>%
        ## dplyr::mutate_if(is.factor, as.character) %>%
        tidyr::complete(!!rlang::sym(facetvar), !!rlang::sym(xcol), fill=list(`freq`=0)) %>%
        {.}
      ## print(dftotal, n=25)
    }
    if (!missing(scaler)) {
      dfcount <- dfcount %>%
        dplyr::mutate(`total` = scaler * `total`) %>%
        {.}
      dftotal <- dftotal %>%
        dplyr::mutate(`total` = scaler * `total`) %>%
        {.}
    }
    totals = dftotal$total
    pos <- rep(totals, each=ncategory)
    barHeightLabel <- unlist(lapply(as.character(totals), function(x) c(rep("", ncategory-1),
                                                                        sprintf(labelFormat, as.numeric(x)))))
    ## print("pos: ----------")
    ## print(pos)
    ## print("barHeightLabel: ----------")
    ## print(barHeightLabel)
    ## print(dfcount)
    if (!is.null(facetvar)) {
      dfcount <- dfcount %>%
        tidyr::complete(!!rlang::sym(facetvar), !!rlang::sym(xcol), !!rlang::sym(fillcol), fill=list(`total`=0)) %>%
        {.}
    } else {
      dfcount <- dfcount %>%
        tidyr::complete(!!rlang::sym(xcol), !!rlang::sym(fillcol), fill=list(`total`=0)) %>%
        {.}
    }
    ## print(dfcount, n=25)
    dfcount <- data.frame(dfcount, pos=pos, barHeightLabel=barHeightLabel)
    if (!missing(verbose) && verbose) {
      print(knitr::kable(dfcount))
    }
    if (is.null(labelCutoff)) {
      dfcount = dfcount %>%
        dplyr::mutate(`total_label` = sprintf(labelFormat, `total`)) %>%
        {.}
    } else {
      dfcount = dfcount %>%
        dplyr::mutate(`total_label` = ifelse(`total` > labelCutoff, sprintf(labelFormat, `total`), "")) %>%
        {.}
    }
    if (orderByHeight) {
      g = ggplot2::ggplot(dfcount, ggplot2::aes_string(x = sprintf("reorder(%s, -total)", xcol), y="total", fill=fillcol)) +
        ggplot2::theme()
    } else {
      g = ggplot2::ggplot(dfcount, ggplot2::aes_string(x=xcol, y="total", fill=fillcol)) +
        ggplot2::theme()
    }
  }
  if (!missing(width)) {
    g <- g +
      ggplot2::geom_bar(stat="identity", position = "stack", width=width)
  } else {
    g <- g +
      ggplot2::geom_bar(stat="identity", position = "stack")
  }
  g <- g +
    ggplot2::ylab(ylabel) +
    ggplot2::xlab(xlabel) +
    ggplot2::labs(title=tit) +
    ggplot2::geom_text(size=3, ggplot2::aes(y=pos, label=barHeightLabel), fontface = "bold", vjust=-0.5) +
    ggplot2::geom_text(size=2.5, position = ggplot2::position_stack(vjust = 0.5),
                       ggplot2::aes(label=`total_label`)) +
    ggplot2::theme()
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
  if (!is.null(facetvar)) {
    if (!is.null(facetNcol)) {
      g <- g +
        ggplot2::facet_wrap(as.formula(paste("~", facetvar)), ncol=facetNcol)
    } else if (!is.null(facetNrow)){
      g <- g +
        ggplot2::facet_wrap(as.formula(paste("~", facetvar)), nrow=facetNrow)
    } else {
      g <- g +
        ggplot2::facet_wrap(as.formula(paste("~", facetvar)))
    }
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
#' gb_agg_ratio(df, groupvar = "Fiscal_Year", numerator_var = c("Electricity_(Cost)", "Gas_(kBtu)", "Oil_(kBtu)", "Steam__(kBtu)", "Chilled_Water_(kBtu)"), denominator_var = "Gross_Sq.Ft")
gb_agg_ratio <- function(df, groupvar, numerator_var, denominator_var, aggfun, valuename, varname) {
  df_group = df %>%
    dplyr::select(one_of(c(groupvar, numerator_var, denominator_var, numerator_var))) %>%
    dplyr::group_by_at(vars(one_of(groupvar))) %>%
    dplyr::summarise_at(vars(one_of(c(numerator_var, denominator_var))), aggfun) %>%
    tidyr::gather_(varname, valuename, numerator_var) %>%
    dplyr::mutate(!!(rlang::sym(valuename)) := !!(rlang::sym(valuename)) / !!(rlang::sym(denominator_var))) %>%
    dplyr::ungroup() %>%
    {.}
  return(df_group)
}

#' National overview comparing 2 years by region: cnt, eui, sqft
#'
#' This function groups the national level plots, may decide on a filter
#' @param category optional, a subset of A, B, C, D, E, I to include
#' @param type optional, a string (e.g. "Office"), or a string vector (e.g. c("Office", "Courthouse")) of building type
#' @param year optional, the year to plot
#' @param region optional, the region to plot
#' @keywords query count
#' @export
#' @examples
#' national_overview(category=c("A", "C", "I"), year=2017)
national_overview_facetRegion <- function(category, type, years, region) {
  pal_values = c("#FFFFB3", "#8DD3C7")
  ## remove 0 sqft and 0 electricity
  df = db.interface::read_table_from_db(dbname = "all", tablename = "eui_by_fy_tag") %>%
    dplyr::filter(`Gross_Sq.Ft` != 0) %>%
    dplyr::filter(`eui_elec` != 0) %>%
    dplyr::mutate(`Fiscal_Year` = factor(`Fiscal_Year`, levels=years)) %>%
    {.}
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
  if (!missing(region)) {
    df <- df %>%
      dplyr::filter(`Region_No.` == region) %>%
      {.}
  }
  df = df %>%
    dplyr::mutate(`Region_No.` = factor(`Region_No.`, levels = as.character(1:11))) %>%
    dplyr::mutate(`Cat` = factor(`Cat`, levels=c("I", "A"))) %>%
    {.}
  if (missing(region)) {
    regionTag = ""
  } else {
    regionTag = sprintf(", region %s", region)
  }
  ## national level plots
  if (missing(region)) {
    p = stackbar(df=df, xcol="Fiscal_Year", fillcol="Cat", ylabel="Building Count", xlabel="Fiscal Year",orderByHeight=FALSE,legendOrient = "v",legendloc = "bottom",
                pal_values = pal_values, tit="Building Count by Region and Category, 2015 vs 2017", verbose=FALSE, facetvar="Region_No.", facetNcol=11, labelCutoff=10)
    print(p)
    p = stackbar(df=df, xcol="Fiscal_Year", fillcol="Cat", ycol="Gross_Sq.Ft", ylabel="Million Gross_Sq.Ft", xlabel="Fiscal Year",orderByHeight=FALSE,legendOrient = "v",legendloc = "bottom",
                pal_values = pal_values, tit="Building Million Gross_Sq.Ft by Region and Category, 2015 vs 2017", labelFormat="%.0f",
                verbose=FALSE, scaler=1e-6, facetvar="Region_No.", facetNcol=11, labelCutoff=5)
    print(p)
    p = stackbar(df=df, xcol="Fiscal_Year", fillcol="Cat", ylabel="Building Count", xlabel="Fiscal Year",
                 legendOrient="v", pal_values = pal_values,legendloc = "bottom",
                 tit=sprintf("Building Count by Category and Building Type, 2015 vs 2017%s", regionTag),
                 orderByHeight=FALSE, verbose=FALSE, facetvar="Building_Type", facetNcol=length(unique(df$`Building_Type`)), labelCutoff=5)
    print(p)
    p = stackbar(df=df, xcol="Fiscal_Year", fillcol="Cat", ycol="Gross_Sq.Ft", ylabel="Million Gross_Sq.Ft", xlabel="Building Type",
                 legendOrient="v", pal_values = pal_values,legendloc = "bottom",
                 tit=sprintf("Building Million Gross_Sq.Ft by Category and Building Type%s", regionTag),
                 labelFormat="%.0f",
                 orderByHeight=TRUE, verbose=FALSE, scaler=1e-6, facetvar="Building_Type", facetNcol=length(unique(df$`Building_Type`)), labelCutoff=1)
    print(p)
    p = stackbar(df=df, xcol="Fiscal_Year", fillcol="Cat", ycol="Total_(kBtu)", ylabel="Billion Btu", xlabel="Fiscal Year",
                 legendOrient="v", pal_values = pal_values,legendloc = "bottom",
                tit="Building Billion Btu by Region and Category",
                labelFormat="%.0f",
                orderByHeight=TRUE, verbose=FALSE, scaler=1e-6, facetvar="Region_No.", facetNcol=11, labelCutoff=300)
    print(p)
    dftemp = df %>%
      dplyr::mutate(`Total_(Cost)` = `Electricity_(Cost)` + `Steam_(Cost)` + `Oil_(Cost)` + `Gas_(Cost)` + `Chilled_Water_(Cost)`) %>%
      {.}
    p = stackbar(df=dftemp, xcol="Fiscal_Year", fillcol="Cat", ycol="Total_(Cost)", ylabel="Million Dollars", xlabel="Fiscal Year",
                 legendOrient="v", pal_values = pal_values,legendloc = "bottom",
                tit="Building Million Dollars by Region and Category",
                labelFormat="%.0f",
                orderByHeight=TRUE, verbose=FALSE, scaler=1e-6, facetvar="Region_No.", facetNcol=11, labelCutoff=5)
    print(p)
    df_agg_eui_region = gb_agg_ratio(df, groupvar = c("Fiscal_Year", "Region_No."), numerator_var = c("Electric_(kBtu)", "Gas_(kBtu)", "Oil_(kBtu)", "Steam_(kBtu)", "Chilled_Water_(kBtu)", "Other_(kBtu)"), denominator_var = "Gross_Sq.Ft", aggfun=sum, valuename="kBtu/sqft", varname="FuelType") %>%
      dplyr::mutate(`Region_No.` = factor(`Region_No.`, levels = as.character(1:11))) %>%
      dplyr::mutate(`FuelType` = gsub("_\\(kBtu\\)", "", `FuelType`)) %>%
      dplyr::mutate(`FuelType`=factor(`FuelType`, levels=c("Gas", "Oil", "Steam", "Chilled_Water", "Electric", "Other"))) %>%
      {.}
    p = stackbar(df=df_agg_eui_region, xcol="Fiscal_Year", fillcol="FuelType", ycol="kBtu/sqft", ylabel="kBtu/sqft",
                 xlabel="Fiscal Year",legendloc = "bottom",
                 legendOrient="v", pal_values = c("#F2B670", "#FFEEBC", "#EB8677", "#BDBBD7", "#8AB0D0", "gray"),
                 tit="kBtu/sqft by region, 2015 vs 2017",
                 labelFormat="%.0f",
                 orderByHeight=FALSE, verbose=FALSE, facetvar="Region_No.", facetNcol=11, labelCutoff=5)
    print(p)
    ## get savings
    dftemp = df_agg_eui_region %>%
      dplyr::ungroup() %>%
      dplyr::mutate(`Fiscal_Year` = substr(`Fiscal_Year`, 1, 4)) %>%
      dplyr::filter(`Fiscal_Year` %in% c(2015, 2017)) %>%
      {.}
    dfsummary = dftemp %>% dplyr::group_by(`Fiscal_Year`, `Region_No.`) %>%
      dplyr::select(-`Gross_Sq.Ft`) %>%
      dplyr::summarise_if(is.numeric, funs(sum)) %>%
      dplyr::ungroup() %>%
      tidyr::spread(`Fiscal_Year`, `kBtu/sqft`) %>%
      dplyr::mutate(`percent_saving` = (`2017` - `2015`) / `2015` * 100) %>%
      {.}
    print("change from 2015 to 2017")
    dfsummary %>%
      print(knitr::kable(dfsummary))
  } else {
    p = stackbar(df=df, xcol="Fiscal_Year", fillcol="Cat", ylabel="Building Count", xlabel="Fiscal Year",
                 legendOrient="v", pal_values = pal_values,legendloc = "bottom",
                tit=sprintf("Building Count by Category and Building Type, 2015 vs 2017%s", regionTag),
                orderByHeight=TRUE, verbose=FALSE, facetvar="Building_Type", facetNcol=length(unique(df$`Building_Type`)), labelCutoff=5)
    print(p)
    p = stackbar(df=df, xcol="Fiscal_Year", fillcol="Cat", ycol="Gross_Sq.Ft", ylabel="Million Gross_Sq.Ft", xlabel="Building Type",
                 legendOrient="v", pal_values = pal_values,legendloc = "bottom",
                tit=sprintf("Building Million Gross_Sq.Ft by Category and Building Type%s", regionTag),
                labelFormat="%.0f",
                orderByHeight=TRUE, verbose=FALSE, scaler=1e-6, facetvar="Building_Type", facetNcol=length(unique(df$`Building_Type`)), labelCutoff=1)
    print(p)
  }
}

#' Get the subject building set
#'
#' This function gets the subject building set
#' @param category optional, a subset of A, B, C, D, E, I to include
#' @param type optional, a string (e.g. "Office"), or a string vector (e.g. c("Office", "Courthouse")) of building type
#' @param year optional, the year to plot
#' @param region optional, the region to plot
#' @keywords query count
#' @export
#' @examples
#'get_filter_set(category=c("A", "I"), year=2017, region="9")
get_filter_set <- function(category, type, year, region) {
  ## remove 0 sqft and 0 electricity
  df = db.interface::read_table_from_db(dbname = "all", tablename = "eui_by_fy_tag") %>%
    dplyr::filter(`Gross_Sq.Ft` != 0) %>%
    dplyr::filter(`eui_elec` != 0) %>%
    {.}
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

#' National overview: cnt, eui, sqft
#'
#' This function groups the national level plots, may decide on a filter
#' @param category optional, a subset of A, B, C, D, E, I to include
#' @param type optional, a string (e.g. "Office"), or a string vector (e.g. c("Office", "Courthouse")) of building type
#' @param year optional, the year to plot
#' @param region optional, the region to plot
#' @keywords query count
#' @export
#' @examples
#' national_overview(category=c("A", "C", "I"), year=2017)
national_overview <- function(category, type, year, region, pal_values) {
  ## remove 0 sqft and 0 electricity
  df = db.interface::read_table_from_db(dbname = "all", tablename = "eui_by_fy_tag") %>%
    dplyr::filter(`Gross_Sq.Ft` != 0) %>%
    dplyr::filter(`eui_elec` != 0) %>%
    {.}
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
  if (missing(region)) {
    regionTag = ""
  } else {
    regionTag = sprintf(", region %s", region)
  }
  df = df %>%
    dplyr::mutate(`Region_No.` = factor(`Region_No.`, levels = as.character(1:11))) %>%
    dplyr::mutate(`Cat` = factor(`Cat`, levels=c("I", "A"))) %>%
    {.}
  p = stackbar(df=df, xcol="Fiscal_Year", fillcol="Cat", ylabel="Building Count", legendloc = "bottom", xlabel="Fiscal Year",orderByHeight=FALSE,
               pal_values = pal_values, tit="Building Category Count by Fiscal Year", verbose=FALSE, labelCutoff=5)
  print(p)
  p = stackbar(df=df, xcol="Fiscal_Year", fillcol="Cat", ycol="Gross_Sq.Ft", ylabel="Million Gross_Sq.Ft", legendloc = "bottom", xlabel="Fiscal Year",orderByHeight=FALSE,
               pal_values = pal_values, tit="Building Million Gross_Sq.Ft by Fiscal Year", labelFormat="%.0f",
               verbose=FALSE, scaler=1e-6, labelCutoff=5)
  print(p)
  if (!missing(year)) {
    df <- df %>%
      dplyr::filter(`Fiscal_Year` == year) %>%
      {.}
  }
  nrecord = nrow(df)
  if (missing(region)) {
    p = stackbar(df=df, xcol="Region_No.", fillcol="Cat", ylabel="Building Count", xlabel="region",orderByHeight=FALSE,
                pal_values = pal_values, tit=sprintf("%s Building Category Count by Region (n = %s)", year, nrecord),
                verbose=FALSE, labelCutoff=5)
    print(p)
    ## ## ## ggsave(file=sprintf("region_report_img/national/cat_cnt_by_region_%s.png", year),
    ## ## ##        width=8, height=4, units="in")
    p = stackbar(df=df, xcol="Region_No.", fillcol="Cat", ycol="Gross_Sq.Ft", ylabel="Million Gross_Sq.Ft",xlabel="region",
                orderByHeight=FALSE, pal_values = pal_values,
                tit=sprintf("%s Building Million Gross_Sq.Ft by Region (n = %s)", year, nrecord), labelFormat="%.0f",
                verbose=FALSE, scaler=1e-6, labelCutoff=5)
    print(p)
  } else {
    p = stackbar(df=df, xcol="Building_Type", fillcol="Cat", ylabel="Building Count", xlabel="Building Type",
                legendOrient="v", pal_values = pal_values,
                tit=sprintf("%s Building Category Count by Building Type (n = %s)%s", year, nrecord, regionTag),
                orderByHeight=TRUE, verbose=FALSE, labelCutoff=5)
    print(p)
    p = stackbar(df=df, xcol="Building_Type", fillcol="Cat", ycol="Gross_Sq.Ft", ylabel="Million Gross_Sq.Ft", xlabel="Building Type",
                legendOrient="v", pal_values = pal_values,
                tit=sprintf("%s Building Category Million Gross_Sq.Ft by Building Type (n = %s)%s", year, nrecord, regionTag),
                labelFormat="%.0f",
                orderByHeight=TRUE, verbose=FALSE, scaler=1e-6, labelCutoff=5)
    print(p)
  }
}

#' Potential dollar saving based on median
#'
#' This function plots the potential dollar savings based on some median eui
#' @param category optional, a subset of A, B, C, D, E, I to include
#' @param type optional, a string (e.g. "Office"), or a string vector (e.g. c("Office", "Courthouse")) of building type
#' @param years optional, the years to plot
#' @param region optional, the region to plot
#' @param reference optional, cbecs, own, or pm
#' @keywords dollar saving median
#' @export
#' @examples
#' dollar_saving(category=c("I", "A"), year=2017, region="9")
dollar_saving <- function(category, type, year, region, method="own") {
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
      dplyr::summarise(`eui_median` = median(`eui_total`)) %>%
      readr::write_csv(sprintf("csv_FY/eui_median_region_%s.csv", region))
  } else if (method == "cbecs") {
    ## read median table of cbecs
    df_median = readr::read_csv("csv_FY/national_medial.csv") %>%
      na.omit() %>%
      dplyr::select(-`PM_type`) %>%
      {.}
    df <- df %>%
      dplyr::left_join(df_median, by="Building_Type") %>%
      {.}
    df %>%
      readr::write_csv(sprintf("csv_FY/join_pm_median_%s_region%s.csv", year, region))
  }
  df <- df %>%
    dplyr::mutate(`Potential_Saving` = (`eui_total` - `eui_median`) * `Gross_Sq.Ft` * (`Total_(Cost)` / `Total_(kBtu)`)) %>%
    dplyr::mutate(`Potential_Saving` = `Potential_Saving` * 1e-6) %>%
    dplyr::mutate(`Building_Number` = ifelse(`Cat` == "I", sprintf("(I) %s", `Building_Number`), `Building_Number`)) %>%
      ## readr::write_csv(sprintf("csv_FY/dollar_saving_own_type_median_%s_region%s.csv", year, region))
    {.}
  p <- df %>%
    ggplot2::ggplot(aes(x = reorder(`Building_Number`, -`Potential_Saving`), y=`Potential_Saving`)) +
    ggplot2::geom_bar(stat="identity") +
    ## ggplot2::facet_grid(~ `Building_Type`) +
    ggplot2::coord_flip() +
    ggplot2::ylab("Million Dollar") +
    ggplot2::xlab("Building Number") +
    ggplot2::ggtitle(sprintf("Potential dollar saving%s (%s)", regionTag, method)) +
    ggplot2::theme_bw()
    ## head() %>%
  print(p)
  if (missing(region)) {
    ggsave(sprintf(file="region_report_img/regional/%s_median_potential_dollar_", method))
  } else {
    ggsave(file=sprintf("region_report_img/regional/%s_median_potential_dollar_region_%s.png", method, region),
           width=5, height=8, units = "in")
  }
}

#' National overview by years
#'
#' This function plots kbtu and cost per sqft by years
#' @param category optional, a subset of A, B, C, D, E, I to include
#' @param type optional, a string (e.g. "Office"), or a string vector (e.g. c("Office", "Courthouse")) of building type
#' @param years optional, the years to plot
#' @param region optional, the region to plot
#' @param pal optional, plotting palette
#' @keywords query count
#' @export
#' @examples
#' national_overview_over_years(category=c("I", "A"), years=c(2015, 2016, 2017))
national_overview_over_years <- function(category, type, years, region, pal) {
  df <- db.interface::read_table_from_db(dbname = "all", tablename = "eui_by_fy_tag") %>%
    dplyr::filter(`Gross_Sq.Ft` != 0) %>%
    dplyr::filter(`eui_elec` != 0) %>%
    {.}
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
  if (!missing(region)) {
    df <- df %>%
      dplyr::filter(`Region_No.` == region) %>%
      {.}
  }
  if (missing(region)) {
    regionTag = ""
  } else {
    regionTag = sprintf(", region %s", region)
  }
  if (missing(pal)) {
    pal="Set3"
  }
  ## total cost and energy drop without divided by sqft
  df %>% dplyr::group_by(`Fiscal_Year`) %>%
    dplyr::summarise_if(is.numeric, funs(sum)) %>%
    readr::write_csv("csv_FY/total_by_fiscal_year.csv")
  dfsummary = df %>% dplyr::group_by(`Fiscal_Year`) %>%
    dplyr::summarise(`count`=n()) %>%
      {.}
  df <- df %>%
    dplyr::left_join(dfsummary) %>%
    dplyr::mutate(`Fiscal_Year` = sprintf("%s\n(n = %s)", `Fiscal_Year`, `count`)) %>%
    {.}
  ## print(head(df))
  ## plot bar width
  width = 0.4
  df_agg_eui = gb_agg_ratio(df, groupvar = "Fiscal_Year", numerator_var = c("Electric_(kBtu)", "Gas_(kBtu)", "Oil_(kBtu)", "Steam_(kBtu)", "Chilled_Water_(kBtu)", "Other_(kBtu)"), denominator_var = "Gross_Sq.Ft", aggfun=sum, valuename="kBtu/sqft", varname="FuelType") %>%
    dplyr::mutate(`FuelType` = gsub("_\\(kBtu\\)", "", `FuelType`)) %>%
    dplyr::mutate(`FuelType`=factor(`FuelType`, levels=c("Gas", "Oil", "Steam", "Chilled_Water", "Electric", "Other"))) %>%
    {.}
  height_of_bar = df_agg_eui %>%
    dplyr::mutate(`Fiscal_Year` = substr(`Fiscal_Year`, 1, 4)) %>%
    dplyr::group_by(`Fiscal_Year`) %>%
    dplyr::summarise(`kBtu/sqft` = sum(`kBtu/sqft`)) %>%
    {.}
  ## print(height_of_bar)
  titleStr = "kBtu / sqft by year"
  if (!missing(region)) {
    titleStr = paste("kBtu / sqft by year, region", region)
  }
  p = stackbar(df=df_agg_eui, xcol="Fiscal_Year", fillcol="FuelType", ycol="kBtu/sqft", ylabel="kBtu/sqft",
               xlabel="Fiscal Year", legendloc = "bottom", legendOrient="h",
               tit=sprintf("kBtu/sqft by year%s", regionTag),
               orderByHeight=FALSE, labelFormat="%.0f", width=width, verbose=FALSE,
               pal_values = c("#F2B670", "#FFEEBC", "#EB8677", "#BDBBD7", "#8AB0D0", "grey"), labelCutoff=5)
  print(p)
  df_agg_cost = gb_agg_ratio(df, groupvar = "Fiscal_Year", numerator_var = c("Electricity_(Cost)", "Gas_(Cost)", "Oil_(Cost)", "Steam_(Cost)", "Chilled_Water_(Cost)", "Other_(Cost)"), denominator_var = "Gross_Sq.Ft", aggfun=sum, valuename="Cost/sqft", varname="FuelType") %>%
    dplyr::mutate(`FuelType` = gsub("_\\(Cost\\)", "", `FuelType`)) %>%
    {.}
  ## print(head(df_agg_cost))
  dflong_cost <- df %>%
    dplyr::select(`Fiscal_Year`, `Electricity_(Cost)`, `Gas_(Cost)`, `Oil_(Cost)`, `Steam_(Cost)`, `Chilled_Water_(Cost)`, `Other_(Cost)`) %>%
    dplyr::group_by(`Fiscal_Year`) %>%
    dplyr::summarise_at(vars(`Electricity_(Cost)`, `Gas_(Cost)`, `Oil_(Cost)`, `Steam_(Cost)`, `Chilled_Water_(Cost)`, `Other_(Cost)`), sum) %>%
    dplyr::ungroup() %>%
    tidyr::gather(`FuelType`, `Cost`, `Electricity_(Cost)`:`Other_(Cost)`) %>%
    dplyr::mutate(`FuelType` = gsub("_\\(Cost\\)", "", `FuelType`)) %>%
    dplyr::mutate(`FuelType`=factor(`FuelType`, levels=c("Gas", "Oil", "Steam", "Chilled_Water", "Electricity", "Other"))) %>%
    {.}
  p = stackbar(df=dflong_cost, xcol="Fiscal_Year", fillcol="FuelType", ycol="Cost", ylabel="Million Dollar",
               xlabel="Fiscal Year", legendloc = "bottom", legendOrient="h",
               tit=sprintf("Energy cost (million dollar) by year%s", regionTag),
               orderByHeight=FALSE, labelFormat="%.0f", width=width, verbose=FALSE,
               pal_values = c("#F2B670", "#FFEEBC", "#EB8677", "#BDBBD7", "#8AB0D0", "grey"), scaler=1e-6, labelCutoff=5)
  print(p)
  dflong_kBtu <- df %>%
    dplyr::select(`Fiscal_Year`, `Electric_(kBtu)`, `Gas_(kBtu)`, `Oil_(kBtu)`, `Steam_(kBtu)`, `Chilled_Water_(kBtu)`, `Other_(kBtu)`) %>%
    dplyr::group_by(`Fiscal_Year`) %>%
    dplyr::summarise_at(vars(`Electric_(kBtu)`, `Gas_(kBtu)`, `Oil_(kBtu)`, `Steam_(kBtu)`, `Chilled_Water_(kBtu)`, `Other_(kBtu)`), sum) %>%
    dplyr::ungroup() %>%
    tidyr::gather(`FuelType`, `kBtu`, `Electric_(kBtu)`:`Other_(kBtu)`) %>%
    dplyr::mutate(`FuelType` = gsub("_\\(kBtu\\)", "", `FuelType`)) %>%
    dplyr::mutate(`FuelType`=factor(`FuelType`, levels=c("Gas", "Oil", "Steam", "Chilled_Water", "Electric", "Other"))) %>%
    {.}
  p = stackbar(df=dflong_kBtu, xcol="Fiscal_Year", fillcol="FuelType", ycol="kBtu", ylabel="Billion Btu",
               xlabel="Fiscal Year", legendloc = "bottom", legendOrient="h",
               tit=sprintf("Billion Btu by year%s", regionTag),
               orderByHeight=FALSE, labelFormat="%.0f", width=width, verbose=FALSE,
               pal_values = c("#F2B670", "#FFEEBC", "#EB8677", "#BDBBD7", "#8AB0D0", "grey"), scaler=1e-6, labelCutoff=300)
  print(p)
  height_of_bar = df_agg_cost %>%
    ## dplyr::mutate(`Fiscal_Year` = substr(`Fiscal_Year`, 1, 4)) %>%
    dplyr::group_by(`Fiscal_Year`) %>%
    dplyr::summarise(`Cost/sqft` = sum(`Cost/sqft`)) %>%
    dplyr::mutate(`FuelType` = "total") %>%
    {.}
  df_agg_cost <- df_agg_cost %>%
    dplyr::bind_rows(height_of_bar) %>%
    {.}
  ## print(head(height_of_bar))
  p <- df_agg_cost %>%
    dplyr::select(-`Gross_Sq.Ft`) %>%
    ## only plot the total
    dplyr::filter(`FuelType` == "total") %>%
    ggplot2::ggplot(ggplot2::aes(x=`Fiscal_Year`, y=`Cost/sqft`, group=`FuelType`, colour=`FuelType`, label=sprintf("%.3f", `Cost/sqft`))) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_text(vjust=-0.5) +
    ## ggplot2::scale_color_brewer(palette=pal) +
    ggplot2::theme(legend.position="bottom") +
    ggplot2::ylim(c(0, 3)) +
    ## ggplot2::scale_color_manual(values=c("#F2B670", "#FFEEBC", "#EB8677", "#BDBBD7", "#8AB0D0", "gray"))
    ggplot2::ggtitle(sprintf("Cost / sqft by year by fuel type%s", regionTag)) +
    ggplot2::theme_bw()
  print(p)
  ## single building kbtu/sqft distribution
  df_singlebuilding <- df %>%
    dplyr::select(`Building_Number`, `Fiscal_Year`, `Electric_(kBtu)`, `Gas_(kBtu)`, `Oil_(kBtu)`, `Steam_(kBtu)`,
                  `Chilled_Water_(kBtu)`, `Gross_Sq.Ft`) %>%
    tidyr::gather(`FuelType`, `kBtu`, `Electric_(kBtu)`:`Chilled_Water_(kBtu)`) %>%
    dplyr::group_by(`Building_Number`, `Fiscal_Year`, `Gross_Sq.Ft`) %>%
    dplyr::summarise(`totalkBtu` = sum(`kBtu`)) %>%
    dplyr::ungroup() %>%
    ## dplyr::mutate(`FuelType` = gsub("_\\(kBtu\\)", "", `FuelType`)) %>%
    dplyr::mutate(`kBtu/sqft` = `totalkBtu` / `Gross_Sq.Ft`) %>%
    {.}
  ## ## print(head(df_singlebuilding))
  p <-
    df_singlebuilding %>%
    dplyr::group_by(`Fiscal_Year`) %>%
    dplyr::summarise(`0` = quantile(`kBtu/sqft`)[1],
                     `25` = quantile(`kBtu/sqft`)[2],
                     `50` = quantile(`kBtu/sqft`)[3],
                     `75` = quantile(`kBtu/sqft`)[4],
                     `100` = quantile(`kBtu/sqft`)[5]) %>%
    dplyr::ungroup() %>%
    tidyr::gather(`quantile`, `value`, `0`:`100`) %>%
    dplyr::mutate(`quantile` = as.numeric(`quantile`)) %>%
    ## head() %>%
    ggplot2::ggplot(ggplot2::aes(x=`Fiscal_Year`, y=`value`, group=`quantile`, colour=`quantile`)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::ylab("kBtu/sqft distribution") +
    ggplot2::ggtitle("Distribution of kBtu/sqft for individual buildings") %>%
    {.}
  print(p)
  ## ## singe building cost/sqft distribution
  ## df_singlebuilding <- df %>%
  ##   dplyr::select(`Building_Number`, `Fiscal_Year`, `Electricity_(Cost)`, `Gas_(Cost)`, `Oil_(Cost)`, `Steam_(Cost)`,
  ##                 `Chilled_Water_(Cost)`, `Gross_Sq.Ft`) %>%
  ##   tidyr::gather(`FuelType`, `Cost`, `Electricity_(Cost)`:`Chilled_Water_(Cost)`) %>%
  ##   dplyr::group_by(`Building_Number`, `Fiscal_Year`, `Gross_Sq.Ft`) %>%
  ##   dplyr::summarise(`totalCost` = sum(`Cost`)) %>%
  ##   dplyr::ungroup() %>%
  ##   ## dplyr::mutate(`FuelType` = gsub("_\\(Cost\\)", "", `FuelType`)) %>%
  ##   dplyr::mutate(`Cost/Sqft` = `totalCost` / `Gross_Sq.Ft`) %>%
  ##   {.}
  ## ## print(head(df_singlebuilding))
  ## p <- df_singlebuilding %>%
  ##   ggplot2::ggplot(ggplot2::aes(x=`Fiscal_Year`, y=`Cost/Sqft`)) +
  ##   ggplot2::geom_boxplot() +
  ##   ggplot2::ylab("Cost/sqft distribution") +
  ##   ggplot2::ggtitle("Distribution of cost/sqft for individual buildings")
  ## print(p)
  ## following comment out
  ## p = stackbar(df=df_agg_cost, xcol="Fiscal_Year", fillcol="FuelType", ycol="Cost/sqft", ylabel="Cost/sqft",
  ##              xlabel="Fiscal Year", legendloc = "bottom", legendOrient="h", tit="Cost / sqft by year",
  ##              orderByHeight=FALSE, labelFormat="%.3f", width=width, verbose=FALSE)
  ## print(p)
  ## this is temp
  ## df %>%
  ##   ggplot2::ggplot(ggplot2::aes(x=`Fiscal_Year`)) +
  ##   ggplot2::geom_bar(stat="count") +
  ##   print()
  ## this is temp ended
  ## ## print(knitr::kable(df_agg_eui_type))
  ## ## for (btype in c("Office")) {
  ## for (btype in c("Office", "Courthouse", "CT/Office", "Other - Public Services")) {
  ##   dftemp = df_agg_eui_type %>%
  ##     dplyr::ungroup() %>%
  ##     dplyr::mutate(`Fiscal_Year` = substr(`Fiscal_Year`, 1, 4)) %>%
  ##     dplyr::filter(`Building_Type` == btype) %>%
  ##     dplyr::filter(`Fiscal_Year` %in% c(2015, 2017)) %>%
  ##     dplyr::select(-`Building_Type`) %>%
  ##     {.}
  ##   dfsummary = dftemp %>% dplyr::group_by(`Fiscal_Year`, `Region_No.`) %>%
  ##     dplyr::select(-`Gross_Sq.Ft`) %>%
  ##     dplyr::summarise_if(is.numeric, funs(sum)) %>%
  ##     dplyr::ungroup() %>%
  ##     tidyr::spread(`Fiscal_Year`, `kBtu/sqft`) %>%
  ##     dplyr::mutate(`percent_saving` = (`2017` - `2015`) / `2015` * 100) %>%
  ##     {.}
  ##       ## readr::write_csv("csv_FY/office_eui.csv")
  ##   ## dfsummary %>%
  ##   print(knitr::kable(dfsummary))
  ##   p = dftemp %>%
  ##     ggplot2::ggplot(ggplot2::aes(x=`Fiscal_Year`, y=`kBtu/sqft`, fill=`FuelType`, label=sprintf("%.0f", `kBtu/sqft`))) +
  ##     ggplot2::geom_bar(stat="identity", position = "stack", width=width) +
  ##     ## ggplot2::ylab(ylabel) +
  ##     ## ggplot2::xlab(xlabel) +
  ##     ggplot2::labs(title=sprintf("EUI trend for %s by region", btype)) +
  ##     ggplot2::geom_text(size=2.5, position = ggplot2::position_stack(vjust = 0.5)) +
  ##     ggplot2::scale_fill_brewer(palette=pal) +
  ##     ggplot2::theme(legend.position="bottom") +
  ##     ggplot2::facet_wrap(~`Region_No.`) +
  ##     ggplot2::theme()
  ##   print(p)
  ## }
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
