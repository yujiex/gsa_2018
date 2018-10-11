#'@importFrom magrittr %>%
NULL
#' Get the count of buildings with region, category (A, C, I), type (office, etc.), and year filters applied
#'
#' This function returns the count group by region, category, and type, if any
#' of the parameters are specified, they will be used in filtering the results
#' @param region optional, a string vector of region numbers, or a single
#'   region number
#' @param category optional, a subset of A, B, C, D, E, I
#' @param type optional, a string (e.g. "Office"), or a string vector (e.g. c("Office", "Courthouse"))of building type
#' @param year optional, a double vector of years, or a single year
#' @param fOrC optional, specify one of "F" (fiscal year) or "C" (calendar year), default to "F"
#' @param gbvars optional, if unspecified, group by all 4 variables, otherwise, only group by the ones specified in gbvars
#' @keywords query count
#' @export
#' @examples
#' get_count(region=1, category=c("A", "C", "I"), "office", "F", gbvars="Cat")
get_count <- function(region, category, type, year, fOrC, gbvars) {
  if (missing(fOrC) || (fOrC == "F")) {
    year_col = "Fiscal_Year"
  } else {
    year_col = "year"
  }
  con = connect("all")
  df =
    dbGetQuery(con, sprintf("SELECT [Region_No.], %s, Cat, Building_Type, lowElectricity, lowGas, highEnoughELectricityGas, zeroSqft FROM eui_by_fy_tag", year_col)) %>%
    as_data_frame() %>%
    {.}
  print(head(df))
  if (!missing(region)) {
    region = as.character(region)
    if (is.vector(region)) {
      df = df %>%
        dplyr::filter(`Region_No.` %in% region) %>%
        {.}
    } else {
      df = df %>%
        dplyr::filter(`Region_No.` == region) %>%
        {.}
    }
  }
  print(head(df))
  if (!missing(category)) {
    df = df %>%
      dplyr::filter(`Cat` %in% category) %>%
      {.}
  }
  print(head(df))
  if (!missing(type)) {
    df = df %>%
      dplyr::filter(`Building_Type` %in% type) %>%
      {.}
  }
  print(head(df))
  if (!missing(year)) {
    if (is.vector(year)) {
      df = df %>%
        dplyr::filter_(sprintf("%s %%in%% c(%s)", year_col, paste(as.character(year), collapse = ","))) %>%
        {.}
    } else {
      df = df %>%
        dplyr::filter_(sprintf("%s == %s", year_col, year)) %>%
        {.}
    }
  }
  print(head(df))
  if (missing(gbvars)) {
    df = df %>%
      dplyr::group_by(`Region_No.`, `Fiscal_Year`, `Cat`, `Building_Type`) %>%
      {.}
  } else {
    df = df %>%
      dplyr::group_by_at(vars(one_of(gbvars))) %>%
      {.}
  }
  df <- df %>%
    dplyr::summarise(count = n(),
                     lowElectricity_n = sum(lowElectricity),
                     lowGas_n = sum(lowGas),
                     highEnoughElectricityGas_n = sum(highEnoughElectricityGas),
                     zeroSqft_n = sum(zeroSqft)) %>%
    {.}
  return(df)
}

main_summary <- function() {
  ## get_count() %>%
  ##   readr::write_csv("csv_FY/summary_results/count_by_region_year_cat_type.csv")
  ## get_count(region=1, category = c("A", "C", "I"), type = "Office", year = c(2014, 2015, 2016), fOrC = "F") %>%
  ##   readr::write_csv("csv_FY/summary_results/count_by_region1_year_cat_type.csv")
  ## region_report <- function(region) {
  ##   print(sprintf("Number of buildings in category A and I: %s"))
  ##   print(sprintf("Number of buildings in category A, C and I: %s"))
  ## }
}

#' ROI for an individual building
#'
#' This function computes ROI for an individual building
#' @param manual_legend_order optional, manual order of legend
#' @keywords roi
#' @export
#' @examples
#' roi(building="IN1703ZZ")
roi <- function(building=building) {
  ecm = db.interface::read_table_from_db(dbname="all", tablename="EUAS_ecm",
                                         cols=c("Building_Number", "high_level_ECM", "Substantial_Completion_Date")) %>%
    tibble::as_data_frame() %>%
    dplyr::filter(`Building_Number`==building) %>%
    dplyr::group_by(`Substantial_Completion_Date`) %>%
    dplyr::summarise(allECM = paste0(`high_level_ECM`, collapse = ";")) %>%
    {.}
  energy = db.interface::read_table_from_db(dbname="all", tablename="EUAS_monthly",
                                            cols=c("Fiscal_Year", "Fiscal_Month", "year", "month",
                                                   "eui_elec", "eui_gas", "Gross_Sq.Ft", "Electric_(kBtu)",
                                                   "Gas_(kBtu)"), building=building) %>%
    tibble::as_data_frame() %>%
    dplyr::mutate(`Date`=zoo::as.Date(zoo::as.yearmon(paste(`year`, `month`), "%Y %m"))) %>%
    {.}
  dfPre = dfElec %>%
    dplyr::filter(`Date` < projectDate) %>%
    dplyr::mutate(`status` = "pre") %>%
    tail(n=36) %>%
    {.}
  dfPre %>%
    readr::write_csv(paste0("csv_FY/tempResultsInRoi/", building, "_elec_pre.csv"))

  dfPost = dfElec %>%
    dplyr::filter(`Date` > projectDate) %>%
    dplyr::mutate(`status` = "post") %>%
    head(n=36) %>%
    {.}
  ## dfPre %>% ggplot(aes(y=`Electric`, x=`ave`)) +
  ##   geom_point() +
  ##   xlab("Monthly average temperature") +
  ##   geom_smooth(method="loess")
  le = loess(Electric ~ ave, data=dfPre)
  summary(le)
  dfPost %>%
    dplyr::mutate(`baseline`=predict(le, ave)) %>%
    dplyr::rename(`actual`=`Electric`) %>%
    readr::write_csv(paste0("csv_FY/tempResultsInRoi/", building, "_elec_post.csv"))
  ## dfSaving = dfPost %>%
  ##   dplyr::mutate(`baseline`=predict(le, ave)) %>%
  ##   dplyr::rename(`actual`=`Electric`) %>%
  ## elecPercent = sum(dfSaving$actual) - sum
  temp = dfPost %>%
    dplyr::mutate(`baseline`=predict(le, ave)) %>%
    dplyr::rename(`actual`=`Electric`) %>%
    dplyr::select(`actual`, `baseline`, `ave`, `Date`) %>%
    na.omit() %>%
    reshape2::melt(id.vars=c("ave", "Date"), variable.name="period", value.name="Electric") %>%
    {.}
  dfDiff = temp %>% dplyr::group_by(period) %>%
    summarise(total=sum(Electric)) %>%
    {.}
  actualBill = (dfDiff %>% dplyr::filter(period=="actual") %>% dplyr::select(`total`))[[1]]
  baselineBill = (dfDiff %>% dplyr::filter(period=="baseline") %>% dplyr::select(`total`))[[1]]
  ## print(actualBill)
  ## print(baselineBill)
  ## elecSaving = (baselineBill - actualBill) / nrow(dfPost) * 12 * area
  ## annual reduction in kbtu
  elecSaving = (baselineBill - actualBill) / nrow(dfPost) * 12
  elecSavePercent = 100 * (baselineBill - actualBill) / baselineBill
  temp %>%
    ggplot(aes(y=Electric, x=Date, color=period)) +
    geom_point() +
    geom_line() +
    ## ylab("Electric (kBtu/sqft)") +
    ylab("Electric (kBtu)") +
    ggtitle(paste("Electric trend, ", building, "\nkBtu reduction: ", format(round(elecSaving, 2),big.mark=",",scientific=FALSE))) +
    theme()
  ggsave(file=paste0("images/", building, "_trend_elec_kbtu.png"), width=4, height=4, units="in")
  dplyr::bind_rows(dfPre, dfPost) %>%
    ggplot(aes(y=Electric, x=ave, color=status)) +
    geom_point() +
    geom_smooth(method = "loess") +
    xlab("Monthly average temperature") +
    ## ylab("Electric (kBtu/sqft)") +
    ylab("Electric (kBtu)") +
    ggtitle(paste("Electric regression fit, ", building)) +
    theme()
  ggsave(file=paste0("images/", building, "_reg_elec_kbtu.png"), width=4, height=4, units="in")
  ## temp %>%
  ##   ggplot(aes(y=Electric, x=ave, color=period)) +
  ##   geom_point() +
  ##   geom_smooth(method = "loess") +
  ##   xlab("Monthly average temperature") +
  ##   theme()
  ## gas saving
  dfGas = df %>%
    ## dplyr::select(-`eui_elec`) %>%
    ## dplyr::rename(`Gas` = `eui_gas`) %>%
    ## dplyr::mutate(`Gas` = `Gas_(kBtu)` / area) %>%
    dplyr::mutate(`Gas` = `Gas_(kBtu)`) %>%
    {.}
  if (building %in% c("SC0028ZZ", "UT0032ZZ")) {
    print(nrow(dfGas))
    dfGas = dfGas %>%
      dplyr::filter(`Gas` > 0) %>%
      {.}
    print(nrow(dfGas))
  }
  dfPre = dfGas %>%
    dplyr::filter(`Date` < projectDate) %>%
    tail(n=36) %>%
    dplyr::mutate(`status` = "pre") %>%
    {.}
  dfPre %>%
    readr::write_csv(paste0("csv_FY/tempResultsInRoi/", building, "_gas_pre.csv"))
  dfPost = dfGas %>%
    dplyr::filter(`Date` > projectDate) %>%
    head(n=36) %>%
    dplyr::mutate(`status` = "post") %>%
    {.}
  gasPost = sum(dfPost$Gas) / nrow(dfPost) * 12
  ## dfPre %>% ggplot(aes(y=`Gas`, x=`ave`)) +
  ##   geom_point() +
  ##   xlab("Monthly average temperature") +
  ##   geom_smooth(method="loess")
  le = loess(Gas ~ ave, data=dfPre)
  summary(le)
  dfPost %>%
    dplyr::mutate(`baseline`=predict(le, ave)) %>%
    dplyr::rename(`actual`=`Gas`) %>%
    readr::write_csv(paste0("csv_FY/tempResultsInRoi/", building, "_gas_post.csv"))
  temp = dfPost %>%
    dplyr::mutate(`baseline`=predict(le, ave)) %>%
    dplyr::rename(`actual`=`Gas`) %>%
    dplyr::select(`actual`, `baseline`, `ave`, `Date`) %>%
    na.omit() %>%
    reshape2::melt(id.vars=c("ave", "Date"), variable.name="period", value.name="Gas") %>%
    {.}

  dfDiff = temp %>% dplyr::group_by(period) %>%
    summarise(total=sum(Gas)) %>%
    {.}

  actualBill = (dfDiff %>% dplyr::filter(period=="actual") %>% dplyr::select(`total`))[[1]]
  baselineBill = (dfDiff %>% dplyr::filter(period=="baseline") %>% dplyr::select(`total`))[[1]]
  ## gasSaving = (baselineBill - actualBill) / nrow(dfPost) * 12 * area
  gasSaving = (baselineBill - actualBill) / nrow(dfPost) * 12
  gasSavePercent = 100 * (baselineBill - actualBill) / baselineBill

  temp %>%
    ggplot(aes(y=Gas, x=Date, color=period)) +
    geom_point() +
    geom_line() +
    ## ylab("Gas (kBtu/sqft)") +
    ylab("Gas (kBtu)") +
    ggtitle(paste("Gas trend, ", building, "\nkBtu reduction: ", format(round(gasSaving, 2),big.mark=",",scientific=FALSE))) +
    theme(legend.position="bottom")
  ggsave(file=paste0("images/", building, "_trend_gas_ktbu.png"), width=4, height=4, units="in")

  dplyr::bind_rows(dfPre, dfPost) %>%
    ggplot(aes(y=Gas, x=ave, color=status)) +
    geom_point() +
    geom_smooth(method = "loess") +
    xlab("Monthly average temperature") +
    ylab("Gas (kBtu)") +
    ggtitle(paste("Gas regression fit, ", building)) +
    theme()
  ggsave(file=paste0("images/", building, "_reg_gas_kbtu.png"), width=4, height=4, units="in")
  return(list("elec"=elecSaving, "gas"=gasSaving, "elecPercent"=elecSavePercent,
              "gasPercent"=gasSavePercent, "area"=area))
}
