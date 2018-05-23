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
