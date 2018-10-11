#' compute ROI for building
#'
#' This function computes ROI of building ECM retrofits, it needs time, cost, weather and energy
#' @param building required, 8 digit building number
#' @keywords roi
#' @export
#' @examples
#' roiBuilding(building="UT0032ZZ")
roiBuilding <- function(building) {
  weather_file = sprintf("roiForECM/data-raw/building_TAVG/compiled/%s_TAVG.feather", building)
  if (!file.exists(weather_file)) {
    getAvgTemp(b=building, path="~/Dropbox/gsa_2017/roiForECM/data-raw")
  }
  timeframe = analysisStartEnd %>%
    dplyr::filter(`Building_Number`==building) %>%
    {.}
  buildingEnergy = energy %>%
    dplyr::select(-`Electric_(kBtu)`, -`Gas_(kBtu)`) %>%
    dplyr::filter(`Building_Number`==building) %>%
    dplyr::mutate(`Date`=lubridate::ymd(sprintf("%.0f-%.0f-01", `year`, `month`))) %>%
    {.}
  weather =
    feather::read_feather(weather_file) %>%
    tibble::as_data_frame() %>%
    dplyr::mutate(`year`=lubridate::year(`Date`),
                  `month`=lubridate::month(`Date`)) %>%
    dplyr::group_by(`year`, `month`) %>%
    dplyr::summarise(`Monthly Mean Temperature`=mean(`TAVG`)) %>%
    dplyr::ungroup() %>%
    {.}
  energy_weather = buildingEnergy %>%
    inner_join(weather, by=c("year", "month")) %>%
    {.}
  buildingUtilityCost =
    utilityCost %>%
    dplyr::filter(`Building_Number`==building) %>%
    dplyr::mutate(`Date`=lubridate::ymd(sprintf("%.0f-%.0f-01", `year`, `month`))) %>%
    dplyr::arrange(desc(`Date`)) %>%
    {.}
  utilityRateElec = mean(head(buildingUtilityCost, n=36)$`Electric ($/KWH)`)
  utilityRateGas = mean(head(buildingUtilityCost, n=36)$`Gas ($/Cubic Ft)`)
  sprintf("Electricity utility rate: %.2f $/KWH", utilityRateElec)
  sprintf("Gas utility rate: %.2f $/Cubic Foot", utilityRateGas)
  ## accumulate model fitting result
  acc_result = NULL
  nPhases = nrow(timeframe)
  sprintf("number of phases = %d", nPhases)
  ## limits for lean plots
  for (i in 1:nPhases) {
    print(sprintf("Phase %s", i))
    pre_start = timeframe$`analysis_start`[i]
    pre_end = timeframe$`action_time`[i]
    post_start = timeframe$`action_time`[i]
    post_end = timeframe$`analysis_end`[i]
    print(sprintf("Pre-retrofit period: %s ---- %s", as.character(pre_start), as.character(pre_end)))
    print(sprintf("Post-retrofit period: %s ---- %s", as.character(post_start), as.character(post_end)))
    dfPre = energy_weather %>%
      dplyr::filter(as.Date(pre_start) <= `Date`, `Date` <= as.Date(pre_end)) %>%
      {.}
    dfPost = energy_weather %>%
      dplyr::filter(post_start <= `Date`, `Date` <= post_end) %>%
      {.}
    ## print(head(dfPre))
    ## print(head(dfPost))
    ## acc_result <-
    ## fitting(building=building, dfPre=dfPre, dfPost=dfPost, colname="Electricity_(KWH)", plotType="elec",
    ##         consumptionUnit="KWH", method=lean.analysis::loess_fit, action_time=as.character(pre_end),
    ##         method_label="loess", acc_result=acc_result)
    acc_result <-
      fitting(building=building, dfPre=dfPre, dfPost=dfPost, colname="Electricity_(KWH)", plotType="elec",
            consumptionUnit="KWH", method=lean.analysis::piecewise_linear,
            action_time=as.character(pre_end), method_label="piecewise", acc_result=acc_result)
    ## acc_result <-
    ## fitting(building=building, dfPre=dfPre, dfPost=dfPost, colname="Electricity_(KWH)", plotType="elec",
    ##         consumptionUnit="KWH", method=lean.analysis::polynomial_deg_2, action_time=as.character(pre_end),
    ##         method_label="poly2", acc_result=acc_result)
    ## acc_result <-
    ## fitting(building=building, dfPre=dfPre, dfPost=dfPost, colname="Gas_(Cubic_Ft)", plotType="gas",
    ##         consumptionUnit="Cubic Foot", method=lean.analysis::loess_fit, action_time=as.character(pre_end),
    ##         method_label="loess", acc_result=acc_result)
    ## acc_result <-
    ## fitting(building=building, dfPre=dfPre, dfPost=dfPost, colname="Gas_(Cubic_Ft)", plotType="gas",
    ##         consumptionUnit="Cubic Foot", method=lean.analysis::piecewise_linear,
    ##         action_time=as.character(pre_end),
    ##         method_label="piecewise", acc_result=acc_result)
    ## acc_result <-
    ## fitting(building=building, dfPre=dfPre, dfPost=dfPost, colname="Gas_(Cubic_Ft)", plotType="gas",
    ##         consumptionUnit="Cubic Foot", method=lean.analysis::polynomial_deg_2, action_time=as.character(pre_end),
    ##         method_label="poly2", acc_result=acc_result)
  }
  if (is.null(acc_result)) {
    print(sprintf("all models failed for %s", building))
    return(NULL)
  }
  acc_result %>%
    readr::write_csv(sprintf("roiForECM/data-raw/fit_result/%s.csv", building))
  dfutilityRate = data.frame(`plotType`=c("elec", "gas"), utilityRate=c(utilityRateElec, utilityRateGas))
  buildingActionCost = timeAndCost%>%
    dplyr::filter(`Building_Number`==building) %>%
    dplyr::rename(`building`=`Building_Number`) %>%
    dplyr::mutate(`action_time`=as.character(`action_time`)) %>%
    {.}
  df_err = acc_result %>%
    tibble::as_data_frame() %>%
    dplyr::mutate(`action_time`=as.character(`action_time`)) %>%
    dplyr::mutate_at(vars(plotType), recode, "elec"="cvrmse_electric", "gas"="cvrmse_gas") %>%
    select(`building`, `cvrmse`, `plotType`, `action_time`, `method_label`) %>%
    tidyr::spread(plotType, cvrmse) %>%
    {.}
  roi_result = acc_result %>%
    dplyr::left_join(dfutilityRate, by="plotType") %>%
    dplyr::select(-`cvrmse`, -`savingPercent`, -`plotType`) %>%
    dplyr::mutate(dollarSaving = utilityRate * saving) %>%
    dplyr::select(-`utilityRate`, -`saving`) %>%
    dplyr::group_by(`building`, `action_time`, `method_label`) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(buildingActionCost, by=c("building", "action_time")) %>%
    dplyr::mutate(`payback_years`=`Cost`/`dollarSaving`) %>%
    dplyr::mutate(`roi`=`dollarSaving`/`Cost`) %>%
    {.}
  ## print(head(df_err))
  ## print(head(roi_result))
  roi_result <- roi_result %>%
    dplyr::inner_join(df_err, by=c("building", "action_time", "method_label")) %>%
    {.}
  ## print(names(roi_result))
  roi_result %>%
    readr::write_csv(sprintf("roiForECM/data-raw/roi_result/%s.csv", building))
  roi_result %>%
    dplyr::select(-building) %>%
    print()
}
