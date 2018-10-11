building = "UT0032ZZ"
# building = "OK0063ZZ"
library(devtools)
library(dplyr)
library(lubridate)
library(ggplot)

devtools::load_all("~/Dropbox/gsa_2017/db.interface")
devtools::load_all("~/Dropbox/gsa_2017/get.noaa.weather")

devtools::load_all("~/Dropbox/gsa_2017/lean.analysis")

load("../data/dfECM.rda")
action =
  dfECM %>%
  dplyr::filter(`Building_Number`==building) %>%
  dplyr::mutate(`action_time`=lubridate::ymd(`Substantial_Completion_Date`)) %>%
  dplyr::select(`Building_Number`, `high_level_ECM`, `detail_level_ECM`, `action_time`) %>%
  {.}
print(action)

## We'll retrieve the before and after period for both actions, this is done in
## another separate script and the start and end of analysis period is in
## analysisStartEnd.R

action_times = action$`action_time`
analysis_start = action_times - lubridate::years(3)
analysis_end = action_times + lubridate::years(3)
nrounds = length(action_times)
if (nrounds > 1) {
  analysis_start[2:nrounds] =
    max(analysis_start[2:nrounds], action_times[1:(nrounds - 1)])
  analysis_end[1:(nrounds - 1)] =
    min(analysis_end[1:(nrounds - 1)], action_times[2:nrounds])
}
timeframe = data.frame(analysis_start=analysis_start, analysis_end=analysis_end, action_time=action_times)

## this part uses pre-processed data frame to get time
load("roiForECM/data/analysisStartEnd.rda")

load("../data/analysisStartEnd.rda")
timeframe = analysisStartEnd %>%
  dplyr::filter(`Building_Number`==building) %>%
  {.}

action %>%
  inner_join(timeframe, by="action_time") %>%
  dplyr::select(-`high_level_ECM`, -`detail_level_ECM`) %>%
  print()


## Then retrieve energy data

load("../data/energy.rda")
buildingEnergy = energy %>%
  dplyr::select(-`Electric_(kBtu)`, -`Gas_(kBtu)`) %>%
  dplyr::filter(`Building_Number`==building) %>%
  dplyr::mutate(`Date`=lubridate::ymd(sprintf("%.0f-%.0f-01", `year`, `month`))) %>%
  {.}
buildingEnergy %>%
  head()

## Plot electricity data and action time

## par(mfrow = c(2,1))
p = buildingEnergy %>%
  ggplot2::ggplot(ggplot2::aes(y=`Electricity_(KWH)`, x=`Date`)) +
  ggplot2::ylab("Electricity(KWH)") +
  ggplot2::xlab("Time") +
  ggplot2::geom_line()
for (t in action_times) {
  p <- p + ggplot2::geom_vline(xintercept=t, linetype="dashed",
                               color="red")
}
p <- p + ggplot2::theme_bw()
print(p)
ggplot2::ggsave(sprintf("%s_elec_trend.png", building), width=8, height=4)
p = buildingEnergy %>%
  ggplot2::ggplot(ggplot2::aes(y=`Gas_(Cubic_Ft)`, x=`Date`)) +
  ggplot2::ylab("Gas(Cubic Foot)") +
  ggplot2::xlab("Time") +
  ggplot2::geom_line()
for (t in action_times) {
  p <- p + ggplot2::geom_vline(xintercept=t, linetype="dashed",
                               color="red")
}
p <- p + ggplot2::theme_bw()
print(p)
ggplot2::ggsave(sprintf("%s_gas_trend.png", building), width=8, height=4)

## Get utility cost

load("../data/utilityCost.rda")
buildingUtilityCost =
  utilityCost %>%
  dplyr::filter(`Building_Number`==building) %>%
  dplyr::mutate(`Date`=lubridate::ymd(sprintf("%.0f-%.0f-01", `year`, `month`))) %>%
  dplyr::arrange(desc(`Date`)) %>%
  {.}
print(head(buildingUtilityCost))

## Plot utility cost

## par(mfrow = c(2,1))
p = buildingUtilityCost %>%
  ggplot2::ggplot(ggplot2::aes(y=`Electric ($/KWH)`, x=`Date`)) +
  ggplot2::xlab("Time") +
  ggplot2::geom_line()
for (t in action_times) {
  p <- p + ggplot2::geom_vline(xintercept=t, linetype="dashed",
                               color="red")
}
p <- p + ggplot2::theme_bw()
print(p)
ggplot2::ggsave(sprintf("%s_elec_cost.png", building), width=8, height=4)
p = buildingUtilityCost %>%
  ggplot2::ggplot(ggplot2::aes(y=`Gas ($/Cubic Ft)`, x=`Date`)) +
  ggplot2::xlab("Time") +
  ggplot2::geom_line()
for (t in action_times) {
  p <- p + ggplot2::geom_vline(xintercept=t, linetype="dashed",
                               color="red")
}
p <- p + ggplot2::theme_bw()
print(p)
ggplot2::ggsave(sprintf("%s_gas_cost.png", building), width=8, height=4)

## Use the most recent three year's average utility cost in the ROI analysis

utilityRateElec = mean(head(buildingUtilityCost, n=36)$`Electric ($/KWH)`)
utilityRateGas = mean(head(buildingUtilityCost, n=36)$`Gas ($/Cubic Ft)`)
sprintf("Electricity utility rate: %.2f $/KWH", utilityRateElec)
sprintf("Gas utility rate: %.2f $/Cubic Foot", utilityRateGas)

## Get weather data (use getWeatherData.R) from NOAA GHCND, variable definitions can be found [here]{ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}

library(feather)
weather =
  feather::read_feather(sprintf("building_TAVG/compiled/%s_TAVG.feather", building)) %>%
  ## feather::read_feather(sprintf("../../data-raw/building_TAVG/compiled/%s_TAVG.feather", building)) %>%
  tibble::as_data_frame() %>%
  dplyr::mutate(`year`=lubridate::year(`Date`),
                `month`=lubridate::month(`Date`)) %>%
  dplyr::group_by(`year`, `month`) %>%
  dplyr::summarise(`Monthly Mean Temperature`=mean(`TAVG`)) %>%
  dplyr::ungroup() %>%
  {.}
print(head(weather))

## Join weather and energy data

energy_weather = buildingEnergy %>%
  inner_join(weather, by=c("year", "month")) %>%
  {.}
print(head(energy_weather))

## Fit models

## fitting <- function(building, dfPre, dfPost, colname, plotType, consumptionUnit, method, method_label, action_time, acc_result) {
##   print(paste(building, colname, plotType, method_label, action_time, sep="--"))
##   ylabel = gsub("_", " ", colname)
##   plotWidth = 6
##   plotHeight = 4
##   if (sum(dfPre[[colname]]==0) > 0.7 * nrow(dfPre)) {
##     print("Too few electric data points")
##   } else {
##     x = dfPre$`Monthly Mean Temperature`
##     y = dfPre[[colname]]
##     modelresult =
##       lean.analysis::k_fold_cv_1d(y=y, x=x, method=method)
##     le = modelresult$output
##     print(summary(le))
##     cvrmse = modelresult$cvrmse
##     print(sprintf("cvrmse: %s", cvrmse))
##     xseq = seq(from=min(x), to=max(x), length.out=100)
##     print("xseq---------")
##     print(head(xseq))
##     print(class(xseq))
##     yseq = predict(le, newdata=data.frame(x=xseq))
##     x_post = dfPost$`Monthly Mean Temperature`
##     y_post = dfPost[[colname]]
##     modelresult_post =
##       lean.analysis::k_fold_cv_1d(y=y_post, x=x_post, method=method)
##     xseq_post = seq(from=min(x_post), to=max(x_post), length.out=100)
##     yseq_post = predict(modelresult_post$output, newdata=data.frame(x=xseq_post))
##     dfPre <- dfPre %>%
##       dplyr::mutate(`status`="pre") %>%
##       {.}
##     dfPost <- dfPost %>%
##       dplyr::mutate(`status`="post") %>%
##       {.}
##     p <-
##       dplyr::bind_rows(dfPre, dfPost) %>%
##       ggplot2::ggplot(ggplot2::aes_string(x="`Monthly Mean Temperature`", y=sprintf("`%s`", colname), colour="status")) +
##       ggplot2::geom_point() +
##       ggplot2::ylab(ylabel) +
##       ggplot2::geom_line(ggplot2::aes(x=x, y=y), data=data.frame(x=xseq, y=yseq, status="pre")) +
##       ggplot2::geom_line(ggplot2::aes(x=x, y=y), data=data.frame(x=xseq_post, y=yseq_post, status="post")) +
##       ggplot2::ggtitle(sprintf("Model: %s, CVRMSE: %.3f", method_label, cvrmse)) +
##       ggplot2::theme_bw() +
##       ggplot2::theme(legend.position="bottom")
##     ggplot2::ggsave(file=sprintf("images/%s_reg_%s_%s_%s.png", building, plotType, method_label, action_time), width=plotWidth, height=plotHeight, units="in")
##   }
##   result <- dfPost %>%
##     dplyr::mutate(`baseline`=predict(le, data.frame(x=`Monthly Mean Temperature`))) %>%
##     dplyr::rename(`actual`=!!rlang::sym(colname)) %>%
##     dplyr::select(`actual`, `baseline`, `Monthly Mean Temperature`, `Date`) %>%
##     na.omit() %>%
##     reshape2::melt(id.vars=c("Monthly Mean Temperature", "Date"), variable.name="period", value.name="consumption") %>%
##     {.}
##   print(head(result))
##   dfDiff = result %>% dplyr::group_by(period) %>%
##     summarise(total=sum(consumption)) %>%
##     {.}
##   print(dfDiff)
##   number_of_months = nrow(dfPost)
##   actualConsumption = (dfDiff %>% dplyr::filter(period=="actual") %>% dplyr::select(`total`))[[1]]
##   baselineConsumption = (dfDiff %>% dplyr::filter(period=="baseline") %>% dplyr::select(`total`))[[1]]
##   print(sprintf("in %s months, the actual consumption is %s", number_of_months , actualConsumption))
##   print(sprintf("in %s months, the predicted baseline consumption is %s", number_of_months, baselineConsumption))
##   ## saving in the unit of calculation
##   saving = (baselineConsumption - actualConsumption) / number_of_months * 12
##   savingPercent = (baselineConsumption - actualConsumption) / baselineConsumption * 100
##   print(sprintf("Saving in %s: %.2f", consumptionUnit, saving))
##   print(sprintf("Saving Percent in %s: %.2f%%", consumptionUnit, savingPercent))
##   ## this part has error band, but not sure how to get models other than loess
##   ## p <- dplyr::bind_rows(dfPre, dfPost) %>%
##   ##   ggplot2::ggplot(ggplot2::aes_string(y=sprintf("`%s`", colname), x="`Monthly Mean Temperature`",
##   ##                                       color="status")) +
##   ##   ggplot2::geom_point() +
##   ##   ggplot2::geom_smooth(method = "loess") +
##   ##   ggplot2::xlab("Monthly average temperature") +
##   ##   ggplot2::ylab(ylabel) +
##   ##   ggplot2::ggtitle(paste("%s regression fit, ", gsub("_", " ", colname), building)) +
##   ##   ggplot2::theme(legend.position="bottom")
##   ## print(p)
##   ## ggplot2::ggsave(file=sprintf("images/%s_reg_%s_%s.png", building, plotType, action_time), width=plotWidth, height=plotHeight, units="in")
##   p <- result %>%
##     ggplot2::ggplot(ggplot2::aes(y=consumption, x=Date, color=period)) +
##     ggplot2::geom_point() +
##     ggplot2::geom_line() +
##     ggplot2::ylab(ylabel) +
##     ggplot2::ggtitle(sprintf("%s actual consumption vs predicted baseline\n%s reduction: %s", building, consumptionUnit, format(round(saving, 2),big.mark=",",scientific=FALSE))) +
##     ggplot2::scale_color_brewer(palette = "Set1") +
##     ggplot2::theme(legend.position="bottom")
##   print(p)
##   ggplot2::ggsave(file=sprintf("images/%s_trend_%s_%s_%s.png", building, plotType, method_label, action_time), width=plotWidth, height=plotHeight, units="in")
##   acc_result = rbind(acc_result, (data.frame(building=building, cvrmse=cvrmse, saving=saving, savingPercent=savingPercent, plotType=plotType, method_label=method_label, action_time=action_time)))
##   return(acc_result)
## }

## load("../data/timeAndCost.rda")
## roiBuilding <- function(building, timeframe, dfPre, dfPost, utilityRateElec, utilityRateGas, timeAndCost) {
##   acc_result = NULL
##   nPhases = nrow(timeframe)
##   sprintf("number of phases = %d", nPhases)
##   for (i in 1:nPhases) {
##     print(sprintf("Phase %s", i))
##     pre_start = timeframe$`analysis_start`[i]
##     pre_end = timeframe$`action_time`[i]
##     post_start = timeframe$`action_time`[i]
##     post_end = timeframe$`analysis_end`[i]
##     print(sprintf("Pre-retrofit period: %s ---- %s", as.character(pre_start), as.character(pre_end)))
##     print(sprintf("Post-retrofit period: %s ---- %s", as.character(post_start), as.character(post_end)))
##     dfPre = energy_weather %>%
##       dplyr::filter(as.Date(pre_start) <= `Date`, `Date` <= as.Date(pre_end)) %>%
##       {.}
##     dfPost = energy_weather %>%
##       dplyr::filter(post_start <= `Date`, `Date` <= post_end) %>%
##       {.}
##     ## print(head(dfPre))
##     ## print(head(dfPost))
##     acc_result <-
##     fitting(building=building, dfPre=dfPre, dfPost=dfPost, colname="Electricity_(KWH)", plotType="elec",
##             consumptionUnit="KWH", method=lean.analysis::loess_fit, action_time=as.character(pre_end),
##             method_label="loess", acc_result=acc_result)
##     acc_result <-
##       fitting(building=building, dfPre=dfPre, dfPost=dfPost, colname="Electricity_(KWH)", plotType="elec",
##             consumptionUnit="KWH", method=lean.analysis::piecewise_linear,
##             action_time=as.character(pre_end), method_label="piecewise", acc_result=acc_result)
##     acc_result <-
##     fitting(building=building, dfPre=dfPre, dfPost=dfPost, colname="Electricity_(KWH)", plotType="elec",
##             consumptionUnit="KWH", method=lean.analysis::polynomial_deg_2, action_time=as.character(pre_end),
##             method_label="poly2", acc_result=acc_result)
##     acc_result <-
##     fitting(building=building, dfPre=dfPre, dfPost=dfPost, colname="Gas_(Cubic_Ft)", plotType="gas",
##             consumptionUnit="Cubic Foot", method=lean.analysis::loess_fit, action_time=as.character(pre_end),
##             method_label="loess", acc_result=acc_result)
##     acc_result <-
##     fitting(building=building, dfPre=dfPre, dfPost=dfPost, colname="Gas_(Cubic_Ft)", plotType="gas",
##             consumptionUnit="Cubic Foot", method=lean.analysis::piecewise_linear,
##             action_time=as.character(pre_end),
##             method_label="piecewise", acc_result=acc_result)
##     acc_result <-
##     fitting(building=building, dfPre=dfPre, dfPost=dfPost, colname="Gas_(Cubic_Ft)", plotType="gas",
##             consumptionUnit="Cubic Foot", method=lean.analysis::polynomial_deg_2, action_time=as.character(pre_end),
##             method_label="poly2", acc_result=acc_result)
##   }
##   acc_result %>%
##     readr::write_csv(sprintf("fit_result/%s.csv", building))
##   dfutilityRate = data.frame(`plotType`=c("elec", "gas"), utilityRate=c(utilityRateElec, utilityRateGas))
##   buildingActionCost = timeAndCost%>%
##     dplyr::filter(`Building_Number`==building) %>%
##     dplyr::rename(`building`=`Building_Number`) %>%
##     dplyr::mutate(`action_time`=as.character(`action_time`)) %>%
##     {.}
##   acc_result %>%
##     dplyr::left_join(dfutilityRate, by="plotType") %>%
##     dplyr::select(-`cvrmse`, -`savingPercent`, -`plotType`) %>%
##     dplyr::mutate(dollarSaving = utilityRate * saving) %>%
##     dplyr::select(-`utilityRate`, -`saving`) %>%
##     dplyr::group_by(`building`, `action_time`, `method_label`) %>%
##     dplyr::summarise_all(sum) %>%
##     dplyr::left_join(buildingActionCost, by=c("building", "action_time")) %>%
##     dplyr::mutate(`payback_years`=`Cost`/`dollarSaving`) %>%
##     dplyr::mutate(`roi`=`dollarSaving`/`Cost`) %>%
##     readr::write_csv(sprintf("roi_result/%s.csv", building))
## }

