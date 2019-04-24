library("dplyr")
library("ggplot2")
library("DBI")

## r = "Occupied Cooling Setpoint Out of Range"
r = "AHU Cooling Valve Leaking"
energytype = "kWh Del Int"
## restrict to summer
time.min.str="2018-06-01"
time.max.str="2018-09-01"
n = 6

## get top 3 buildings
allbuilding = readr::read_csv("rule_summary.csv") %>%
  tibble::as_data_frame() %>%
  dplyr::filter(rule==r) %>%
  {.}

con <- DBI::dbConnect(RSQLite::SQLite(), "../../csv_FY/db/all.db")
has_lat_lon =
  DBI::dbGetQuery(con, sprintf("SELECT * FROM EUAS_latlng_2")) %>%
  dplyr::filter(`Building_Number` %in% allbuilding$building) %>%
  tibble::as_data_frame() %>%
  .$`Building_Number`
DBI::dbDisconnect(con)

topn <- allbuilding %>%
  dplyr::filter(building %in% has_lat_lon) %>%
  dplyr::arrange(desc(eCost)) %>%
  dplyr::slice(1:n) %>%
  .$building

gsalink_buildings = topn

devtools::load_all("../../db.interface")

devtools::load_all("../../get.noaa.weather")

devtools::load_all("../../lean.analysis")

devtools::load_all("../../summarise.and.plot")

start_str = "2018-01-01"
end_str = "2018-12-31"
path = "~/Dropbox/gsa_2017/faultDetectionSkySpark/data-raw/gsalink_weather"
v = "TAVG"

devtools::load_all("../../roiForECM")

## devtools::load_all("../../get.noaa.weather")

for (b in gsalink_buildings[4:4]) {
  print(b)
  years = as.integer(substr(start_str, 1, 4)):(as.integer(substr(end_str, 1, 4)))
  print(years)
  if (!file.exists(sprintf("gsalink_weather/%s_%s.csv", b, years[[1]]))) {
    result = get.noaa.weather::compile_weather_isd_main(useSavedData=FALSE, years=years, building=b, step="hour")
    result %>%
      readr::write_csv(sprintf("gsalink_weather/%s_%s.csv", b, years[[1]]))
  }
}

namelookup = readr::read_csv("../data/gsalink_name_in_rulefile.csv") %>%
  tibble::as_data_frame() %>%
  {.}

component.group.lookup = readr::read_csv("building_component.csv") %>%
  tibble::as_data_frame() %>%
  dplyr::rename(equipRef=equipRefBackup) %>%
  {.}

## plot kwh per deg F during spark vs not
for (b in gsalink_buildings[4:4]) {
  ## change here when got real occ hour info
  occ_hour_start = 8
  occ_hour_end = 17
  weekdays = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  name = paste0(namelookup[namelookup$building==b,]$name, " ")
  dfenergy = readr::read_csv(sprintf("building_energy/%s_%s.csv", b, energytype))
  dfrule = readr::read_csv(sprintf("ruleStartEndByBuilding/%s_2018.csv", b))
  print(head(dfrule))
  ## change to setpoint temperature
  dfweather = readr::read_csv(sprintf("gsalink_weather/%s_2018.csv", b))
  dfweather <- dfweather %>%
    dplyr::mutate(Timestamp=sprintf("%s %s:00:00", date, hour)) %>%
    dplyr::mutate(Timestamp=as.POSIXct(Timestamp, format="%Y%m%d %H:%M:%S", tz="UTC")) %>%
    dplyr::rename(`F`=`wt_temperatureFhour`) %>%
    dplyr::select(Timestamp, F) %>%
    {.}
  tz = dfrule[["tz"]][1]
  time.min=as.POSIXct(time.min.str, tz=tz)
  time.max=as.POSIXct(time.max.str, tz=tz)
  dfleft = tibble::tibble(Timestamp=seq(from=time.min, to=time.max, by="mins"))
  dfleft.whole = tibble::tibble(Timestamp=seq(from=min(dfrule$startPosix), to=max(dfrule$endPosix), by="mins"))
  dfenergy <- dfenergy %>%
    dplyr::mutate(Timestamp=as.POSIXct(Timestamp, format="%m/%d/%Y %I:%M:%S %p", tz=tz)) %>%
    dplyr::filter(Timestamp>=time.min) %>%
    dplyr::filter(Timestamp<=time.max) %>%
    dplyr::filter(!!rlang::sym(energytype) >= 0) %>%
    dplyr::right_join(dfleft, by="Timestamp") %>%
    dplyr::mutate(groupvar = ifelse(!is.na(!!rlang::sym(energytype)), Timestamp, NA)) %>%
    tidyr::fill(groupvar) %>%
    dplyr::group_by(groupvar) %>%
    dplyr::mutate(!!rlang::sym(energytype):=first(!!rlang::sym(energytype)) / n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-groupvar) %>%
    {.}
  print(dfenergy, n=30)
  dfrule <- dfrule %>%
    dplyr::filter(rule==r) %>%
    dplyr::mutate(equipRef=substr(equipRef, 32, nchar(equipRef))) %>%
    dplyr::mutate(equipRef=gsub(name, "", equipRef)) %>%
    dplyr::left_join(component.group.lookup, by=c("building", "equipRef")) %>%
    dplyr::mutate(count=1) %>%
    dplyr::mutate(eCost=as.numeric(gsub("$", "", eCost, fixed=TRUE))) %>%
    dplyr::mutate(ecost.per.min=eCost/(durationSecond/60)) %>%
    {.}
  print(head(dfrule))
  dfresult = summarise.and.plot::agg_interval(df=dfrule, start="startPosix", end="endPosix",
                                              group="group", value="count",
                                              time.epsilon=0) %>%
    dplyr::rename(Timestamp=time)
  summarise.and.plot::scan_agg(df=dfrule, start="startPosix", end="endPosix",
                               group="group", value="count",
                               time.epsilon=0.1)
  ggplot2::ggsave(sprintf("../plots/%s_%s_2018.png", b, r, energytype))
  extra.times = setdiff(dfresult[["Timestamp"]], dfleft.whole[["Timestamp"]])
  if (length(extra.times) != 0L) {
    stop (paste0("The data had unexpected values in the time column; some are: ",
                 paste(head(extra.times), collapse=", ")))
  }
  dfresult <- dfresult %>%
    dplyr::filter(row.kind!="pre-delta") %>%
    tidyr::complete(group, Timestamp=dfleft.whole$Timestamp) %>%
    dplyr::group_by(group) %>%
    dplyr::arrange(Timestamp) %>%
    dplyr::mutate(value.agg=zoo::na.locf0(value.agg)) %>%
    dplyr::mutate(value.agg=zoo::na.fill(value.agg, 0)) %>%
    dplyr::ungroup(group) %>%
    dplyr::filter(Timestamp>=time.min) %>%
    dplyr::filter(Timestamp<=time.max) %>%
    {.}
  dfweather <- dfweather %>%
    dplyr::right_join(dfleft, by="Timestamp") %>%
    tidyr::fill(F) %>%
    {.}
  df <- dfenergy %>%
    dplyr::left_join(dfresult, by="Timestamp") %>%
    dplyr::left_join(dfweather, by="Timestamp") %>%
    dplyr::mutate(`rulePresent`=ifelse(value.agg>0, "Yes", "No")) %>%
    dplyr::mutate(hour=as.numeric(format(Timestamp, "%H"))) %>%
    dplyr::mutate(day=format(Timestamp, "%A")) %>%
    dplyr::mutate(is.occupied=ifelse((hour <= occ_hour_end) & (hour >= occ_hour_start) & (day %in% weekdays), "Occupied", "Un-occupied")) %>%
    {.}
  df %>%
    dplyr::rename(`num.component.has.warning`=`value.agg`) %>%
    dplyr::select(-`row.kind`) %>%
    readr::write_csv(sprintf("building_rule_energy_weather/%s_%s_%s_2018.csv",
                             b, r, energytype))
  their.estimate <-
    summarise.and.plot::agg_interval(df=dfrule, start="startPosix",
                                     end="endPosix", group="group",
                                     value="ecost.per.min", time.epsilon=0) %>%
    dplyr::filter(row.kind!="pre-delta") %>%
    dplyr::rename(Timestamp=time) %>%
    tidyr::complete(group, Timestamp=dfleft.whole$Timestamp) %>%
    dplyr::group_by(group) %>%
    dplyr::arrange(Timestamp) %>%
    dplyr::mutate(value.agg=zoo::na.locf0(value.agg)) %>%
    dplyr::mutate(value.agg=zoo::na.fill(value.agg, 0)) %>%
    dplyr::ungroup(group) %>%
    dplyr::filter(Timestamp>=time.min) %>%
    dplyr::filter(Timestamp<=time.max) %>%
    dplyr::mutate(hour=as.numeric(format(Timestamp, "%H"))) %>%
    dplyr::mutate(day=format(Timestamp, "%A")) %>%
    dplyr::mutate(is.occupied=ifelse((hour <= occ_hour_end) & (hour >= occ_hour_start) & (day %in% weekdays), "Occupied", "Un-occupied")) %>%
    {.}
  ## output raw minute level
  their.estimate %>%
    dplyr::rename(ecost.per.min=value.agg) %>%
    readr::write_csv(sprintf("rule_ecost_minute/%s_%s_%s_2018.csv",
                             b, r, energytype))
  p <- df %>%
    ggplot2::ggplot(aes(x=F, y=(!!rlang::sym(energytype)), colour=rulePresent)) +
    ggplot2::geom_point(size=0.3) +
    geom_smooth(method='lm', size=1.5) +
    ggplot2::ggtitle(label=sprintf("%s %s, %s", b, energytype, r),
                     subtitle = sprintf("%s -- %s", time.min, time.max)) +
    ggplot2::facet_wrap(group~is.occupied) +
    ggplot2::ylab(sprintf("%s per minute", energytype)) +
    ggplot2::xlab("outdoor temperature (F)") +
    ggplot2::theme()
  print(p)
  ggplot2::ggsave(sprintf("../plots/%s_%s_%s_2018.png", b, r, energytype))
}

## write to data files group by component and rule
for (b in gsalink_buildings[4:4]) {

  b = gsalink_buildings[4]

  ## change here when got real occ hour info
  occ_hour_start = 8
  occ_hour_end = 17
  weekdays = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  name = paste0(namelookup[namelookup$building==b,]$name, " ")

  dfleft = tibble::tibble(Timestamp=seq(from=time.min, to=time.max, by="mins"))
  dfleft.whole = tibble::tibble(Timestamp=seq(from=min(dfrule$startPosix), to=max(dfrule$endPosix), by="mins"))
  dfrule = readr::read_csv(sprintf("ruleStartEndByBuilding/%s_2018.csv", b))
  print(head(dfrule))
  ## change to setpoint temperature
  dfweather = readr::read_csv(sprintf("gsalink_weather/%s_2018.csv", b))
  dfweather <- dfweather %>%
    dplyr::mutate(Timestamp=sprintf("%s %s:00:00", date, hour)) %>%
    dplyr::mutate(Timestamp=as.POSIXct(Timestamp, format="%Y%m%d %H:%M:%S", tz="UTC")) %>%
    dplyr::rename(`F`=`wt_temperatureFhour`) %>%
    dplyr::select(Timestamp, F) %>%
    dplyr::filter(Timestamp>=time.min) %>%
    dplyr::filter(Timestamp<=time.max) %>%
    {.}
  tz = dfrule[["tz"]][1]
  time.min=as.POSIXct(time.min.str, tz=tz)
  time.max=as.POSIXct(time.max.str, tz=tz)
  dfenergy = readr::read_csv(sprintf("building_energy/%s_%s.csv", b, energytype))
  dfenergy <- dfenergy %>%
    dplyr::mutate(Timestamp=as.POSIXct(Timestamp, format="%m/%d/%Y %I:%M:%S %p", tz=tz)) %>>%
    dplyr::filter(Timestamp>=time.min) %>%
    dplyr::filter(Timestamp<=time.max) %>%
    dplyr::mutate(!!rlang::sym(energytype) := pmax(!!rlang::sym(energytype), 0)) %>%
    dplyr::mutate(Timestamp=as.POSIXct(round(Timestamp, "hours"), tz=tz)) %>>%
    dplyr::group_by(Timestamp) %>%
    dplyr::summarise(!!rlang::sym(energytype):=sum(!!rlang::sym(energytype))) %>%
    dplyr::ungroup() %>%
    {.}
  dfrule <- dfrule %>%
    ## dplyr::filter(rule==r) %>%
    dplyr::mutate(equipRef=substr(equipRef, 32, nchar(equipRef))) %>%
    dplyr::mutate(equipRef=gsub(name, "", equipRef)) %>%
    dplyr::left_join(component.group.lookup, by=c("building", "equipRef")) %>%
    dplyr::mutate(count=1) %>%
    dplyr::mutate(eCost=as.numeric(gsub("$", "", eCost, fixed=TRUE))) %>%
    dplyr::mutate(ecost.per.min=eCost/(durationSecond/60)) %>%
    tidyr::unite(`groupvar`, `rule`, `group`, sep="----") %>%
    {.}
  print(head(dfrule))
  dfresult = summarise.and.plot::agg_interval(df=dfrule, start="startPosix", end="endPosix",
                                              group="groupvar", value="count",
                                              time.epsilon=0) %>%
    dplyr::rename(Timestamp=time)
  extra.times = setdiff(dfresult[["Timestamp"]], dfleft.whole[["Timestamp"]])
  if (length(extra.times) != 0L) {
    stop (paste0("The data had unexpected values in the time column; some are: ",
                 paste(head(extra.times), collapse=", ")))
  }
  dfresult <- dfresult %>%
    dplyr::filter(row.kind!="pre-delta") %>%
    tidyr::complete(groupvar, Timestamp=dfleft.whole$Timestamp) %>%
    dplyr::group_by(groupvar) %>%
    dplyr::arrange(Timestamp) %>%
    dplyr::mutate(value.agg=zoo::na.locf0(value.agg)) %>%
    dplyr::mutate(value.agg=zoo::na.fill(value.agg, 0)) %>%
    dplyr::ungroup(groupvar) %>%
    dplyr::filter(Timestamp>=time.min) %>%
    dplyr::filter(Timestamp<=time.max) %>%
    dplyr::mutate(Timestamp=as.POSIXct(round(Timestamp, "hours"))) %>>%
    dplyr::group_by(groupvar, Timestamp) %>%
    dplyr::summarise(value.agg=sum(value.agg)) %>%
    dplyr::ungroup() %>%
    {.}
  df <- dfenergy %>%
    dplyr::left_join(dfresult, by="Timestamp") %>%
    dplyr::left_join(dfweather, by="Timestamp") %>%
    dplyr::mutate(`rulePresent`=ifelse(value.agg>0, "Yes", "No")) %>%
    dplyr::mutate(hour=as.numeric(format(Timestamp, "%H"))) %>%
    dplyr::mutate(day=format(Timestamp, "%A")) %>%
    dplyr::mutate(is.occupied=ifelse((hour <= occ_hour_end) & (hour >= occ_hour_start) & (day %in% weekdays), "Occupied", "Un-occupied")) %>%
    {.}
  df %>%
    dplyr::rename(`num.component.has.warning`=`value.agg`) %>%
    dplyr::select(-rulePresent) %>%
    tidyr::spread(groupvar, num.component.has.warning) %>%
    readr::write_csv(sprintf("building_rule_energy_weather/%s_%s_2018.csv",
                             b, energytype))
}

for (b in gsalink_buildings) {
  df = readr::read_csv(sprintf("building_rule_energy_weather/%s_%s_%s_2018.csv",
                               b, r, energytype))
  p <- df %>%
    dplyr::mutate(num.component.has.warning=as.factor(num.component.has.warning)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x=num.component.has.warning,
                                      y=(!!rlang::sym(energytype)), colour=is.occupied)) +
    ggplot2::facet_wrap(group~is.occupied) +
    ggplot2::ggtitle(label=sprintf("%s %s, %s", b, energytype, r),
                     subtitle = sprintf("%s -- %s", time.min, time.max)) +
  ggplot2::ggsave(sprintf("../plots/ncomp_%s_%s_%s_2018.png", b, r, energytype))
}

## read in the data from file

for (b in gsalink_buildings) {
  df =
    readr::read_csv(sprintf("building_rule_energy_weather/%s_%s_%s_2018.csv",
                            b, r, energytype)) %>%
    dplyr::rename(value.agg=num.component.has.warning)

  head(df)

  ## compute counterfactual
  dfcmp = df %>%
    dplyr::group_by(group, is.occupied) %>%
    dplyr::do({
            dfnospark = .[.$value.agg==0,]
            dfwithspark = .[.$value.agg>0,]
            y = dfnospark[[energytype]]
            x = dfnospark[["F"]]
            model.no.spark = lm(y ~ x)
            print(summary(model.no.spark))
            new=data.frame(x=dfwithspark$F)
            ## counterfactual
            yhat = predict(model.no.spark, newdata = new)
            y = dfwithspark[[energytype]]
            x = dfwithspark[["F"]]
            model.with.spark = lm(y ~ x)
            print(summary(model.with.spark))
            yfitted = fitted.values(model.with.spark)
            asdf <- data.frame(F=x, modeled.with.spark=yfitted, modeled.no.spark=yhat)
            asdf
          }) %>%
    ## dplyr::distinct(group, F, modeled.with.spark, modeled.no.spark) %>%
    {.}

  dfcmp %>%
    tidyr::gather(status, !!rlang::sym(energytype), modeled.with.spark:modeled.no.spark) %>%
    ggplot2::ggplot(aes(x=F, y=(!!rlang::sym(energytype)), colour=status)) +
    ggplot2::geom_point(size=0.3) +
    ggplot2::ggtitle(label=sprintf("%s %s, %s", b, energytype, r),
                    subtitle = sprintf("%s -- %s", time.min, time.max)) +
    ggplot2::facet_wrap(group~is.occupied) +
    ggplot2::ylab(sprintf("%s per minute", energytype)) +
    ggplot2::xlab("outdoor temperature (F)") +
    ggplot2::theme()

  df1 = dfcmp %>%
    dplyr::mutate(with.minus.without = modeled.with.spark - modeled.no.spark) %>%
    dplyr::group_by(group, is.occupied) %>%
    dplyr::summarise(with.minus.without.dollar = sum(with.minus.without) * 0.1) %>%
    dplyr::rename(costdiff = with.minus.without.dollar) %>%
    dplyr::mutate(status="model estimate") %>%
    {.}

  df2 <- readr::read_csv(sprintf("rule_ecost_minute/%s_%s_%s_2018.csv",
                          b, r, energytype),
                  col_types = readr::cols(ecost.per.min = readr::col_double())) %>%
    dplyr::group_by(group, is.occupied) %>%
    dplyr::summarise(costdiff = sum(ecost.per.min)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(status="their estimate") %>%
    {.}

  df1 %>%
    dplyr::bind_rows(df2) %>%
    tidyr::spread(status, costdiff) %>%
    readr::write_csv(sprintf("cmp/%s_%s_%s_2018.csv",
                            b, r, energytype))
}
