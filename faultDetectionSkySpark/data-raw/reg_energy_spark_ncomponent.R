library("dplyr")
library("ggplot2")
library("DBI")
library("pipeR")

library("glmnet")

energytype = "kWh Del Int"
## restrict to summer, change this to the time range you wanted
## time.min.str="2018-06-01"
## time.max.str="2018-09-01"
time.min.str="2018-01-01"
time.max.str="2018-12-31"

if ((as.integer(substr(time.min.str, 1, 4)) != 2018) && (as.integer(substr(time.max.str, 1, 4)) != 2018)) {
  print("Wrong time range")
}

allbuilding = readr::read_csv("../data/has_energy_ecost.csv") %>%
  tibble::as_data_frame() %>%
  dplyr::select(building) %>%
  {.}

con <- DBI::dbConnect(RSQLite::SQLite(), "../../csv_FY/db/all.db")
has_lat_lon =
  DBI::dbGetQuery(con, sprintf("SELECT * FROM EUAS_latlng_2")) %>%
  dplyr::filter(`Building_Number` %in% allbuilding$building) %>%
  tibble::as_data_frame() %>%
  .$`Building_Number`
DBI::dbDisconnect(con)

gsalink_buildings <- allbuilding %>%
  dplyr::filter(building %in% has_lat_lon) %>%
  .$building

namelookup = readr::read_csv("../data/gsalink_name_in_rulefile.csv") %>%
  tibble::as_data_frame() %>%
  {.}

component.group.lookup = readr::read_csv("building_component.csv") %>%
  tibble::as_data_frame() %>%
  dplyr::rename(equipRef=equipRefBackup) %>%
  {.}

start_str = "2018-01-01"
end_str = "2018-12-31"
path = "~/Dropbox/gsa_2017/faultDetectionSkySpark/data-raw/gsalink_weather"
v = "TAVG"

devtools::load_all("../../db.interface")

devtools::load_all("../../get.noaa.weather")

devtools::load_all("../../summarise.and.plot")

## get weather data
for (b in gsalink_buildings[3:56]) {
  print(b)
  years = as.integer(substr(start_str, 1, 4)):(as.integer(substr(end_str, 1, 4)))
  print(years)
  if (!file.exists(sprintf("gsalink_weather/%s_%s.csv", b, years[[1]]))) {
    result = get.noaa.weather::compile_weather_isd_main(useSavedData=FALSE, years=years, building=b, step="hour")
    result %>%
      readr::write_csv(sprintf("gsalink_weather/%s_%s.csv", b, years[[1]]))
  }
}

for (b in gsalink_buildings[13:13]) {
  print(b)
  ## change here when got real occ hour info
  occ_hour_start = 8
  occ_hour_end = 17
  weekdays = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  name = paste0(namelookup[namelookup$building==b,]$name, " ")
  dfrule = readr::read_csv(sprintf("ruleStartEndByBuilding/%s_2018.csv", b))
  print(head(dfrule))
  tz = dfrule[["tz"]][1]
  time.min=as.POSIXct(time.min.str, tz=tz)
  time.max=as.POSIXct(time.max.str, tz=tz)
  print(time.min)
  print(time.max)
  dfleft = tibble::tibble(Timestamp=seq(from=time.min, to=time.max, by="mins"))
  ## dfleft.whole = tibble::tibble(Timestamp=seq(from=min(dfrule$startPosix), to=max(dfrule$endPosix), by="mins"))
  dfweather = readr::read_csv(sprintf("gsalink_weather/%s_2018.csv", b))
  dfweather <- dfweather %>%
    dplyr::mutate(Timestamp=sprintf("%s %s:00:00", date, hour)) %>%
    dplyr::mutate(Timestamp=as.POSIXct(Timestamp, format="%Y%m%d %H:%M:%S", tz="UTC")) %>%
    dplyr::rename(`F`=`wt_temperatureFhour`) %>%
    dplyr::select(Timestamp, F) %>%
    dplyr::filter(Timestamp>=time.min) %>%
    dplyr::filter(Timestamp<=time.max) %>%
    {.}
  attr(dfweather$Timestamp, "tzone") <- tz
  dfenergy = readr::read_csv(sprintf("building_energy/%s_%s.csv", b, energytype))
  dfenergy <- dfenergy %>%
    dplyr::mutate(Timestamp=as.POSIXct(Timestamp, format="%m/%d/%Y %I:%M:%S %p", tz=tz)) %>>%
    dplyr::filter(Timestamp>=time.min) %>%
    dplyr::filter(Timestamp<=time.max) %>%
    ## fill 0 for negative energy value
    dplyr::mutate(!!rlang::sym(energytype) := pmax(!!rlang::sym(energytype), 0)) %>%
    dplyr::mutate(Timestamp=as.POSIXct(round(Timestamp, "hours"), tz=tz)) %>>%
    dplyr::group_by(Timestamp) %>%
    dplyr::summarise(!!rlang::sym(energytype):=sum(!!rlang::sym(energytype))) %>%
    dplyr::ungroup() %>%
    {.}
  dfrule <- dfrule %>%
    dplyr::mutate(equipRef=substr(equipRef, 32, nchar(equipRef))) %>%
    dplyr::mutate(equipRef=gsub(name, "", equipRef)) %>%
    dplyr::left_join(component.group.lookup, by=c("building", "equipRef")) %>%
    dplyr::mutate(count=1) %>%
    dplyr::mutate(eCost=ifelse(is.na(eCost), NA, as.numeric(gsub("$", "", eCost, fixed=TRUE)))) %>%
    ## following groups by component group (group) and rule
    ## tidyr::unite(`groupvar`, `rule`, `group`, sep="----") %>%
    ## following two lines group by rule
    dplyr::mutate(groupvar=rule) %>%
    dplyr::select(-rule, -group) %>%
    {.}
  ## change groupvar to rule if you do not want to group by both rule and group, and only want to group by rule
  dfresult = summarise.and.plot::agg_interval(df=dfrule, start="startPosix", end="endPosix",
                                              group="groupvar", value="count",
                                              time.epsilon=0) %>%
    dplyr::rename(Timestamp=time)
  extra.times = setdiff(dfresult[["Timestamp"]], dfleft[["Timestamp"]])
  ## extra.times = setdiff(dfresult[["Timestamp"]], dfleft.whole[["Timestamp"]])
  if (length(extra.times) != 0L) {
    stop (paste0("The data had unexpected values in the time column; some are: ",
                 paste(head(extra.times), collapse=", ")))
  }
  dfresult <- dfresult %>%
    dplyr::filter(row.kind!="pre-delta") %>%
    tidyr::complete(groupvar, Timestamp=dfleft$Timestamp) %>%
    ## tidyr::complete(groupvar, Timestamp=dfleft.whole$Timestamp) %>%
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
    dplyr::mutate(hour=as.numeric(format(Timestamp, "%H"))) %>%
    dplyr::mutate(day=format(Timestamp, "%A")) %>%
    dplyr::mutate(is.occupied=ifelse((hour <= occ_hour_end) & (hour >= occ_hour_start) & (day %in% weekdays), "Occupied", "Un-occupied")) %>%
    dplyr::left_join(dfresult, by="Timestamp") %>%
    dplyr::left_join(dfweather, by="Timestamp") %>%
    {.}
  df %>%
    dplyr::mutate(`local.time`=format(Timestamp, tz=tz)) %>%
    dplyr::select(Timestamp, local.time, everything()) %>%
    dplyr::rename(`total.duration.minutes`=`value.agg`) %>%
    tidyr::spread(groupvar, `total.duration.minutes`) %>%
    readr::write_csv(sprintf("building_rule_energy_weather/hourly/%s_%s_2018.csv",
                             b, energytype))
}

fitting <- function(method, y, x, x.to.predict) {
  if (nrow(df) > 0) {
    ## this usually return NA's for some coef, meaning there are colinear input features
    if (method == "linear") {
      occ_out = lm(y ~ x)
      print(summary(occ_out))
      fitted.values = fitted.values(occ_out)
      print(head(fitted.values))
      print(sum(fitted.values))
      newdata = x.to.predict %>%
        tibble:::as.tibble()
      predicted.values = predict(occ_out, newdata=newdata)
      print(head(predicted.values))
      print(sum(predicted.values))
      return(list("fitted.values"=fitted.values, "predicted.values"=predicted.values))
    } else if (method == "ridge") {
      lambdas <- 10^seq(3, -2, by = -.1)
      cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas)
      ## print(cv_fit$lambda.min)
      ## plot(cv_fit)
      ## fit <- cv_fit$glmnet.fit
      ## print(summary(fit))
      ## print(tibble::as.tibble(as.matrix(coef(cv_fit, s = "lambda.min"))))
      ## print(coef(cv_fit, s = "lambda.min") %>>%
      ##       {
      ##         tibble::tibble(Covariate=rownames(.),
      ##                        Coefficient=as.vector(as.matrix(.)) %>>%
      ##                          {dplyr::if_else(.==0, NA_real_, .)}
      ##                        )
      ##       }, n=Inf)
      print(coef(cv_fit, s="lambda.min") %>>%
            round(4-ceiling(max(log10(abs(.)))))
            )
      fitted.values = as.vector(predict(cv_fit, newx=x, type="response"))
      predicted.values = as.vector(predict(cv_fit, newx=x.to.predict, type="response"))
      return(list("fitted.values"=fitted.values, "predicted.values"=predicted.values))
    }
  }
}

method = "ridge"
## method = "linear"
r = "AHU Cooling Valve Leaking"
component = "AHU"
gsalink_buildings="OH0192ZZ"
for (b in gsalink_buildings[1:1]) {
  df = readr::read_csv(sprintf("building_rule_energy_weather/%s_%s_2018.csv",
                               b, energytype)) %>%
    tibble::as.tibble() %>%
    {.}
  if (nrow(df) == 0) {
    next
  }
  print(b)
  ## print(head(df))
  df_occ = df %>%
    dplyr::filter(is.occupied == "Occupied") %>%
    dplyr::select(-is.occupied, -hour, -day) %>%
    na.omit() %>%
    {.}
  time = df_occ$Timestamp
  y = df_occ[[energytype]]
  x = df_occ %>%
    dplyr::select(-!!rlang::sym(energytype), -Timestamp) %>%
    as.matrix()
  x.to.predict = df_occ %>%
    dplyr::select(-!!rlang::sym(energytype), -Timestamp) %>%
    dplyr::mutate_at(vars(starts_with(sprintf("%s----%s", r, component))), funs({0})) %>%
    as.matrix() %>%
    {.}
  result = fitting(method, y, x, x.to.predict)
  to.plot =
    tibble(time = time, y=result$fitted.values, type="predicted with rule") %>%
    dplyr::bind_rows(tibble(time = time, y=result$predicted.values, type="predicted without rule")) %>%
    dplyr::bind_rows(tibble(time = time, y=y, type="actual")) %>%
    {.}
  p <- to.plot %>%
    ggplot2::ggplot(ggplot2::aes(x=time, y=y, colour=type)) +
    ggplot2::geom_line() +
    ## ggplot2::geom_point(ggplot2::aes(x=time, y=y, type="actual"), data=tibble(time = time, y=y)) +
    ## ggplot2::ylim(c(0, 5000)) +
    ggplot2::theme()
  print(p)
  ## df_unocc = df %>%
  ##   dplyr::filter(is.occupied == "Un-occupied") %>%
  ##   dplyr::select(-is.occupied) %>%
  ##   {.}
  ## if (nrow(df_occ) > 0) {
  ##   unocc_out = lm(`kWh Del Int` ~ .)
  ## }
}
