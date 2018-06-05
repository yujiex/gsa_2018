#' Main function
#'
#' This function chains all the steps of lean analysis of one single building, fixme: noaa found a station with empty data for building CA0000SS
#' @param energy, a data frame containing 4 columns, year, month,
#'   eui_elec(electricity kBtu/sqft), eui_gas(gas kBtu/sqft)
#' @param latitude numeric, latitude of the building
#' @param longitude numeric, longitude of the building
#' @param lat_lon_df if latitude and longitude are missing, user needs to input
#'   a dataframe containing three columns: latitude, longitude, Building_Number (an id uniquely identifies the building)
#' @param isd_data optional, returned by rnoaa::isd_stations(refresh = TRUE)
#' @param radius (numeric) Radius (in km) to search from the lat,lon
#'   coordinates, used in isd_station_search
#' @param limit the maximum nearest stations returned for each building
#' @param id optional, a unique identifier for a building, used to join other static data
#' @param plotType optional, "base", "elec", "gas"
#' @param debugFlag optional, flag of debugging, with temp file saved to db_build_temp_csv
#' @keywords lean
#' @export
#' @examples
#' lean_analysis(lat_lon_df, radius=100, limit=5)
lean_analysis <- function (energy, latitude, longitude, lat_lon_df, radius=100, limit=5, id, plotType, debugFlag) {
  ## get the years of data to download
  if (missing(id)) {
    id = "XXXXXXXX"
  }
  if (missing(debugFlag)) {
    debug=FALSE
  }
  years = unique(energy$year)
  ## print("-----------------id----------------")
  ## print(id)
  if (file.exists(sprintf("csv_FY/weather/energy_temp/%s.csv", id))) {
    ## print("load from file")
    df = readr::read_csv(sprintf("csv_FY/weather/energy_temp/%s.csv", id))
  } else {
    print("create file")
    if (missing(latitude)) {
      weather = get.noaa.weather::compile_weather_isd_main(useSavedData=FALSE, years=years, lat_lon_df=lat_lon_df)
    } else {
      weather = get.noaa.weather::compile_weather_isd_main(useSavedData=FALSE, years=years, latitude=latitude, longitude=longitude, building=id)
    }
    print(head(weather))
    weather <- weather %>%
      dplyr::mutate(`year`=as.numeric(`year`), `month`=as.numeric(`month`)) %>%
      {.}
    ## join weather and energy
    if (debugFlag) {
      weather %>%
        readr::write_csv("csv_FY/db_build_temp_csv/weather.csv")
      energy %>%
        readr::write_csv("csv_FY/db_build_temp_csv/energy.csv")
    }
    df = energy %>%
      dplyr::left_join(weather) %>%
      dplyr::filter(!is.na(wt_temperatureFmonth)) %>%
      {.}
    df %>%
      readr::write_csv(sprintf("csv_FY/weather/energy_temp/%s.csv", id))
  }
  ## print("-----------head of df----------------")
  ## print(head(df))
  yElec = df$`eui_elec`
  yGas = df$`eui_gas`
  x = df$`wt_temperatureFmonth`
  ## print("-----------fit electricity ----------------")
  resultElec <- polynomial_deg_2(y=yElec, x=x)
  ## print("-----------fit gas----------------")
  resultGas <- polynomial_deg_2(y=yGas, x=x)
  fitted_display = plot_fit(yElec=yElec, yGas=yGas, x=x, resultElec=resultElec,
           resultGas=resultGas, plotType=plotType, id=id,
           methodName="polynomial degree 2")
  ggplot2::ggsave(file=sprintf("region_report_img/lean/%s_%s.png", plotType, id), width = 2, height=2, units="in")
  return(fitted_display)
}

#' Get energy and temperature data for building
#'
#' This function get energy and temperature of a building into a data frame
#'   eui_elec(electricity kBtu/sqft), eui_gas(gas kBtu/sqft)
#' @param id required, Building ID (Building_Number), used in output filename
#' @param latitude numeric, latitude of the building
#' @param longitude numeric, longitude of the building
#' @param lat_lon_df if latitude and longitude are missing, user needs to input
#'   a dataframe containing three columns: latitude, longitude, Building_Number (an id uniquely identifies the building)
#' @param debugFlag optional, flag of debugging, with temp file saved to db_build_temp_csv
#' @keywords lean
#' @export
#' @examples
#' lean_analysis(lat_lon_df, radius=100, limit=5)
## need to try out
get_energy_weather_file <- function(id, latitude, longitude, lat_lon_df, debugFlag=FALSE) {
  print("create file")
  if (missing(latitude)) {
    weather = get.noaa.weather::compile_weather_isd_main(useSavedData=FALSE, years=years, lat_lon_df=lat_lon_df)
  } else {
    weather = get.noaa.weather::compile_weather_isd_main(useSavedData=FALSE, years=years, latitude=latitude, longitude=longitude, building=id)
  }
  print(head(weather))
  weather <- weather %>%
    dplyr::mutate(`year`=as.numeric(`year`), `month`=as.numeric(`month`)) %>%
    {.}
  ## join weather and energy
  if (debugFlag) {
    weather %>%
      readr::write_csv("csv_FY/db_build_temp_csv/weather.csv")
    energy %>%
      readr::write_csv("csv_FY/db_build_temp_csv/energy.csv")
  }
  df = energy %>%
    dplyr::left_join(weather) %>%
    dplyr::filter(!is.na(wt_temperatureFmonth)) %>%
    {.}
  df %>%
    readr::write_csv(sprintf("csv_FY/weather/energy_temp/%s.csv", id))
  return(df)
}

#' Lean analysis tester
#'
#' This function tests the main lean analysis routine, with inputs from the db
#' @keywords lean test
#' @export
#' @examples
#' test_lean_analysis_db()
test_lean_analysis_db <- function() {
  building="AK0000AA"
  energy = db.interface::read_table_from_db(dbname="all", tablename="EUAS_monthly",
                                            cols=c("Fiscal_Year", "Fiscal_Month", "year", "month", "eui_elec", "eui_gas"), building=building) %>%
    dplyr::arrange(-`Fiscal_Year`, -`Fiscal_Month`) %>%
    head(n=36)
  print("--------head of energy---------")
  print(energy)
  lat_lon_df = db.interface::get_lat_lon_df(building=building)
  print("--------head of lat_lon_df---------")
  print(head(lat_lon_df))
  ## energy = readr::read_csv("csv_FY/db_build_temp_csv/energy.csv")
  ## weather = readr::read_csv("csv_FY/db_build_temp_csv/weather.csv")
  lean_analysis(energy = energy, lat_lon_df = lat_lon_df, id=building)
  ## you can also access the function in this way
  ## lean_analysis(energy = energy, latitude=61.2, longitude=-150, id=building)
}

#' Lean analysis for a subset of building
#'
#' This function tests the main lean analysis routine, with inputs from the db
#' @param region
#' @param buildingType
#' @param year
#' @param plotType "base", "elec", "gas"
#' @param category a vector of "A", "I", etc.
#' @keywords lean test
#' @export
#' @examples
#' test_lean_analysis_db()
plot_lean_subset <- function(region, buildingType, year, plotType, category) {
  buildings = db.interface::get_buildings(region=region, buildingType=buildingType, year=year, category=category)
  counter = 1
  ## buildings <- buildings[(buildings %in% c("CA0000SS", "CA0273ZZ"))]
  ## for (building in buildings[1:1]) {
  ## for (building in buildings[(23+14+11):length(buildings)]) {
  ## for (building in buildings[23+14:length(buildings)]) {
  ## for (building in buildings[23:length(buildings)]) {
  ## for (building in c("CA0000OO")) {
  acc=NULL
  if (file.exists(sprintf("csv_FY/%s_lean_score.csv", plotType))) {
    dfscore = readr::read_csv(sprintf("csv_FY/%s_lean_score.csv", plotType)) %>%
      dplyr::arrange(desc(`score`)) %>%
      head(n=20)
    buildings = dfscore$Building_Number
  }
  for (building in buildings) {
    ## print(sprintf("plot %s %s ---------------", counter, building))
    energy = db.interface::read_table_from_db(dbname="all", tablename="EUAS_monthly",
                                              cols=c("Fiscal_Year", "Fiscal_Month", "year", "month", "eui_elec", "eui_gas"), building=building) %>%
      dplyr::arrange(-`Fiscal_Year`, -`Fiscal_Month`) %>%
      head(n=36)
    ## print("--------head of energy---------")
    ## print(energy)
    lat_lon_df = db.interface::get_lat_lon_df(building=building)
    ## print("--------head of lat_lon_df---------")
    ## print(head(lat_lon_df))
    lean_result = lean_analysis(energy = energy, lat_lon_df = lat_lon_df, id=building, plotType=plotType, debug=TRUE)
    ## print("--------lean result---------")
    ## print(lean_result)
    counter = counter + 1
    acc = rbind(acc, data.frame(Building_Number = building, score=lean_result$score))
  }
  acc %>%
    dplyr::mutate(type = plotType) %>%
    readr::write_csv(sprintf("csv_FY/%s_lean_score_region_%s.csv", plotType, region))
}

#' Lean analysis for a subset of building
#'
#' This function tests the main lean analysis routine, with inputs from the db
#' @param region
#' @param buildingType
#' @param year
#' @param category a vector of "A", "I", etc.
#' @param plotType optional, "elec", "gas"
#' @param method
#' @param lowRange require curves plotted have left end of x range lower than lowRange
#' @param highRange require curves plotted have right end of x range higher than highRange
#' @param debugFlag default to FALSE, set to true to print plot
#' @param plotXLimits x limits for plotting, e.g. c(20, 100)
#' @param plotYLimits y limits for plotting, e.g. c(20, 100)
#' @keywords lean test
#' @export
#' @examples
#' test_lean_analysis_db()
stacked_fit_plot <- function(region, buildingType, year, category, plotType, method, methodLabel, lowRange,
                             highRange, debugFlag=FALSE, plotXLimits, plotYLimits) {
  datafile = sprintf("region_report_img/stack_lean/%s_stack_lean_region_%s_%s.csv", plotType, region, methodLabel)
  imagefile = sprintf("region_report_img/stack_lean/%s_stack_lean_region_%s_%s.png", plotType, region, methodLabel)
  print(datafile)
  if (plotType == "elec") {
    keyword = "electricity"
  } else if (plotType == "gas") {
    keyword = "gas"
  }
  if (file.exists(datafile)) {
    print("load cached result")
    dfData = readr::read_csv(datafile) %>%
      as.data.frame() %>%
      {.}
    if (plotType == "elec") {
      ## select data with large enough temperature range
      if (!missing(lowRange)) {
        buildingInRange <- dfData %>%
          dplyr::group_by(`Building_Number`) %>%
          dplyr::summarise(`low` = min(`xseq`), `high`=max(`xseq`)) %>%
          dplyr::filter(`low` < lowRange) %>%
          {.}
        if(!missing(highRange)) {
          buildingInRange <- buildingInRange  %>%
            dplyr::filter(`high` > highRange) %>%
            {.}
        }
        dfData <- dfData %>%
          dplyr::filter(`Building_Number` %in% buildingInRange$Building_Number) %>%
          {.}
      }
      toDisplay <- dfData %>%
        dplyr::select(`Building_Number`, `highLabel`) %>%
        dplyr::group_by(`Building_Number`, `highLabel`) %>%
        slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(`highLabel`) %>%
        slice(c(1:2,(n()-4):n())) %>%
        {.}
      print(nrow(toDisplay))
    } else if (plotType == "gas") {
      toDisplay <- dfData %>%
        dplyr::select(`Building_Number`, `lowLabel`) %>%
        dplyr::group_by(`Building_Number`, `lowLabel`) %>%
        slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(`lowLabel`) %>%
        slice(c(1:2,(n()-4):n())) %>%
        {.}
    }
    displaySet = unique(toDisplay$Building_Number)
    print("displaySet")
    print(displaySet)
    ## print(sprintf("before filter %s", nrow(dfData)))
    dfData <- dfData %>%
      dplyr::filter(`Building_Number` %in% displaySet)
    ## print(sprintf("before filter %s", nrow(dfData)))
  } else {
    buildings = db.interface::get_buildings(region=region, buildingType=buildingType, year=year, category=category)
    counter = 1
    dfData = NULL
    for (building in buildings) {
      print(sprintf("Fit %s, %s", counter, building))
      if (file.exists(sprintf("csv_FY/weather/energy_temp/%s.csv", building))) {
        df = readr::read_csv(sprintf("csv_FY/weather/energy_temp/%s.csv", building))
      } else {
        lat_lon_df = db.interface::get_lat_lon_df(building=building)
        get_energy_weather_file(id=building, lat_lon_df=lat_lon_df, debugFlag=FALSE)
      }
      if (debugFlag) {
        print(df)
        p1 <- df %>%
          ggplot2::ggplot(ggplot2::aes(x = `wt_temperatureFmonth`, y=`eui_elec`, color=`year`)) +
          ggplot2::geom_point()
        print(p1)
      }
      if (plotType == "elec") {
        y <- df$`eui_elec`
      } else if (plotType == "gas") {
        y <- df$`eui_gas`
      }
      if (sum(y == 0) > 32/36) {
        print(sprintf("too man zero y values for building %s", building))
        next
      }
      x <- df$`wt_temperatureFmonth`
      if (methodLabel == "piecewise") {
        output = method(y, x, h=1)$output
      } else {
        output = method(y, x)$output
      }
      xseq <- seq(from=min(x), to=max(x), length.out=200)
      yseq <- predict(output, newdata = data.frame(x=xseq))
      predLabels <- predict(output, newdata=data.frame(x=c(30, 80)))
      ## print(sprintf("y estimation at 30F: %.1f", predict(output, newdata=data.frame(x=30))))
      ## print(sprintf("y estimation at 80F: %.1f", predict(output, newdata=data.frame(x=80))))
      dfData = rbind(dfData, data.frame(Building_Number = building, xseq=xseq, yseq=yseq,
                                  lowLabel=predLabels[[1]],
                                  highLabel=predLabels[[2]]))
      counter = counter + 1
    }
    dfData %>%
      readr::write_csv(datafile)
  }
  p <- dfData %>%
    ggplot2::ggplot(ggplot2::aes(x=xseq, y=yseq, group=Building_Number, color=Building_Number)) +
    ggplot2::geom_line(size=1) +
    ggplot2::ggtitle(sprintf("%s stacked lean plot for region %s (method: %s)", keyword, region, methodLabel)) +
    ggplot2::xlab("Average Monthly Temperature (F)") +
    ggplot2::ylab(sprintf("%s kBtu/sqft/mo.", keyword)) +
    ggplot2::xlim(c(20, 100)) +
    ggplot2::theme()
  if (plotType == "elec") {
    p <- p +
      ggplot2::geom_text(ggplot2::aes(x=80, y=highLabel, label=sprintf("%.1f", highLabel))) +
      ggplot2::geom_vline(xintercept=80, linetype="dashed")
  } else if (plotType == "gas") {
    p <- p +
      ggplot2::geom_text(ggplot2::aes(x=30, y=lowLabel, label=sprintf("%.1f", lowLabel))) +
      ggplot2::geom_vline(xintercept=30, linetype="dashed")
  }
  print(p)
  ggsave(imagefile, width=8, height=6,
         unit="in")
}
