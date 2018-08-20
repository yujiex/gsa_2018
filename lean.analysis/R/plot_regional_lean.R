#' plot regional report individual yellow blue red plots
#'
#' This function get energy and temperature of a building into a data frame
#'   eui_elec(electricity kBtu/sqft), eui_gas(gas kBtu/sqft)
#' @param region required, region to plot
#' @param suffix optional, suffix to attach to image file names, choose between
#' @param elec_col optional, column for plotting the blue part, choose among:
#'   eui_elec, eui_elec_source, eui_cooling, eui_cooling_source
#' @param gas_col optional, column for plotting the red part choose among:
#'   eui_gas, eui_gas_source, eui_heating, eui_heating_source
#' @param presuffix optional, tail identifier of data source (source_heating_cooling, source_electric_gas)
#' @keywords lean report image tex
#' @export
#' @examples
#' plot_regional (region=9, suffix="source_electric_gas", elec_col="eui_elec_source", gas_col="eui_gas_source")
plot_regional <- function (region, suffix="source_electric_gas", elec_col="eui_elec_source", gas_col="eui_gas_source") {
  region=region
  suffix = suffix
  print(suffix)
  print(elec_col)
  print(gas_col)
  range_file = sprintf("~/Dropbox/gsa_2017/csv_FY/base_lean_score_region_%s_%s.csv", region, suffix)
  if (file.exists(range_file)) {
    dfrange = readr::read_csv(range_file)
    xlimits = c(min(dfrange$`xrange_left`), max(dfrange$`xrange_right`))
    ylimits = c(-1, max(dfrange$`yrange_top`))
  } else {
    xlimits = NULL
    ylimits = NULL
  }
  print("xlimits")
  print(xlimits)
  print("ylimits")
  print(ylimits)
  plot_lean_subset(region=region, buildingType="Office", year=2017, plotType="base", category=c("I", "A"), plotXLimit=xlimits, plotYLimit=ylimits, elec_col=elec_col, gas_col=gas_col, suffix=suffix)
  range_file = sprintf("~/Dropbox/gsa_2017/csv_FY/base_lean_score_region_%s_%s.csv", region, suffix)
  if (file.exists(range_file)) {
    dfrange = readr::read_csv(range_file)
    xlimits = c(min(dfrange$`xrange_left`), max(dfrange$`xrange_right`))
    ylimits = c(-1, max(dfrange$`yrange_top`))
  } else {
    xlimits = NULL
    ylimits = NULL
  }
  print("xlimits")
  print(xlimits)
  print("ylimits")
  print(ylimits)
  ## rerun to get the right global scale
  plot_lean_subset(region=region, buildingType="Office", year=2017, plotType="base", category=c("I", "A"), plotXLimit=xlimits, plotYLimit=ylimits, elec_col=elec_col, gas_col=gas_col, suffix=suffix)
  ## plot_lean_subset(region=region, buildingType="Office", year=2017, plotType="gas", category=c("I", "A"), plotXLimit=xlimits, plotYLimit=ylimits, elec_col=elec_col, gas_col=gas_col, suffix=suffix)
  plot_lean_subset(region=region, buildingType="Office", year=2017, plotType="elec", category=c("I", "A"), plotXLimit=xlimits, plotYLimit=ylimits, elec_col=elec_col, gas_col=gas_col, suffix=suffix)
  presuffix = sprintf("_%s", suffix)
  if (region == 9) {
    generate_lean_tex(plotType="base", region=region, topn=8, botn=4, category="I", presuffix=presuffix)
    generate_lean_tex(plotType="base", region=region, topn=4, botn=4, category="A", presuffix=presuffix)
    generate_lean_tex(plotType="gas", region=region, topn=20, botn=0, presuffix=presuffix)
    generate_lean_tex(plotType="elec", region=region, topn=20, botn=0, presuffix=presuffix)
  } else {
    generate_lean_tex(plotType="base", region=region, topn=20, botn=0, presuffix=presuffix)
    generate_lean_tex(plotType="gas", region=region, topn=20, botn=0, presuffix=presuffix)
    generate_lean_tex(plotType="elec", region=region, topn=20, botn=0, presuffix=presuffix)
  }
}
