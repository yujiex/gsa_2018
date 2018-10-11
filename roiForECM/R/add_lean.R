#' Plot lean in roi?
#'
#' This function computes ROI of building ECM retrofits, it needs time, cost, weather and energy
#' @param building required, 8 digit building number
#' @keywords roi
#' @export
#' @examples
#' roiBuilding(building="UT0032ZZ")
add_lean <- function(building) {
  plotWidth = 4
  plotHeight = 4
  margin = 3
  timeframe = analysisStartEnd %>%
    dplyr::filter(`Building_Number`==building) %>%
    {.}
  buildingEUI = eui %>%
    dplyr::filter(`Building_Number`==building) %>%
    dplyr::mutate(`Date`=lubridate::ymd(sprintf("%.0f-%.0f-01", `year`, `month`))) %>%
    {.}
  weather =
    feather::read_feather(sprintf("roiForECM/data-raw/building_TAVG/compiled/%s_TAVG.feather", building)) %>%
    tibble::as_data_frame() %>%
    dplyr::mutate(`year`=lubridate::year(`Date`),
                  `month`=lubridate::month(`Date`)) %>%
    dplyr::group_by(`year`, `month`) %>%
    dplyr::summarise(`Monthly Mean Temperature`=mean(`TAVG`)) %>%
    dplyr::ungroup() %>%
    {.}
  energy_weather = buildingEUI %>%
    inner_join(weather, by=c("year", "month")) %>%
    {.}
  action_print = actionCollapse %>%
    dplyr::filter(`Building_Number`==building) %>%
    {.}
  elec_col = "eui_elec"
  gas_col = "eui_gas"
  plotXLimit = NULL
  plotYLimit = NULL
  plotXLimit = c(min(energy_weather$`Monthly Mean Temperature`) - margin, max(energy_weather$`Monthly Mean Temperature`) + margin)
  plotYLimit = c(-1 - margin, max(energy_weather[[elec_col]], energy_weather[[gas_col]]) + margin)
  ## print("plotXLimit")
  ## print(plotXLimit)
  ## print("plotYLimit")
  ## print(plotYLimit)
  nPhases = nrow(timeframe)
  ## sprintf("number of phases = %d", nPhases)
  ## limits for lean plots
  for (i in 1:nPhases) {
    pre_start = timeframe$`analysis_start`[i]
    pre_end = timeframe$`action_time`[i]
    post_start = timeframe$`action_time`[i]
    post_end = timeframe$`analysis_end`[i]
    ## print(sprintf("Pre-retrofit period: %s ---- %s", as.character(pre_start), as.character(pre_end)))
    ## print(sprintf("Post-retrofit period: %s ---- %s", as.character(post_start), as.character(post_end)))
    print(sprintf("Retrofit time: %s", as.character(action_print$`action_time`[i])))
    cat(action_print$`high_level_collapse`[i])
    dfPre = energy_weather %>%
      dplyr::filter(as.Date(pre_start) <= `Date`, `Date` <= as.Date(pre_end)) %>%
      {.}
    dfPost = energy_weather %>%
      dplyr::filter(post_start <= `Date`, `Date` <= post_end) %>%
      {.}
    p1 = plot_lean_df(df=dfPre, elec_col=elec_col, gas_col=gas_col, building=building, plotXLimit=plotXLimit, plotYLimit=plotYLimit, starttime=pre_start, endtime=pre_end, plotWidth=plotWidth, plotHeight=plotHeight)
    p2 = plot_lean_df(df=dfPost, elec_col=elec_col, gas_col=gas_col, building=building, plotXLimit=plotXLimit, plotYLimit=plotYLimit, starttime=post_start, endtime=post_end, plotWidth=plotWidth, plotHeight=plotHeight)
    p1 <- p1 + ggplot2::ggtitle("pre")
    p2 <- p2 + ggplot2::ggtitle("post")
    gridExtra::grid.arrange(p1, p2, nrow=1)
    ## multiplot(p1, p2, cols=2)
  }
}

plot_lean_df <- function(df, elec_col, gas_col, building, plotXLimit, plotYLimit, starttime, endtime, plotWidth, plotHeight) {
  yElec = df[[elec_col]]
  yGas = df[[gas_col]]
  x = df$`Monthly Mean Temperature`
  resultElec <- lean.analysis::polynomial_deg_2(y=yElec, x=x)
  resultGas <- lean.analysis::polynomial_deg_2(y=yGas, x=x)
  fitted_display =
    lean.analysis::plot_fit(yElec=yElec, yGas=yGas, x=x, resultElec=resultElec,
                            resultGas=resultGas, plotType="base", id=building,
                            methodName="polynomial degree 2", plotXLimit=plotXLimit, plotYLimit=plotYLimit,
                            xLabelPrefix="",
                            plotPoint=TRUE, debugFlag=TRUE)
  ## print(fitted_display$img)
  ggplot2::ggsave(file=sprintf("roiForECM/data-raw/images/lean/%s_%s_%s.png", building, starttime, endtime), width=plotWidth, height=plotHeight, units="in")
  return(fitted_display$img)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
