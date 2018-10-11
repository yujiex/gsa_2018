#' fitting models and plot fit to predict baseline consumption
#'
#' This function predict the counterfactual electricity KWH or gas cubic foot consumption
#' @param building required, 8 digit building number
#' @param dfPre required, dataframe for pre retrofit energy
#' @param dfPost required, dataframe for post retrofit energy
#' @param colname column to use as the response variable
#' @param plotType elec or gas
#' @param consumptionUnit unit used for the response variable
#' @param method a function to fit the model of y ~ x
#' @param method_label string to label the method name
#' @param action_time retrofit time
#' @param acc_result accumulator data frame for fitting result
#' @keywords fitting energy consumption
#' @export
#' @examples
#' fitting(building=building, dfPre=dfPre, dfPost=dfPost, colname="Electricity_(KWH)", plotType="elec",
#'         consumptionUnit="KWH", method=lean.analysis::loess_fit, action_time=as.character(pre_end),
#'         method_label="loess", acc_result=acc_result)
fitting <- function(building, dfPre, dfPost, colname, plotType, consumptionUnit, method, method_label, action_time, acc_result) {
  ## print(paste(building, colname, plotType, method_label, action_time, sep="--"))
  ylabel = gsub("_", " ", colname)
  plotWidth = 6
  plotHeight = 4
  if (sum(dfPre[[colname]]!=0) < 5) {
    print(sprintf("Too few %s data points", plotType))
    return(acc_result)
  } else {
    x = dfPre$`Monthly Mean Temperature`
    y = dfPre[[colname]]
    modelresult =
      lean.analysis::k_fold_cv_1d(y=y, x=x, method=method)
    le = modelresult$output
    if (is.null(le)) {
      print("final model for pre-retrofit period is null")
      return(acc_result)
    }
    ## print(summary(le))
    cvrmse = modelresult$cvrmse
    ## print(sprintf("cvrmse: %s", cvrmse))
    xseq = seq(from=min(x), to=max(x), length.out=100)
    ## print("xseq---------")
    ## print(head(xseq))
    ## print(class(xseq))
    yseq = predict(le, newdata=data.frame(x=xseq))
    x_post = dfPost$`Monthly Mean Temperature`
    y_post = dfPost[[colname]]
    modelresult_post =
      lean.analysis::k_fold_cv_1d(y=y_post, x=x_post, method=method)
    if (is.null(modelresult_post$output)) {
      print("final model for post-retrofit period is null")
      return(acc_result)
    }
    xseq_post = seq(from=min(x_post), to=max(x_post), length.out=100)
    yseq_post = predict(modelresult_post$output, newdata=data.frame(x=xseq_post))
    dfPre <- dfPre %>%
      dplyr::mutate(`status`="pre") %>%
      {.}
    dfPost <- dfPost %>%
      dplyr::mutate(`status`="post") %>%
      {.}
    ## model pre and post period plot
    p <-
      dplyr::bind_rows(dfPre, dfPost) %>%
      ggplot2::ggplot(ggplot2::aes_string(x="`Monthly Mean Temperature`", y=sprintf("`%s`", colname), colour="status")) +
      ggplot2::geom_point() +
      ggplot2::ylab(ylabel) +
      ggplot2::geom_line(ggplot2::aes(x=x, y=y), data=data.frame(x=xseq, y=yseq, status="pre")) +
      ggplot2::geom_line(ggplot2::aes(x=x, y=y), data=data.frame(x=xseq_post, y=yseq_post, status="post")) +
      ggplot2::ggtitle(sprintf("Model: %s, CVRMSE: %.3f", method_label, cvrmse)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position="bottom")
    print(p)
    ggplot2::ggsave(file=sprintf("roiForECM/data-raw/images/%s_reg_%s_%s_%s.png", building, plotType, method_label, action_time), width=plotWidth, height=plotHeight, units="in")
  }
  result <- dfPost %>%
    dplyr::mutate(`baseline`=predict(le, data.frame(x=`Monthly Mean Temperature`))) %>%
    dplyr::rename(`actual`=!!rlang::sym(colname)) %>%
    dplyr::select(`actual`, `baseline`, `Monthly Mean Temperature`, `Date`) %>%
    na.omit() %>%
    reshape2::melt(id.vars=c("Monthly Mean Temperature", "Date"), variable.name="period", value.name="consumption") %>%
    {.}
  ## print(head(result))
  dfDiff = result %>% dplyr::group_by(period) %>%
    summarise(total=sum(consumption)) %>%
    {.}
  ## print(dfDiff)
  number_of_months = nrow(dfPost)
  actualConsumption = (dfDiff %>% dplyr::filter(period=="actual") %>% dplyr::select(`total`))[[1]]
  baselineConsumption = (dfDiff %>% dplyr::filter(period=="baseline") %>% dplyr::select(`total`))[[1]]
  ## print(sprintf("in %s months, the actual consumption is %s", number_of_months , actualConsumption))
  ## print(sprintf("in %s months, the predicted baseline consumption is %s", number_of_months, baselineConsumption))
  ## saving in the unit of calculation
  saving = (baselineConsumption - actualConsumption) / number_of_months * 12
  savingPercent = (baselineConsumption - actualConsumption) / baselineConsumption * 100
  ## print(sprintf("Saving in %s: %.2f", consumptionUnit, saving))
  ## print(sprintf("Saving Percent in %s: %.2f%%", consumptionUnit, savingPercent))
  ## this part has error band, but not sure how to get models other than loess
  ## p <- dplyr::bind_rows(dfPre, dfPost) %>%
  ##   ggplot2::ggplot(ggplot2::aes_string(y=sprintf("`%s`", colname), x="`Monthly Mean Temperature`",
  ##                                       color="status")) +
  ##   ggplot2::geom_point() +
  ##   ggplot2::geom_smooth(method = "loess") +
  ##   ggplot2::xlab("Monthly average temperature") +
  ##   ggplot2::ylab(ylabel) +
  ##   ggplot2::ggtitle(paste("%s regression fit, ", gsub("_", " ", colname), building)) +
  ##   ggplot2::theme(legend.position="bottom")
  ## print(p)
  ## ggplot2::ggsave(file=sprintf("images/%s_reg_%s_%s.png", building, plotType, action_time), width=plotWidth, height=plotHeight, units="in")
  ## savings plot over time
  p <- result %>%
    ggplot2::ggplot(ggplot2::aes(y=consumption, x=Date, color=period)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::ylab(ylabel) +
    ggplot2::ggtitle(sprintf("%s actual consumption vs predicted baseline (%s)\n%s reduction: %s", building, method_label, consumptionUnit, format(round(saving, 2),big.mark=",",scientific=FALSE))) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="bottom")
  print(p)
  ggplot2::ggsave(file=sprintf("roiForECM/data-raw/images/%s_trend_%s_%s_%s.png", building, plotType, method_label, action_time), width=plotWidth, height=plotHeight, units="in")
  ## savings plot group by month data prepare, plot is going to be done in python
  bymonth =
    result %>%
    dplyr::mutate(`month`=as.numeric(substr(`Date`, 6, 7))) %>%
    dplyr::select(-`Monthly Mean Temperature`, `Date`) %>%
    dplyr::group_by(`month`, `period`) %>%
    dplyr::summarise(`consumption`=mean(`consumption`)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(`period`, `consumption`) %>%
    {.}
  bymonth %>%
    readr::write_csv(sprintf("roiForECM/data-raw/to_python/%s_%s_%s_%s.csv", building, plotType, method_label, action_time))
  acc_result = rbind(acc_result, (data.frame(building=building, cvrmse=cvrmse, saving=saving, savingPercent=savingPercent, plotType=plotType, method_label=method_label, action_time=action_time)))
  return(acc_result)
}
