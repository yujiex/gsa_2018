#'@importFrom magrittr %>%
NULL
# This file contains fitting method implementations of the package, each method
# will output "base load", and fitted values

#' Polynomial degree 2
#'
#' This function fits a polynomial of degree 2: y = b0 + b1x + b2x^2
#' @param y the vector of the response variable
#' @param x the vector of the 1-d covariate variable
#' @keywords polynomial
#' @export
#' @examples
#' polynomial_deg_2(y, x)
polynomial_deg_2 <- function(y, x) {
  if (sum(y == 0) == length(y)) {
    return(list(baseload=0, output=NULL, argmin=min(x), b0=0, b1=0, b2=0))
  }
  output = lm(y ~ x+I(x^2))
  ## print(summary(output))
  coefficients = coef(output)
  b0 = coefficients[[1]]
  b1 = coefficients[[2]]
  b2 = coefficients[[3]]
  axisOfSymmetry = (-1/2) * (b1/b2)
  lowerbound = min(x)
  upperbound = max(x)
  ## print("lowerbound, axisOfSymmetry, upperbound")
  ## print(lowerbound)
  ## print(axisOfSymmetry)
  ## print(upperbound)
  if ((lowerbound <= axisOfSymmetry) && (axisOfSymmetry <= upperbound)) {
    baseload = min(predict(output, newdata = data.frame(x = c(lowerbound, axisOfSymmetry, upperbound))))
    argmin = c(lowerbound, axisOfSymmetry, upperbound)[which.min(predict(output, newdata = data.frame(x = c(lowerbound, axisOfSymmetry, upperbound))))]
  } else {
    baseload = min(predict(output, newdata = data.frame(x = c(lowerbound, upperbound))))
    argmin = c(lowerbound, upperbound)[which.min(predict(output, newdata = data.frame(x=c(lowerbound, upperbound))))]
  }
  baseload = max(baseload, 0)
  ## print(sprintf("base load %s is achieved at %s", baseload, argmin))
  return(list(baseload=baseload, output=output, argmin=argmin, b0=b0, b1=b1, b2=b2))
}

#' Piecewise Linear Regression
#'
#' This function fits a piecewise linear regression
#' @param y the vector of the response variable
#' @param x the vector of the 1-d covariate variable
#' @param h optional, the h in the segmented::seg.control
#' @keywords piecewise linear
#' @export
#' @examples
#' piecewise_linear(y, x)
piecewise_linear <- function(y, x, h) {
  lin.mod = lm(y ~ x)
  ## print("duplicated")
  ## print(duplicated(x))
  ## manually add jitter
  ## x <- x + rnorm(length(x))
  ## print("y:")
  ## print(y)
  ## print("x:")
  ## print(x)
  ## print(lin.mod)
  ## plot(y ~ x)
  ## segmentedFit <- segmented::segmented(lin.mod, seg.Z = ~x, psi=60)
  tryCatch({
    segmentedFit <- segmented::segmented(lin.mod, seg.Z = ~x, psi=median(x), segmented::seg.control(h = h))
    ## plot(segmentedFit, add=TRUE)
    return(list(output=segmentedFit))},
    warning = function(w) {
      print("warning")
      print(w)},
    error = function(e) {
      print(e)
      h <- h * 0.1
      print(sprintf("reducing h to: %s", h))
      piecewise_linear(y, x, h)}
    )
}

#' Plot the fit
#'
#' This function plots the data and the fit line / curve
#' @param y the vector of the response variable
#' @param x the vector of the 1-d covariate variable
#' @param resultElec the result returned by polynomial_deg_2 for electricity
#' @param resultGas the result returned by polynomial_deg_2 for gas
#' @param plotType the type of plots: "base_elec", "base_gas", "elec", "gas"
#' @param id optional, unique identifier of a building, default to "XXXXXXXX"
#' @param methodName optional, the name of the method
#' @param plotXLimit optional, the range of x axis, e.g. c(10, 100)
#' @param plotYLimit optional, the range of y axis, e.g. c(10, 100)
#' @param xLabelPrefix optional, the prefix of x label
#' @keywords polynomial
#' @export
#' @examples
#' plot_fit(y=df$`eui_elect`, x=df$`wt_temperatureFmonth`, output, color="red", methodName=NULL)
plot_fit <- function(yElec, yGas, x, resultElec, resultGas, plotType, id, methodName, plotXLimit=NULL,
                     plotYLimit=NULL, xLabelPrefix="", plotPoint=FALSE) {
  if (missing(id)) {
    id = "XXXXXXXX"
  }
  xseq = seq(from=min(x), to=max(x), length.out=200)
  if (!is.null(resultElec$output)) {
    yElecSeq = predict(resultElec$output, newdata = data.frame(`x` = seq(from=min(x), to=max(x), length.out=200)))
    yElecFitted = fitted.values(resultElec$output)
  } else {
    yElecSeq = 0
    yElecFitted = 0
  }
  if (!is.null(resultGas$output)) {
    yGasSeq = predict(resultGas$output, newdata = data.frame(`x` = seq(from=min(x), to=max(x), length.out=200)))
    yGasFitted = fitted.values(resultGas$output)
  } else {
    yGasSeq = 0
    yGasFitted = 0
  }
  yTotalSeq = yElecSeq + yGasSeq
  gas_line_color = '#C63631'
  gas_mk_color = '#C63631'
  elec_line_color = '#68C5EF'
  total_line_color = '#666666'
  elec_mk_color = '#68C5EF'
  base_gas_color = 'orange'
  base_elec_color = '#F7EE58'
  base_elec_line_color = 'gray'
  base_gas_line_color = '#E47A3A'
  paleAlpha = 0.1
  fullAlpha = 1.0
  data_point_size = 0.5
  alpha_base_gas = paleAlpha
  alpha_base_elec = paleAlpha
  alpha_gas = paleAlpha
  alpha_elec = paleAlpha
  ## font sizes
  fitted_display_size = 4
  theme_text_size = 12
  if (plotType == "base") {
    alpha_base_elec = fullAlpha
    alpha_base_gas = fullAlpha
    fitted_display = sprintf("%.1f", (resultElec$baseload + resultGas$baseload) * 12)
  } else if (plotType == "elec") {
    alpha_elec = fullAlpha
    fitted_display = sprintf("%.1f", (mean(yElecFitted) - resultElec$baseload) * 12)
  } else if (plotType == "gas") {
    alpha_gas = fullAlpha
    fitted_display = sprintf("%.1f", (mean(yGasFitted) - resultGas$baseload) * 12)
  }
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=xseq, y=yElecSeq + resultGas$baseload), colour=elec_line_color) +
    ggplot2::geom_segment(ggplot2::aes(x=min(x), xend=max(x), y=resultElec$baseload, yend=resultElec$baseload), linetype="dashed", color = base_elec_line_color, size=0.5) +
    ggplot2::geom_line(ggplot2::aes(x=xseq, y=yGasSeq + resultElec$baseload), colour=gas_line_color) +
    ggplot2::geom_segment(ggplot2::aes(x=min(x), xend=max(x), y=resultElec$baseload + resultGas$baseload,
                                       yend=resultElec$baseload + resultGas$baseload),
                          linetype="dashed", color = base_gas_line_color, size=0.5) +
    ggplot2::geom_line(ggplot2::aes(x=xseq, y=yTotalSeq), colour=total_line_color) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size=theme_text_size))
  ## fill base load
  p <- p +
    ggplot2::geom_ribbon(ggplot2::aes(x=xseq, ymin=0, ymax=resultElec$baseload), fill=base_elec_color,
                         alpha=alpha_base_elec) +
    ggplot2::geom_ribbon(ggplot2::aes(x=xseq, ymin=resultElec$baseload, ymax=resultElec$baseload + resultGas$baseload),
                         fill=base_gas_color, alpha=alpha_base_gas)
  if (plotPoint) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(x=x, y=yElec), colour=elec_line_color, size=data_point_size) +
      ggplot2::geom_point(ggplot2::aes(x=x, y=yGas + resultElec$baseload), colour=gas_line_color, size=data_point_size)
  }
  lowerElec = 1
  upperElec = length(xseq)
  lowerGas = 1
  upperGas = upperElec
  ## not sure if I need these
  ## if ((resultElec$b2 > 0) && ((min(x) < resultElec$argmin) && (resultElec$argmin < max(x)))) {
  ##   lowerElec = min(which(xseq > resultElec$argmin))
  ## }
  ## if ((resultGas$b2 > 0) && ((min(x) < resultGas$argmin) && (resultGas$argmin < max(x)))) {
  ##   upperGas = max(which(xseq < resultGas$argmin))
  ## }
  ## not sure if I need these end
  ## print(x)
  ## print(xseq[lowerElec:upperElec])
  ## print("ymin")
  ## print(resultElec$baseload + resultGas$baseload)
  ## print("ymax")
  ## print((yElecSeq + resultGas$baseload)[lowerElec:upperElec])
  p <- p +
    ggplot2::geom_ribbon(ggplot2::aes(x=xseq[lowerElec:upperElec], ymin=resultElec$baseload + resultGas$baseload,
                             ymax=(yElecSeq + resultGas$baseload)[lowerElec:upperElec]), fill=elec_mk_color,
                         alpha=alpha_elec) +
    ggplot2::geom_ribbon(ggplot2::aes(x=xseq[lowerGas:upperGas], ymin=resultElec$baseload + resultGas$baseload,
                             ymax=(yGasSeq + resultElec$baseload)[lowerGas:upperGas]), fill=gas_mk_color,
                         alpha=alpha_gas)
  p <- p +
    ggplot2::xlab(paste0(xLabelPrefix, id)) +
    ggplot2::ylab(NULL) +
    ## ggplot2::ylab("kBtu/sqft/mo.") +
    ggplot2::geom_text(ggplot2::aes(x=mean(c(min(x), max(x))), y=0.7*resultElec$baseload, label=fitted_display), size=fitted_display_size)
  if (plotType == "base") {
    label_x_loc = mean(c(min(x), max(x)))
    label_y_loc = 0.7*resultElec$baseload - 2
    ## removed label for base gas load
    ## p <- p +
      ## ggplot2::geom_text(ggplot2::aes(x=label_x_loc, y=label_y_loc,
      ##                                 label=sprintf("base gas: %.2f", resultGas$baseload)), colour=base_gas_color, size=3)
  }
  if (!is.null(plotXLimit)) {
    p <- p +
      ggplot2::xlim(plotXLimit)
  }
  if (!is.null(plotYLimit)) {
    p <- p +
      ggplot2::ylim(plotYLimit)
  }
  print(p)
  return(list(img = p, score=fitted_display, xrange_left=min(x), xrange_right=max(x), yrange_top=max(yTotalSeq)))
}

#' Testing fitting methods
#'
#' This function tests various fitting methods implemented in this file
#' @keywords test fit
#' @export
#' @examples
#' polynomial_deg_2(y, x)
test_fit <- function() {
  print(getwd())
  id = "AK0000AA"
  df = readr::read_csv("csv_FY/db_build_temp_csv/df.csv")
  yElec = df$`eui_elec`
  yGas = df$`eui_gas`
  x = df$`wt_temperatureFmonth`
  resultElec <- polynomial_deg_2(y=yElec, x=x)
  resultGas <- polynomial_deg_2(y=yGas, x=x)
  plot_fit(yElec=yElec, yGas=yGas, x=x, resultElec=resultElec, resultGas=resultGas, plotType="base",
           id=id, methodName="polynomial degree 2")
}
