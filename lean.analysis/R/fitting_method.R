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
    return(list(baseload=0, output=NULL, argmin=min(x), b0=0, b1=0, b2=0, cvrmse=NA))
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
  y_hat = fitted.values(output)
  cvrmse = CVRMSE(y=y, y_hat=y_hat, n_par=3)
  ## print(sprintf("base load %s is achieved at %s", baseload, argmin))
  return(list(baseload=baseload, output=output, argmin=argmin, b0=b0, b1=b1, b2=b2, cvrmse=cvrmse))
}

#' local polynomial regression
#'
#' This function fits a local polynomial regression model
#' @param y the vector of the response variable
#' @param x the vector of the 1-d covariate variable
#' @keywords local polynomial
#' @export
#' @examples
#' loess_fit(y, x)
loess_fit <- function(y, x) {
  loess_result = loess(y ~ x)
  ## didn't compute cvrmse, since don't know number of parameters
  return(list(output=loess_result, cvrmse=NA))
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
piecewise_linear <- function(y, x, h=1) {
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
    if (h < 1e-50) {
      print("fail to fit the segmented model")
      return(list(output=NULL, cvrmse=NA))
    }
    segmentedFit <- segmented::segmented(lin.mod, seg.Z = ~x, psi=median(x), segmented::seg.control(h = h))
    y_hat = (segmented::broken.line(segmentedFit,link=FALSE)$fit)
    cvrmse = CVRMSE(y=y, y_hat=y_hat, n_par=4)
    plot(y ~ x)
    plot(segmentedFit,add=TRUE,link=FALSE,lwd=2,col=2:3, lty=c(1,3))
    lines(segmentedFit,col=2,pch=19,bottom=FALSE,lwd=2)
    points(segmentedFit,col=4, link=FALSE)
    return(list(output=segmentedFit, cvrmse=cvrmse))},
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

#' Return CVRMSE of the estimate
#'
#' This function computes the cvrmse of the estimate
#' @param y the vector of the response variable
#' @param y_hat the vector of fitted values
#' @param n_par number of parameters in the model, optional, if not supplied the n - n_par is removed
CVRMSE <- function(y, y_hat, n_par) {
  n = length(y)
  if (!missing(n_par)) {
    return(sqrt(sum((y_hat - y) ^ 2) / (n - n_par))/mean(y))
  } else {
    return(sqrt(sum((y - y_hat)^2)) / n / mean(y))
  }
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
#' @param plotPoint optional, whether to show the data points
#' @param plotTitle optional, whether to show the plot title, with cvrmse
#' @param debugFlag optional, if set, save data to data frame
#' @keywords polynomial
#' @export
#' @examples
#' plot_fit(y=df$`eui_elect`, x=df$`wt_temperatureFmonth`, output, color="red", methodName=NULL)
plot_fit <- function(yElec, yGas, x, resultElec, resultGas, plotType, id, methodName, plotXLimit=NULL,
                     plotYLimit=NULL, xLabelPrefix="", plotPoint=FALSE, plotTitle=FALSE, debugFlag=FALSE,
                     titleText="") {
  if (missing(id)) {
    id = "XXXXXXXX"
  }
  xseq = seq(from=min(x), to=max(x), length.out=200)
  if (!is.null(resultElec$output)) {
    yElecSeq = predict(resultElec$output, newdata = data.frame(`x` = seq(from=min(x), to=max(x), length.out=200)))
    yElecFitted = fitted.values(resultElec$output)
    elec_cvrmse = resultElec$cvrmse
  } else {
    yElecSeq = 0
    yElecFitted = 0
    elec_cvrmse = NA
  }
  if (!is.null(resultGas$output)) {
    yGasSeq = predict(resultGas$output, newdata = data.frame(`x` = seq(from=min(x), to=max(x), length.out=200)))
    yGasFitted = fitted.values(resultGas$output)
    gas_cvrmse = resultGas$cvrmse
  } else {
    yGasSeq = 0
    yGasFitted = 0
    gas_cvrmse = NA
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
  elec_heating_mk_color = '#EFD743'
  paleAlpha = 0.1
  fullAlpha = 1.0
  data_point_size = 0.5
  alpha_base_gas = paleAlpha
  alpha_base_elec = paleAlpha
  alpha_gas = paleAlpha
  alpha_elec = paleAlpha
  alpha_elec_heating = paleAlpha
  ## font sizes
  fitted_display_size = 4
  theme_text_size = 12
  title_font_size = 8
  title_font_family = "ActivGrotesk"
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
  ## deciding boundaries for fill color and stacking gas and electricity heating
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
  lowerElec = 1
  upperElec = length(xseq)
  lowerGas = 1
  upperGas = upperElec
  yElecHeating = 0
  yElecHeatingFitted = rep(0, 36)
  ## not sure if I need these
  ## print(min(x))
  ## print(resultElec$argmin)
  ## print(max(x))
  if ((resultElec$b2 > 0) && ((min(x) < resultElec$argmin) && (resultElec$argmin < max(x)))) {
    lowerElec = min(which(xseq > resultElec$argmin))
    yElecHeating = c((yElecSeq - resultElec$baseload)[1:lowerElec],
                     rep(0, upperElec - lowerElec))
    yElecHeatingFitted = yElecFitted[which(x < yElecSeq[lowerElec])]
    yElecHeatingFitted = c(yElecHeatingFitted, rep(0, 36 - length(yElecHeatingFitted)))
  }
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
  if (plotType == "base") {
    alpha_base_elec = fullAlpha
    alpha_base_gas = fullAlpha
    alpha_elec_heating = fullAlpha
    fitted_display = sprintf("%.1f", (resultElec$baseload + resultGas$baseload) * 12)
  } else if (plotType == "elec") {
    alpha_elec = fullAlpha
    ## fitted_display = sprintf("%.1f", (mean(yElecFitted) - resultElec$baseload) * 12)
    ## only count for the "cooling" part of the curve
    print(sprintf("lowerElec: %s", lowerElec))
    print(sprintf("yElecSeq[lowerElec]: %s", yElecSeq[lowerElec]))
    print(sprintf("xseq[lowerElec]: %s", xseq[lowerElec]))
    fitted_display = sprintf("%.1f", (mean(yElecFitted[which(x >= xseq[lowerElec])]) - resultElec$baseload) * 12)
  } else if (plotType == "gas") {
    alpha_gas = fullAlpha
    alpha_elec_heating = fullAlpha
    ## fitted_display = sprintf("%.1f", (mean(yGasFitted) - resultGas$baseload) * 12)
    ## only count for the "heating" part of the curve
    fitted_display = sprintf("%.1f", (mean(yGasFitted + yElecHeatingFitted) - resultGas$baseload) * 12)
    print("length of the two")
    print(length(yGasFitted))
    print(length(yElecHeatingFitted))
  }
  ## fill base load
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(ggplot2::aes(x=xseq, ymin=0, ymax=resultElec$baseload), fill=base_elec_color,
                         alpha=alpha_base_elec) +
    ggplot2::geom_ribbon(ggplot2::aes(x=xseq, ymin=resultElec$baseload, ymax=resultElec$baseload + resultGas$baseload),
                         fill=base_gas_color, alpha=alpha_base_gas)
  data.frame("yElecSeq"=yElecSeq, "yGasSeq"=yGasSeq, "yElecHeating"=yElecHeating, "base_gas"=resultGas$baseload,
             "base_elec"=resultElec$baseload) %>%
    readr::write_csv("~/Dropbox/gsa_2017/temp/fitting.csv")
  p <- p +
  ## p <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=xseq, y=yElecSeq + resultGas$baseload), colour=elec_line_color) +
    ggplot2::geom_segment(ggplot2::aes(x=min(x), xend=max(x), y=resultElec$baseload, yend=resultElec$baseload), linetype="dashed", color = base_elec_line_color, size=0.5) +
    ggplot2::geom_line(ggplot2::aes(x=xseq, y=yGasSeq + resultElec$baseload + yElecHeating), colour=gas_line_color) +
    ggplot2::geom_segment(ggplot2::aes(x=min(x), xend=max(x), y=resultElec$baseload + resultGas$baseload,
                                       yend=resultElec$baseload + resultGas$baseload),
                          linetype="dashed", color = base_gas_line_color, size=0.5) +
    ggplot2::geom_line(ggplot2::aes(x=xseq, y=yTotalSeq), colour=total_line_color)
  if (plotTitle) {
    p <- p +
      ## ggplot2::ggtitle(sprintf("cvrmse: blue (%.2f), red (%.2f)", elec_cvrmse, gas_cvrmse))
      ggplot2::ggtitle(paste0(xLabelPrefix, id))
  }
  p <- p +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size=theme_text_size),
                   plot.title = ggplot2::element_text(size=title_font_size, family = title_font_family))
  if (plotPoint) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(x=x, y=yElec + resultGas$baseload), colour=elec_line_color, size=data_point_size) +
      ggplot2::geom_point(ggplot2::aes(x=x, y=yGas + resultElec$baseload), colour=gas_line_color, size=data_point_size)
  }
  p <- p +
    ggplot2::geom_ribbon(ggplot2::aes(x=xseq[1:lowerElec], ymin=resultElec$baseload + resultGas$baseload,
                                      ymax=(resultElec$baseload + resultGas$baseload + yElecHeating)[1:lowerElec]),
                         fill=elec_heating_mk_color,
                         alpha=alpha_elec_heating) +
    ## ggplot2::geom_ribbon(ggplot2::aes(x=xseq[1:lowerElec], ymin=resultElec$baseload + resultGas$baseload,
    ##                                   ymax=(yElecSeq + resultGas$baseload)[1:lowerElec]), fill=base_elec_color,
    ##                      alpha=alpha_base_elec) +
    ggplot2::geom_ribbon(ggplot2::aes(x=xseq[lowerElec:upperElec], ymin=resultElec$baseload + resultGas$baseload,
                             ymax=(yElecSeq + resultGas$baseload)[lowerElec:upperElec]), fill=elec_mk_color,
                         alpha=alpha_elec) +
    ggplot2::geom_ribbon(ggplot2::aes(x=xseq[lowerGas:upperGas],
                                      ymin=resultElec$baseload + resultGas$baseload + yElecHeating,
                             ymax=(yGasSeq + resultElec$baseload + yElecHeating)[lowerGas:upperGas]), fill=gas_mk_color,
                         alpha=alpha_gas)
  p <- p +
    ## uncomment to plot building ID
    ## ggplot2::xlab(paste0(xLabelPrefix, id)) +
    ggplot2::xlab(NULL) +
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
  ## print(p)
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

#' K fold cross validation with 1d input feature x
#'
#' This function computes the k fold cross validation for y and x being one
#' dimensional, for now it only estimate the error. Later will implement
#' parameter tuning
#' @param y the vector of the response variable
#' @param x the vector of the 1-d covariate variable
#' @keywords cross validation
#' @export
#' @examples
#' k_fold_cv_1d(y=y, x=x, method=lean.analysis::loess_fit)
k_fold_cv_1d <- function(y, x, method, kfold=5) {
  set.seed(0)
  n = length(y)
  ## print(sprintf("n = %d", n))
  rand_idx = sample(n)
  ## random shuffle
  y <- y[rand_idx]
  x <- x[rand_idx]
  cvrmses = NULL
  for (i in 1:kfold) {
    all_idx = 1:n
    train_idx = all_idx[all_idx %% kfold!=(i - 1)]
    test_idx = all_idx[all_idx %% kfold==(i - 1)]
    ## print("train_idx: ")
    ## print(train_idx)
    ## print("test_idx")
    ## print(test_idx)
    fit_fold = method(y=y[train_idx], x=x[train_idx])$output
    if (is.null(fit_fold)) {
      next
    }
    ## print(summary(fit_fold))
    ## print("x[test_idx]")
    ## print(x[test_idx])
    y_test_hat = predict(fit_fold, newdata=data.frame(x=x[test_idx]))
    y_test = y[test_idx]
    ## could add in other error metrics
    cvrmse = lean.analysis::CVRMSE(y_test, y_test_hat)
    ## print(sprintf("cvrmse for iteration %d: %.2f", i, cvrmse))
    cvrmses = c(cvrmses, cvrmse)
  }
  cvrmses = cvrmses[!is.na(cvrmses)]
  ## finalModel = loess(y ~ x)
  finalModel = method(y, x)$output
  return(list(output = finalModel, cvrmse = mean(cvrmses)))
}
