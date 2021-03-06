% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{stackbar}
\alias{stackbar}
\title{Stacked bar plot}
\usage{
stackbar(df, xcol, fillcol, ycol, orderByHeight, ylabel, xlabel, tit, legendloc,
  legendOrient, pal, pal_values, labelFormat, width, verbose, scaler,
  facetvar = NULL, facetNcol = NULL, labelCutoff = NULL,
  manual_legend_order = ggplot2::waiver(), showLabel = TRUE)
}
\arguments{
\item{df}{required, a dataframe to plot}

\item{xcol}{the horizontal variable}

\item{fillcol}{the fill column}

\item{ycol}{the y column}

\item{ylabel}{required, label for y axis}

\item{xlabel}{required, label for x axis}

\item{tit}{plot title}

\item{legendloc}{legend location: top, bottom, c(0.8, 0.2)}

\item{legendOrient}{legend orientation: "v" or "h"}

\item{pal}{color palette}

\item{pal_values}{colors in the palette to use}

\item{labelFormat}{optional, string format function of label}

\item{width}{optional, the width of the bars}

\item{verbose}{if TRUE print data to the output}

\item{scaler}{optional, if supplied, scale the input}

\item{facetNcol}{optional, number of columns in facet plot}

\item{labelCutoff}{optional, upper bound for label to be visible}

\item{manual_legend_order}{optional, manual order of legend}

\item{showLabel}{optional, whether to show count on top and in each section of stacked bar}

\item{aggfun}{count or sum}
}
\description{
This function produces a stacked bar plot, with labels for each stacked
segment and also the height of the bar. The height labeling part is copied
from: https://gist.github.com/svigneau/05148a7031172c2bc70d
}
\examples{
df = tibble::tibble(Fiscal_Year=2013L, Cat=c("A","B","B"))
stackbar(df=df, xcol="Fiscal_Year", fillcol="Cat", ylabel="Building Count", tit="EUAS Building Count By Category", orderByHeight=TRUE, xlabel="XLABEL", legendloc="bottom", legendOrient="horizontal")
stackbar(df=df, xcol="Fiscal_Year", fillcol="Cat", ylabel="Building Count", tit="EUAS Building Count By Category", orderByHeight=TRUE, xlabel="XLABEL", legendloc="bottom", legendOrient="horizontal", manual_legend_order=c("B","A"))
}
\keyword{count}
\keyword{query}
