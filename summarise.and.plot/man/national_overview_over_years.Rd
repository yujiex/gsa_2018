% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{national_overview_over_years}
\alias{national_overview_over_years}
\title{National overview by years}
\usage{
national_overview_over_years(category, type, years, region, pal)
}
\arguments{
\item{category}{optional, a subset of A, B, C, D, E, I to include}

\item{type}{optional, a string (e.g. "Office"), or a string vector (e.g. c("Office", "Courthouse")) of building type}

\item{years}{optional, the years to plot}

\item{region}{optional, the region to plot}

\item{pal}{optional, plotting palette}
}
\description{
This function plots kbtu and cost per sqft by years
}
\examples{
national_overview_over_years(category=c("I", "A"), years=c(2015, 2016, 2017))
}
\keyword{count}
\keyword{query}
