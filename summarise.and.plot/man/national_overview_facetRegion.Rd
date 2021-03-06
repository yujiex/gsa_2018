% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{national_overview_facetRegion}
\alias{national_overview_facetRegion}
\title{National overview comparing 2 years by region: cnt, eui, sqft}
\usage{
national_overview_facetRegion(category, type, years, region)
}
\arguments{
\item{category}{optional, a subset of A, B, C, D, E, I to include}

\item{type}{optional, a string (e.g. "Office"), or a string vector (e.g. c("Office", "Courthouse")) of building type}

\item{region}{optional, the region to plot}

\item{year}{optional, the year to plot}
}
\description{
This function groups the national level plots, may decide on a filter
}
\examples{
national_overview(category=c("A", "C", "I"), year=2017)
}
\keyword{count}
\keyword{query}
