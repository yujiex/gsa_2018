% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_process.R
\name{generate_building_by_building_cmp}
\alias{generate_building_by_building_cmp}
\title{Generate side by side lean plot comparison}
\usage{
generate_building_by_building_cmp()
}
\arguments{
\item{df}{required, a data frame with the image identifiers, and a column
named "score"}
}
\description{
This function generates a tex file to be included to display two sets of
plots in two columns. One set of plots have suffixes
"*source_electric_gas.png", in folder "lean/", the other set of plots ends
with "*source_heating_cooling", the rows are aligned by base_xxxxxxxx, where
"xxxxxxxx" is the building ID. When a building only has one plot, fill the
other missing one with a dummy blank plot
}
\examples{
polynomial_deg_2(y, x)
}
\keyword{image}
\keyword{tex}
\keyword{to}
