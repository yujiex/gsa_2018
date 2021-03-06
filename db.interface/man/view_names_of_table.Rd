% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_db.R
\name{view_names_of_table}
\alias{view_names_of_table}
\title{Print the head of a table "tablename" in a sqlite database "dbname"}
\usage{
view_names_of_table(dbname, tablename, path)
}
\arguments{
\item{dbname}{required, the name string of the database, e.g. "all.db" has name "all"}

\item{tablename}{required, the name string of the table to view}

\item{path}{optional, the path to .db file, default is "csv_FY/db/"}
}
\description{
This function returns the column names of a table in a sqlite database
}
\examples{
view_names_of_table(dbname="all", tablename="EUAS_type")
}
\keyword{names}
\keyword{sqlite}
\keyword{table}
\keyword{view}
