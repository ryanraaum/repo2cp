% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openalex.R
\name{openalex2cp}
\alias{openalex2cp}
\title{Convert openalex json to citeproc csl-data}
\usage{
openalex2cp(this_json, format = "citeproc-json")
}
\arguments{
\item{this_json}{The json data from openalex.}

\item{format}{The format for return of the processed data.}
}
\value{
A object of the selected return \code{format}.
}
\description{
Convert openalex json to citeproc csl-data
}
\examples{
\dontrun{
these_results <- oa_query(entity = "works", doi = "10.1371/journal.pcbi.1009061") |>
                  oa_request()
this_result <- these_results[[1]]

openalex2cp(this_result)
}
}
