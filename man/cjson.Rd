% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{cjson}
\alias{cjson}
\title{Combine JSON elements}
\usage{
cjson(...)
}
\arguments{
\item{...}{Either several JSON items or one list of JSON items}
}
\value{
A JSON array
}
\description{
Combine JSON elements
}
\examples{
a <- crossref2cp(cr_journal_article)
b <- crossref2cp(cr_journal_article)
cjson(a, b)

cjson(list(a, b))
}
